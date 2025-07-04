//! I/O operations with effect system integration

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use fluentai_core::ast::EffectType;
use anyhow::{anyhow, Result};

/// Register all I/O functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // File operations
        StdlibFunction::effectful(
            "file-read",
            file_read,
            1,
            Some(1),
            vec![EffectType::IO],
            "Read entire file contents"
        ),
        StdlibFunction::effectful(
            "file-write",
            file_write,
            2,
            Some(2),
            vec![EffectType::IO],
            "Write content to file"
        ),
        StdlibFunction::effectful(
            "file-append",
            file_append,
            2,
            Some(2),
            vec![EffectType::IO],
            "Append content to file"
        ),
        StdlibFunction::effectful(
            "file-delete",
            file_delete,
            1,
            Some(1),
            vec![EffectType::IO],
            "Delete a file"
        ),
        StdlibFunction::effectful(
            "file-exists?",
            file_exists,
            1,
            Some(1),
            vec![EffectType::IO],
            "Check if file exists"
        ),
        
        // Directory operations
        StdlibFunction::effectful(
            "dir-list",
            dir_list,
            1,
            Some(1),
            vec![EffectType::IO],
            "List directory contents"
        ),
        StdlibFunction::effectful(
            "dir-create",
            dir_create,
            1,
            Some(1),
            vec![EffectType::IO],
            "Create a directory"
        ),
        StdlibFunction::effectful(
            "current-directory",
            current_directory,
            0,
            Some(0),
            vec![EffectType::IO],
            "Get current working directory"
        ),
        
        // Console I/O
        StdlibFunction::effectful(
            "read-line",
            read_line,
            0,
            Some(0),
            vec![EffectType::IO],
            "Read a line from stdin"
        ),
        StdlibFunction::effectful(
            "print-line",
            print_line,
            1,
            None,
            vec![EffectType::IO],
            "Print values followed by newline"
        ),
        StdlibFunction::effectful(
            "print",
            print,
            1,
            None,
            vec![EffectType::IO],
            "Print values without newline"
        ),
        
        // JSON operations
        StdlibFunction::pure("json-parse", json_parse, 1, Some(1), "Parse JSON string"),
        StdlibFunction::pure("json-stringify", json_stringify, 1, Some(1), "Convert value to JSON string"),
    ]);
}

// File operations
// Note: These are placeholder implementations that would need to integrate
// with the effect system for proper sandboxing and async support

fn file_read(args: &[Value]) -> Result<Value> {
    crate::io_effects::file_read_with_effects(args)
}

fn file_write(args: &[Value]) -> Result<Value> {
    crate::io_effects::file_write_with_effects(args)
}

fn file_append(args: &[Value]) -> Result<Value> {
    crate::io_effects::file_append_with_effects(args)
}

fn file_delete(args: &[Value]) -> Result<Value> {
    crate::io_effects::file_delete_with_effects(args)
}

fn file_exists(args: &[Value]) -> Result<Value> {
    crate::io_effects::file_exists_with_effects(args)
}

// Directory operations

fn dir_list(args: &[Value]) -> Result<Value> {
    crate::io_effects::dir_list_with_effects(args)
}

fn dir_create(args: &[Value]) -> Result<Value> {
    crate::io_effects::dir_create_with_effects(args)
}

fn current_directory(args: &[Value]) -> Result<Value> {
    crate::io_effects::current_directory_with_effects(args)
}

// Console I/O

fn read_line(args: &[Value]) -> Result<Value> {
    crate::io_effects::read_line_with_effects(args)
}

fn print_line(args: &[Value]) -> Result<Value> {
    crate::io_effects::print_line_with_effects(args)
}

fn print(args: &[Value]) -> Result<Value> {
    crate::io_effects::print_with_effects(args)
}

// JSON operations

fn json_parse(args: &[Value]) -> Result<Value> {
    let json_str = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("json-parse: expected string")),
    };
    
    // Parse JSON string to serde_json::Value
    let json_value: serde_json::Value = serde_json::from_str(json_str)
        .map_err(|e| anyhow!("json-parse: {}", e))?;
    
    // Convert serde_json::Value to FluentAi Value
    json_to_value(&json_value)
}

fn json_stringify(args: &[Value]) -> Result<Value> {
    let json_value = value_to_json(&args[0])?;
    let json_str = serde_json::to_string(&json_value)
        .map_err(|e| anyhow!("json-stringify: {}", e))?;
    Ok(Value::String(json_str))
}

// Helper functions for JSON conversion

fn json_to_value(json: &serde_json::Value) -> Result<Value> {
    match json {
        serde_json::Value::Null => Ok(Value::Nil),
        serde_json::Value::Bool(b) => Ok(Value::Bool(*b)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Value::Int(i))
            } else if let Some(f) = n.as_f64() {
                Ok(Value::Float(f))
            } else {
                Err(anyhow!("json-parse: number out of range"))
            }
        }
        serde_json::Value::String(s) => Ok(Value::String(s.clone())),
        serde_json::Value::Array(arr) => {
            let mut values = Vec::new();
            for item in arr {
                values.push(json_to_value(item)?);
            }
            Ok(Value::List(values))
        }
        serde_json::Value::Object(obj) => {
            let mut map = rustc_hash::FxHashMap::default();
            for (key, val) in obj {
                map.insert(key.clone(), json_to_value(val)?);
            }
            Ok(Value::Map(map))
        }
    }
}

fn value_to_json(value: &Value) -> Result<serde_json::Value> {
    match value {
        Value::Nil => Ok(serde_json::Value::Null),
        Value::Bool(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Int(i) => Ok(serde_json::json!(*i)),
        Value::Float(f) => Ok(serde_json::json!(*f)),
        Value::String(s) => Ok(serde_json::Value::String(s.clone())),
        Value::List(items) => {
            let mut arr = Vec::new();
            for item in items {
                arr.push(value_to_json(item)?);
            }
            Ok(serde_json::Value::Array(arr))
        }
        Value::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (key, val) in map {
                obj.insert(key.clone(), value_to_json(val)?);
            }
            Ok(serde_json::Value::Object(obj))
        }
        _ => Err(anyhow!("json-stringify: cannot serialize functions, promises, or channels")),
    }
}