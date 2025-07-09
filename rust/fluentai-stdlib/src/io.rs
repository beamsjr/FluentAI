//! I/O operations with effect system integration

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use crate::vm_bridge::StdlibContext;
use fluentai_core::ast::EffectType;
use anyhow::{anyhow, Result};

/// Helper to perform IO effects through the effect context
fn perform_io_effect(context: &mut StdlibContext, operation: &str, args: &[Value]) -> Result<Value> {
    let effect_context = context.effect_context();
    effect_context.perform_sync(EffectType::IO, operation, args)
        .map_err(|e| anyhow!("IO effect error: {}", e))
}

/// Register all I/O functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // File operations
        StdlibFunction::effectful_with_context(
            "file-read",
            file_read_ctx,
            1,
            Some(1),
            vec![EffectType::IO],
            "Read entire file contents"
        ),
        StdlibFunction::effectful_with_context(
            "file-write",
            file_write_ctx,
            2,
            Some(2),
            vec![EffectType::IO],
            "Write content to file"
        ),
        StdlibFunction::effectful_with_context(
            "file-append",
            file_append_ctx,
            2,
            Some(2),
            vec![EffectType::IO],
            "Append content to file"
        ),
        StdlibFunction::effectful_with_context(
            "file-delete",
            file_delete_ctx,
            1,
            Some(1),
            vec![EffectType::IO],
            "Delete a file"
        ),
        StdlibFunction::effectful_with_context(
            "file-exists?",
            file_exists_ctx,
            1,
            Some(1),
            vec![EffectType::IO],
            "Check if file exists"
        ),
        
        // Directory operations
        StdlibFunction::effectful_with_context(
            "dir-list",
            dir_list_ctx,
            1,
            Some(1),
            vec![EffectType::IO],
            "List directory contents"
        ),
        StdlibFunction::effectful_with_context(
            "dir-create",
            dir_create_ctx,
            1,
            Some(1),
            vec![EffectType::IO],
            "Create a directory"
        ),
        StdlibFunction::effectful_with_context(
            "current-directory",
            current_directory_ctx,
            0,
            Some(0),
            vec![EffectType::IO],
            "Get current working directory"
        ),
        
        // Console I/O
        StdlibFunction::effectful_with_context(
            "read-line",
            read_line_ctx,
            0,
            Some(0),
            vec![EffectType::IO],
            "Read a line from stdin"
        ),
        StdlibFunction::effectful_with_context(
            "print-line",
            print_line_ctx,
            1,
            None,
            vec![EffectType::IO],
            "Print values followed by newline"
        ),
        StdlibFunction::effectful_with_context(
            "print",
            print_ctx,
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

// Context-aware file operations

fn file_read_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "read_file", args)
}

fn file_write_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "write_file", args)
}

fn file_append_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "append_file", args)
}

fn file_delete_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "delete_file", args)
}

fn file_exists_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "file_exists", args)
}

// Directory operations

fn dir_list_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "list_dir", args)
}

fn dir_create_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "create_dir", args)
}

fn current_directory_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "current_dir", args)
}

// Console I/O

fn read_line_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "read_line", args)
}

fn print_line_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "print_line", args)
}

fn print_ctx(context: &mut StdlibContext, args: &[Value]) -> Result<Value> {
    perform_io_effect(context, "print", args)
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
        serde_json::Value::Bool(b) => Ok(Value::Boolean(*b)),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(Value::Integer(i))
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
        Value::Boolean(b) => Ok(serde_json::Value::Bool(*b)),
        Value::Integer(i) => Ok(serde_json::json!(*i)),
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