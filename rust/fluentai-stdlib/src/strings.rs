//! String manipulation functions

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};

/// Register all string functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Basic operations
        StdlibFunction::pure(
            "string-concat",
            string_concat,
            1,
            None,
            "Concatenate strings",
        ),
        StdlibFunction::pure(
            "string-length",
            string_length,
            1,
            Some(1),
            "Get string length",
        ),
        StdlibFunction::pure("substring", substring, 2, Some(3), "Extract substring"),
        StdlibFunction::pure(
            "string-ref",
            string_ref,
            2,
            Some(2),
            "Get character at index",
        ),
        // Manipulation
        StdlibFunction::pure(
            "string-split",
            string_split,
            2,
            Some(2),
            "Split string by delimiter",
        ),
        StdlibFunction::pure(
            "string-join",
            string_join,
            2,
            Some(2),
            "Join strings with delimiter",
        ),
        StdlibFunction::pure("string-trim", string_trim, 1, Some(1), "Trim whitespace"),
        StdlibFunction::pure(
            "string-upcase",
            string_upcase,
            1,
            Some(1),
            "Convert to uppercase",
        ),
        StdlibFunction::pure(
            "string-downcase",
            string_downcase,
            1,
            Some(1),
            "Convert to lowercase",
        ),
        StdlibFunction::pure(
            "string-capitalize",
            string_capitalize,
            1,
            Some(1),
            "Capitalize first letter",
        ),
        // Queries
        StdlibFunction::pure(
            "string-contains?",
            string_contains,
            2,
            Some(2),
            "Check if string contains substring",
        ),
        StdlibFunction::pure(
            "string-starts-with?",
            string_starts_with,
            2,
            Some(2),
            "Check if string starts with prefix",
        ),
        StdlibFunction::pure(
            "string-ends-with?",
            string_ends_with,
            2,
            Some(2),
            "Check if string ends with suffix",
        ),
        StdlibFunction::pure(
            "string-empty?",
            string_empty,
            1,
            Some(1),
            "Check if string is empty",
        ),
        // Transformations
        StdlibFunction::pure(
            "string-replace",
            string_replace,
            3,
            Some(3),
            "Replace occurrences in string",
        ),
        StdlibFunction::pure(
            "string->list",
            string_to_list,
            1,
            Some(1),
            "Convert string to list of characters",
        ),
        StdlibFunction::pure(
            "list->string",
            list_to_string,
            1,
            Some(1),
            "Convert list of characters to string",
        ),
        StdlibFunction::pure(
            "string->number",
            string_to_number,
            1,
            Some(1),
            "Parse string to number",
        ),
        StdlibFunction::pure(
            "number->string",
            number_to_string,
            1,
            Some(1),
            "Convert number to string",
        ),
        // Character operations
        StdlibFunction::pure(
            "char->int",
            char_to_int,
            1,
            Some(1),
            "Get ASCII/Unicode value of character",
        ),
        StdlibFunction::pure(
            "int->char",
            int_to_char,
            1,
            Some(1),
            "Convert ASCII/Unicode value to character",
        ),
        // Symbol operations
        StdlibFunction::pure(
            "symbol->string",
            symbol_to_string,
            1,
            Some(1),
            "Convert symbol to string (identity function due to VM limitations)",
        ),
        // Universal to_string function for f-string interpolation
        StdlibFunction::pure(
            "to_string",
            to_string,
            1,
            Some(1),
            "Convert any value to its string representation",
        ),
    ]);
}

// Basic operations

fn string_concat(args: &[Value]) -> Result<Value> {
    let mut result = String::new();

    for arg in args {
        match arg {
            Value::String(s) => result.push_str(s),
            _ => return Err(anyhow!("string-concat: expected strings")),
        }
    }

    Ok(Value::String(result))
}

fn string_length(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::Integer(s.chars().count() as i64)),
        _ => Err(anyhow!("string-length: expected string")),
    }
}

fn substring(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("substring: expected string")),
    };

    let start = match &args[1] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("substring: expected integer start index")),
    };

    let chars: Vec<char> = s.chars().collect();

    let end = if args.len() > 2 {
        match &args[2] {
            Value::Integer(i) => *i as usize,
            _ => return Err(anyhow!("substring: expected integer end index")),
        }
    } else {
        chars.len()
    };

    if start > chars.len() || end > chars.len() || start > end {
        return Err(anyhow!("substring: invalid indices"));
    }

    let result: String = chars[start..end].iter().collect();
    Ok(Value::String(result))
}

fn string_ref(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-ref: expected string")),
    };

    let index = match &args[1] {
        Value::Integer(i) => *i as usize,
        _ => return Err(anyhow!("string-ref: expected integer index")),
    };

    let chars: Vec<char> = s.chars().collect();

    if index >= chars.len() {
        return Err(anyhow!("string-ref: index out of bounds"));
    }

    Ok(Value::String(chars[index].to_string()))
}

// Manipulation

fn string_split(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-split: expected string")),
    };

    let delimiter = match &args[1] {
        Value::String(d) => d,
        _ => return Err(anyhow!("string-split: expected string delimiter")),
    };

    let parts: Vec<Value> = if delimiter.is_empty() {
        // Split into individual characters
        s.chars().map(|c| Value::String(c.to_string())).collect()
    } else {
        s.split(delimiter)
            .map(|p| Value::String(p.to_string()))
            .collect()
    };

    Ok(Value::List(parts))
}

fn string_join(args: &[Value]) -> Result<Value> {
    let delimiter = match &args[0] {
        Value::String(d) => d,
        _ => return Err(anyhow!("string-join: expected string delimiter")),
    };

    let list = match &args[1] {
        Value::List(items) => items,
        _ => return Err(anyhow!("string-join: expected list")),
    };

    let mut parts = Vec::new();
    for item in list {
        match item {
            Value::String(s) => parts.push(s.clone()),
            _ => return Err(anyhow!("string-join: list must contain only strings")),
        }
    }

    Ok(Value::String(parts.join(delimiter)))
}

fn string_trim(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.trim().to_string())),
        _ => Err(anyhow!("string-trim: expected string")),
    }
}

fn string_upcase(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_uppercase())),
        _ => Err(anyhow!("string-upcase: expected string")),
    }
}

fn string_downcase(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::String(s.to_lowercase())),
        _ => Err(anyhow!("string-downcase: expected string")),
    }
}

fn string_capitalize(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            let mut chars = s.chars();
            match chars.next() {
                None => Ok(Value::String(String::new())),
                Some(first) => {
                    let capitalized =
                        first.to_uppercase().collect::<String>() + &chars.as_str().to_lowercase();
                    Ok(Value::String(capitalized))
                }
            }
        }
        _ => Err(anyhow!("string-capitalize: expected string")),
    }
}

// Queries

fn string_contains(args: &[Value]) -> Result<Value> {
    let haystack = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-contains?: expected string")),
    };

    let needle = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-contains?: expected string pattern")),
    };

    Ok(Value::Boolean(haystack.contains(needle)))
}

fn string_starts_with(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-starts-with?: expected string")),
    };

    let prefix = match &args[1] {
        Value::String(p) => p,
        _ => return Err(anyhow!("string-starts-with?: expected string prefix")),
    };

    Ok(Value::Boolean(s.starts_with(prefix)))
}

fn string_ends_with(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-ends-with?: expected string")),
    };

    let suffix = match &args[1] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-ends-with?: expected string suffix")),
    };

    Ok(Value::Boolean(s.ends_with(suffix)))
}

fn string_empty(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::Boolean(s.is_empty())),
        _ => Err(anyhow!("string-empty?: expected string")),
    }
}

// Transformations

fn string_replace(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-replace: expected string")),
    };

    let from = match &args[1] {
        Value::String(f) => f,
        _ => return Err(anyhow!("string-replace: expected string pattern")),
    };

    let to = match &args[2] {
        Value::String(t) => t,
        _ => return Err(anyhow!("string-replace: expected string replacement")),
    };

    Ok(Value::String(s.replace(from, to)))
}

fn string_to_list(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            let chars: Vec<Value> = s.chars().map(|c| Value::String(c.to_string())).collect();
            Ok(Value::List(chars))
        }
        _ => Err(anyhow!("string->list: expected string")),
    }
}

fn list_to_string(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::List(items) => {
            let mut result = String::new();
            for item in items {
                match item {
                    Value::String(s) => {
                        if s.chars().count() != 1 {
                            return Err(anyhow!("list->string: expected single-character strings"));
                        }
                        result.push_str(s);
                    }
                    _ => return Err(anyhow!("list->string: expected list of strings")),
                }
            }
            Ok(Value::String(result))
        }
        _ => Err(anyhow!("list->string: expected list")),
    }
}

fn string_to_number(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            // Try parsing as integer first
            if let Ok(i) = s.parse::<i64>() {
                Ok(Value::Integer(i))
            } else if let Ok(f) = s.parse::<f64>() {
                Ok(Value::Float(f))
            } else {
                Err(anyhow!("string->number: invalid number format"))
            }
        }
        _ => Err(anyhow!("string->number: expected string")),
    }
}

fn number_to_string(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::String(i.to_string())),
        Value::Float(f) => Ok(Value::String(f.to_string())),
        _ => Err(anyhow!("number->string: expected number")),
    }
}

// Character operations

fn char_to_int(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            if chars.len() != 1 {
                return Err(anyhow!("char->int: expected single character string"));
            }
            Ok(Value::Integer(chars[0] as i64))
        }
        _ => Err(anyhow!("char->int: expected string")),
    }
}

fn int_to_char(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => {
            if *i < 0 || *i > 0x10FFFF {
                return Err(anyhow!("int->char: invalid Unicode code point"));
            }
            if let Some(c) = char::from_u32(*i as u32) {
                Ok(Value::String(c.to_string()))
            } else {
                Err(anyhow!("int->char: invalid Unicode code point"))
            }
        }
        _ => Err(anyhow!("int->char: expected integer")),
    }
}

// Symbol operations

fn symbol_to_string(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            // In the current VM implementation, symbols are represented as strings
            // This is an identity function, but we keep it for compatibility
            // TODO: When symbols are properly implemented in the VM, this should
            // check for a Symbol type and convert it to String
            Ok(Value::String(s.clone()))
        }
        _ => Err(anyhow!(
            "symbol->string: expected symbol (currently represented as string)"
        )),
    }
}

// Universal to_string for f-string interpolation
fn to_string(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Integer(i) => Ok(Value::String(i.to_string())),
        Value::Float(f) => Ok(Value::String(f.to_string())),
        Value::String(s) => Ok(Value::String(s.clone())),
        Value::Boolean(b) => Ok(Value::String(b.to_string())),
        Value::Nil => Ok(Value::String("nil".to_string())),
        Value::List(items) => {
            // Convert list to string representation like "[1, 2, 3]"
            let item_strings: Vec<String> = items
                .iter()
                .map(|v| match to_string(&[v.clone()]) {
                    Ok(Value::String(s)) => s,
                    _ => "?".to_string(),
                })
                .collect();
            Ok(Value::String(format!("[{}]", item_strings.join(", "))))
        }
        Value::Map(map) => {
            // Convert map to string representation like "{key1: value1, key2: value2}"
            let mut pairs: Vec<String> = map
                .iter()
                .map(|(k, v)| {
                    match to_string(&[v.clone()]) {
                        Ok(Value::String(vs)) => format!("{}: {}", k, vs),
                        _ => format!("{}: ?", k),
                    }
                })
                .collect();
            pairs.sort(); // For consistent ordering
            Ok(Value::String(format!("{{{}}}", pairs.join(", "))))
        }
        Value::Tagged { tag, values } => {
            // Convert tagged value to string like "Tag(value1, value2)"
            let value_strings: Vec<String> = values
                .iter()
                .map(|v| match to_string(&[v.clone()]) {
                    Ok(Value::String(s)) => s,
                    _ => "?".to_string(),
                })
                .collect();
            Ok(Value::String(format!("{}({})", tag, value_strings.join(", "))))
        }
        Value::Function { .. } => Ok(Value::String("<function>".to_string())),
        Value::NativeFunction { .. } => Ok(Value::String("<native-function>".to_string())),
        Value::Procedure(_) => Ok(Value::String("<procedure>".to_string())),
        Value::Vector(items) => {
            // Convert vector to string representation like "#[1, 2, 3]"
            let item_strings: Vec<String> = items
                .iter()
                .map(|v| match to_string(&[v.clone()]) {
                    Ok(Value::String(s)) => s,
                    _ => "?".to_string(),
                })
                .collect();
            Ok(Value::String(format!("#[{}]", item_strings.join(", "))))
        }
        Value::Symbol(s) => Ok(Value::String(format!("'{}", s))),
        Value::Promise(_) => Ok(Value::String("<promise>".to_string())),
        Value::Future { .. } => Ok(Value::String("<future>".to_string())),
        Value::Channel(_) => Ok(Value::String("<channel>".to_string())),
        Value::Actor(_) => Ok(Value::String("<actor>".to_string())),
        Value::Error { kind, message, .. } => Ok(Value::String(format!("<error: {}: {}>", kind, message))),
        Value::Cell(_) => Ok(Value::String("<cell>".to_string())),
        Value::Module { name, .. } => Ok(Value::String(format!("<module: {}>", name))),
        Value::GcHandle(_) => Ok(Value::String("<gc-handle>".to_string())),
        Value::Set(items) => {
            // Convert set to string representation like "#{1, 2, 3}"
            let item_strings: Vec<String> = items
                .iter()
                .map(|v| match to_string(&[v.clone()]) {
                    Ok(Value::String(s)) => s,
                    _ => "?".to_string(),
                })
                .collect();
            Ok(Value::String(format!("#{{{}}}", item_strings.join(", "))))
        }
    }
}
