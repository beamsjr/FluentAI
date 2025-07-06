//! String manipulation functions

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};

/// Register all string functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Basic operations
        StdlibFunction::pure("string-concat", string_concat, 1, None, "Concatenate strings"),
        StdlibFunction::pure("string-length", string_length, 1, Some(1), "Get string length"),
        StdlibFunction::pure("substring", substring, 2, Some(3), "Extract substring"),
        StdlibFunction::pure("string-ref", string_ref, 2, Some(2), "Get character at index"),
        
        // Manipulation
        StdlibFunction::pure("string-split", string_split, 2, Some(2), "Split string by delimiter"),
        StdlibFunction::pure("string-join", string_join, 2, Some(2), "Join strings with delimiter"),
        StdlibFunction::pure("string-trim", string_trim, 1, Some(1), "Trim whitespace"),
        StdlibFunction::pure("string-upcase", string_upcase, 1, Some(1), "Convert to uppercase"),
        StdlibFunction::pure("string-downcase", string_downcase, 1, Some(1), "Convert to lowercase"),
        StdlibFunction::pure("string-capitalize", string_capitalize, 1, Some(1), "Capitalize first letter"),
        
        // Queries
        StdlibFunction::pure("string-contains?", string_contains, 2, Some(2), "Check if string contains substring"),
        StdlibFunction::pure("string-starts-with?", string_starts_with, 2, Some(2), "Check if string starts with prefix"),
        StdlibFunction::pure("string-ends-with?", string_ends_with, 2, Some(2), "Check if string ends with suffix"),
        StdlibFunction::pure("string-empty?", string_empty, 1, Some(1), "Check if string is empty"),
        
        // Transformations
        StdlibFunction::pure("string-replace", string_replace, 3, Some(3), "Replace occurrences in string"),
        StdlibFunction::pure("string->list", string_to_list, 1, Some(1), "Convert string to list of characters"),
        StdlibFunction::pure("list->string", list_to_string, 1, Some(1), "Convert list of characters to string"),
        StdlibFunction::pure("string->number", string_to_number, 1, Some(1), "Parse string to number"),
        StdlibFunction::pure("number->string", number_to_string, 1, Some(1), "Convert number to string"),
        
        // Character operations
        StdlibFunction::pure("char->int", char_to_int, 1, Some(1), "Get ASCII/Unicode value of character"),
        StdlibFunction::pure("int->char", int_to_char, 1, Some(1), "Convert ASCII/Unicode value to character"),
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
        Value::String(s) => Ok(Value::Int(s.chars().count() as i64)),
        _ => Err(anyhow!("string-length: expected string")),
    }
}

fn substring(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("substring: expected string")),
    };
    
    let start = match &args[1] {
        Value::Int(i) => *i as usize,
        _ => return Err(anyhow!("substring: expected integer start index")),
    };
    
    let chars: Vec<char> = s.chars().collect();
    
    let end = if args.len() > 2 {
        match &args[2] {
            Value::Int(i) => *i as usize,
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
        Value::Int(i) => *i as usize,
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
        s.split(delimiter).map(|p| Value::String(p.to_string())).collect()
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
                    let capitalized = first.to_uppercase().collect::<String>() 
                        + &chars.as_str().to_lowercase();
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
    
    Ok(Value::Bool(haystack.contains(needle)))
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
    
    Ok(Value::Bool(s.starts_with(prefix)))
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
    
    Ok(Value::Bool(s.ends_with(suffix)))
}

fn string_empty(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => Ok(Value::Bool(s.is_empty())),
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
            let chars: Vec<Value> = s.chars()
                .map(|c| Value::String(c.to_string()))
                .collect();
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
                Ok(Value::Int(i))
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
        Value::Int(i) => Ok(Value::String(i.to_string())),
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
            Ok(Value::Int(chars[0] as i64))
        }
        _ => Err(anyhow!("char->int: expected string")),
    }
}

fn int_to_char(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::Int(i) => {
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