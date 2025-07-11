//! Extended string manipulation functions
//!
//! This module provides additional string functions including:
//! - pad-left, pad-right, pad-center
//! - repeat
//! - reverse
//! - index-of
//! - format

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};

/// Register extended string functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Padding functions
        StdlibFunction::pure(
            "string-pad-left",
            string_pad_left,
            2,
            Some(3),
            "Pad string on the left",
        ),
        StdlibFunction::pure(
            "string-pad-right",
            string_pad_right,
            2,
            Some(3),
            "Pad string on the right",
        ),
        StdlibFunction::pure(
            "string-pad-center",
            string_pad_center,
            2,
            Some(3),
            "Pad string on both sides",
        ),
        // Other string functions
        StdlibFunction::pure(
            "string-repeat",
            string_repeat,
            2,
            Some(2),
            "Repeat string n times",
        ),
        StdlibFunction::pure(
            "string-reverse",
            string_reverse,
            1,
            Some(1),
            "Reverse a string",
        ),
        StdlibFunction::pure(
            "string-index-of",
            string_index_of,
            2,
            Some(3),
            "Find index of substring",
        ),
        StdlibFunction::pure(
            "string-format",
            string_format,
            1,
            None,
            "Format string with arguments",
        ),
    ]);
}

// Padding functions

fn string_pad_left(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-pad-left: expected string")),
    };

    let width = match &args[1] {
        Value::Integer(w) => *w as usize,
        _ => return Err(anyhow!("string-pad-left: expected integer width")),
    };

    let pad_char = if args.len() > 2 {
        match &args[2] {
            Value::String(p) => {
                let chars: Vec<char> = p.chars().collect();
                if chars.len() != 1 {
                    return Err(anyhow!("string-pad-left: pad must be single character"));
                }
                chars[0]
            }
            _ => return Err(anyhow!("string-pad-left: expected string pad character")),
        }
    } else {
        ' ' // Default pad character
    };

    let current_len = s.chars().count();
    if current_len >= width {
        Ok(Value::String(s.clone()))
    } else {
        let padding = pad_char.to_string().repeat(width - current_len);
        Ok(Value::String(padding + s))
    }
}

fn string_pad_right(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-pad-right: expected string")),
    };

    let width = match &args[1] {
        Value::Integer(w) => *w as usize,
        _ => return Err(anyhow!("string-pad-right: expected integer width")),
    };

    let pad_char = if args.len() > 2 {
        match &args[2] {
            Value::String(p) => {
                let chars: Vec<char> = p.chars().collect();
                if chars.len() != 1 {
                    return Err(anyhow!("string-pad-right: pad must be single character"));
                }
                chars[0]
            }
            _ => return Err(anyhow!("string-pad-right: expected string pad character")),
        }
    } else {
        ' ' // Default pad character
    };

    let current_len = s.chars().count();
    if current_len >= width {
        Ok(Value::String(s.clone()))
    } else {
        let padding = pad_char.to_string().repeat(width - current_len);
        Ok(Value::String(s.clone() + &padding))
    }
}

fn string_pad_center(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-pad-center: expected string")),
    };

    let width = match &args[1] {
        Value::Integer(w) => *w as usize,
        _ => return Err(anyhow!("string-pad-center: expected integer width")),
    };

    let pad_char = if args.len() > 2 {
        match &args[2] {
            Value::String(p) => {
                let chars: Vec<char> = p.chars().collect();
                if chars.len() != 1 {
                    return Err(anyhow!("string-pad-center: pad must be single character"));
                }
                chars[0]
            }
            _ => return Err(anyhow!("string-pad-center: expected string pad character")),
        }
    } else {
        ' ' // Default pad character
    };

    let current_len = s.chars().count();
    if current_len >= width {
        Ok(Value::String(s.clone()))
    } else {
        let total_padding = width - current_len;
        let left_padding = total_padding / 2;
        let right_padding = total_padding - left_padding;

        let left_pad = pad_char.to_string().repeat(left_padding);
        let right_pad = pad_char.to_string().repeat(right_padding);

        Ok(Value::String(left_pad + s + &right_pad))
    }
}

// Other string functions

fn string_repeat(args: &[Value]) -> Result<Value> {
    let s = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-repeat: expected string")),
    };

    let count = match &args[1] {
        Value::Integer(n) => {
            if *n < 0 {
                return Err(anyhow!("string-repeat: count must be non-negative"));
            }
            *n as usize
        }
        _ => return Err(anyhow!("string-repeat: expected integer count")),
    };

    Ok(Value::String(s.repeat(count)))
}

fn string_reverse(args: &[Value]) -> Result<Value> {
    match &args[0] {
        Value::String(s) => {
            let reversed: String = s.chars().rev().collect();
            Ok(Value::String(reversed))
        }
        _ => Err(anyhow!("string-reverse: expected string")),
    }
}

fn string_index_of(args: &[Value]) -> Result<Value> {
    let haystack = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-index-of: expected string")),
    };

    let needle = match &args[1] {
        Value::String(n) => n,
        _ => return Err(anyhow!("string-index-of: expected string pattern")),
    };

    let start_index = if args.len() > 2 {
        match &args[2] {
            Value::Integer(i) => {
                if *i < 0 {
                    return Err(anyhow!("string-index-of: start index must be non-negative"));
                }
                *i as usize
            }
            _ => return Err(anyhow!("string-index-of: expected integer start index")),
        }
    } else {
        0
    };

    // Convert to character indices for proper Unicode handling
    let haystack_chars: Vec<char> = haystack.chars().collect();
    let needle_chars: Vec<char> = needle.chars().collect();

    if needle_chars.is_empty() {
        // Empty string is found at the start position
        return Ok(Value::Integer(start_index as i64));
    }

    if start_index >= haystack_chars.len() {
        return Ok(Value::Integer(-1)); // Not found
    }

    // Search for the needle starting from start_index
    for i in start_index..=(haystack_chars.len().saturating_sub(needle_chars.len())) {
        if haystack_chars[i..].starts_with(&needle_chars) {
            return Ok(Value::Integer(i as i64));
        }
    }

    Ok(Value::Integer(-1)) // Not found
}

fn string_format(args: &[Value]) -> Result<Value> {
    if args.is_empty() {
        return Err(anyhow!("string-format: expected format string"));
    }

    let format_str = match &args[0] {
        Value::String(s) => s,
        _ => return Err(anyhow!("string-format: expected string format")),
    };

    let mut result = String::new();
    let mut chars = format_str.chars().peekable();
    let mut arg_index = 1;

    while let Some(ch) = chars.next() {
        if ch == '{' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '}' {
                    // {} - consume next argument
                    chars.next(); // consume the '}'
                    if arg_index < args.len() {
                        result.push_str(&format_value(&args[arg_index], false));
                        arg_index += 1;
                    } else {
                        // Not enough arguments - leave placeholder as is
                        result.push_str("{}");
                    }
                } else if next_ch == '{' {
                    // {{ - literal brace
                    chars.next(); // consume the second '{'
                    result.push('{');
                } else {
                    // Just a single { - keep as is
                    result.push(ch);
                }
            } else {
                // Trailing { at end of string
                result.push(ch);
            }
        } else if ch == '}' {
            if let Some(&next_ch) = chars.peek() {
                if next_ch == '}' {
                    // }} - literal brace
                    chars.next(); // consume the second '}'
                    result.push('}');
                } else {
                    // Just a single } - keep as is
                    result.push(ch);
                }
            } else {
                // Trailing } at end of string
                result.push(ch);
            }
        } else {
            result.push(ch);
        }
    }

    Ok(Value::String(result))
}

/// Format a value for output
fn format_value(value: &Value, machine_readable: bool) -> String {
    match value {
        Value::String(s) => {
            if machine_readable {
                format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
            } else {
                s.clone()
            }
        }
        Value::Integer(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::Boolean(b) => b.to_string(),
        Value::List(items) => {
            let formatted: Vec<String> = items
                .iter()
                .map(|v| format_value(v, machine_readable))
                .collect();
            format!("[{}]", formatted.join(" "))
        }
        Value::Map(map) => {
            let formatted: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("{}: {}", k, format_value(v, machine_readable)))
                .collect();
            format!("{{{}}}", formatted.join(", "))
        }
        Value::Nil => "nil".to_string(),
        Value::Tagged { tag, values } => {
            let formatted: Vec<String> = values
                .iter()
                .map(|v| format_value(v, machine_readable))
                .collect();
            format!("{}({})", tag, formatted.join(", "))
        }
        _ => "<unprintable>".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_string_pad_left() {
        // Basic padding
        let result =
            string_pad_left(&[Value::String("hi".to_string()), Value::Integer(5)]).unwrap();
        assert_eq!(result, Value::String("   hi".to_string()));

        // Custom pad character
        let result = string_pad_left(&[
            Value::String("hi".to_string()),
            Value::Integer(5),
            Value::String("*".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("***hi".to_string()));

        // String already long enough
        let result =
            string_pad_left(&[Value::String("hello".to_string()), Value::Integer(3)]).unwrap();
        assert_eq!(result, Value::String("hello".to_string()));
    }

    #[test]
    fn test_string_pad_right() {
        let result =
            string_pad_right(&[Value::String("hi".to_string()), Value::Integer(5)]).unwrap();
        assert_eq!(result, Value::String("hi   ".to_string()));

        let result = string_pad_right(&[
            Value::String("hi".to_string()),
            Value::Integer(5),
            Value::String("-".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("hi---".to_string()));
    }

    #[test]
    fn test_string_pad_center() {
        let result =
            string_pad_center(&[Value::String("hi".to_string()), Value::Integer(5)]).unwrap();
        assert_eq!(result, Value::String(" hi  ".to_string()));

        let result = string_pad_center(&[
            Value::String("hi".to_string()),
            Value::Integer(6),
            Value::String("*".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("**hi**".to_string()));
    }

    #[test]
    fn test_string_repeat() {
        let result = string_repeat(&[Value::String("ha".to_string()), Value::Integer(3)]).unwrap();
        assert_eq!(result, Value::String("hahaha".to_string()));

        let result = string_repeat(&[Value::String("x".to_string()), Value::Integer(0)]).unwrap();
        assert_eq!(result, Value::String("".to_string()));
    }

    #[test]
    fn test_string_reverse() {
        let result = string_reverse(&[Value::String("hello".to_string())]).unwrap();
        assert_eq!(result, Value::String("olleh".to_string()));

        // Unicode test
        let result = string_reverse(&[Value::String("café".to_string())]).unwrap();
        assert_eq!(result, Value::String("éfac".to_string()));
    }

    #[test]
    fn test_string_index_of() {
        let result = string_index_of(&[
            Value::String("hello world".to_string()),
            Value::String("world".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::Integer(6));

        let result = string_index_of(&[
            Value::String("hello world".to_string()),
            Value::String("xyz".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::Integer(-1));

        // With start index
        let result = string_index_of(&[
            Value::String("hello hello".to_string()),
            Value::String("hello".to_string()),
            Value::Integer(1),
        ])
        .unwrap();
        assert_eq!(result, Value::Integer(6));
    }

    #[test]
    fn test_string_format() {
        // Basic formatting
        let result = string_format(&[
            Value::String("Hello, {}!".to_string()),
            Value::String("World".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("Hello, World!".to_string()));

        // Multiple placeholders
        let result = string_format(&[
            Value::String("{} + {} = {}".to_string()),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(5),
        ])
        .unwrap();
        assert_eq!(result, Value::String("2 + 3 = 5".to_string()));

        // Literal braces
        let result = string_format(&[Value::String("{{escaped}}".to_string())]).unwrap();
        assert_eq!(result, Value::String("{escaped}".to_string()));

        // Not enough arguments
        let result = string_format(&[
            Value::String("Only {}".to_string()),
            Value::String("one".to_string()),
            Value::String("extra".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("Only one".to_string()));

        // Fewer args than placeholders
        let result = string_format(&[
            Value::String("{} and {}".to_string()),
            Value::String("one".to_string()),
        ])
        .unwrap();
        assert_eq!(result, Value::String("one and {}".to_string()));
    }
}
