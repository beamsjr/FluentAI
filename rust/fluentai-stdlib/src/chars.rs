//! Character predicate and manipulation functions
//!
//! This module provides functions to test character properties
//! and perform character manipulations.

use crate::registry::{StdlibFunction, StdlibRegistry};
use crate::value::Value;
use anyhow::{anyhow, Result};

/// Register all character functions
pub fn register(registry: &mut StdlibRegistry) {
    registry.register_all(vec![
        // Character predicates
        StdlibFunction::pure("char-alphabetic?", char_alphabetic, 1, Some(1), "Test if character is alphabetic"),
        StdlibFunction::pure("char-numeric?", char_numeric, 1, Some(1), "Test if character is numeric"),
        StdlibFunction::pure("char-alphanumeric?", char_alphanumeric, 1, Some(1), "Test if character is alphanumeric"),
        StdlibFunction::pure("char-whitespace?", char_whitespace, 1, Some(1), "Test if character is whitespace"),
        StdlibFunction::pure("char-upper-case?", char_upper_case, 1, Some(1), "Test if character is uppercase"),
        StdlibFunction::pure("char-lower-case?", char_lower_case, 1, Some(1), "Test if character is lowercase"),
        StdlibFunction::pure("char-control?", char_control, 1, Some(1), "Test if character is control character"),
        StdlibFunction::pure("char-ascii?", char_ascii, 1, Some(1), "Test if character is ASCII"),
        
        // Character case conversion
        StdlibFunction::pure("char-upcase", char_upcase, 1, Some(1), "Convert character to uppercase"),
        StdlibFunction::pure("char-downcase", char_downcase, 1, Some(1), "Convert character to lowercase"),
        
        // Character comparisons
        StdlibFunction::pure("char=?", char_equal, 2, Some(2), "Test if two characters are equal"),
        StdlibFunction::pure("char<?", char_less_than, 2, Some(2), "Test if first character is less than second"),
        StdlibFunction::pure("char>?", char_greater_than, 2, Some(2), "Test if first character is greater than second"),
        StdlibFunction::pure("char<=?", char_less_equal, 2, Some(2), "Test if first character is less than or equal to second"),
        StdlibFunction::pure("char>=?", char_greater_equal, 2, Some(2), "Test if first character is greater than or equal to second"),
        
        // Case-insensitive comparisons
        StdlibFunction::pure("char-ci=?", char_equal_ci, 2, Some(2), "Case-insensitive character equality"),
        StdlibFunction::pure("char-ci<?", char_less_than_ci, 2, Some(2), "Case-insensitive less than"),
        StdlibFunction::pure("char-ci>?", char_greater_than_ci, 2, Some(2), "Case-insensitive greater than"),
        StdlibFunction::pure("char-ci<=?", char_less_equal_ci, 2, Some(2), "Case-insensitive less than or equal"),
        StdlibFunction::pure("char-ci>=?", char_greater_equal_ci, 2, Some(2), "Case-insensitive greater than or equal"),
    ]);
}

/// Helper function to extract a single character from a Value
fn get_char(value: &Value, func_name: &str) -> Result<char> {
    match value {
        Value::String(s) => {
            let chars: Vec<char> = s.chars().collect();
            if chars.len() != 1 {
                Err(anyhow!("{}: expected single character string, got {} characters", func_name, chars.len()))
            } else {
                Ok(chars[0])
            }
        }
        _ => Err(anyhow!("{}: expected string", func_name)),
    }
}

// Character predicates

fn char_alphabetic(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-alphabetic?")?;
    Ok(Value::Boolean(c.is_alphabetic()))
}

fn char_numeric(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-numeric?")?;
    Ok(Value::Boolean(c.is_numeric()))
}

fn char_alphanumeric(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-alphanumeric?")?;
    Ok(Value::Boolean(c.is_alphanumeric()))
}

fn char_whitespace(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-whitespace?")?;
    Ok(Value::Boolean(c.is_whitespace()))
}

fn char_upper_case(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-upper-case?")?;
    Ok(Value::Boolean(c.is_uppercase()))
}

fn char_lower_case(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-lower-case?")?;
    Ok(Value::Boolean(c.is_lowercase()))
}

fn char_control(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-control?")?;
    Ok(Value::Boolean(c.is_control()))
}

fn char_ascii(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-ascii?")?;
    Ok(Value::Boolean(c.is_ascii()))
}

// Character case conversion

fn char_upcase(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-upcase")?;
    // to_uppercase() returns an iterator because some characters expand to multiple
    let uppercase: String = c.to_uppercase().collect();
    Ok(Value::String(uppercase))
}

fn char_downcase(args: &[Value]) -> Result<Value> {
    let c = get_char(&args[0], "char-downcase")?;
    // to_lowercase() returns an iterator because some characters expand to multiple
    let lowercase: String = c.to_lowercase().collect();
    Ok(Value::String(lowercase))
}

// Character comparisons

fn char_equal(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char=?")?;
    let c2 = get_char(&args[1], "char=?")?;
    Ok(Value::Boolean(c1 == c2))
}

fn char_less_than(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char<?")?;
    let c2 = get_char(&args[1], "char<?")?;
    Ok(Value::Boolean(c1 < c2))
}

fn char_greater_than(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char>?")?;
    let c2 = get_char(&args[1], "char>?")?;
    Ok(Value::Boolean(c1 > c2))
}

fn char_less_equal(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char<=?")?;
    let c2 = get_char(&args[1], "char<=?")?;
    Ok(Value::Boolean(c1 <= c2))
}

fn char_greater_equal(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char>=?")?;
    let c2 = get_char(&args[1], "char>=?")?;
    Ok(Value::Boolean(c1 >= c2))
}

// Case-insensitive comparisons

fn char_equal_ci(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char-ci=?")?;
    let c2 = get_char(&args[1], "char-ci=?")?;
    
    // Convert both to lowercase for comparison
    let c1_lower: String = c1.to_lowercase().collect();
    let c2_lower: String = c2.to_lowercase().collect();
    
    Ok(Value::Boolean(c1_lower == c2_lower))
}

fn char_less_than_ci(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char-ci<?")?;
    let c2 = get_char(&args[1], "char-ci<?")?;
    
    let c1_lower: String = c1.to_lowercase().collect();
    let c2_lower: String = c2.to_lowercase().collect();
    
    Ok(Value::Boolean(c1_lower < c2_lower))
}

fn char_greater_than_ci(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char-ci>?")?;
    let c2 = get_char(&args[1], "char-ci>?")?;
    
    let c1_lower: String = c1.to_lowercase().collect();
    let c2_lower: String = c2.to_lowercase().collect();
    
    Ok(Value::Boolean(c1_lower > c2_lower))
}

fn char_less_equal_ci(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char-ci<=?")?;
    let c2 = get_char(&args[1], "char-ci<=?")?;
    
    let c1_lower: String = c1.to_lowercase().collect();
    let c2_lower: String = c2.to_lowercase().collect();
    
    Ok(Value::Boolean(c1_lower <= c2_lower))
}

fn char_greater_equal_ci(args: &[Value]) -> Result<Value> {
    let c1 = get_char(&args[0], "char-ci>=?")?;
    let c2 = get_char(&args[1], "char-ci>=?")?;
    
    let c1_lower: String = c1.to_lowercase().collect();
    let c2_lower: String = c2.to_lowercase().collect();
    
    Ok(Value::Boolean(c1_lower >= c2_lower))
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_char_predicates() {
        // Test alphabetic
        assert_eq!(
            char_alphabetic(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_alphabetic(&[Value::String("5".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test numeric
        assert_eq!(
            char_numeric(&[Value::String("5".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_numeric(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test alphanumeric
        assert_eq!(
            char_alphanumeric(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_alphanumeric(&[Value::String("5".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_alphanumeric(&[Value::String("!".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test whitespace
        assert_eq!(
            char_whitespace(&[Value::String(" ".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_whitespace(&[Value::String("\t".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_whitespace(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test case predicates
        assert_eq!(
            char_upper_case(&[Value::String("A".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_upper_case(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        assert_eq!(
            char_lower_case(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_lower_case(&[Value::String("A".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test control characters
        assert_eq!(
            char_control(&[Value::String("\n".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_control(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        // Test ASCII
        assert_eq!(
            char_ascii(&[Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_ascii(&[Value::String("€".to_string())]).unwrap(),
            Value::Boolean(false)
        );
    }
    
    #[test]
    fn test_char_case_conversion() {
        assert_eq!(
            char_upcase(&[Value::String("a".to_string())]).unwrap(),
            Value::String("A".to_string())
        );
        assert_eq!(
            char_downcase(&[Value::String("A".to_string())]).unwrap(),
            Value::String("a".to_string())
        );
        
        // Test with special characters that expand
        assert_eq!(
            char_upcase(&[Value::String("ß".to_string())]).unwrap(),
            Value::String("SS".to_string())
        );
    }
    
    #[test]
    fn test_char_comparisons() {
        // Basic comparisons
        assert_eq!(
            char_equal(&[Value::String("a".to_string()), Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_equal(&[Value::String("a".to_string()), Value::String("b".to_string())]).unwrap(),
            Value::Boolean(false)
        );
        
        assert_eq!(
            char_less_than(&[Value::String("a".to_string()), Value::String("b".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_greater_than(&[Value::String("b".to_string()), Value::String("a".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        
        // Case-insensitive comparisons
        assert_eq!(
            char_equal_ci(&[Value::String("a".to_string()), Value::String("A".to_string())]).unwrap(),
            Value::Boolean(true)
        );
        assert_eq!(
            char_less_than_ci(&[Value::String("a".to_string()), Value::String("B".to_string())]).unwrap(),
            Value::Boolean(true)
        );
    }
    
    #[test]
    fn test_error_handling() {
        // Empty string
        assert!(char_alphabetic(&[Value::String("".to_string())]).is_err());
        
        // Multi-character string
        assert!(char_alphabetic(&[Value::String("ab".to_string())]).is_err());
        
        // Non-string value
        assert!(char_alphabetic(&[Value::Integer(42)]).is_err());
    }
}