//! Integration tests for extended string functions

use fluentai_stdlib::{init_stdlib, value::Value};

#[test]
fn test_string_pad_functions() {
    let registry = init_stdlib();
    
    // Test pad-left
    let pad_left = registry.get("string-pad-left").unwrap();
    let result = pad_left.call(&[Value::String("hi".to_string()), Value::Int(5)]).unwrap();
    assert_eq!(result, Value::String("   hi".to_string()));
    
    // Test pad-right with custom character
    let pad_right = registry.get("string-pad-right").unwrap();
    let result = pad_right.call(&[
        Value::String("test".to_string()), 
        Value::Int(8), 
        Value::String("*".to_string())
    ]).unwrap();
    assert_eq!(result, Value::String("test****".to_string()));
    
    // Test pad-center
    let pad_center = registry.get("string-pad-center").unwrap();
    let result = pad_center.call(&[
        Value::String("center".to_string()), 
        Value::Int(10)
    ]).unwrap();
    assert_eq!(result, Value::String("  center  ".to_string()));
}

#[test]
fn test_string_repeat() {
    let registry = init_stdlib();
    let repeat = registry.get("string-repeat").unwrap();
    
    let result = repeat.call(&[Value::String("na".to_string()), Value::Int(4)]).unwrap();
    assert_eq!(result, Value::String("nananana".to_string()));
    
    // Test with zero repetitions
    let result = repeat.call(&[Value::String("test".to_string()), Value::Int(0)]).unwrap();
    assert_eq!(result, Value::String("".to_string()));
}

#[test]
fn test_string_reverse() {
    let registry = init_stdlib();
    let reverse = registry.get("string-reverse").unwrap();
    
    let result = reverse.call(&[Value::String("hello world".to_string())]).unwrap();
    assert_eq!(result, Value::String("dlrow olleh".to_string()));
    
    // Test with Unicode
    let result = reverse.call(&[Value::String("café ☕".to_string())]).unwrap();
    assert_eq!(result, Value::String("☕ éfac".to_string()));
}

#[test]
fn test_string_index_of() {
    let registry = init_stdlib();
    let index_of = registry.get("string-index-of").unwrap();
    
    // Basic search
    let result = index_of.call(&[
        Value::String("hello world".to_string()),
        Value::String("world".to_string())
    ]).unwrap();
    assert_eq!(result, Value::Int(6));
    
    // Not found
    let result = index_of.call(&[
        Value::String("hello world".to_string()),
        Value::String("xyz".to_string())
    ]).unwrap();
    assert_eq!(result, Value::Int(-1));
    
    // With start index
    let result = index_of.call(&[
        Value::String("hello hello world".to_string()),
        Value::String("hello".to_string()),
        Value::Int(1)
    ]).unwrap();
    assert_eq!(result, Value::Int(6));
    
    // Empty needle
    let result = index_of.call(&[
        Value::String("test".to_string()),
        Value::String("".to_string())
    ]).unwrap();
    assert_eq!(result, Value::Int(0));
}

#[test]
fn test_string_format() {
    let registry = init_stdlib();
    let format = registry.get("string-format").unwrap();
    
    // Basic formatting
    let result = format.call(&[
        Value::String("Hello ~a, welcome to ~a!".to_string()),
        Value::String("Alice".to_string()),
        Value::String("FluentAI".to_string())
    ]).unwrap();
    assert_eq!(result, Value::String("Hello Alice, welcome to FluentAI!".to_string()));
    
    // Different format specifiers
    let result = format.call(&[
        Value::String("~s is ~d years old and has ~f dollars".to_string()),
        Value::String("Bob".to_string()),
        Value::Int(25),
        Value::Float(123.45)
    ]).unwrap();
    assert_eq!(result, Value::String("\"Bob\" is 25 years old and has 123.45 dollars".to_string()));
    
    // Special characters
    let result = format.call(&[
        Value::String("Line 1~%Line 2~%~%100~~ complete".to_string())
    ]).unwrap();
    assert_eq!(result, Value::String("Line 1\nLine 2\n\n100~ complete".to_string()));
    
    // Lists and maps
    let result = format.call(&[
        Value::String("List: ~a, Map: ~a".to_string()),
        Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
        Value::Map(vec![
            ("key".to_string(), Value::String("value".to_string()))
        ].into_iter().collect())
    ]).unwrap();
    assert_eq!(result, Value::String("List: [1 2 3], Map: {key: value}".to_string()));
}

#[test]
fn test_error_handling() {
    let registry = init_stdlib();
    
    // Test pad with non-single character
    let pad_left = registry.get("string-pad-left").unwrap();
    let result = pad_left.call(&[
        Value::String("hi".to_string()), 
        Value::Int(5), 
        Value::String("**".to_string())
    ]);
    assert!(result.is_err());
    
    // Test repeat with negative count
    let repeat = registry.get("string-repeat").unwrap();
    let result = repeat.call(&[Value::String("test".to_string()), Value::Int(-1)]);
    assert!(result.is_err());
    
    // Test format with insufficient arguments
    let format = registry.get("string-format").unwrap();
    let result = format.call(&[Value::String("~a ~a".to_string()), Value::String("only one".to_string())]);
    assert!(result.is_err());
}