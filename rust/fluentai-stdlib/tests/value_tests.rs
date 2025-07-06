//! Tests for value module

use fluentai_stdlib::value::Value;
use rustc_hash::FxHashMap;

#[test]
fn test_value_display() {
    // Test Display implementation for all Value variants
    assert_eq!(Value::Nil.to_string(), "nil");
    assert_eq!(Value::Bool(true).to_string(), "true");
    assert_eq!(Value::Bool(false).to_string(), "false");
    assert_eq!(Value::Int(42).to_string(), "42");
    assert_eq!(Value::Float(3.14).to_string(), "3.14");
    assert_eq!(Value::String("hello".to_string()).to_string(), "\"hello\"");
    
    // Test list display
    let list = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ]);
    assert_eq!(list.to_string(), "[1, 2, 3]");
    
    // Test empty list
    assert_eq!(Value::List(vec![]).to_string(), "[]");
    
    // Test nested list
    let nested = Value::List(vec![
        Value::Int(1),
        Value::List(vec![Value::Int(2), Value::Int(3)]),
    ]);
    assert_eq!(nested.to_string(), "[1, [2, 3]]");
    
    // Test map display
    let mut map = FxHashMap::default();
    map.insert("x".to_string(), Value::Int(1));
    map.insert("y".to_string(), Value::Int(2));
    let map_value = Value::Map(map);
    let map_str = map_value.to_string();
    // Map order may vary, so check it contains the elements
    assert!(map_str.starts_with('{'));
    assert!(map_str.ends_with('}'));
    assert!(map_str.contains("\"x\": 1"));
    assert!(map_str.contains("\"y\": 2"));
    
    // Test function display
    assert_eq!(Value::Function { chunk_id: 0, env: vec![] }.to_string(), "<function>");
    
    // Test promise display
    assert_eq!(Value::Promise(123).to_string(), "<promise:123>");
    
    // Test channel display
    assert_eq!(Value::Channel(42).to_string(), "<channel:42>");
    
    // Test cell display
    assert_eq!(Value::Cell(5).to_string(), "<cell:5>");
    
    // Test tagged value display
    assert_eq!(
        Value::Tagged { tag: "point".to_string(), values: vec![Value::Int(3), Value::Int(4)] }.to_string(),
        "point(3, 4)"
    );
}

#[test]
fn test_value_equality() {
    // Test basic equality
    assert_eq!(Value::Nil, Value::Nil);
    assert_eq!(Value::Bool(true), Value::Bool(true));
    assert_ne!(Value::Bool(true), Value::Bool(false));
    assert_eq!(Value::Int(42), Value::Int(42));
    assert_ne!(Value::Int(42), Value::Int(43));
    assert_eq!(Value::String("hello".to_string()), Value::String("hello".to_string()));
    assert_ne!(Value::String("hello".to_string()), Value::String("world".to_string()));
    
    // Test list equality
    assert_eq!(
        Value::List(vec![Value::Int(1), Value::Int(2)]),
        Value::List(vec![Value::Int(1), Value::Int(2)])
    );
    assert_ne!(
        Value::List(vec![Value::Int(1), Value::Int(2)]),
        Value::List(vec![Value::Int(2), Value::Int(1)])
    );
    
    // Test cross-type inequality
    assert_ne!(Value::Int(42), Value::Float(42.0));
    assert_ne!(Value::Bool(true), Value::Int(1));
}

#[test]
fn test_value_complex_structures() {
    // Test complex nested structure
    let complex = Value::Map({
        let mut map = FxHashMap::default();
        map.insert("list".to_string(), Value::List(vec![
            Value::Int(1),
            Value::Bool(true),
            Value::String("test".to_string()),
        ]));
        map.insert("nested".to_string(), Value::Map({
            let mut inner = FxHashMap::default();
            inner.insert("a".to_string(), Value::Int(10));
            inner.insert("b".to_string(), Value::Float(3.14));
            inner
        }));
        map
    });
    
    // Just ensure it can be displayed without panicking
    let _ = complex.to_string();
}