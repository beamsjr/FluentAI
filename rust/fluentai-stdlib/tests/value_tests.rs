//! Tests for value module

use fluentai_stdlib::value::Value;
use rustc_hash::FxHashMap;

#[test]
fn test_value_display() {
    // Test Display implementation for all Value variants
    assert_eq!(Value::Nil.to_string(), "nil");
    assert_eq!(Value::Boolean(true).to_string(), "#t");
    assert_eq!(Value::Boolean(false).to_string(), "#f");
    assert_eq!(Value::Integer(42).to_string(), "42");
    assert_eq!(Value::Float(3.14).to_string(), "3.14");
    assert_eq!(Value::String("hello".to_string()).to_string(), "\"hello\"");
    
    // Test list display
    let list = Value::List(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    assert_eq!(list.to_string(), "(1 2 3)");
    
    // Test empty list
    assert_eq!(Value::List(vec![]).to_string(), "()");
    
    // Test nested list
    let nested = Value::List(vec![
        Value::Integer(1),
        Value::List(vec![Value::Integer(2), Value::Integer(3)]),
    ]);
    assert_eq!(nested.to_string(), "(1 (2 3))");
    
    // Test map display - core Value displays maps as #<hashmap>
    let mut map = FxHashMap::default();
    map.insert("x".to_string(), Value::Integer(1));
    map.insert("y".to_string(), Value::Integer(2));
    let map_value = Value::Map(map);
    assert_eq!(map_value.to_string(), "#<hashmap>");
    
    // Test function display
    assert_eq!(Value::Function { chunk_id: 0, env: vec![] }.to_string(), "#<function>");
    
    // Test promise display
    assert_eq!(Value::Promise(123).to_string(), "#<promise:123>");
    
    // Test channel display
    assert_eq!(Value::Channel(42).to_string(), "#<channel:42>");
    
    // Test cell display
    assert_eq!(Value::Cell(5).to_string(), "#<cell:5>");
    
    // Test tagged value display
    assert_eq!(
        Value::Tagged { tag: "point".to_string(), values: vec![Value::Integer(3), Value::Integer(4)] }.to_string(),
        "point(3 4)"
    );
}

#[test]
fn test_value_equality() {
    // Test basic equality
    assert_eq!(Value::Nil, Value::Nil);
    assert_eq!(Value::Boolean(true), Value::Boolean(true));
    assert_ne!(Value::Boolean(true), Value::Boolean(false));
    assert_eq!(Value::Integer(42), Value::Integer(42));
    assert_ne!(Value::Integer(42), Value::Integer(43));
    assert_eq!(Value::String("hello".to_string()), Value::String("hello".to_string()));
    assert_ne!(Value::String("hello".to_string()), Value::String("world".to_string()));
    
    // Test list equality
    assert_eq!(
        Value::List(vec![Value::Integer(1), Value::Integer(2)]),
        Value::List(vec![Value::Integer(1), Value::Integer(2)])
    );
    assert_ne!(
        Value::List(vec![Value::Integer(1), Value::Integer(2)]),
        Value::List(vec![Value::Integer(2), Value::Integer(1)])
    );
    
    // Test cross-type inequality
    assert_ne!(Value::Integer(42), Value::Float(42.0));
    assert_ne!(Value::Boolean(true), Value::Integer(1));
}

#[test]
fn test_value_complex_structures() {
    // Test complex nested structure
    let complex = Value::Map({
        let mut map = FxHashMap::default();
        map.insert("list".to_string(), Value::List(vec![
            Value::Integer(1),
            Value::Boolean(true),
            Value::String("test".to_string()),
        ]));
        map.insert("nested".to_string(), Value::Map({
            let mut inner = FxHashMap::default();
            inner.insert("a".to_string(), Value::Integer(10));
            inner.insert("b".to_string(), Value::Float(3.14));
            inner
        }));
        map
    });
    
    // Just ensure it can be displayed without panicking
    let _ = complex.to_string();
}