//! Tests for JIT string runtime operations

use fluentai_jit::runtime;
use fluentai_jit::value::{TaggedValue, value_to_tagged};
use fluentai_core::value::Value;

#[test]
fn test_runtime_string_len() {
    let s = Value::String("hello".to_string());
    let tagged = value_to_tagged(&s);
    let result_tagged = runtime::jit_runtime_string_len(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Integer(n) => assert_eq!(n, 5, "Expected string length 5"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_runtime_string_concat() {
    let a = Value::String("hello".to_string());
    let b = Value::String(" world".to_string());
    let a_tagged = value_to_tagged(&a);
    let b_tagged = value_to_tagged(&b);
    
    let result_tagged = runtime::jit_runtime_string_concat(a_tagged.0 as i64, b_tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::String(s) => assert_eq!(s, "hello world", "Expected concatenated string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_runtime_string_upper() {
    let s = Value::String("hello".to_string());
    let tagged = value_to_tagged(&s);
    let result_tagged = runtime::jit_runtime_string_upper(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::String(s) => assert_eq!(s, "HELLO", "Expected uppercase string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_runtime_string_lower() {
    let s = Value::String("HELLO".to_string());
    let tagged = value_to_tagged(&s);
    let result_tagged = runtime::jit_runtime_string_lower(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::String(s) => assert_eq!(s, "hello", "Expected lowercase string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_runtime_string_len_error() {
    let n = Value::Integer(42);
    let tagged = value_to_tagged(&n);
    let result_tagged = runtime::jit_runtime_string_len(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "TypeError");
            assert!(message.contains("string"));
        }
        _ => panic!("Expected error result for non-string input, got: {:?}", result),
    }
}