//! Tests for JIT list runtime operations

use fluentai_jit::runtime;
use fluentai_jit::value::{TaggedValue, value_to_tagged};
use fluentai_core::value::Value;

#[test]
fn test_runtime_make_list() {
    let items = vec![
        value_to_tagged(&Value::Integer(1)).0 as i64,
        value_to_tagged(&Value::Integer(2)).0 as i64,
        value_to_tagged(&Value::Integer(3)).0 as i64,
    ];
    
    let result_tagged = runtime::jit_runtime_make_list(3, items.as_ptr());
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::List(l) => {
            assert_eq!(l.len(), 3);
            assert!(matches!(l[0], Value::Integer(1)));
            assert!(matches!(l[1], Value::Integer(2)));
            assert!(matches!(l[2], Value::Integer(3)));
        }
        _ => panic!("Expected list result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_len() {
    let list = Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
    let tagged = value_to_tagged(&list);
    let result_tagged = runtime::jit_runtime_list_len(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Integer(n) => assert_eq!(n, 3, "Expected list length 3"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_empty() {
    // Test empty list
    let empty_list = Value::List(vec![]);
    let tagged = value_to_tagged(&empty_list);
    let result_tagged = runtime::jit_runtime_list_empty(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Integer(n) => assert_eq!(n, 1, "Expected 1 (true) for empty list"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
    
    // Test non-empty list
    let non_empty_list = Value::List(vec![Value::Integer(42)]);
    let tagged = value_to_tagged(&non_empty_list);
    let result_tagged = runtime::jit_runtime_list_empty(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Integer(n) => assert_eq!(n, 0, "Expected 0 (false) for non-empty list"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_head() {
    let list = Value::List(vec![Value::Integer(42), Value::Integer(99)]);
    let tagged = value_to_tagged(&list);
    let result_tagged = runtime::jit_runtime_list_head(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Integer(n) => assert_eq!(n, 42, "Expected first element"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_tail() {
    let list = Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
    let tagged = value_to_tagged(&list);
    let result_tagged = runtime::jit_runtime_list_tail(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::List(l) => {
            assert_eq!(l.len(), 2);
            assert!(matches!(l[0], Value::Integer(2)));
            assert!(matches!(l[1], Value::Integer(3)));
        }
        _ => panic!("Expected list result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_cons() {
    let elem = Value::Integer(0);
    let list = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
    let elem_tagged = value_to_tagged(&elem);
    let list_tagged = value_to_tagged(&list);
    
    let result_tagged = runtime::jit_runtime_list_cons(elem_tagged.0 as i64, list_tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::List(l) => {
            assert_eq!(l.len(), 3);
            assert!(matches!(l[0], Value::Integer(0)));
            assert!(matches!(l[1], Value::Integer(1)));
            assert!(matches!(l[2], Value::Integer(2)));
        }
        _ => panic!("Expected list result, got {:?}", result),
    }
}

#[test]
fn test_runtime_list_head_empty_error() {
    let list = Value::List(vec![]);
    let tagged = value_to_tagged(&list);
    let result_tagged = runtime::jit_runtime_list_head(tagged.0 as i64);
    let result = TaggedValue(result_tagged as u64).to_value();
    
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "RuntimeError");
            assert!(message.contains("empty list"));
        }
        _ => panic!("Expected error result for empty list"),
    }
}