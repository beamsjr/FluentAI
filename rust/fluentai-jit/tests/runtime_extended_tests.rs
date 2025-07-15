//! Tests for extended runtime functions

use fluentai_jit::runtime_extended::*;
use fluentai_jit::value::{TaggedValue, value_to_tagged};
use fluentai_core::value::Value;

#[test]
fn test_map_operations() {
    // Test creating an empty map
    let map = jit_runtime_make_map(0, std::ptr::null());
    let value = TaggedValue(map as u64).to_value();
    assert!(matches!(value, Value::Map(m) if m.is_empty()));
    
    // Test creating a map with key-value pairs
    let key1 = value_to_tagged(&Value::String("foo".to_string())).0 as i64;
    let val1 = value_to_tagged(&Value::Integer(42)).0 as i64;
    let key2 = value_to_tagged(&Value::String("bar".to_string())).0 as i64;
    let val2 = value_to_tagged(&Value::String("baz".to_string())).0 as i64;
    
    let args = vec![key1, val1, key2, val2];
    let map = jit_runtime_make_map(4, args.as_ptr());
    let map_value = TaggedValue(map as u64).to_value();
    
    match map_value {
        Value::Map(m) => {
            assert_eq!(m.len(), 2);
            assert_eq!(m.get("foo"), Some(&Value::Integer(42)));
            assert_eq!(m.get("bar"), Some(&Value::String("baz".to_string())));
        }
        _ => panic!("Expected map"),
    }
    
    // Test map get
    let key = value_to_tagged(&Value::String("foo".to_string())).0 as i64;
    let result = jit_runtime_map_get(map, key);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Integer(42));
    
    // Test map contains
    let contains = jit_runtime_map_contains(map, key);
    let contains_value = TaggedValue(contains as u64).to_value();
    assert_eq!(contains_value, Value::Integer(1)); // true
    
    // Test map size
    let size = jit_runtime_map_size(map);
    let size_value = TaggedValue(size as u64).to_value();
    assert_eq!(size_value, Value::Integer(2));
}

#[test]
fn test_vector_operations() {
    // Test creating empty vector
    let vec = jit_runtime_make_vector(0, std::ptr::null());
    let value = TaggedValue(vec as u64).to_value();
    assert!(matches!(value, Value::Vector(v) if v.is_empty()));
    
    // Test creating vector with elements
    let elem1 = value_to_tagged(&Value::Integer(1)).0 as i64;
    let elem2 = value_to_tagged(&Value::Integer(2)).0 as i64;
    let elem3 = value_to_tagged(&Value::Integer(3)).0 as i64;
    
    let args = vec![elem1, elem2, elem3];
    let vec = jit_runtime_make_vector(3, args.as_ptr());
    let vec_value = TaggedValue(vec as u64).to_value();
    
    match vec_value {
        Value::Vector(v) => {
            assert_eq!(v.len(), 3);
            assert_eq!(v[0], Value::Integer(1));
            assert_eq!(v[1], Value::Integer(2));
            assert_eq!(v[2], Value::Integer(3));
        }
        _ => panic!("Expected vector"),
    }
    
    // Test vector length
    let len = jit_runtime_vector_len(vec);
    let len_value = TaggedValue(len as u64).to_value();
    assert_eq!(len_value, Value::Integer(3));
    
    // Test vector get
    let idx = value_to_tagged(&Value::Integer(1)).0 as i64;
    let elem = jit_runtime_vector_get(vec, idx);
    let elem_value = TaggedValue(elem as u64).to_value();
    assert_eq!(elem_value, Value::Integer(2));
    
    // Test vector push
    let new_elem = value_to_tagged(&Value::Integer(4)).0 as i64;
    let new_vec = jit_runtime_vector_push(vec, new_elem);
    let new_vec_value = TaggedValue(new_vec as u64).to_value();
    
    match new_vec_value {
        Value::Vector(v) => {
            assert_eq!(v.len(), 4);
            assert_eq!(v[3], Value::Integer(4));
        }
        _ => panic!("Expected vector"),
    }
}

#[test]
fn test_type_checking() {
    // Test is_type for different values
    let int_val = value_to_tagged(&Value::Integer(42)).0 as i64;
    assert_eq!(TaggedValue(jit_runtime_is_type(int_val, 0) as u64).to_value(), Value::Integer(1)); // is integer
    assert_eq!(TaggedValue(jit_runtime_is_type(int_val, 1) as u64).to_value(), Value::Integer(0)); // not float
    
    let str_val = value_to_tagged(&Value::String("hello".to_string())).0 as i64;
    assert_eq!(TaggedValue(jit_runtime_is_type(str_val, 2) as u64).to_value(), Value::Integer(1)); // is string
    
    let list_val = value_to_tagged(&Value::List(vec![])).0 as i64;
    assert_eq!(TaggedValue(jit_runtime_is_type(list_val, 6) as u64).to_value(), Value::Integer(1)); // is list
    
    // Test type_name
    let type_name = jit_runtime_type_name(int_val);
    let name_value = TaggedValue(type_name as u64).to_value();
    assert_eq!(name_value, Value::String("integer".to_string()));
    
    let type_name = jit_runtime_type_name(str_val);
    let name_value = TaggedValue(type_name as u64).to_value();
    assert_eq!(name_value, Value::String("string".to_string()));
}

#[test]
fn test_error_handling() {
    // Test creating an error
    let kind = value_to_tagged(&Value::String("RuntimeError".to_string())).0 as i64;
    let msg = value_to_tagged(&Value::String("Something went wrong".to_string())).0 as i64;
    
    let error = jit_runtime_make_error(kind, msg);
    let error_value = TaggedValue(error as u64).to_value();
    
    match error_value {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "RuntimeError");
            assert_eq!(message, "Something went wrong");
        }
        _ => panic!("Expected error"),
    }
    
    // Test is_error
    let is_err = jit_runtime_is_error(error);
    assert_eq!(TaggedValue(is_err as u64).to_value(), Value::Integer(1));
    
    let not_err = value_to_tagged(&Value::Integer(42)).0 as i64;
    let is_err = jit_runtime_is_error(not_err);
    assert_eq!(TaggedValue(is_err as u64).to_value(), Value::Integer(0));
}

#[test]
fn test_closure_operations() {
    // Test creating a closure
    let chunk_id = 42;
    let capture1 = value_to_tagged(&Value::Integer(10)).0 as i64;
    let capture2 = value_to_tagged(&Value::String("captured".to_string())).0 as i64;
    
    let captures = vec![capture1, capture2];
    let closure = jit_runtime_make_closure(chunk_id, 2, captures.as_ptr());
    let closure_value = TaggedValue(closure as u64).to_value();
    
    match closure_value {
        Value::Function { chunk_id: id, env } => {
            assert_eq!(id, 42);
            assert_eq!(env.len(), 2);
            assert_eq!(env[0], Value::Integer(10));
            assert_eq!(env[1], Value::String("captured".to_string()));
        }
        _ => panic!("Expected function"),
    }
    
    // Test accessing closure environment
    let env0 = jit_runtime_closure_get_env(closure, 0);
    let env0_value = TaggedValue(env0 as u64).to_value();
    assert_eq!(env0_value, Value::Integer(10));
    
    let env1 = jit_runtime_closure_get_env(closure, 1);
    let env1_value = TaggedValue(env1 as u64).to_value();
    assert_eq!(env1_value, Value::String("captured".to_string()));
}

#[test]
fn test_float_operations() {
    // Test float addition
    let a = value_to_tagged(&Value::Float(1.5)).0 as i64;
    let b = value_to_tagged(&Value::Float(2.5)).0 as i64;
    
    let result = jit_runtime_float_add(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Float(4.0));
    
    // Test mixed int/float addition
    let a = value_to_tagged(&Value::Integer(3)).0 as i64;
    let b = value_to_tagged(&Value::Float(2.5)).0 as i64;
    
    let result = jit_runtime_float_add(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Float(5.5));
    
    // Test float subtraction
    let a = value_to_tagged(&Value::Float(5.0)).0 as i64;
    let b = value_to_tagged(&Value::Float(2.0)).0 as i64;
    
    let result = jit_runtime_float_sub(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Float(3.0));
    
    // Test float multiplication
    let a = value_to_tagged(&Value::Float(3.0)).0 as i64;
    let b = value_to_tagged(&Value::Float(4.0)).0 as i64;
    
    let result = jit_runtime_float_mul(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Float(12.0));
    
    // Test float division
    let a = value_to_tagged(&Value::Float(10.0)).0 as i64;
    let b = value_to_tagged(&Value::Float(2.0)).0 as i64;
    
    let result = jit_runtime_float_div(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Float(5.0));
    
    // Test division by zero
    let a = value_to_tagged(&Value::Float(10.0)).0 as i64;
    let b = value_to_tagged(&Value::Float(0.0)).0 as i64;
    
    let result = jit_runtime_float_div(a, b);
    let result_value = TaggedValue(result as u64).to_value();
    assert!(matches!(result_value, Value::Error { kind, .. } if kind == "ArithmeticError"));
}

#[test]
fn test_channel_operations() {
    // Test creating a channel
    let channel = jit_runtime_make_channel();
    let channel_value = TaggedValue(channel as u64).to_value();
    assert!(matches!(channel_value, Value::Channel(_)));
    
    // Test channel send (placeholder - returns nil on success)
    let value = value_to_tagged(&Value::Integer(42)).0 as i64;
    let result = jit_runtime_channel_send(channel, value);
    let result_value = TaggedValue(result as u64).to_value();
    assert_eq!(result_value, Value::Nil);
    
    // Test channel receive (placeholder - returns error for now)
    let result = jit_runtime_channel_recv(channel);
    let result_value = TaggedValue(result as u64).to_value();
    assert!(matches!(result_value, Value::Error { .. }));
}

#[test]
fn test_promise_operations() {
    // Test creating a promise
    let promise = jit_runtime_make_promise();
    let promise_value = TaggedValue(promise as u64).to_value();
    assert!(matches!(promise_value, Value::Promise(_)));
    
    // Test promise await (placeholder - returns error for now)
    let result = jit_runtime_promise_await(promise);
    let result_value = TaggedValue(result as u64).to_value();
    assert!(matches!(result_value, Value::Error { .. }));
}