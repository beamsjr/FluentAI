//! Tests for functional module

use fluentai_stdlib::init_stdlib;
use fluentai_stdlib::value::Value;

#[test]
fn test_identity() {
    let stdlib = init_stdlib();
    let identity = stdlib.get("identity").unwrap();

    let val = Value::Integer(42);
    let result = identity.call(&[val.clone()]);
    assert_eq!(result.unwrap(), val);
}

#[test]
fn test_identity_error() {
    let stdlib = init_stdlib();
    let identity = stdlib.get("identity").unwrap();

    let result = identity.call(&[]);
    assert!(result.is_err());
}

#[test]
fn test_replicate() {
    let stdlib = init_stdlib();
    let replicate = stdlib.get("replicate").unwrap();

    let result = replicate.call(&[Value::String("hello".to_string()), Value::Integer(3)]);

    let expected = Value::List(vec![
        Value::String("hello".to_string()),
        Value::String("hello".to_string()),
        Value::String("hello".to_string()),
    ]);

    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_replicate_zero() {
    let stdlib = init_stdlib();
    let replicate = stdlib.get("replicate").unwrap();

    let result = replicate.call(&[Value::String("hello".to_string()), Value::Integer(0)]);

    assert_eq!(result.unwrap(), Value::List(vec![]));
}

#[test]
fn test_replicate_error_wrong_args() {
    let stdlib = init_stdlib();
    let replicate = stdlib.get("replicate").unwrap();

    let result = replicate.call(&[Value::Integer(3)]);
    assert!(result.is_err());
}

#[test]
fn test_replicate_error_non_integer() {
    let stdlib = init_stdlib();
    let replicate = stdlib.get("replicate").unwrap();

    let result = replicate.call(&[
        Value::String("hello".to_string()),
        Value::String("not a number".to_string()),
    ]);
    assert!(result.is_err());
}

#[test]
fn test_repeat() {
    let stdlib = init_stdlib();
    let repeat = stdlib.get("repeat").unwrap();

    // Test repeat function (arguments: value, count)
    let result = repeat.call(&[Value::String("x".to_string()), Value::Integer(2)]);
    assert_eq!(
        result.unwrap(),
        Value::List(vec![
            Value::String("x".to_string()),
            Value::String("x".to_string()),
        ])
    );
}

#[test]
fn test_chunk() {
    let stdlib = init_stdlib();
    let chunk = stdlib.get("chunk").unwrap();

    let result = chunk.call(&[
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
        ]),
        Value::Integer(2),
    ]);

    let expected = Value::List(vec![
        Value::List(vec![Value::Integer(1), Value::Integer(2)]),
        Value::List(vec![Value::Integer(3), Value::Integer(4)]),
        Value::List(vec![Value::Integer(5)]),
    ]);

    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_chunk_exact_division() {
    let stdlib = init_stdlib();
    let chunk = stdlib.get("chunk").unwrap();

    let result = chunk.call(&[
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
            Value::Integer(6),
        ]),
        Value::Integer(3),
    ]);

    let expected = Value::List(vec![
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]),
        Value::List(vec![
            Value::Integer(4),
            Value::Integer(5),
            Value::Integer(6),
        ]),
    ]);

    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_chunk_empty_list() {
    let stdlib = init_stdlib();
    let chunk = stdlib.get("chunk").unwrap();

    let result = chunk.call(&[Value::List(vec![]), Value::Integer(2)]);

    assert_eq!(result.unwrap(), Value::List(vec![]));
}

#[test]
fn test_chunk_invalid_size() {
    let stdlib = init_stdlib();
    let chunk = stdlib.get("chunk").unwrap();

    let result = chunk.call(&[Value::List(vec![Value::Integer(1)]), Value::Integer(0)]);

    assert!(result.is_err());
}

#[test]
fn test_sliding_window() {
    let stdlib = init_stdlib();
    let sliding_window = stdlib.get("sliding-window").unwrap();

    let result = sliding_window.call(&[
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
        ]),
        Value::Integer(3),
    ]);

    let expected = Value::List(vec![
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]),
        Value::List(vec![
            Value::Integer(2),
            Value::Integer(3),
            Value::Integer(4),
        ]),
        Value::List(vec![
            Value::Integer(3),
            Value::Integer(4),
            Value::Integer(5),
        ]),
    ]);

    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_sliding_window_size_one() {
    let stdlib = init_stdlib();
    let sliding_window = stdlib.get("sliding-window").unwrap();

    let result = sliding_window.call(&[
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]),
        Value::Integer(1),
    ]);

    let expected = Value::List(vec![
        Value::List(vec![Value::Integer(1)]),
        Value::List(vec![Value::Integer(2)]),
        Value::List(vec![Value::Integer(3)]),
    ]);

    assert_eq!(result.unwrap(), expected);
}

#[test]
fn test_sliding_window_larger_than_list() {
    let stdlib = init_stdlib();
    let sliding_window = stdlib.get("sliding-window").unwrap();

    let result = sliding_window.call(&[
        Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]),
        Value::Integer(5),
    ]);

    assert_eq!(result.unwrap(), Value::List(vec![]));
}

#[test]
fn test_sliding_window_empty_list() {
    let stdlib = init_stdlib();
    let sliding_window = stdlib.get("sliding-window").unwrap();

    let result = sliding_window.call(&[Value::List(vec![]), Value::Integer(2)]);

    assert_eq!(result.unwrap(), Value::List(vec![]));
}

#[test]
fn test_functional_vm_integration_errors() {
    let stdlib = init_stdlib();

    // Test functions that require VM integration
    let compose = stdlib.get("compose").unwrap();
    assert!(compose.call(&[Value::Nil, Value::Nil]).is_err());

    let curry = stdlib.get("curry").unwrap();
    assert!(curry.call(&[Value::Nil]).is_err());

    let memoize = stdlib.get("memoize").unwrap();
    assert!(memoize.call(&[Value::Nil]).is_err());
}
