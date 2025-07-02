//! Tests for core stdlib functions

use claudelang_stdlib::value::Value;
use claudelang_stdlib::{init_stdlib, StdlibRegistry};

fn setup() -> StdlibRegistry {
    init_stdlib()
}

#[test]
fn test_length() {
    let stdlib = setup();
    let length_fn = stdlib.get("length").unwrap();
    
    // Test empty list
    let result = length_fn.call(&[Value::List(vec![])]).unwrap();
    assert_eq!(result, Value::Int(0));
    
    // Test non-empty list
    let result = length_fn.call(&[Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
    ])]).unwrap();
    assert_eq!(result, Value::Int(3));
}

#[test]
fn test_append() {
    let stdlib = setup();
    let append_fn = stdlib.get("append").unwrap();
    
    let list = Value::List(vec![Value::Int(1), Value::Int(2)]);
    let elem = Value::Int(3);
    
    let result = append_fn.call(&[list, elem]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Int(1));
            assert_eq!(items[1], Value::Int(2));
            assert_eq!(items[2], Value::Int(3));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_reverse() {
    let stdlib = setup();
    let reverse_fn = stdlib.get("reverse").unwrap();
    
    let list = Value::List(vec![
        Value::Int(1),
        Value::Int(2),
        Value::Int(3),
        Value::Int(4),
    ]);
    
    let result = reverse_fn.call(&[list]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 4);
            assert_eq!(items[0], Value::Int(4));
            assert_eq!(items[1], Value::Int(3));
            assert_eq!(items[2], Value::Int(2));
            assert_eq!(items[3], Value::Int(1));
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_range() {
    let stdlib = setup();
    let range_fn = stdlib.get("range").unwrap();
    
    // Test single argument (0 to n)
    let result = range_fn.call(&[Value::Int(5)]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 5);
            for (i, item) in items.iter().enumerate() {
                assert_eq!(item, &Value::Int(i as i64));
            }
        }
        _ => panic!("Expected list"),
    }
    
    // Test two arguments (start to end)
    let result = range_fn.call(&[Value::Int(2), Value::Int(7)]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 5);
            for (i, item) in items.iter().enumerate() {
                assert_eq!(item, &Value::Int((i + 2) as i64));
            }
        }
        _ => panic!("Expected list"),
    }
    
    // Test three arguments (start to end with step)
    let result = range_fn.call(&[Value::Int(0), Value::Int(10), Value::Int(2)]).unwrap();
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 5);
            assert_eq!(items, vec![
                Value::Int(0),
                Value::Int(2),
                Value::Int(4),
                Value::Int(6),
                Value::Int(8),
            ]);
        }
        _ => panic!("Expected list"),
    }
}

#[test]
fn test_max_min() {
    let stdlib = setup();
    let max_fn = stdlib.get("max").unwrap();
    let min_fn = stdlib.get("min").unwrap();
    
    let args = vec![
        Value::Int(5),
        Value::Int(2),
        Value::Int(8),
        Value::Int(1),
        Value::Int(6),
    ];
    
    let max_result = max_fn.call(&args).unwrap();
    assert_eq!(max_result, Value::Int(8));
    
    let min_result = min_fn.call(&args).unwrap();
    assert_eq!(min_result, Value::Int(1));
    
    // Test with floats
    let float_args = vec![
        Value::Float(3.14),
        Value::Float(2.71),
        Value::Float(1.41),
    ];
    
    let max_float = max_fn.call(&float_args).unwrap();
    assert_eq!(max_float, Value::Float(3.14));
    
    let min_float = min_fn.call(&float_args).unwrap();
    assert_eq!(min_float, Value::Float(1.41));
}

#[test]
fn test_abs() {
    let stdlib = setup();
    let abs_fn = stdlib.get("abs").unwrap();
    
    // Test positive integer
    assert_eq!(abs_fn.call(&[Value::Int(42)]).unwrap(), Value::Int(42));
    
    // Test negative integer
    assert_eq!(abs_fn.call(&[Value::Int(-42)]).unwrap(), Value::Int(42));
    
    // Test positive float
    assert_eq!(abs_fn.call(&[Value::Float(3.14)]).unwrap(), Value::Float(3.14));
    
    // Test negative float
    assert_eq!(abs_fn.call(&[Value::Float(-3.14)]).unwrap(), Value::Float(3.14));
}

#[test]
fn test_type_predicates() {
    let stdlib = setup();
    
    // Test int?
    let int_pred = stdlib.get("int?").unwrap();
    assert_eq!(int_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(true));
    assert_eq!(int_pred.call(&[Value::Float(3.14)]).unwrap(), Value::Bool(false));
    assert_eq!(int_pred.call(&[Value::String("hello".to_string())]).unwrap(), Value::Bool(false));
    
    // Test float?
    let float_pred = stdlib.get("float?").unwrap();
    assert_eq!(float_pred.call(&[Value::Float(3.14)]).unwrap(), Value::Bool(true));
    assert_eq!(float_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(false));
    
    // Test string?
    let string_pred = stdlib.get("string?").unwrap();
    assert_eq!(string_pred.call(&[Value::String("hello".to_string())]).unwrap(), Value::Bool(true));
    assert_eq!(string_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(false));
    
    // Test list?
    let list_pred = stdlib.get("list?").unwrap();
    assert_eq!(list_pred.call(&[Value::List(vec![])]).unwrap(), Value::Bool(true));
    assert_eq!(list_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(false));
    
    // Test bool?
    let bool_pred = stdlib.get("bool?").unwrap();
    assert_eq!(bool_pred.call(&[Value::Bool(true)]).unwrap(), Value::Bool(true));
    assert_eq!(bool_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(false));
    
    // Test nil?
    let nil_pred = stdlib.get("nil?").unwrap();
    assert_eq!(nil_pred.call(&[Value::Nil]).unwrap(), Value::Bool(true));
    assert_eq!(nil_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(false));
    
    // Test number?
    let number_pred = stdlib.get("number?").unwrap();
    assert_eq!(number_pred.call(&[Value::Int(42)]).unwrap(), Value::Bool(true));
    assert_eq!(number_pred.call(&[Value::Float(3.14)]).unwrap(), Value::Bool(true));
    assert_eq!(number_pred.call(&[Value::String("42".to_string())]).unwrap(), Value::Bool(false));
}

#[test]
fn test_comparison_functions() {
    let stdlib = setup();
    
    // Test >
    let gt = stdlib.get(">").unwrap();
    assert_eq!(gt.call(&[Value::Int(5), Value::Int(3)]).unwrap(), Value::Bool(true));
    assert_eq!(gt.call(&[Value::Int(3), Value::Int(5)]).unwrap(), Value::Bool(false));
    assert_eq!(gt.call(&[Value::Float(3.14), Value::Float(2.71)]).unwrap(), Value::Bool(true));
    
    // Test <=
    let le = stdlib.get("<=").unwrap();
    assert_eq!(le.call(&[Value::Int(3), Value::Int(5)]).unwrap(), Value::Bool(true));
    assert_eq!(le.call(&[Value::Int(5), Value::Int(5)]).unwrap(), Value::Bool(true));
    assert_eq!(le.call(&[Value::Int(6), Value::Int(5)]).unwrap(), Value::Bool(false));
    
    // Test >=
    let ge = stdlib.get(">=").unwrap();
    assert_eq!(ge.call(&[Value::Int(5), Value::Int(3)]).unwrap(), Value::Bool(true));
    assert_eq!(ge.call(&[Value::Int(5), Value::Int(5)]).unwrap(), Value::Bool(true));
    assert_eq!(ge.call(&[Value::Int(4), Value::Int(5)]).unwrap(), Value::Bool(false));
    
    // Test !=
    let ne = stdlib.get("!=").unwrap();
    assert_eq!(ne.call(&[Value::Int(5), Value::Int(3)]).unwrap(), Value::Bool(true));
    assert_eq!(ne.call(&[Value::Int(5), Value::Int(5)]).unwrap(), Value::Bool(false));
    assert_eq!(ne.call(&[Value::String("hello".to_string()), Value::String("world".to_string())]).unwrap(), Value::Bool(true));
    assert_eq!(ne.call(&[Value::String("hello".to_string()), Value::String("hello".to_string())]).unwrap(), Value::Bool(false));
}

#[test]
fn test_xor() {
    let stdlib = setup();
    let xor = stdlib.get("xor").unwrap();
    
    assert_eq!(xor.call(&[Value::Bool(true), Value::Bool(true)]).unwrap(), Value::Bool(false));
    assert_eq!(xor.call(&[Value::Bool(true), Value::Bool(false)]).unwrap(), Value::Bool(true));
    assert_eq!(xor.call(&[Value::Bool(false), Value::Bool(true)]).unwrap(), Value::Bool(true));
    assert_eq!(xor.call(&[Value::Bool(false), Value::Bool(false)]).unwrap(), Value::Bool(false));
}

#[test]
fn test_modulo() {
    let stdlib = setup();
    let modulo = stdlib.get("mod").unwrap();
    
    assert_eq!(modulo.call(&[Value::Int(10), Value::Int(3)]).unwrap(), Value::Int(1));
    assert_eq!(modulo.call(&[Value::Int(20), Value::Int(5)]).unwrap(), Value::Int(0));
    assert_eq!(modulo.call(&[Value::Int(7), Value::Int(4)]).unwrap(), Value::Int(3));
    
    // Test division by zero
    assert!(modulo.call(&[Value::Int(10), Value::Int(0)]).is_err());
}