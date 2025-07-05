//! Unit tests for unboxed value representation

#[cfg(test)]
mod tests {
    use super::super::unboxed::*;
    use crate::bytecode::Value;
    use crate::safety::{PromiseId, ChannelId};
    use rustc_hash::FxHashMap;
    
    mod basic_unboxed_values {
        use super::*;
        
        #[test]
        fn test_int_unboxed() {
            let val = UnboxedValue::Int(42);
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "42");
            assert_eq!(format!("{:?}", val), "42");
            
            let val_neg = UnboxedValue::Int(-123);
            assert!(val_neg.is_truthy());
            assert_eq!(format!("{}", val_neg), "-123");
            
            let val_zero = UnboxedValue::Int(0);
            assert!(val_zero.is_truthy()); // 0 is truthy
        }
        
        #[test]
        fn test_float_unboxed() {
            let val = UnboxedValue::Float(3.14);
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "3.14");
            assert_eq!(format!("{:?}", val), "3.14");
            
            let val_zero = UnboxedValue::Float(0.0);
            assert!(val_zero.is_truthy()); // 0.0 is truthy
            
            let val_neg = UnboxedValue::Float(-2.5);
            assert!(val_neg.is_truthy());
        }
        
        #[test]
        fn test_bool_unboxed() {
            let val_true = UnboxedValue::Bool(true);
            assert!(val_true.is_truthy());
            assert_eq!(format!("{}", val_true), "true");
            
            let val_false = UnboxedValue::Bool(false);
            assert!(!val_false.is_truthy());
            assert_eq!(format!("{}", val_false), "false");
        }
        
        #[test]
        fn test_nil_unboxed() {
            let val = UnboxedValue::Nil;
            assert!(!val.is_truthy());
            assert_eq!(format!("{}", val), "nil");
            assert_eq!(format!("{:?}", val), "nil");
        }
    }
    
    mod boxed_values {
        use super::*;
        
        #[test]
        fn test_string_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::String("hello".to_string())));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "\"hello\"");
            
            let val_empty = UnboxedValue::Boxed(Box::new(BoxedValue::String(String::new())));
            assert!(val_empty.is_truthy()); // Empty string is truthy
            assert_eq!(format!("{}", val_empty), "\"\"");
        }
        
        #[test]
        fn test_list_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::List(vec![
                UnboxedValue::Int(1),
                UnboxedValue::Int(2),
                UnboxedValue::Int(3),
            ])));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "[1 2 3]");
            
            let val_empty = UnboxedValue::Boxed(Box::new(BoxedValue::List(vec![])));
            assert!(val_empty.is_truthy()); // Empty list is truthy
            assert_eq!(format!("{}", val_empty), "[]");
        }
        
        #[test]
        fn test_closure_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Closure {
                chunk_id: 5,
                captured_env: vec![UnboxedValue::Int(42)],
                param_count: 2,
            }));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<closure:5 params:2>");
        }
        
        #[test]
        fn test_native_func_boxed() {
            fn test_func(_stack: &mut Vec<UnboxedValue>) -> Result<UnboxedValue, String> {
                Ok(UnboxedValue::Int(42))
            }
            
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::NativeFunc {
                name: "test_func".to_string(),
                arity: 1,
                func: test_func,
            }));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<native:test_func arity:1>");
        }
        
        #[test]
        fn test_cell_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Cell(123)));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<cell:123>");
        }
        
        #[test]
        fn test_promise_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Promise(PromiseId(456))));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<promise:456>");
        }
        
        #[test]
        fn test_channel_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Channel(ChannelId(789))));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<channel:789>");
        }
        
        #[test]
        fn test_tagged_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Tagged {
                tag: "Some".to_string(),
                values: vec![UnboxedValue::Int(42)],
            }));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "(Some 42)");
            
            let val_empty = UnboxedValue::Boxed(Box::new(BoxedValue::Tagged {
                tag: "None".to_string(),
                values: vec![],
            }));
            assert!(val_empty.is_truthy());
            assert_eq!(format!("{}", val_empty), "(None)");
        }
        
        #[test]
        fn test_module_boxed() {
            let mut exports = FxHashMap::default();
            exports.insert("x".to_string(), UnboxedValue::Int(42));
            exports.insert("y".to_string(), UnboxedValue::Bool(true));
            
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::Module {
                name: "TestModule".to_string(),
                exports,
            }));
            assert!(val.is_truthy());
            assert_eq!(format!("{}", val), "<module TestModule with 2 exports>");
        }
    }
    
    mod value_conversion {
        use super::*;
        
        #[test]
        fn test_from_value_primitives() {
            assert!(matches!(UnboxedValue::from_value(Value::Nil), UnboxedValue::Nil));
            assert!(matches!(UnboxedValue::from_value(Value::Bool(true)), UnboxedValue::Bool(true)));
            assert!(matches!(UnboxedValue::from_value(Value::Int(42)), UnboxedValue::Int(42)));
            assert!(matches!(UnboxedValue::from_value(Value::Float(3.14)), UnboxedValue::Float(f) if f == 3.14));
        }
        
        #[test]
        fn test_from_value_string() {
            let val = UnboxedValue::from_value(Value::String("test".to_string()));
            match val {
                UnboxedValue::Boxed(boxed) => match &*boxed {
                    BoxedValue::String(s) => assert_eq!(s, "test"),
                    _ => panic!("Expected String"),
                },
                _ => panic!("Expected Boxed"),
            }
        }
        
        #[test]
        fn test_from_value_list() {
            let val = UnboxedValue::from_value(Value::List(vec![
                Value::Int(1),
                Value::Int(2),
            ]));
            match val {
                UnboxedValue::Boxed(boxed) => match &*boxed {
                    BoxedValue::List(items) => {
                        assert_eq!(items.len(), 2);
                        assert!(matches!(items[0], UnboxedValue::Int(1)));
                        assert!(matches!(items[1], UnboxedValue::Int(2)));
                    },
                    _ => panic!("Expected List"),
                },
                _ => panic!("Expected Boxed"),
            }
        }
        
        #[test]
        fn test_from_value_function() {
            let val = UnboxedValue::from_value(Value::Function {
                chunk_id: 5,
                env: vec![Value::Int(42)],
            });
            match val {
                UnboxedValue::Boxed(boxed) => match &*boxed {
                    BoxedValue::Closure { chunk_id, captured_env, param_count } => {
                        assert_eq!(*chunk_id, 5);
                        assert_eq!(captured_env.len(), 1);
                        assert_eq!(*param_count, 0); // Default param count
                    },
                    _ => panic!("Expected Closure"),
                },
                _ => panic!("Expected Boxed"),
            }
        }
        
        #[test]
        fn test_from_value_map() {
            let mut map = FxHashMap::default();
            map.insert("test".to_string(), Value::Int(42));
            let val = UnboxedValue::from_value(Value::Map(map));
            // Maps are not yet supported, converted to string
            match val {
                UnboxedValue::Boxed(boxed) => match &*boxed {
                    BoxedValue::String(s) => assert_eq!(s, "<map>"),
                    _ => panic!("Expected String placeholder for map"),
                },
                _ => panic!("Expected Boxed"),
            }
        }
        
        #[test]
        fn test_to_value_primitives() {
            assert_eq!(UnboxedValue::Nil.to_value(), Value::Nil);
            assert_eq!(UnboxedValue::Bool(false).to_value(), Value::Bool(false));
            assert_eq!(UnboxedValue::Int(-42).to_value(), Value::Int(-42));
            assert_eq!(UnboxedValue::Float(2.718).to_value(), Value::Float(2.718));
        }
        
        #[test]
        fn test_to_value_string() {
            let unboxed = UnboxedValue::Boxed(Box::new(BoxedValue::String("hello".to_string())));
            assert_eq!(unboxed.to_value(), Value::String("hello".to_string()));
        }
        
        #[test]
        fn test_to_value_list() {
            let unboxed = UnboxedValue::Boxed(Box::new(BoxedValue::List(vec![
                UnboxedValue::Int(1),
                UnboxedValue::Bool(true),
            ])));
            let value = unboxed.to_value();
            match value {
                Value::List(items) => {
                    assert_eq!(items.len(), 2);
                    assert_eq!(items[0], Value::Int(1));
                    assert_eq!(items[1], Value::Bool(true));
                },
                _ => panic!("Expected List"),
            }
        }
        
        #[test]
        fn test_round_trip_conversion() {
            let values = vec![
                Value::Nil,
                Value::Bool(true),
                Value::Int(42),
                Value::Float(3.14),
                Value::String("test".to_string()),
                Value::List(vec![Value::Int(1), Value::Int(2)]),
                Value::Cell(123),
                Value::Promise(PromiseId(456)),
                Value::Channel(ChannelId(789)),
                Value::Tagged {
                    tag: "Test".to_string(),
                    values: vec![Value::Int(42)],
                },
            ];
            
            for original in values {
                let unboxed = UnboxedValue::from_value(original.clone());
                let converted_back = unboxed.to_value();
                assert_eq!(original, converted_back);
            }
        }
    }
    
    mod arithmetic_operations {
        use super::*;
        
        #[test]
        fn test_add_int_int() {
            let a = UnboxedValue::Int(10);
            let b = UnboxedValue::Int(32);
            let result = a.add(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Int(42)));
        }
        
        #[test]
        fn test_add_float_float() {
            let a = UnboxedValue::Float(3.0);
            let b = UnboxedValue::Float(0.14);
            let result = a.add(&b).unwrap();
            match result {
                UnboxedValue::Float(f) => assert!((f - 3.14).abs() < 0.0001),
                _ => panic!("Expected Float"),
            }
        }
        
        #[test]
        fn test_add_int_float() {
            let a = UnboxedValue::Int(3);
            let b = UnboxedValue::Float(0.14);
            let result = a.add(&b).unwrap();
            match result {
                UnboxedValue::Float(f) => assert!((f - 3.14).abs() < 0.0001),
                _ => panic!("Expected Float"),
            }
        }
        
        #[test]
        fn test_add_float_int() {
            let a = UnboxedValue::Float(3.14);
            let b = UnboxedValue::Int(1);
            let result = a.add(&b).unwrap();
            match result {
                UnboxedValue::Float(f) => assert!((f - 4.14).abs() < 0.0001),
                _ => panic!("Expected Float"),
            }
        }
        
        #[test]
        fn test_add_overflow_to_float() {
            let a = UnboxedValue::Int(i64::MAX);
            let b = UnboxedValue::Int(1);
            let result = a.add(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Float(_)));
        }
        
        #[test]
        fn test_add_type_error() {
            let a = UnboxedValue::Int(42);
            let b = UnboxedValue::Bool(true);
            let result = a.add(&b);
            assert!(result.is_err());
        }
        
        #[test]
        fn test_sub_int_int() {
            let a = UnboxedValue::Int(50);
            let b = UnboxedValue::Int(8);
            let result = a.sub(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Int(42)));
        }
        
        #[test]
        fn test_sub_underflow_to_float() {
            let a = UnboxedValue::Int(i64::MIN);
            let b = UnboxedValue::Int(1);
            let result = a.sub(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Float(_)));
        }
        
        #[test]
        fn test_mul_int_int() {
            let a = UnboxedValue::Int(6);
            let b = UnboxedValue::Int(7);
            let result = a.mul(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Int(42)));
        }
        
        #[test]
        fn test_mul_overflow_to_float() {
            let a = UnboxedValue::Int(i64::MAX / 2);
            let b = UnboxedValue::Int(3);
            let result = a.mul(&b).unwrap();
            assert!(matches!(result, UnboxedValue::Float(_)));
        }
        
        #[test]
        fn test_mul_mixed_types() {
            let a = UnboxedValue::Int(6);
            let b = UnboxedValue::Float(7.0);
            let result = a.mul(&b).unwrap();
            match result {
                UnboxedValue::Float(f) => assert!((f - 42.0).abs() < 0.0001),
                _ => panic!("Expected Float"),
            }
        }
    }
    
    mod equality_operations {
        use super::*;
        
        #[test]
        fn test_eq_nil() {
            assert!(UnboxedValue::Nil.eq(&UnboxedValue::Nil));
            assert!(!UnboxedValue::Nil.eq(&UnboxedValue::Bool(false)));
        }
        
        #[test]
        fn test_eq_bool() {
            assert!(UnboxedValue::Bool(true).eq(&UnboxedValue::Bool(true)));
            assert!(UnboxedValue::Bool(false).eq(&UnboxedValue::Bool(false)));
            assert!(!UnboxedValue::Bool(true).eq(&UnboxedValue::Bool(false)));
        }
        
        #[test]
        fn test_eq_int() {
            assert!(UnboxedValue::Int(42).eq(&UnboxedValue::Int(42)));
            assert!(!UnboxedValue::Int(42).eq(&UnboxedValue::Int(43)));
        }
        
        #[test]
        fn test_eq_float() {
            assert!(UnboxedValue::Float(3.14).eq(&UnboxedValue::Float(3.14)));
            assert!(!UnboxedValue::Float(3.14).eq(&UnboxedValue::Float(2.718)));
        }
        
        #[test]
        fn test_eq_int_float() {
            assert!(UnboxedValue::Int(42).eq(&UnboxedValue::Float(42.0)));
            assert!(UnboxedValue::Float(42.0).eq(&UnboxedValue::Int(42)));
            assert!(!UnboxedValue::Int(42).eq(&UnboxedValue::Float(42.1)));
        }
        
        #[test]
        fn test_eq_different_types() {
            assert!(!UnboxedValue::Int(0).eq(&UnboxedValue::Bool(false)));
            assert!(!UnboxedValue::Int(1).eq(&UnboxedValue::Bool(true)));
            assert!(!UnboxedValue::Float(0.0).eq(&UnboxedValue::Nil));
        }
        
        #[test]
        fn test_eq_boxed_pointer() {
            let val1 = UnboxedValue::Boxed(Box::new(BoxedValue::String("test".to_string())));
            let val2 = UnboxedValue::Boxed(Box::new(BoxedValue::String("test".to_string())));
            // Boxed values use pointer equality, so different boxes are not equal
            assert!(!val1.eq(&val2));
            
            // Same reference should be equal
            assert!(val1.eq(&val1));
        }
    }
    
    mod clone_tests {
        use super::*;
        
        #[test]
        fn test_clone_primitives() {
            let values = vec![
                UnboxedValue::Nil,
                UnboxedValue::Bool(true),
                UnboxedValue::Int(42),
                UnboxedValue::Float(3.14),
            ];
            
            for val in values {
                let cloned = val.clone();
                assert!(val.eq(&cloned) || matches!((val, cloned), (UnboxedValue::Float(_), UnboxedValue::Float(_))));
            }
        }
        
        #[test]
        fn test_clone_boxed() {
            let val = UnboxedValue::Boxed(Box::new(BoxedValue::String("test".to_string())));
            let _cloned = val.clone();
            // Cloned boxed values will have different pointers
        }
    }
}