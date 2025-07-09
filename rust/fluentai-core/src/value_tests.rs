#[cfg(test)]
mod tests {
    use crate::ast::NodeId;
    use crate::value::*;
    use rustc_hash::FxHashMap;
    use std::sync::Arc;

    // ===== Basic Value Tests =====

    #[test]
    fn test_value_type_predicates() {
        // Integer
        let int_val = Value::Integer(42);
        assert!(int_val.is_integer());
        assert!(int_val.is_number());
        assert!(!int_val.is_float());
        assert!(!int_val.is_string());

        // Float
        let float_val = Value::Float(3.14);
        assert!(float_val.is_float());
        assert!(float_val.is_number());
        assert!(!float_val.is_integer());

        // String
        let string_val = Value::String("hello".to_string());
        assert!(string_val.is_string());
        assert!(!string_val.is_symbol());

        // Symbol
        let symbol_val = Value::Symbol("foo".to_string());
        assert!(symbol_val.is_symbol());
        assert!(!symbol_val.is_string());

        // Boolean
        let bool_val = Value::Boolean(true);
        assert!(bool_val.is_boolean());

        // Nil
        let nil_val = Value::Nil;
        assert!(nil_val.is_nil());

        // List
        let list_val = Value::List(vec![]);
        assert!(list_val.is_list());
        assert!(!list_val.is_vector());

        // Vector
        let vector_val = Value::Vector(vec![]);
        assert!(vector_val.is_vector());
        assert!(!vector_val.is_list());

        // Map
        let map_val = Value::Map(FxHashMap::default());
        assert!(map_val.is_map());

        // Tagged
        let tagged_val = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![],
        };
        assert!(tagged_val.is_tagged());
    }

    #[test]
    fn test_procedure_creation() {
        let body_id = NodeId::new(1).unwrap();
        let proc = Procedure {
            name: Some("add".to_string()),
            params: vec!["x".to_string(), "y".to_string()],
            body: body_id,
            env: None,
        };

        let proc_val = Value::Procedure(Arc::new(proc));
        assert!(proc_val.is_procedure());
        assert!(proc_val.is_callable());
        assert!(!proc_val.is_list());
    }

    #[test]
    fn test_native_function() {
        let native_fn = Value::NativeFunction {
            name: "print".to_string(),
            arity: 1,
            function: Arc::new(|args| {
                assert_eq!(args.len(), 1);
                Ok(Value::Nil)
            }),
        };

        assert!(native_fn.is_callable());
        assert!(!native_fn.is_procedure());
    }

    // ===== Type Conversion Tests =====

    #[test]
    fn test_as_integer() {
        let int_val = Value::Integer(42);
        assert_eq!(int_val.as_integer().unwrap(), 42);

        let float_val = Value::Float(3.14);
        assert!(float_val.as_integer().is_err());

        let err = float_val.as_integer().unwrap_err();
        match err {
            ValueError::TypeError { expected, actual } => {
                assert_eq!(expected, "integer");
                assert_eq!(actual, "float");
            }
            _ => panic!("Wrong error type"),
        }
    }

    #[test]
    fn test_as_float() {
        let float_val = Value::Float(3.14);
        assert_eq!(float_val.as_float().unwrap(), 3.14);

        let int_val = Value::Integer(42);
        assert!(int_val.as_float().is_err());
    }

    #[test]
    fn test_as_number() {
        let int_val = Value::Integer(42);
        assert_eq!(int_val.as_number().unwrap(), 42.0);

        let float_val = Value::Float(3.14);
        assert_eq!(float_val.as_number().unwrap(), 3.14);

        let string_val = Value::String("hello".to_string());
        assert!(string_val.as_number().is_err());
    }

    #[test]
    fn test_as_string() {
        let string_val = Value::String("hello".to_string());
        assert_eq!(string_val.as_string().unwrap(), "hello");

        let symbol_val = Value::Symbol("world".to_string());
        assert!(symbol_val.as_string().is_err());
    }

    #[test]
    fn test_as_symbol() {
        let symbol_val = Value::Symbol("foo".to_string());
        assert_eq!(symbol_val.as_symbol().unwrap(), "foo");

        let string_val = Value::String("bar".to_string());
        assert!(string_val.as_symbol().is_err());
    }

    #[test]
    fn test_as_boolean() {
        let bool_val = Value::Boolean(true);
        assert_eq!(bool_val.as_boolean().unwrap(), true);

        let nil_val = Value::Nil;
        assert!(nil_val.as_boolean().is_err());
    }

    #[test]
    fn test_as_list() {
        let items = vec![Value::Integer(1), Value::Integer(2)];
        let list_val = Value::List(items.clone());
        let list_ref = list_val.as_list().unwrap();
        assert_eq!(list_ref.len(), 2);

        let vector_val = Value::Vector(vec![]);
        assert!(vector_val.as_list().is_err());
    }

    #[test]
    fn test_as_vector() {
        let items = vec![Value::Integer(1), Value::Integer(2)];
        let vector_val = Value::Vector(items.clone());
        let vec_ref = vector_val.as_vector().unwrap();
        assert_eq!(vec_ref.len(), 2);

        let list_val = Value::List(vec![]);
        assert!(list_val.as_vector().is_err());
    }

    #[test]
    fn test_as_map() {
        let mut map = FxHashMap::default();
        map.insert("key".to_string(), Value::Integer(42));
        let map_val = Value::Map(map);
        let map_ref = map_val.as_map().unwrap();
        assert_eq!(map_ref.len(), 1);

        let list_val = Value::List(vec![]);
        assert!(list_val.as_map().is_err());
    }

    #[test]
    fn test_as_procedure() {
        let body_id = NodeId::new(1).unwrap();
        let proc = Procedure {
            name: None,
            params: vec![],
            body: body_id,
            env: None,
        };
        let proc_val = Value::Procedure(Arc::new(proc));
        let proc_ref = proc_val.as_procedure().unwrap();
        assert_eq!(proc_ref.params.len(), 0);

        let int_val = Value::Integer(42);
        assert!(int_val.as_procedure().is_err());
    }

    #[test]
    fn test_as_tagged() {
        let tagged_val = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(42)],
        };
        let (tag, values) = tagged_val.as_tagged().unwrap();
        assert_eq!(tag, "Some");
        assert_eq!(values.len(), 1);

        let nil_val = Value::Nil;
        assert!(nil_val.as_tagged().is_err());
    }

    // ===== Type Name Tests =====

    #[test]
    fn test_type_names() {
        assert_eq!(Value::Integer(42).type_name(), "integer");
        assert_eq!(Value::Float(3.14).type_name(), "float");
        assert_eq!(Value::String("hi".to_string()).type_name(), "string");
        assert_eq!(Value::Symbol("sym".to_string()).type_name(), "symbol");
        assert_eq!(Value::Boolean(true).type_name(), "boolean");
        assert_eq!(Value::Nil.type_name(), "nil");
        assert_eq!(Value::List(vec![]).type_name(), "list");
        assert_eq!(Value::Vector(vec![]).type_name(), "vector");
        assert_eq!(Value::Map(FxHashMap::default()).type_name(), "map");
        assert_eq!(
            Value::Tagged {
                tag: "T".to_string(),
                values: vec![]
            }
            .type_name(),
            "tagged"
        );

        let native_fn = Value::NativeFunction {
            name: "f".to_string(),
            arity: 0,
            function: Arc::new(|_| Ok(Value::Nil)),
        };
        assert_eq!(native_fn.type_name(), "native-function");

        let proc = Value::Procedure(Arc::new(Procedure {
            name: None,
            params: vec![],
            body: NodeId::new(1).unwrap(),
            env: None,
        }));
        assert_eq!(proc.type_name(), "procedure");
    }

    // ===== Truthiness Tests =====

    #[test]
    fn test_is_truthy() {
        // Only nil and false are falsy
        assert!(!Value::Nil.is_truthy());
        assert!(!Value::Boolean(false).is_truthy());

        // Everything else is truthy
        assert!(Value::Boolean(true).is_truthy());
        assert!(Value::Integer(0).is_truthy());
        assert!(Value::Integer(42).is_truthy());
        assert!(Value::Float(0.0).is_truthy());
        assert!(Value::String("".to_string()).is_truthy());
        assert!(Value::String("hello".to_string()).is_truthy());
        assert!(Value::List(vec![]).is_truthy());
        assert!(Value::Vector(vec![]).is_truthy());
        assert!(Value::Map(FxHashMap::default()).is_truthy());
    }

    // ===== Equality Tests =====

    #[test]
    fn test_value_equality() {
        // Same values are equal
        assert_eq!(Value::Integer(42), Value::Integer(42));
        assert_eq!(Value::Float(3.14), Value::Float(3.14));
        assert_eq!(
            Value::String("hello".to_string()),
            Value::String("hello".to_string())
        );
        assert_eq!(
            Value::Symbol("foo".to_string()),
            Value::Symbol("foo".to_string())
        );
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_eq!(Value::Nil, Value::Nil);

        // Different values are not equal
        assert_ne!(Value::Integer(42), Value::Integer(43));
        assert_ne!(Value::Boolean(true), Value::Boolean(false));
        assert_ne!(
            Value::String("hello".to_string()),
            Value::String("world".to_string())
        );

        // Different types are not equal
        assert_ne!(Value::Integer(42), Value::Float(42.0));
        assert_ne!(Value::String("42".to_string()), Value::Integer(42));
    }

    #[test]
    fn test_float_equality_epsilon() {
        // Floats use epsilon comparison
        let val1 = Value::Float(1.0);
        let val2 = Value::Float(1.0 + f64::EPSILON / 2.0);
        assert_eq!(val1, val2);

        let val3 = Value::Float(1.0);
        let val4 = Value::Float(1.0 + f64::EPSILON * 2.0);
        assert_ne!(val3, val4);
    }

    #[test]
    fn test_list_equality() {
        let list1 = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
        let list2 = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
        let list3 = Value::List(vec![Value::Integer(1), Value::Integer(3)]);

        assert_eq!(list1, list2);
        assert_ne!(list1, list3);

        // Empty lists are equal
        assert_eq!(Value::List(vec![]), Value::List(vec![]));
    }

    #[test]
    fn test_deep_equality() {
        // Test nested structures
        let nested1 = Value::List(vec![
            Value::Integer(1),
            Value::List(vec![Value::Integer(2), Value::Integer(3)]),
        ]);
        let nested2 = Value::List(vec![
            Value::Integer(1),
            Value::List(vec![Value::Integer(2), Value::Integer(3)]),
        ]);
        let nested3 = Value::List(vec![
            Value::Integer(1),
            Value::List(vec![Value::Integer(2), Value::Integer(4)]),
        ]);

        assert!(nested1.deep_eq(&nested2));
        assert!(!nested1.deep_eq(&nested3));
    }

    #[test]
    fn test_map_deep_equality() {
        let mut map1 = FxHashMap::default();
        map1.insert("a".to_string(), Value::Integer(1));
        map1.insert("b".to_string(), Value::Integer(2));

        let mut map2 = FxHashMap::default();
        map2.insert("b".to_string(), Value::Integer(2));
        map2.insert("a".to_string(), Value::Integer(1));

        let mut map3 = FxHashMap::default();
        map3.insert("a".to_string(), Value::Integer(1));
        map3.insert("b".to_string(), Value::Integer(3));

        assert!(Value::Map(map1.clone()).deep_eq(&Value::Map(map2)));
        assert!(!Value::Map(map1).deep_eq(&Value::Map(map3)));
    }

    #[test]
    fn test_procedure_equality() {
        let body_id = NodeId::new(1).unwrap();
        let proc1 = Arc::new(Procedure {
            name: Some("add".to_string()),
            params: vec!["x".to_string(), "y".to_string()],
            body: body_id,
            env: None,
        });

        let proc2 = Arc::new(Procedure {
            name: Some("add".to_string()),
            params: vec!["x".to_string(), "y".to_string()],
            body: body_id,
            env: None,
        });

        // Different Arc instances but same content
        assert!(proc1.as_ref() == proc2.as_ref());

        // Using Value equality - checks Arc pointer equality
        let val1 = Value::Procedure(proc1.clone());
        let val2 = Value::Procedure(proc1.clone());
        let val3 = Value::Procedure(proc2);

        assert_eq!(val1, val2); // Same Arc
        assert_ne!(val1, val3); // Different Arc
    }

    // ===== Numeric Comparison Tests =====

    #[test]
    fn test_numeric_comparison() {
        use std::cmp::Ordering;

        // Integer comparison
        let int1 = Value::Integer(10);
        let int2 = Value::Integer(20);
        assert_eq!(int1.compare_numeric(&int2).unwrap(), Ordering::Less);
        assert_eq!(int2.compare_numeric(&int1).unwrap(), Ordering::Greater);
        assert_eq!(int1.compare_numeric(&int1).unwrap(), Ordering::Equal);

        // Float comparison
        let float1 = Value::Float(3.14);
        let float2 = Value::Float(2.71);
        assert_eq!(float1.compare_numeric(&float2).unwrap(), Ordering::Greater);

        // Mixed comparison
        let int_val = Value::Integer(5);
        let float_val = Value::Float(5.0);
        assert_eq!(
            int_val.compare_numeric(&float_val).unwrap(),
            Ordering::Equal
        );

        let float_val2 = Value::Float(4.5);
        assert_eq!(
            int_val.compare_numeric(&float_val2).unwrap(),
            Ordering::Greater
        );
    }

    #[test]
    fn test_numeric_comparison_errors() {
        let num = Value::Integer(42);
        let string = Value::String("hello".to_string());

        assert!(num.compare_numeric(&string).is_err());

        let err = num.compare_numeric(&string).unwrap_err();
        match err {
            ValueError::InvalidOperation(msg) => {
                assert!(msg.contains("Cannot compare"));
            }
            _ => panic!("Wrong error type"),
        }
    }

    // ===== Display Tests =====

    #[test]
    fn test_value_display() {
        assert_eq!(format!("{}", Value::Integer(42)), "42");
        assert_eq!(format!("{}", Value::Float(3.14)), "3.14");
        assert_eq!(
            format!("{}", Value::String("hello".to_string())),
            "\"hello\""
        );
        assert_eq!(format!("{}", Value::Symbol("foo".to_string())), "foo");
        assert_eq!(format!("{}", Value::Boolean(true)), "#t");
        assert_eq!(format!("{}", Value::Boolean(false)), "#f");
        assert_eq!(format!("{}", Value::Nil), "nil");

        // List display
        let list = Value::List(vec![
            Value::Integer(1),
            Value::Integer(2),
            Value::Integer(3),
        ]);
        assert_eq!(format!("{}", list), "(1 2 3)");

        // Empty list
        assert_eq!(format!("{}", Value::List(vec![])), "()");

        // Vector display
        let vector = Value::Vector(vec![Value::Integer(1), Value::Integer(2)]);
        assert_eq!(format!("{}", vector), "#(1 2)");

        // Procedure display
        let proc = Value::Procedure(Arc::new(Procedure {
            name: None,
            params: vec![],
            body: NodeId::new(1).unwrap(),
            env: None,
        }));
        assert_eq!(format!("{}", proc), "#<procedure>");

        // Native function display
        let native = Value::NativeFunction {
            name: "print".to_string(),
            arity: 1,
            function: Arc::new(|_| Ok(Value::Nil)),
        };
        assert_eq!(format!("{}", native), "#<native:print>");

        // Tagged display
        let tagged = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(42)],
        };
        assert_eq!(format!("{}", tagged), "Some(42)");

        // Map display
        assert_eq!(
            format!("{}", Value::Map(FxHashMap::default())),
            "#<hashmap>"
        );
    }

    #[test]
    fn test_nested_display() {
        let nested = Value::List(vec![
            Value::Integer(1),
            Value::List(vec![Value::Integer(2), Value::Integer(3)]),
            Value::Symbol("foo".to_string()),
        ]);
        assert_eq!(format!("{}", nested), "(1 (2 3) foo)");
    }

    // ===== Debug Tests =====

    #[test]
    fn test_value_debug() {
        let int_val = Value::Integer(42);
        assert_eq!(format!("{:?}", int_val), "Integer(42)");

        let string_val = Value::String("hello".to_string());
        assert_eq!(format!("{:?}", string_val), "String(\"hello\")");

        let list_val = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
        assert!(format!("{:?}", list_val).contains("Integer(1)"));
        assert!(format!("{:?}", list_val).contains("Integer(2)"));
    }

    // ===== Tagged Value Tests =====

    #[test]
    fn test_tagged_values() {
        let option_some = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(42)],
        };

        let option_none = Value::Tagged {
            tag: "None".to_string(),
            values: vec![],
        };

        assert!(option_some.is_tagged());
        assert!(option_none.is_tagged());

        let (tag, values) = option_some.as_tagged().unwrap();
        assert_eq!(tag, "Some");
        assert_eq!(values.len(), 1);

        let (tag, values) = option_none.as_tagged().unwrap();
        assert_eq!(tag, "None");
        assert_eq!(values.len(), 0);
    }

    #[test]
    fn test_tagged_equality() {
        let some1 = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(42)],
        };

        let some2 = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(42)],
        };

        let some3 = Value::Tagged {
            tag: "Some".to_string(),
            values: vec![Value::Integer(43)],
        };

        let none = Value::Tagged {
            tag: "None".to_string(),
            values: vec![],
        };

        assert_eq!(some1, some2);
        assert_ne!(some1, some3);
        assert_ne!(some1, none);

        // Deep equality
        assert!(some1.deep_eq(&some2));
        assert!(!some1.deep_eq(&some3));
    }

    // ===== Error Tests =====

    #[test]
    fn test_value_error_type_error() {
        let err = ValueError::TypeError {
            expected: "integer",
            actual: "string",
        };

        assert_eq!(
            format!("{}", err),
            "Type error: expected integer, got string"
        );
        assert!(err.to_string().contains("Type error"));

        // Test debug format
        let debug_str = format!("{:?}", err);
        assert!(debug_str.contains("TypeError"));
    }

    #[test]
    fn test_value_error_index_out_of_bounds() {
        let err = ValueError::IndexOutOfBounds {
            index: 5,
            length: 3,
        };

        assert_eq!(
            format!("{}", err),
            "Index 5 out of bounds for list of length 3"
        );
        assert!(err.to_string().contains("out of bounds"));
    }

    #[test]
    fn test_value_error_key_not_found() {
        let err = ValueError::KeyNotFound("missing_key".to_string());

        assert_eq!(format!("{}", err), "Key not found: missing_key");
        assert!(err.to_string().contains("Key not found"));
    }

    #[test]
    fn test_value_error_invalid_operation() {
        let err = ValueError::InvalidOperation("Cannot divide by zero".to_string());

        assert_eq!(
            format!("{}", err),
            "Invalid operation: Cannot divide by zero"
        );
        assert!(err.to_string().contains("Invalid operation"));
    }

    #[test]
    fn test_value_error_conversion_error() {
        let err = ValueError::ConversionError {
            from: "string",
            to: "integer",
            reason: "invalid number format".to_string(),
        };

        assert_eq!(
            format!("{}", err),
            "Cannot convert string to integer: invalid number format"
        );
        assert!(err.to_string().contains("Cannot convert"));
    }

    #[test]
    fn test_value_error_division_by_zero() {
        let err = ValueError::DivisionByZero;

        assert_eq!(format!("{}", err), "Division by zero");
    }

    #[test]
    fn test_value_error_arity_mismatch() {
        let err = ValueError::ArityMismatch {
            expected: 2,
            actual: 3,
        };

        assert_eq!(format!("{}", err), "Function expects 2 arguments, got 3");
        assert!(err.to_string().contains("expects 2 arguments"));
    }

    #[test]
    fn test_value_error_equality() {
        let err1 = ValueError::TypeError {
            expected: "integer",
            actual: "string",
        };
        let err2 = ValueError::TypeError {
            expected: "integer",
            actual: "string",
        };
        let err3 = ValueError::TypeError {
            expected: "float",
            actual: "string",
        };

        assert_eq!(err1, err2);
        assert_ne!(err1, err3);

        // Test clone
        let err_clone = err1.clone();
        assert_eq!(err1, err_clone);
    }

    #[test]
    fn test_value_error_std_error() {
        use std::error::Error;

        let err = ValueError::DivisionByZero;

        // Test that it implements std::error::Error
        let _: &dyn Error = &err;

        // Source should be None for these errors
        assert!(err.source().is_none());
    }

    #[test]
    fn test_value_result_type() {
        // Test Ok case
        let result: ValueResult<i32> = Ok(42);
        assert_eq!(result.unwrap(), 42);

        // Test Err case
        let result: ValueResult<i32> = Err(ValueError::DivisionByZero);
        assert!(result.is_err());
        match result {
            Err(ValueError::DivisionByZero) => {}
            _ => panic!("Wrong error type"),
        }
    }

    // ===== Additional Coverage Tests =====

    #[test]
    fn test_deep_eq_edge_cases() {
        // Test deep_eq with different types
        assert!(!Value::Integer(42).deep_eq(&Value::String("42".to_string())));
        assert!(!Value::List(vec![]).deep_eq(&Value::Vector(vec![])));
        assert!(!Value::Symbol("sym".to_string()).deep_eq(&Value::String("sym".to_string())));

        // Test deep_eq with NativeFunction
        let native1 = Value::NativeFunction {
            name: "test".to_string(),
            arity: 1,
            function: Arc::new(|_| Ok(Value::Nil)),
        };
        let native2 = Value::NativeFunction {
            name: "test".to_string(),
            arity: 1,
            function: Arc::new(|_| Ok(Value::Nil)),
        };
        // Different function pointers, so not equal
        assert!(!native1.deep_eq(&native2));

        // Test deep_eq with complex nested structures
        let map1 = {
            let mut m = FxHashMap::default();
            m.insert(
                "list".to_string(),
                Value::List(vec![Value::Integer(1), Value::Integer(2)]),
            );
            m.insert("value".to_string(), Value::String("test".to_string()));
            Value::Map(m)
        };

        let map2 = {
            let mut m = FxHashMap::default();
            m.insert(
                "list".to_string(),
                Value::List(vec![Value::Integer(1), Value::Integer(2)]),
            );
            m.insert("value".to_string(), Value::String("test".to_string()));
            Value::Map(m)
        };

        assert!(map1.deep_eq(&map2));

        // Test with missing key
        let map3 = {
            let mut m = FxHashMap::default();
            m.insert(
                "list".to_string(),
                Value::List(vec![Value::Integer(1), Value::Integer(2)]),
            );
            Value::Map(m)
        };

        assert!(!map1.deep_eq(&map3));
    }

    #[test]
    fn test_procedure_with_environment() {
        let mut env = FxHashMap::default();
        env.insert("x".to_string(), Value::Integer(10));
        env.insert("y".to_string(), Value::String("captured".to_string()));

        let proc = Procedure {
            name: Some("closure".to_string()),
            params: vec!["z".to_string()],
            body: NodeId::new(1).unwrap(),
            env: Some(env),
        };

        let proc_val = Value::Procedure(Arc::new(proc));
        assert!(proc_val.is_procedure());

        let proc_ref = proc_val.as_procedure().unwrap();
        assert_eq!(proc_ref.name, Some("closure".to_string()));
        assert_eq!(proc_ref.params.len(), 1);
        assert!(proc_ref.env.is_some());

        let env_ref = proc_ref.env.as_ref().unwrap();
        assert_eq!(env_ref.len(), 2);
    }

    #[test]
    fn test_native_function_execution() {
        let counter = Arc::new(std::sync::Mutex::new(0));
        let counter_clone = counter.clone();

        let native = Value::NativeFunction {
            name: "increment".to_string(),
            arity: 1,
            function: Arc::new(move |args| {
                if args.len() != 1 {
                    return Err(ValueError::ArityMismatch {
                        expected: 1,
                        actual: args.len(),
                    });
                }

                let n = args[0].as_integer()?;
                let mut count = counter_clone.lock().unwrap();
                *count += 1;
                Ok(Value::Integer(n + 1))
            }),
        };

        // Test function properties
        assert!(native.is_callable());
        assert!(!native.is_procedure());

        // Test execution through the function pointer
        if let Value::NativeFunction { function, .. } = &native {
            let result = function(&[Value::Integer(5)]).unwrap();
            assert_eq!(result, Value::Integer(6));

            // Test arity error
            let err = function(&[]).unwrap_err();
            match err {
                ValueError::ArityMismatch {
                    expected: 1,
                    actual: 0,
                } => {}
                _ => panic!("Wrong error type"),
            }
        }

        // Verify side effect
        assert_eq!(*counter.lock().unwrap(), 1);
    }

    #[test]
    fn test_display_edge_cases() {
        // Test nested list display
        let nested = Value::List(vec![
            Value::List(vec![Value::Integer(1)]),
            Value::List(vec![Value::List(vec![Value::Integer(2)])]),
        ]);
        assert_eq!(format!("{}", nested), "((1) ((2)))");

        // Test tagged with multiple values
        let tagged = Value::Tagged {
            tag: "Triple".to_string(),
            values: vec![
                Value::Integer(1),
                Value::String("two".to_string()),
                Value::Boolean(true),
            ],
        };
        assert_eq!(format!("{}", tagged), "Triple(1 \"two\" #t)");

        // Test empty tagged
        let empty_tagged = Value::Tagged {
            tag: "Empty".to_string(),
            values: vec![],
        };
        assert_eq!(format!("{}", empty_tagged), "Empty()");

        // Test procedure with name
        let named_proc = Value::Procedure(Arc::new(Procedure {
            name: Some("my-func".to_string()),
            params: vec!["x".to_string(), "y".to_string()],
            body: NodeId::new(1).unwrap(),
            env: None,
        }));
        assert_eq!(format!("{}", named_proc), "#<procedure>");
    }

    #[test]
    fn test_numeric_comparison_nan() {
        // Test NaN comparison
        let nan = Value::Float(f64::NAN);
        let num = Value::Float(1.0);

        // NaN comparisons: NaN < x and NaN > x are both false, so it returns Equal
        let result = nan.compare_numeric(&num).unwrap();
        assert_eq!(result, std::cmp::Ordering::Equal);

        // NaN compared to itself also returns Equal (even though NaN != NaN)
        let nan2 = Value::Float(f64::NAN);
        let result2 = nan.compare_numeric(&nan2).unwrap();
        assert_eq!(result2, std::cmp::Ordering::Equal);

        // This is a limitation of the current implementation
        // In IEEE 754, NaN comparisons should be unordered
    }

    #[test]
    fn test_float_special_values() {
        // Test infinity
        let inf = Value::Float(f64::INFINITY);
        let neg_inf = Value::Float(f64::NEG_INFINITY);
        let num = Value::Float(1000.0);

        assert_eq!(inf.type_name(), "float");
        assert!(inf.is_float());
        assert!(inf.is_number());

        // Display of special values
        assert_eq!(format!("{}", inf), "inf");
        assert_eq!(format!("{}", neg_inf), "-inf");

        // Comparisons with infinity
        use std::cmp::Ordering;
        assert_eq!(inf.compare_numeric(&num).unwrap(), Ordering::Greater);
        assert_eq!(neg_inf.compare_numeric(&num).unwrap(), Ordering::Less);
    }

    #[test]
    fn test_procedure_equality_with_env() {
        let mut env1 = FxHashMap::default();
        env1.insert("x".to_string(), Value::Integer(1));

        let mut env2 = FxHashMap::default();
        env2.insert("x".to_string(), Value::Integer(2));

        let proc1 = Procedure {
            name: Some("func".to_string()),
            params: vec!["y".to_string()],
            body: NodeId::new(1).unwrap(),
            env: Some(env1),
        };

        let proc2 = Procedure {
            name: Some("func".to_string()),
            params: vec!["y".to_string()],
            body: NodeId::new(1).unwrap(),
            env: Some(env2),
        };

        // Same structure but different environments
        // The PartialEq implementation doesn't compare environments
        assert_eq!(proc1, proc2);
    }

    #[test]
    fn test_truthy_edge_cases() {
        // Test various edge cases for truthiness
        assert!(Value::Integer(-1).is_truthy());
        assert!(Value::Float(-0.0).is_truthy());
        assert!(Value::Float(f64::NAN).is_truthy());
        assert!(Value::Symbol("false".to_string()).is_truthy());
        assert!(Value::Symbol("nil".to_string()).is_truthy());

        // Tagged values are always truthy
        let none_like = Value::Tagged {
            tag: "None".to_string(),
            values: vec![],
        };
        assert!(none_like.is_truthy());
    }
}
