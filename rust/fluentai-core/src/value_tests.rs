#[cfg(test)]
mod tests {
    use crate::value::*;
    use crate::ast::NodeId;
    use std::sync::Arc;
    use rustc_hash::FxHashMap;

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
        let tagged_val = Value::Tagged { tag: "Some".to_string(), values: vec![] };
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
        assert_eq!(Value::Tagged { tag: "T".to_string(), values: vec![] }.type_name(), "tagged");
        
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
        assert_eq!(Value::String("hello".to_string()), Value::String("hello".to_string()));
        assert_eq!(Value::Symbol("foo".to_string()), Value::Symbol("foo".to_string()));
        assert_eq!(Value::Boolean(true), Value::Boolean(true));
        assert_eq!(Value::Nil, Value::Nil);
        
        // Different values are not equal
        assert_ne!(Value::Integer(42), Value::Integer(43));
        assert_ne!(Value::Boolean(true), Value::Boolean(false));
        assert_ne!(Value::String("hello".to_string()), Value::String("world".to_string()));
        
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
        assert_eq!(int_val.compare_numeric(&float_val).unwrap(), Ordering::Equal);
        
        let float_val2 = Value::Float(4.5);
        assert_eq!(int_val.compare_numeric(&float_val2).unwrap(), Ordering::Greater);
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
        assert_eq!(format!("{}", Value::String("hello".to_string())), "\"hello\"");
        assert_eq!(format!("{}", Value::Symbol("foo".to_string())), "foo");
        assert_eq!(format!("{}", Value::Boolean(true)), "#t");
        assert_eq!(format!("{}", Value::Boolean(false)), "#f");
        assert_eq!(format!("{}", Value::Nil), "nil");
        
        // List display
        let list = Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
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
        assert_eq!(format!("{}", Value::Map(FxHashMap::default())), "#<hashmap>");
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
}