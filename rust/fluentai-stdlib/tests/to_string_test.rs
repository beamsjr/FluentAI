use fluentai_stdlib::{init_stdlib, value::Value};

    #[test]
    fn test_to_string_numbers() {
        let registry = init_stdlib();
        let to_string = registry.get("to_string").expect("to_string should exist");
        
        // Test integer
        let result = to_string.call(&[Value::Integer(42)]).unwrap();
        assert_eq!(result, Value::String("42".to_string()));
        
        // Test float
        let result = to_string.call(&[Value::Float(3.14)]).unwrap();
        assert_eq!(result, Value::String("3.14".to_string()));
    }
    
    #[test]
    fn test_to_string_strings() {
        let registry = init_stdlib();
        let to_string = registry.get("to_string").expect("to_string should exist");
        
        // Test string (should be identity)
        let result = to_string.call(&[Value::String("hello".to_string())]).unwrap();
        assert_eq!(result, Value::String("hello".to_string()));
    }
    
    #[test]
    fn test_to_string_collections() {
        let registry = init_stdlib();
        let to_string = registry.get("to_string").expect("to_string should exist");
        
        // Test list
        let list = Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
        let result = to_string.call(&[list]).unwrap();
        assert_eq!(result, Value::String("[1, 2, 3]".to_string()));
        
        // Test empty list
        let empty_list = Value::List(vec![]);
        let result = to_string.call(&[empty_list]).unwrap();
        assert_eq!(result, Value::String("[]".to_string()));
    }
    
    #[test]
    fn test_to_string_map() {
        let registry = init_stdlib();
        let to_string = registry.get("to_string").expect("to_string should exist");
        
        // Test map
        let mut map = rustc_hash::FxHashMap::default();
        map.insert("a".to_string(), Value::Integer(1));
        map.insert("b".to_string(), Value::Integer(2));
        
        let result = to_string.call(&[Value::Map(map)]).unwrap();
        // Map order might vary, so just check it contains the expected parts
        let result_str = match result {
            Value::String(s) => s,
            _ => panic!("Expected string result"),
        };
        assert!(result_str.contains("a: 1"));
        assert!(result_str.contains("b: 2"));
        assert!(result_str.starts_with("{"));
        assert!(result_str.ends_with("}"));
    }
    
    #[test]
    fn test_to_string_special_values() {
        let registry = init_stdlib();
        let to_string = registry.get("to_string").expect("to_string should exist");
        
        // Test nil
        let result = to_string.call(&[Value::Nil]).unwrap();
        assert_eq!(result, Value::String("nil".to_string()));
        
        // Test boolean
        let result = to_string.call(&[Value::Boolean(true)]).unwrap();
        assert_eq!(result, Value::String("true".to_string()));
        
        let result = to_string.call(&[Value::Boolean(false)]).unwrap();
        assert_eq!(result, Value::String("false".to_string()));
    }