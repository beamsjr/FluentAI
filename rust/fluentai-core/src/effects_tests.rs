#[cfg(test)]
mod tests {
    use crate::effects::*;
    use crate::ast::EffectType;
    use crate::value::Value;
    use crate::error::Error;
    use std::collections::HashSet;
    
    // ===== EffectSet Tests =====
    
    #[test]
    fn test_effect_set_new() {
        let effects = EffectSet::new();
        assert_eq!(effects.effects.len(), 0);
    }
    
    #[test]
    fn test_effect_set_pure() {
        let effects = EffectSet::pure();
        assert!(effects.is_pure());
        assert!(effects.contains(EffectType::Pure));
        assert_eq!(effects.effects.len(), 1);
    }
    
    #[test]
    fn test_add_effect() {
        let mut effects = EffectSet::new();
        
        // Add IO effect
        effects.add(EffectType::IO);
        assert!(effects.contains(EffectType::IO));
        assert!(!effects.is_pure());
        
        // Add another effect
        effects.add(EffectType::State);
        assert!(effects.contains(EffectType::IO));
        assert!(effects.contains(EffectType::State));
        assert_eq!(effects.effects.len(), 2);
    }
    
    #[test]
    fn test_pure_effect_removal() {
        let mut effects = EffectSet::pure();
        assert!(effects.is_pure());
        
        // Adding non-pure effect should remove Pure
        effects.add(EffectType::IO);
        assert!(!effects.contains(EffectType::Pure));
        assert!(effects.contains(EffectType::IO));
        assert!(!effects.is_pure());
    }
    
    #[test]
    fn test_adding_pure_to_non_pure() {
        let mut effects = EffectSet::new();
        effects.add(EffectType::IO);
        
        // Adding Pure to non-pure set should keep it
        effects.add(EffectType::Pure);
        assert!(effects.contains(EffectType::Pure));
        assert!(effects.contains(EffectType::IO));
        assert!(!effects.is_pure()); // Not pure because it has other effects
    }
    
    #[test]
    fn test_effect_set_union() {
        let mut effects1 = EffectSet::new();
        effects1.add(EffectType::IO);
        effects1.add(EffectType::State);
        
        let mut effects2 = EffectSet::new();
        effects2.add(EffectType::Error);
        effects2.add(EffectType::State); // Duplicate
        
        effects1.union(&effects2);
        
        // Should contain all unique effects
        assert!(effects1.contains(EffectType::IO));
        assert!(effects1.contains(EffectType::State));
        assert!(effects1.contains(EffectType::Error));
        assert_eq!(effects1.effects.len(), 3);
    }
    
    #[test]
    fn test_union_with_pure() {
        let mut pure_effects = EffectSet::pure();
        let mut io_effects = EffectSet::new();
        io_effects.add(EffectType::IO);
        
        pure_effects.union(&io_effects);
        
        // Pure should be removed when unioning with non-pure
        assert!(!pure_effects.contains(EffectType::Pure));
        assert!(pure_effects.contains(EffectType::IO));
        assert!(!pure_effects.is_pure());
    }
    
    #[test]
    fn test_effect_set_iter() {
        let mut effects = EffectSet::new();
        effects.add(EffectType::IO);
        effects.add(EffectType::State);
        effects.add(EffectType::Error);
        
        let collected: HashSet<_> = effects.iter().cloned().collect();
        assert_eq!(collected.len(), 3);
        assert!(collected.contains(&EffectType::IO));
        assert!(collected.contains(&EffectType::State));
        assert!(collected.contains(&EffectType::Error));
    }
    
    #[test]
    fn test_all_effect_types() {
        let mut effects = EffectSet::new();
        
        // Add all effect types
        effects.add(EffectType::Pure);
        effects.add(EffectType::IO);
        effects.add(EffectType::State);
        effects.add(EffectType::Error);
        effects.add(EffectType::Time);
        effects.add(EffectType::Network);
        effects.add(EffectType::Random);
        effects.add(EffectType::Dom);
        effects.add(EffectType::Async);
        effects.add(EffectType::Concurrent);
        
        // Pure was removed when we added non-pure effects, so we have 9 types
        assert_eq!(effects.effects.len(), 9);
        
        // Verify each one (Pure was removed)
        assert!(!effects.contains(EffectType::Pure));
        assert!(effects.contains(EffectType::IO));
        assert!(effects.contains(EffectType::State));
        assert!(effects.contains(EffectType::Error));
        assert!(effects.contains(EffectType::Time));
        assert!(effects.contains(EffectType::Network));
        assert!(effects.contains(EffectType::Random));
        assert!(effects.contains(EffectType::Dom));
        assert!(effects.contains(EffectType::Async));
        assert!(effects.contains(EffectType::Concurrent));
    }
    
    // ===== DefaultEffectHandler Tests =====
    
    #[test]
    fn test_default_handler_io_print() {
        let handler = DefaultEffectHandler;
        let args = vec![Value::String("Hello, World!".to_string())];
        
        let result = handler.handle_io("print", &args).unwrap();
        assert_eq!(result, Value::Nil);
    }
    
    #[test]
    fn test_default_handler_io_print_empty() {
        let handler = DefaultEffectHandler;
        let args = vec![];
        
        // Should still succeed but print nothing
        let result = handler.handle_io("print", &args).unwrap();
        assert_eq!(result, Value::Nil);
    }
    
    #[test]
    fn test_default_handler_io_unknown() {
        let handler = DefaultEffectHandler;
        let args = vec![Value::String("test".to_string())];
        
        let result = handler.handle_io("unknown_operation", &args);
        assert!(result.is_err());
        
        match result.unwrap_err() {
            Error::UnknownEffect(msg) => {
                assert_eq!(msg, "IO:unknown_operation");
            }
            _ => panic!("Wrong error type"),
        }
    }
    
    #[test]
    fn test_default_handler_state() {
        let handler = DefaultEffectHandler;
        let args = vec![Value::String("key".to_string()), Value::Integer(42)];
        
        let result = handler.handle_state("get", &args);
        assert!(result.is_err());
        
        match result.unwrap_err() {
            Error::UnknownEffect(msg) => {
                assert_eq!(msg, "State operations not implemented");
            }
            _ => panic!("Wrong error type"),
        }
    }
    
    #[test]
    fn test_default_handler_error_raise() {
        let handler = DefaultEffectHandler;
        let args = vec![Value::String("Something went wrong".to_string())];
        
        let result = handler.handle_error("raise", &args);
        assert!(result.is_err());
        
        match result.unwrap_err() {
            Error::Runtime(msg) => {
                assert_eq!(msg, "\"Something went wrong\"");
            }
            _ => panic!("Wrong error type"),
        }
    }
    
    #[test]
    fn test_default_handler_error_raise_empty() {
        let handler = DefaultEffectHandler;
        let args = vec![];
        
        let result = handler.handle_error("raise", &args);
        assert!(result.is_err());
        
        match result.unwrap_err() {
            Error::Runtime(msg) => {
                assert_eq!(msg, "Error raised");
            }
            _ => panic!("Wrong error type"),
        }
    }
    
    #[test]
    fn test_default_handler_error_unknown() {
        let handler = DefaultEffectHandler;
        let args = vec![];
        
        let result = handler.handle_error("unknown_error_op", &args);
        assert!(result.is_err());
        
        match result.unwrap_err() {
            Error::UnknownEffect(msg) => {
                assert_eq!(msg, "Error:unknown_error_op");
            }
            _ => panic!("Wrong error type"),
        }
    }
    
    // ===== Integration Tests =====
    
    #[test]
    fn test_multiple_io_operations() {
        let handler = DefaultEffectHandler;
        
        // Test multiple print operations
        let args1 = vec![Value::String("First line".to_string())];
        let args2 = vec![Value::Integer(42)];
        let args3 = vec![Value::List(vec![Value::Integer(1), Value::Integer(2)])];
        
        assert!(handler.handle_io("print", &args1).is_ok());
        assert!(handler.handle_io("print", &args2).is_ok());
        assert!(handler.handle_io("print", &args3).is_ok());
    }
    
    #[test]
    fn test_effect_set_operations() {
        // Test complex effect set operations
        let mut effects1 = EffectSet::pure();
        let mut effects2 = EffectSet::new();
        effects2.add(EffectType::IO);
        effects2.add(EffectType::State);
        
        let mut effects3 = EffectSet::new();
        effects3.add(EffectType::Error);
        
        // Union operations
        effects1.union(&effects2);
        effects1.union(&effects3);
        
        assert!(!effects1.is_pure());
        assert!(effects1.contains(EffectType::IO));
        assert!(effects1.contains(EffectType::State));
        assert!(effects1.contains(EffectType::Error));
        assert!(!effects1.contains(EffectType::Pure));
    }
    
    #[test]
    fn test_effect_handler_trait_object() {
        // Test that DefaultEffectHandler can be used as trait object
        let handler: Box<dyn EffectHandler> = Box::new(DefaultEffectHandler);
        
        let args = vec![Value::String("From trait object".to_string())];
        let result = handler.handle_io("print", &args);
        assert!(result.is_ok());
    }
    
    // ===== Custom EffectHandler Implementation Test =====
    
    use std::sync::{Arc, Mutex};
    
    struct TestEffectHandler {
        io_calls: Arc<Mutex<Vec<String>>>,
    }
    
    impl TestEffectHandler {
        fn new() -> Self {
            Self {
                io_calls: Arc::new(Mutex::new(Vec::new())),
            }
        }
        
        fn get_io_calls(&self) -> Vec<String> {
            self.io_calls.lock().unwrap().clone()
        }
    }
    
    impl EffectHandler for TestEffectHandler {
        fn handle_io(&self, operation: &str, _args: &[Value]) -> crate::Result<Value> {
            self.io_calls.lock().unwrap().push(operation.to_string());
            
            match operation {
                "test_op" => Ok(Value::String("test_result".to_string())),
                _ => Ok(Value::Nil),
            }
        }
        
        fn handle_state(&self, _operation: &str, _args: &[Value]) -> crate::Result<Value> {
            Ok(Value::Nil)
        }
        
        fn handle_error(&self, _operation: &str, _args: &[Value]) -> crate::Result<Value> {
            Ok(Value::Nil)
        }
    }
    
    #[test]
    fn test_custom_effect_handler() {
        let handler = TestEffectHandler::new();
        
        // Test custom operation
        let result = handler.handle_io("test_op", &[]).unwrap();
        assert_eq!(result, Value::String("test_result".to_string()));
        
        // Test tracking
        handler.handle_io("op1", &[]).unwrap();
        handler.handle_io("op2", &[]).unwrap();
        
        let calls = handler.get_io_calls();
        assert_eq!(calls, vec!["test_op", "op1", "op2"]);
    }
}