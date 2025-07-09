//! Tests for VM bridge functionality

use fluentai_stdlib::vm_bridge::*;
use fluentai_stdlib::value::Value;
use anyhow::Result;
use std::sync::{Arc, Mutex};
use fluentai_effects::EffectContext;

// Mock VM callback for testing
struct MockVMCallback {
    call_log: Mutex<Vec<(Value, Vec<Value>)>>,
    return_value: Value,
}

impl MockVMCallback {
    fn new(return_value: Value) -> Self {
        Self {
            call_log: Mutex::new(Vec::new()),
            return_value,
        }
    }
    
    #[allow(dead_code)]
    fn get_call_log(&self) -> Vec<(Value, Vec<Value>)> {
        self.call_log.lock().unwrap().clone()
    }
}

impl VMCallback for MockVMCallback {
    fn call_function(&mut self, func: &Value, args: &[Value]) -> Result<Value> {
        self.call_log.lock().unwrap().push((func.clone(), args.to_vec()));
        Ok(self.return_value.clone())
    }
    
    fn effect_context(&self) -> Arc<EffectContext> {
        Arc::new(EffectContext::default())
    }
}

#[test]
fn test_noop_vm_callback() {
    let mut callback = NoOpVMCallback;
    
    let func = Value::String("test-func".to_string());
    let args = vec![Value::Integer(42)];
    
    let result = callback.call_function(&func, &args);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("VM callback not available"));
}

#[test]
fn test_stdlib_context_default() {
    let context = StdlibContext::default();
    assert!(context.vm_callback.is_none());
}

#[test]
fn test_stdlib_context_with_callback() {
    let callback = Box::new(MockVMCallback::new(Value::Integer(100)));
    let mut context = StdlibContext::with_callback(callback);
    
    assert!(context.vm_callback.is_some());
    
    // Test calling a function
    let func = Value::String("add".to_string());
    let args = vec![Value::Integer(1), Value::Integer(2)];
    
    let result = context.call_function(&func, &args);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(100));
}

#[test]
fn test_stdlib_context_without_callback() {
    let mut context = StdlibContext::default();
    
    let func = Value::String("test".to_string());
    let args = vec![];
    
    let result = context.call_function(&func, &args);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("No VM callback available"));
}

#[test]
fn test_mock_vm_callback_logging() {
    let mock = MockVMCallback::new(Value::String("result".to_string()));
    let callback = Box::new(mock);
    
    // Can't easily test the logging since we need to move the callback into StdlibContext
    // This is more of a compile-time test to ensure the trait is properly implemented
    let mut context = StdlibContext::with_callback(callback);
    
    // Make multiple calls
    context.call_function(&Value::String("func1".to_string()), &[Value::Integer(1)]).unwrap();
    context.call_function(&Value::String("func2".to_string()), &[Value::Integer(2), Value::Integer(3)]).unwrap();
    
    // The call log is inside the moved callback, so we can't access it here
    // In a real scenario, you'd use Arc<Mutex<>> or similar for shared state
}

#[test]
fn test_vm_callback_trait_object() {
    // Test that VMCallback can be used as a trait object
    let callbacks: Vec<Box<dyn VMCallback>> = vec![
        Box::new(NoOpVMCallback),
        Box::new(MockVMCallback::new(Value::Nil)),
    ];
    
    assert_eq!(callbacks.len(), 2);
}

#[test]
fn test_different_value_types() {
    let test_values = vec![
        Value::Nil,
        Value::Boolean(true),
        Value::Integer(42),
        Value::Float(3.14),
        Value::String("test".to_string()),
        Value::List(vec![Value::Integer(1), Value::Integer(2)]),
    ];
    
    for value in test_values {
        let mut callback = MockVMCallback::new(value.clone());
        let result = callback.call_function(&Value::String("func".to_string()), &[]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), value);
    }
}

// Test a realistic scenario where VM bridge might be used
#[test]
fn test_higher_order_function_scenario() {
    // Simulate a map operation where we need to call a VM function for each element
    let list = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
    let mapper_func = Value::String("double".to_string()); // Pretend this is a VM function
    
    // Create a callback that doubles the input
    struct DoubleCallback;
    impl VMCallback for DoubleCallback {
        fn call_function(&mut self, _func: &Value, args: &[Value]) -> Result<Value> {
            if let Some(Value::Integer(n)) = args.first() {
                Ok(Value::Integer(n * 2))
            } else {
                Err(anyhow::anyhow!("Expected integer argument"))
            }
        }
        
        fn effect_context(&self) -> Arc<EffectContext> {
            Arc::new(EffectContext::default())
        }
    }
    
    let mut context = StdlibContext::with_callback(Box::new(DoubleCallback));
    
    // Simulate mapping over the list
    let mut results = Vec::new();
    for item in list {
        let result = context.call_function(&mapper_func, &[item]).unwrap();
        results.push(result);
    }
    
    assert_eq!(results, vec![Value::Integer(2), Value::Integer(4), Value::Integer(6)]);
}