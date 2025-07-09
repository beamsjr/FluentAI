//! Tests for the effect runtime system

use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use fluentai_effects::{EffectContext, EffectHandler, EffectRuntime, ErrorHandler, StateHandler};
use std::sync::Arc;

#[test]
fn test_runtime_creation() {
    let runtime = EffectRuntime::new();
    assert!(runtime.is_ok());
}

#[test]
fn test_context_with_default_handlers() {
    let context = EffectContext::default();

    // Should be able to perform effects
    let result = context.perform_sync(EffectType::Time, "now", &[]);

    assert!(result.is_ok());
}

#[test]
fn test_context_handler_registration() {
    let context = EffectContext::new();

    struct TestHandler;

    #[async_trait]
    impl EffectHandler for TestHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::IO
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> Result<Value, Error> {
            match operation {
                "test" => Ok(Value::String("test result".to_string())),
                _ => Err(Error::Runtime("Unknown operation".to_string())),
            }
        }
    }

    context.register_handler(Arc::new(TestHandler));

    let result = context.perform_sync(EffectType::IO, "test", &[]);

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String("test result".to_string()));
}

#[test]
fn test_context_isolation() {
    let context1 = EffectContext::default();
    let context2 = EffectContext::default();

    // Set state in context1
    context1
        .perform_sync(
            EffectType::State,
            "set",
            &[Value::String("test".to_string()), Value::Integer(1)],
        )
        .unwrap();

    // Context2 should not see context1's state
    let result = context2
        .perform_sync(
            EffectType::State,
            "get",
            &[Value::String("test".to_string())],
        )
        .unwrap();

    assert_eq!(result, Value::Nil);
}

#[tokio::test]
async fn test_context_async_effects() {
    let context = EffectContext::default();

    // Test async effect execution
    let result = context.perform_async(EffectType::Time, "now", &[]).await;

    assert!(result.is_ok());
}

#[test]
fn test_context_error_propagation() {
    let context = EffectContext::default();

    // Test that errors are properly propagated
    let result = context.perform_sync(
        EffectType::IO,
        "read_file",
        &[Value::String("/nonexistent/file.txt".to_string())],
    );

    assert!(result.is_err());
}

#[test]
fn test_context_concurrent_access() {
    let context = Arc::new(EffectContext::default());
    let mut handles = vec![];

    // Spawn multiple threads accessing the context
    for i in 0..10 {
        let context_clone = context.clone();
        let handle = std::thread::spawn(move || {
            context_clone.perform_sync(
                EffectType::State,
                "set",
                &[
                    Value::String(format!("thread_{}", i)),
                    Value::Integer(i as i64),
                ],
            )
        });
        handles.push(handle);
    }

    // Wait for all threads
    for handle in handles {
        handle.join().unwrap().unwrap();
    }

    // Verify all values were set
    for i in 0..10 {
        let result = context
            .perform_sync(
                EffectType::State,
                "get",
                &[Value::String(format!("thread_{}", i))],
            )
            .unwrap();

        assert_eq!(result, Value::Integer(i as i64));
    }
}

#[test]
fn test_context_handler_override() {
    let context = EffectContext::new();

    // Add initial time handler
    context.register_handler(Arc::new(fluentai_effects::TimeHandler::new()));

    // Original handler
    let original_result = context.perform_sync(EffectType::Time, "now", &[]).unwrap();

    assert!(matches!(original_result, Value::Integer(_)));

    // Override with custom handler
    struct MockTimeHandler;

    #[async_trait]
    impl EffectHandler for MockTimeHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::Time
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> Result<Value, Error> {
            match operation {
                "now" => Ok(Value::Integer(12345)),
                _ => Err(Error::Runtime("Unknown operation".to_string())),
            }
        }
    }

    context.register_handler(Arc::new(MockTimeHandler));

    // Should use overridden handler
    let new_result = context.perform_sync(EffectType::Time, "now", &[]).unwrap();

    assert_eq!(new_result, Value::Integer(12345));
}

#[test]
fn test_multiple_operations() {
    let context = EffectContext::default();

    // Set initial state
    context
        .perform_sync(
            EffectType::State,
            "set",
            &[Value::String("counter".to_string()), Value::Integer(0)],
        )
        .unwrap();

    // Simulate transaction with multiple operations
    let operations = vec![
        (
            "set",
            vec![Value::String("temp".to_string()), Value::Integer(100)],
        ),
        ("get", vec![Value::String("counter".to_string())]),
        (
            "set",
            vec![Value::String("counter".to_string()), Value::Integer(5)],
        ),
        ("delete", vec![Value::String("temp".to_string())]),
    ];

    for (op, args) in operations {
        context.perform_sync(EffectType::State, op, &args).unwrap();
    }

    // Verify final state
    let counter = context
        .perform_sync(
            EffectType::State,
            "get",
            &[Value::String("counter".to_string())],
        )
        .unwrap();

    assert_eq!(counter, Value::Integer(5));

    let temp = context
        .perform_sync(
            EffectType::State,
            "get",
            &[Value::String("temp".to_string())],
        )
        .unwrap();

    assert_eq!(temp, Value::Nil);
}

#[test]
fn test_runtime_with_context() {
    // Test that runtime can be used with effects
    let runtime = EffectRuntime::new().unwrap();
    let context = EffectContext::default();

    // Use runtime to execute async effect
    let result =
        runtime.block_on(async { context.perform_async(EffectType::Time, "now", &[]).await });

    assert!(result.is_ok());
}

#[test]
fn test_effect_composition() {
    let context = EffectContext::default();

    // Test composing multiple effects
    let results = vec![
        context.perform_sync(EffectType::Time, "now", &[]),
        context.perform_sync(EffectType::Random, "float", &[]),
        context.perform_sync(
            EffectType::State,
            "set",
            &[Value::String("composed".to_string()), Value::Integer(42)],
        ),
    ];

    // All should succeed
    for result in &results {
        assert!(result.is_ok());
    }

    // Verify state was set
    let state_result = context
        .perform_sync(
            EffectType::State,
            "get",
            &[Value::String("composed".to_string())],
        )
        .unwrap();

    assert_eq!(state_result, Value::Integer(42));
}
