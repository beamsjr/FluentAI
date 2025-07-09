//! Integration tests for the effect system

use fluentai_core::value::Value;
use fluentai_effects::provider::*;
use fluentai_effects::*;
use std::sync::Arc;

#[test]
fn test_effect_handler_provider() {
    // Create a provider with default handlers
    let provider = EffectHandlerBuilder::new().with_defaults().build();

    // Create an effect context from the provider
    let context = provider.create_context().unwrap();

    // Test IO effect
    let result = context.perform_sync(
        EffectType::IO,
        "print",
        &[Value::String("Hello".to_string())],
    );
    assert!(result.is_ok());

    // Test State effect
    let result = context.perform_sync(
        EffectType::State,
        "set",
        &[Value::String("key".to_string()), Value::Integer(42)],
    );
    assert!(result.is_ok());

    let result = context.perform_sync(
        EffectType::State,
        "get",
        &[Value::String("key".to_string())],
    );
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(42));
}

#[test]
fn test_hierarchical_providers() {
    // Create parent provider with IO handler
    let parent = Arc::new(
        EffectHandlerBuilder::new()
            .with_handler(Arc::new(handlers::IOHandler::new()))
            .build(),
    );

    // Create child provider with State handler
    let child = EffectHandlerProvider::create_child(parent.clone());
    child.register_singleton(Arc::new(handlers::StateHandler::new()));

    // Child should have access to both handlers
    let io_handler = child.get_handler(EffectType::IO);
    assert!(io_handler.is_ok());

    let state_handler = child.get_handler(EffectType::State);
    assert!(state_handler.is_ok());

    // Parent should only have IO handler
    let parent_state_handler = parent.get_handler(EffectType::State);
    assert!(parent_state_handler.is_err());
}

#[test]
fn test_custom_effect_handler() {
    use async_trait::async_trait;

    struct TestHandler {
        counter: std::sync::Mutex<i64>,
    }

    impl TestHandler {
        fn new() -> Self {
            Self {
                counter: std::sync::Mutex::new(0),
            }
        }
    }

    #[async_trait]
    impl EffectHandler for TestHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::State
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
            match operation {
                "increment" => {
                    let mut counter = self.counter.lock().unwrap();
                    *counter += 1;
                    Ok(Value::Integer(*counter))
                }
                "get" => {
                    let counter = self.counter.lock().unwrap();
                    Ok(Value::Integer(*counter))
                }
                _ => Err(fluentai_core::error::Error::Runtime(format!(
                    "Unknown operation: {}",
                    operation
                ))),
            }
        }
    }

    let provider = EffectHandlerBuilder::new()
        .with_handler(Arc::new(TestHandler::new()))
        .build();

    let context = provider.create_context().unwrap();

    // Test custom handler
    let result = context.perform_sync(EffectType::State, "increment", &[]);
    assert_eq!(result.unwrap(), Value::Integer(1));

    let result = context.perform_sync(EffectType::State, "increment", &[]);
    assert_eq!(result.unwrap(), Value::Integer(2));

    let result = context.perform_sync(EffectType::State, "get", &[]);
    assert_eq!(result.unwrap(), Value::Integer(2));
}

#[tokio::test]
async fn test_async_effects() {
    let context = EffectContext::default();

    // Test async time effect
    let result = context.perform_async(EffectType::Time, "now", &[]).await;
    assert!(result.is_ok());

    match result.unwrap() {
        Value::Integer(timestamp) => {
            assert!(timestamp > 0);
        }
        _ => panic!("Expected integer timestamp"),
    }
}

#[test]
fn test_effect_context_with_multiple_handlers() {
    let context = EffectContext::default();

    // Test that all default handlers are registered
    let io_result = context.perform_sync(
        EffectType::IO,
        "print",
        &[Value::String("test".to_string())],
    );
    assert!(io_result.is_ok());

    let state_result = context.perform_sync(
        EffectType::State,
        "set",
        &[Value::String("test".to_string()), Value::Boolean(true)],
    );
    assert!(state_result.is_ok());

    let time_result = context.perform_sync(EffectType::Time, "now", &[]);
    assert!(time_result.is_ok());

    let random_result = context.perform_sync(
        EffectType::Random,
        "int",
        &[Value::Integer(0), Value::Integer(100)],
    );
    assert!(random_result.is_ok());
    match random_result.unwrap() {
        Value::Integer(n) => assert!(n >= 0 && n < 100),
        _ => panic!("Expected integer"),
    }
}

#[test]
fn test_effect_error_handling() {
    let context = EffectContext::default();

    // Test invalid operation
    let result = context.perform_sync(EffectType::IO, "invalid_operation", &[]);
    assert!(result.is_err());

    // Test invalid arguments
    let result = context.perform_sync(EffectType::IO, "read_file", &[Value::Integer(42)]);
    assert!(result.is_err());

    // Test missing handler
    let empty_context = EffectContext::new();
    let result = empty_context.perform_sync(
        EffectType::IO,
        "print",
        &[Value::String("test".to_string())],
    );
    assert!(result.is_err());
}
