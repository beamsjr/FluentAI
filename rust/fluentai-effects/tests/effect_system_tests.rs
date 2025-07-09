//! Comprehensive tests for the effect system infrastructure

use fluentai_core::{ast::EffectType, error::Error, value::Value};
use fluentai_effects::{
    AsyncHandler, ConcurrentHandler, DomHandler, EffectContext, EffectHandler,
    EffectHandlerProvider, EffectRuntime, ErrorHandler, IOHandler, NetworkHandler, RandomHandler,
    StateHandler, TimeHandler,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;

#[test]
fn test_effect_runtime_creation() {
    // Test default runtime creation
    let runtime = EffectRuntime::default();

    // Test explicit creation
    let runtime2 = EffectRuntime::new();
    assert!(runtime2.is_ok());
}

#[test]
fn test_effect_runtime_spawn() {
    let runtime = EffectRuntime::new().unwrap();

    // Test spawning a simple task using block_on
    runtime.block_on(async {
        let handle = tokio::spawn(async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            42
        });

        let result = handle.await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), 42);
    });
}

#[test]
fn test_effect_runtime_block_on() {
    let runtime = EffectRuntime::new().unwrap();

    // Test blocking on a future
    let result = runtime.block_on(async {
        tokio::time::sleep(Duration::from_millis(10)).await;
        "completed"
    });

    assert_eq!(result, "completed");
}

#[test]
fn test_effect_runtime_handle() {
    let runtime = EffectRuntime::new().unwrap();
    let handle = runtime.handle();

    runtime.block_on(async {
        // Test spawning via handle
        let task = handle.spawn(async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            100
        });

        let result = timeout(Duration::from_millis(100), task).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap().unwrap(), 100);
    });
}

#[test]
fn test_effect_handler_provider_default() {
    let provider = EffectHandlerProvider::new();

    // Register default handlers
    provider.register_singleton(Arc::new(IOHandler::new()));
    provider.register_singleton(Arc::new(StateHandler::new()));
    provider.register_singleton(Arc::new(ErrorHandler::new()));
    provider.register_singleton(Arc::new(TimeHandler::new()));
    provider.register_singleton(Arc::new(RandomHandler::new()));
    provider.register_singleton(Arc::new(NetworkHandler::new()));
    provider.register_singleton(Arc::new(AsyncHandler::new()));
    provider.register_singleton(Arc::new(ConcurrentHandler::new()));
    provider.register_singleton(Arc::new(DomHandler::new()));

    // Test that all default handlers are available
    let handler_types = vec![
        EffectType::IO,
        EffectType::State,
        EffectType::Error,
        EffectType::Time,
        EffectType::Random,
        EffectType::Network,
        EffectType::Async,
        EffectType::Concurrent,
        EffectType::Dom,
    ];

    for effect_type in handler_types {
        let handler = provider.get_handler(effect_type);
        assert!(
            handler.is_ok(),
            "Handler for {:?} should be available",
            effect_type
        );
        assert_eq!(handler.unwrap().effect_type(), effect_type);
    }
}

#[test]
fn test_effect_handler_provider_custom() {
    let provider = EffectHandlerProvider::new();

    // Initially empty
    assert!(provider.get_handler(EffectType::IO).is_err());

    // Register a handler
    provider.register_singleton(Arc::new(IOHandler::new()));

    // Now it should be available
    let handler = provider.get_handler(EffectType::IO);
    assert!(handler.is_ok());
    assert_eq!(handler.unwrap().effect_type(), EffectType::IO);
}

#[test]
fn test_effect_handler_provider_singleton_behavior() {
    let provider = EffectHandlerProvider::new();

    // Register singleton handler
    provider.register_singleton(Arc::new(StateHandler::new()));

    // Get handler multiple times - should be same instance
    let handler1 = provider.get_handler(EffectType::State).unwrap();
    let handler2 = provider.get_handler(EffectType::State).unwrap();

    // Set value through handler1
    handler1
        .handle_sync(
            "set",
            &[Value::String("shared_key".to_string()), Value::Integer(42)],
        )
        .unwrap();

    // Should be visible through handler2 (same instance)
    let result = handler2
        .handle_sync("get", &[Value::String("shared_key".to_string())])
        .unwrap();

    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_effect_context_concurrent_access() {
    use std::sync::Arc;
    use std::thread;

    let context = Arc::new(EffectContext::default());
    let mut handles = vec![];

    // Spawn multiple threads performing effects
    for i in 0..10 {
        let ctx = Arc::clone(&context);
        let handle = thread::spawn(move || {
            // Each thread performs different effects
            match i % 3 {
                0 => {
                    // IO operations
                    ctx.perform_sync(
                        EffectType::IO,
                        "print",
                        &[Value::String(format!("Thread {}", i))],
                    )
                }
                1 => {
                    // Time operations
                    ctx.perform_sync(EffectType::Time, "now", &[])
                }
                _ => {
                    // Random operations
                    ctx.perform_sync(
                        EffectType::Random,
                        "int",
                        &[Value::Integer(1), Value::Integer(100)],
                    )
                }
            }
        });
        handles.push(handle);
    }

    // All operations should complete without panics
    for handle in handles {
        let result = handle.join().unwrap();
        assert!(result.is_ok());
    }
}

#[tokio::test]
async fn test_effect_context_async_sync_interop() {
    let context = EffectContext::default();

    // Test sync handler through async interface
    let result = context
        .perform_async(EffectType::Random, "float", &[])
        .await;

    assert!(result.is_ok());
    if let Value::Float(f) = result.unwrap() {
        assert!(f >= 0.0 && f < 1.0);
    } else {
        panic!("Expected float value");
    }
}

#[test]
fn test_effect_handler_state_isolation() {
    let provider = EffectHandlerProvider::new();

    // Register default handlers
    provider.register_singleton(Arc::new(IOHandler::new()));
    provider.register_singleton(Arc::new(StateHandler::new()));
    provider.register_singleton(Arc::new(ErrorHandler::new()));
    provider.register_singleton(Arc::new(TimeHandler::new()));
    provider.register_singleton(Arc::new(RandomHandler::new()));
    provider.register_singleton(Arc::new(NetworkHandler::new()));
    provider.register_singleton(Arc::new(AsyncHandler::new()));
    provider.register_singleton(Arc::new(ConcurrentHandler::new()));
    provider.register_singleton(Arc::new(DomHandler::new()));

    // Get two references to the same handler
    let handler1 = provider.get_handler(EffectType::State).unwrap();
    let handler2 = provider.get_handler(EffectType::State).unwrap();

    // They should share the same state (same instance)
    handler1
        .handle_sync(
            "set",
            &[Value::String("shared".to_string()), Value::Integer(42)],
        )
        .unwrap();

    let result = handler2
        .handle_sync("get", &[Value::String("shared".to_string())])
        .unwrap();

    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_effect_handler_error_propagation_chain() {
    let context = EffectContext::default();

    // Test error propagation through multiple layers
    let result = context.perform_sync(
        EffectType::Error,
        "raise",
        &[Value::String("Initial error".to_string())],
    );

    assert!(result.is_err());
    let error = result.unwrap_err();
    assert_eq!(error.to_string(), "Runtime error: Initial error");

    // Test wrapping errors
    match error {
        Error::Runtime(msg) => {
            let wrapped = Error::Runtime(format!("Wrapped: {}", msg));
            assert_eq!(wrapped.to_string(), "Runtime error: Wrapped: Initial error");
        }
        _ => panic!("Expected runtime error"),
    }
}

#[test]
fn test_effect_system_memory_safety() {
    // Test that dropping handlers doesn't cause issues
    {
        let provider = EffectHandlerProvider::new();

        // Register default handlers
        provider.register_singleton(Arc::new(IOHandler::new()));
        provider.register_singleton(Arc::new(StateHandler::new()));
        provider.register_singleton(Arc::new(ErrorHandler::new()));
        provider.register_singleton(Arc::new(TimeHandler::new()));
        provider.register_singleton(Arc::new(RandomHandler::new()));
        provider.register_singleton(Arc::new(NetworkHandler::new()));
        provider.register_singleton(Arc::new(AsyncHandler::new()));
        provider.register_singleton(Arc::new(ConcurrentHandler::new()));
        provider.register_singleton(Arc::new(DomHandler::new()));
        let handler = provider.get_handler(EffectType::State).unwrap();

        handler
            .handle_sync(
                "set",
                &[
                    Value::String("temp".to_string()),
                    Value::String("data".to_string()),
                ],
            )
            .unwrap();

        // handler will be dropped here
    }

    // Create new provider and handler
    let provider = EffectHandlerProvider::new();

    // Register default handlers
    provider.register_singleton(Arc::new(IOHandler::new()));
    provider.register_singleton(Arc::new(StateHandler::new()));
    provider.register_singleton(Arc::new(ErrorHandler::new()));
    provider.register_singleton(Arc::new(TimeHandler::new()));
    provider.register_singleton(Arc::new(RandomHandler::new()));
    provider.register_singleton(Arc::new(NetworkHandler::new()));
    provider.register_singleton(Arc::new(AsyncHandler::new()));
    provider.register_singleton(Arc::new(ConcurrentHandler::new()));
    provider.register_singleton(Arc::new(DomHandler::new()));
    let handler = provider.get_handler(EffectType::State).unwrap();

    // Should work independently
    let result = handler
        .handle_sync("get", &[Value::String("temp".to_string())])
        .unwrap();

    assert_eq!(result, Value::Nil); // New handler shouldn't see old data
}

#[tokio::test]
async fn test_async_effect_cancellation() {
    let handler = AsyncHandler::new();

    // Create a promise with a dummy function arg so it doesn't auto-resolve
    let promise_id = handler
        .handle_sync("promise", &[Value::String("dummy".to_string())])
        .unwrap();

    // Try to await the unresolved promise
    let args = vec![promise_id];
    let result = handler.handle_async("await", &args).await;

    // Should return an error because promise is not yet resolved
    assert!(result.is_err());
    match result {
        Err(Error::Runtime(msg)) => assert!(msg.contains("not yet resolved")),
        _ => panic!("Expected 'not yet resolved' error"),
    }
}

#[test]
fn test_effect_handler_operation_discovery() {
    struct DiscoverableHandler {
        operations: Vec<&'static str>,
    }

    #[async_trait::async_trait]
    impl EffectHandler for DiscoverableHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::IO
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> Result<Value, Error> {
            if operation == "list_operations" {
                let ops = self
                    .operations
                    .iter()
                    .map(|&s| Value::String(s.to_string()))
                    .collect();
                Ok(Value::List(ops))
            } else if self.operations.contains(&operation) {
                Ok(Value::Nil)
            } else {
                Err(Error::Runtime(format!("Unknown operation: {}", operation)))
            }
        }
    }

    let handler = DiscoverableHandler {
        operations: vec!["read", "write", "delete", "list_operations"],
    };

    let result = handler.handle_sync("list_operations", &[]).unwrap();
    if let Value::List(ops) = result {
        assert_eq!(ops.len(), 4);
        assert!(ops.contains(&Value::String("read".to_string())));
        assert!(ops.contains(&Value::String("write".to_string())));
        assert!(ops.contains(&Value::String("delete".to_string())));
        assert!(ops.contains(&Value::String("list_operations".to_string())));
    } else {
        panic!("Expected list of operations");
    }
}

#[test]
fn test_effect_context_builder_pattern() {
    // Test building a custom effect context
    let context = EffectContext::new();

    // Add handlers one by one
    context.register_handler(Arc::new(IOHandler::new()));
    context.register_handler(Arc::new(StateHandler::new()));
    context.register_handler(Arc::new(TimeHandler::new()));

    // Verify only registered handlers work
    assert!(context
        .perform_sync(
            EffectType::IO,
            "print",
            &[Value::String("test".to_string())]
        )
        .is_ok());
    assert!(context
        .perform_sync(
            EffectType::State,
            "get",
            &[Value::String("key".to_string())]
        )
        .is_ok());
    assert!(context.perform_sync(EffectType::Time, "now", &[]).is_ok());

    // Unregistered handlers should fail
    assert!(context
        .perform_sync(
            EffectType::Random,
            "int",
            &[Value::Integer(1), Value::Integer(10)]
        )
        .is_err());
}

#[test]
fn test_value_type_conversions() {
    let handler = StateHandler::new();

    // Test storing different value types
    let test_values = vec![
        ("nil", Value::Nil),
        ("bool_true", Value::Boolean(true)),
        ("bool_false", Value::Boolean(false)),
        ("int_pos", Value::Integer(42)),
        ("int_neg", Value::Integer(-42)),
        ("int_zero", Value::Integer(0)),
        ("float_pos", Value::Float(3.14)),
        ("float_neg", Value::Float(-3.14)),
        ("float_zero", Value::Float(0.0)),
        ("float_inf", Value::Float(f64::INFINITY)),
        ("float_neg_inf", Value::Float(f64::NEG_INFINITY)),
        ("string_empty", Value::String("".to_string())),
        ("string_unicode", Value::String("🦀 Rust 🚀".to_string())),
        ("list_empty", Value::List(vec![])),
        (
            "list_nested",
            Value::List(vec![
                Value::Integer(1),
                Value::List(vec![Value::Integer(2), Value::Integer(3)]),
                Value::Integer(4),
            ]),
        ),
    ];

    for (key, value) in test_values {
        let set_result =
            handler.handle_sync("set", &[Value::String(key.to_string()), value.clone()]);
        assert!(set_result.is_ok(), "Failed to set {}", key);

        let get_result = handler.handle_sync("get", &[Value::String(key.to_string())]);
        assert!(get_result.is_ok(), "Failed to get {}", key);
        let retrieved = get_result.unwrap();
        // Special handling for float comparisons (infinity values)
        match (&retrieved, &value) {
            (Value::Float(a), Value::Float(b)) if a.is_infinite() && b.is_infinite() => {
                assert_eq!(
                    a.is_sign_positive(),
                    b.is_sign_positive(),
                    "Infinity sign mismatch for {}",
                    key
                );
            }
            _ => {
                assert_eq!(retrieved, value, "Value mismatch for {}", key);
            }
        }
    }
}

#[test]
fn test_effect_handler_lifecycle() {
    // Test handler creation, usage, and cleanup
    let handler = StateHandler::new();

    // Initial state
    let result = handler.handle_sync("get", &[Value::String("counter".to_string())]);
    assert_eq!(result.unwrap(), Value::Nil);

    // Use handler
    for i in 0..100 {
        handler
            .handle_sync(
                "set",
                &[Value::String(format!("item_{}", i)), Value::Integer(i)],
            )
            .unwrap();
    }

    // Verify data
    for i in 0..100 {
        let result = handler
            .handle_sync("get", &[Value::String(format!("item_{}", i))])
            .unwrap();
        assert_eq!(result, Value::Integer(i));
    }

    // Clean up
    for i in 0..100 {
        handler
            .handle_sync("delete", &[Value::String(format!("item_{}", i))])
            .unwrap();
    }

    // Verify cleanup
    for i in 0..100 {
        let result = handler
            .handle_sync("get", &[Value::String(format!("item_{}", i))])
            .unwrap();
        assert_eq!(result, Value::Nil);
    }
}
