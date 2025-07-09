//! Additional comprehensive tests for the effect system

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
    let _runtime = EffectRuntime::default();

    // Test explicit creation
    let runtime2 = EffectRuntime::new();
    assert!(runtime2.is_ok());
}

#[test]
fn test_effect_runtime_spawn() {
    // Create runtime outside of async context
    let runtime = EffectRuntime::new().unwrap();

    // Use block_on to run async code
    runtime.block_on(async {
        // Test spawning a simple task
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
fn test_effect_handler_provider_registration() {
    let provider = EffectHandlerProvider::new();

    // Register handlers
    provider.register_singleton(Arc::new(IOHandler::new()));
    provider.register_singleton(Arc::new(StateHandler::new()));

    // Verify they're available
    assert!(provider.get_handler(EffectType::IO).is_ok());
    assert!(provider.get_handler(EffectType::State).is_ok());

    // Unregistered handler should fail
    assert!(provider.get_handler(EffectType::Network).is_err());
}

#[test]
fn test_effect_handler_provider_factory() {
    let provider = EffectHandlerProvider::new();

    // Register a factory
    provider.register_factory(EffectType::State, || {
        Arc::new(StateHandler::new()) as Arc<dyn EffectHandler>
    });

    // Get handler twice
    let handler1 = provider.get_handler(EffectType::State).unwrap();
    let handler2 = provider.get_handler(EffectType::State).unwrap();

    // Should be singleton (same instance)
    handler1
        .handle_sync(
            "set",
            &[Value::String("test".to_string()), Value::Integer(42)],
        )
        .unwrap();

    let result = handler2
        .handle_sync("get", &[Value::String("test".to_string())])
        .unwrap();

    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_effect_context_creation_from_provider() {
    let provider = EffectHandlerProvider::new();

    // Register handlers
    provider.register_singleton(Arc::new(IOHandler::new()));
    provider.register_singleton(Arc::new(StateHandler::new()));
    provider.register_singleton(Arc::new(TimeHandler::new()));

    // Create context from provider
    let context = provider.create_context();
    assert!(context.is_ok());

    let context = context.unwrap();

    // Test that handlers work
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
}

#[test]
fn test_effect_handler_provider_hierarchy() {
    let parent = Arc::new(EffectHandlerProvider::new());
    parent.register_singleton(Arc::new(IOHandler::new()));
    parent.register_singleton(Arc::new(StateHandler::new()));

    let child = EffectHandlerProvider::create_child(parent.clone());
    child.register_singleton(Arc::new(TimeHandler::new()));

    // Child can access parent's handlers
    assert!(child.get_handler(EffectType::IO).is_ok());
    assert!(child.get_handler(EffectType::State).is_ok());
    assert!(child.get_handler(EffectType::Time).is_ok());

    // Parent doesn't have child's handlers
    assert!(parent.get_handler(EffectType::Time).is_err());
}

#[test]
fn test_custom_effect_handler() {
    struct CounterHandler {
        count: std::sync::Mutex<i64>,
    }

    impl CounterHandler {
        fn new() -> Self {
            Self {
                count: std::sync::Mutex::new(0),
            }
        }
    }

    #[async_trait::async_trait]
    impl EffectHandler for CounterHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::State
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> Result<Value, Error> {
            match operation {
                "increment" => {
                    let mut count = self.count.lock().unwrap();
                    *count += 1;
                    Ok(Value::Integer(*count))
                }
                "decrement" => {
                    let mut count = self.count.lock().unwrap();
                    *count -= 1;
                    Ok(Value::Integer(*count))
                }
                "get" => {
                    let count = self.count.lock().unwrap();
                    Ok(Value::Integer(*count))
                }
                _ => Err(Error::Runtime("Unknown operation".to_string())),
            }
        }
    }

    let handler = Arc::new(CounterHandler::new());

    // Test operations
    assert_eq!(handler.handle_sync("get", &[]).unwrap(), Value::Integer(0));
    assert_eq!(
        handler.handle_sync("increment", &[]).unwrap(),
        Value::Integer(1)
    );
    assert_eq!(
        handler.handle_sync("increment", &[]).unwrap(),
        Value::Integer(2)
    );
    assert_eq!(
        handler.handle_sync("decrement", &[]).unwrap(),
        Value::Integer(1)
    );
    assert_eq!(handler.handle_sync("get", &[]).unwrap(), Value::Integer(1));
}

#[test]
fn test_effect_context_thread_safety() {
    use std::thread;

    let context = Arc::new(EffectContext::default());
    let mut handles = vec![];

    for i in 0..10 {
        let ctx = Arc::clone(&context);
        let handle = thread::spawn(move || {
            // Each thread performs different operations
            let result = ctx.perform_sync(EffectType::Time, "now", &[]);
            assert!(result.is_ok());

            // Add small delay to increase chance of race conditions
            thread::sleep(Duration::from_micros(100));

            let result = ctx.perform_sync(EffectType::Random, "float", &[]);
            assert!(result.is_ok());
        });
        handles.push(handle);
    }

    // All threads should complete successfully
    for handle in handles {
        handle.join().unwrap();
    }
}

#[tokio::test]
async fn test_async_handler_operations() {
    let handler = AsyncHandler::new();

    // Test promise creation
    let promise_result = handler.handle_sync("promise", &[]);
    assert!(promise_result.is_ok());

    // Test that await is async
    assert!(handler.is_async_operation("await"));
    assert!(handler.is_async_operation("all"));
    assert!(handler.is_async_operation("race"));

    // Test sync operations through async interface
    let result = handler.handle_async("promise", &[]).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_network_handler_async() {
    let handler = NetworkHandler::new();

    // All network operations should be async
    assert!(handler.is_async_operation("fetch"));
    assert!(handler.is_async_operation("post"));
    assert!(handler.is_async_operation("put"));
    assert!(handler.is_async_operation("delete"));

    // Sync calls should fail
    let result = handler.handle_sync("fetch", &[Value::String("http://example.com".to_string())]);
    assert!(result.is_err());
}

#[test]
fn test_concurrent_handler_operations() {
    let handler = ConcurrentHandler::new();

    // Test channel creation
    let channel_result = handler.handle_sync("channel", &[]);
    assert!(channel_result.is_ok());

    // Test mutex creation with initial value
    let mutex_result = handler.handle_sync("mutex", &[Value::Integer(0)]);
    assert!(mutex_result.is_ok());

    // Test spawn (should require function/closure)
    let spawn_result = handler.handle_sync("spawn", &[]);
    assert!(spawn_result.is_err());
}

#[test]
fn test_effect_handler_error_handling() {
    let handler = ErrorHandler::new();

    // Test raising errors
    let result = handler.handle_sync("raise", &[Value::String("Test error".to_string())]);
    assert!(result.is_err());

    match result {
        Err(Error::Runtime(msg)) => assert_eq!(msg, "Test error"),
        _ => panic!("Expected runtime error"),
    }

    // Test with non-string argument
    let result = handler.handle_sync("raise", &[Value::Integer(42)]);
    assert!(result.is_err());
}

#[test]
fn test_value_types_in_handlers() {
    let handler = StateHandler::new();

    // Test all value types
    let test_cases = vec![
        ("nil", Value::Nil),
        ("bool", Value::Boolean(true)),
        ("int", Value::Integer(42)),
        ("float", Value::Float(3.14)),
        ("string", Value::String("test".to_string())),
        (
            "list",
            Value::List(vec![Value::Integer(1), Value::Integer(2)]),
        ),
        (
            "map",
            Value::Map({
                let mut map = rustc_hash::FxHashMap::default();
                map.insert("key".to_string(), Value::String("value".to_string()));
                map
            }),
        ),
    ];

    for (key, value) in test_cases {
        // Set value
        let result = handler.handle_sync("set", &[Value::String(key.to_string()), value.clone()]);
        assert!(result.is_ok());

        // Get value back
        let result = handler.handle_sync("get", &[Value::String(key.to_string())]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), value);
    }
}

#[test]
fn test_handler_isolation() {
    // Create two separate state handlers
    let handler1 = StateHandler::new();
    let handler2 = StateHandler::new();

    // Set value in first handler
    handler1
        .handle_sync(
            "set",
            &[Value::String("test".to_string()), Value::Integer(100)],
        )
        .unwrap();

    // Second handler should not see it
    let result = handler2
        .handle_sync("get", &[Value::String("test".to_string())])
        .unwrap();

    assert_eq!(result, Value::Nil);
}

#[test]
fn test_time_handler_operations() {
    let handler = TimeHandler::new();

    // Test now
    let now_result = handler.handle_sync("now", &[]);
    assert!(now_result.is_ok());
    if let Value::Integer(timestamp) = now_result.unwrap() {
        assert!(timestamp > 0);
    } else {
        panic!("Expected integer timestamp");
    }

    // Test sleep with short duration
    let start = std::time::SystemTime::now();
    let sleep_result = handler.handle_sync("sleep", &[Value::Integer(50)]);
    let elapsed = start.elapsed().unwrap();

    assert!(sleep_result.is_ok());
    assert!(elapsed >= Duration::from_millis(50));

    // Test format
    let format_result = handler.handle_sync(
        "format",
        &[
            Value::Integer(1640995200000), // 2022-01-01 00:00:00 UTC
            Value::String("%Y-%m-%d".to_string()),
        ],
    );
    assert!(format_result.is_ok());
}

#[test]
fn test_random_handler_determinism() {
    let handler1 = RandomHandler::new();
    let handler2 = RandomHandler::new();

    // Set same seed
    handler1
        .handle_sync("seed", &[Value::Integer(12345)])
        .unwrap();
    handler2
        .handle_sync("seed", &[Value::Integer(12345)])
        .unwrap();

    // Generate numbers - should be the same
    for _ in 0..5 {
        let r1 = handler1
            .handle_sync("int", &[Value::Integer(1), Value::Integer(100)])
            .unwrap();
        let r2 = handler2
            .handle_sync("int", &[Value::Integer(1), Value::Integer(100)])
            .unwrap();
        assert_eq!(r1, r2);
    }
}

#[test]
fn test_dom_handler_in_non_browser() {
    let handler = DomHandler::new();

    // All DOM operations should fail in non-browser environment
    let operations = vec![
        "get_element",
        "create_element",
        "set_attribute",
        "append_child",
        "add_event_listener",
    ];

    for op in operations {
        let result = handler.handle_sync(op, &[Value::String("test".to_string())]);
        assert!(
            result.is_err(),
            "DOM operation {} should fail in non-browser",
            op
        );
    }
}

#[test]
fn test_effect_context_missing_handler() {
    let context = EffectContext::new();

    // No handlers registered
    let result = context.perform_sync(
        EffectType::IO,
        "print",
        &[Value::String("test".to_string())],
    );

    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("No handler registered"));
    }
}

#[test]
fn test_effect_runtime_handle() {
    // Create runtime outside of async context
    let runtime = EffectRuntime::new().unwrap();
    let handle = runtime.handle();

    runtime.block_on(async {
        // Spawn task via handle
        let task = handle.spawn(async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            "done"
        });

        let result = timeout(Duration::from_secs(1), task).await;
        assert!(result.is_ok());
        assert_eq!(result.unwrap().unwrap(), "done");
    });
}

#[test]
fn test_effect_provider_remove_handler() {
    let provider = EffectHandlerProvider::new();

    // Register handler
    let handler = Arc::new(IOHandler::new());
    provider.register_singleton(handler.clone());
    assert!(provider.get_handler(EffectType::IO).is_ok());

    // Remove handler
    provider.remove_handler(EffectType::IO);
    assert!(provider.get_handler(EffectType::IO).is_err());
}
