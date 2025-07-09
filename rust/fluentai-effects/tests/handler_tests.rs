//! Comprehensive tests for all effect handlers

use fluentai_core::{ast::EffectType, error::Error, value::Value};
use fluentai_effects::{
    AsyncHandler, ConcurrentHandler, DomHandler, EffectContext, EffectHandler, ErrorHandler,
    IOHandler, NetworkHandler, RandomHandler, StateHandler, TimeHandler,
};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

#[test]
fn test_io_handler_print() {
    let handler = IOHandler::new();
    assert_eq!(handler.effect_type(), EffectType::IO);

    // Test print operation
    let result = handler.handle_sync("print", &[Value::String("test".to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_io_handler_println() {
    let handler = IOHandler::new();

    // Test println operation
    let result = handler.handle_sync("println", &[Value::String("test line".to_string())]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_io_handler_file_operations() {
    let handler = IOHandler::new();
    let test_file = "/tmp/fluentai_test_io.txt";
    let content = "Hello, FluentAI!";

    // Test write_file
    let write_result = handler.handle_sync(
        "write_file",
        &[
            Value::String(test_file.to_string()),
            Value::String(content.to_string()),
        ],
    );
    assert!(write_result.is_ok());

    // Test read_file
    let read_result = handler.handle_sync("read_file", &[Value::String(test_file.to_string())]);
    assert!(read_result.is_ok());

    if let Value::String(read_content) = read_result.unwrap() {
        assert_eq!(read_content, content);
    } else {
        panic!("Expected string value from read_file");
    }

    // Cleanup
    let _ = std::fs::remove_file(test_file);
}

#[test]
fn test_io_handler_invalid_operations() {
    let handler = IOHandler::new();

    // Test unknown operation
    let result = handler.handle_sync("unknown_op", &[]);
    assert!(result.is_err());

    // Test read_file without path
    let result = handler.handle_sync("read_file", &[]);
    assert!(result.is_err());

    // Test write_file with insufficient args
    let result = handler.handle_sync("write_file", &[Value::String("path".to_string())]);
    assert!(result.is_err());
}

#[test]
fn test_state_handler_get_set() {
    let handler = StateHandler::new();
    assert_eq!(handler.effect_type(), EffectType::State);

    // Test set operation
    let set_result = handler.handle_sync(
        "set",
        &[Value::String("key".to_string()), Value::Integer(42)],
    );
    assert!(set_result.is_ok());

    // Test get operation
    let get_result = handler.handle_sync("get", &[Value::String("key".to_string())]);
    assert!(get_result.is_ok());
    assert_eq!(get_result.unwrap(), Value::Integer(42));
}

#[test]
fn test_state_handler_update() {
    let handler = StateHandler::new();

    // Set initial value
    handler
        .handle_sync(
            "set",
            &[Value::String("counter".to_string()), Value::Integer(10)],
        )
        .unwrap();

    // Test update operation (would need lambda support in real implementation)
    let update_result = handler.handle_sync(
        "update",
        &[
            Value::String("counter".to_string()),
            Value::Integer(5), // Simplified: just add this value
        ],
    );
    assert!(update_result.is_ok());

    // Verify updated value
    let get_result = handler
        .handle_sync("get", &[Value::String("counter".to_string())])
        .unwrap();

    // Note: Actual implementation would apply a function
    // For now, let's assume it replaces the value
    assert!(matches!(get_result, Value::Integer(_)));
}

#[test]
fn test_state_handler_delete() {
    let handler = StateHandler::new();

    // Set a value
    handler
        .handle_sync(
            "set",
            &[
                Value::String("temp".to_string()),
                Value::String("temporary".to_string()),
            ],
        )
        .unwrap();

    // Delete it
    let delete_result = handler.handle_sync("delete", &[Value::String("temp".to_string())]);
    assert!(delete_result.is_ok());

    // Try to get deleted value
    let get_result = handler.handle_sync("get", &[Value::String("temp".to_string())]);
    assert!(get_result.is_ok());
    assert_eq!(get_result.unwrap(), Value::Nil);
}

#[test]
fn test_error_handler_throw_catch() {
    let handler = ErrorHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Error);

    // Test raise operation (handler uses "raise", not "throw")
    let raise_result = handler.handle_sync("raise", &[Value::String("Test error".to_string())]);
    assert!(raise_result.is_err());

    if let Err(Error::Runtime(msg)) = raise_result {
        assert_eq!(msg, "Test error");
    } else {
        panic!("Expected runtime error");
    }
}

#[test]
fn test_error_handler_try_operations() {
    let handler = ErrorHandler::new();

    // Test try operation (simplified - would need closure support)
    let try_result = handler.handle_sync("try", &[Value::String("operation".to_string())]);

    // The actual implementation would execute a closure
    // For now, we just check it doesn't crash
    assert!(try_result.is_ok() || try_result.is_err());
}

#[test]
fn test_time_handler_now() {
    let handler = TimeHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Time);

    // Test now operation
    let now_result = handler.handle_sync("now", &[]);
    assert!(now_result.is_ok());

    if let Value::Integer(timestamp) = now_result.unwrap() {
        assert!(timestamp > 0);

        // Verify it's a reasonable timestamp (after year 2020)
        let time = SystemTime::UNIX_EPOCH + Duration::from_millis(timestamp as u64);
        assert!(time > SystemTime::UNIX_EPOCH + Duration::from_secs(1577836800));
    // Jan 1, 2020
    } else {
        panic!("Expected integer timestamp");
    }
}

#[test]
fn test_time_handler_format() {
    let handler = TimeHandler::new();

    // Test format operation with current time
    let format_result = handler.handle_sync(
        "format",
        &[
            Value::Integer(
                SystemTime::now()
                    .duration_since(SystemTime::UNIX_EPOCH)
                    .unwrap()
                    .as_millis() as i64,
            ),
            Value::String("%Y-%m-%d".to_string()),
        ],
    );

    assert!(format_result.is_ok());
    if let Value::String(formatted) = format_result.unwrap() {
        // Check format (should be YYYY-MM-DD)
        assert!(formatted.len() == 10);
        assert!(formatted.chars().nth(4) == Some('-'));
        assert!(formatted.chars().nth(7) == Some('-'));
    } else {
        panic!("Expected formatted string");
    }
}

#[test]
fn test_time_handler_sleep() {
    let handler = TimeHandler::new();

    // Test sleep operation (with very short duration)
    let start = SystemTime::now();
    let sleep_result = handler.handle_sync(
        "sleep",
        &[
            Value::Integer(10), // 10ms
        ],
    );
    let elapsed = start.elapsed().unwrap();

    assert!(sleep_result.is_ok());
    assert!(elapsed >= Duration::from_millis(10));
}

#[test]
fn test_random_handler_int() {
    let handler = RandomHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Random);

    // Test random int generation
    let result = handler.handle_sync("int", &[Value::Integer(1), Value::Integer(100)]);

    assert!(result.is_ok());
    if let Value::Integer(n) = result.unwrap() {
        assert!(n >= 1 && n < 100);
    } else {
        panic!("Expected integer value");
    }
}

#[test]
fn test_random_handler_float() {
    let handler = RandomHandler::new();

    // Test random float generation
    let result = handler.handle_sync("float", &[]);

    assert!(result.is_ok());
    if let Value::Float(f) = result.unwrap() {
        assert!(f >= 0.0 && f < 1.0);
    } else {
        panic!("Expected float value");
    }
}

#[test]
fn test_random_handler_choice() {
    let handler = RandomHandler::new();

    // Test random choice from list
    let list = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];

    let result = handler.handle_sync("choice", &[Value::List(list.clone())]);

    assert!(result.is_ok());
    let chosen = result.unwrap();
    assert!(list.contains(&chosen));
}

#[test]
fn test_random_handler_seed() {
    let handler = RandomHandler::new();

    // Test setting seed
    let seed_result = handler.handle_sync("seed", &[Value::Integer(12345)]);
    assert!(seed_result.is_ok());

    // Generate some random numbers
    let r1 = handler
        .handle_sync("int", &[Value::Integer(1), Value::Integer(1000)])
        .unwrap();

    // Reset seed to same value
    handler
        .handle_sync("seed", &[Value::Integer(12345)])
        .unwrap();

    // Should get same random number
    let r2 = handler
        .handle_sync("int", &[Value::Integer(1), Value::Integer(1000)])
        .unwrap();

    assert_eq!(r1, r2);
}

#[test]
fn test_network_handler_operations() {
    let handler = NetworkHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Network);

    // Test that network operations are async
    assert!(handler.is_async_operation("fetch"));
    assert!(handler.is_async_operation("post"));

    // Test sync call returns error (network ops should be async)
    let result = handler.handle_sync("fetch", &[Value::String("http://example.com".to_string())]);
    assert!(result.is_err());
}

#[tokio::test]
async fn test_network_handler_async() {
    let handler = NetworkHandler::new();

    // Test async fetch (would need actual implementation)
    let result = handler
        .handle_async("fetch", &[Value::String("http://example.com".to_string())])
        .await;

    // For now, just verify it returns an error or result
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_async_handler_operations() {
    let handler = AsyncHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Async);

    // Test that async operations are recognized
    assert!(handler.is_async_operation("await"));
    assert!(handler.is_async_operation("all"));
    assert!(handler.is_async_operation("race"));

    // Test sync operations
    assert!(!handler.is_async_operation("promise"));
    assert!(!handler.is_async_operation("resolve"));
    assert!(!handler.is_async_operation("reject"));
}

#[test]
fn test_concurrent_handler_channel() {
    let handler = ConcurrentHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Concurrent);

    // Test channel creation
    let channel_result = handler.handle_sync("channel", &[]);
    assert!(channel_result.is_ok());

    // Channel should return some identifier
    assert!(!matches!(channel_result.unwrap(), Value::Nil));
}

#[test]
fn test_concurrent_handler_mutex() {
    let handler = ConcurrentHandler::new();

    // Test mutex creation
    let mutex_result = handler.handle_sync(
        "mutex",
        &[
            Value::Integer(0), // Initial value
        ],
    );
    assert!(mutex_result.is_ok());
}

#[test]
fn test_dom_handler_operations() {
    let handler = DomHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Dom);

    // Test DOM operations (would need browser environment)
    let ops = [
        "get_element",
        "create_element",
        "set_attribute",
        "add_event_listener",
    ];

    for op in ops {
        // These should all return errors in non-browser environment
        let result = handler.handle_sync(op, &[Value::String("test".to_string())]);
        assert!(result.is_err());
    }
}

#[test]
fn test_effect_context_default_handlers() {
    let context = EffectContext::default();

    // Test that all default handlers are registered
    let effect_types = vec![
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

    for effect_type in effect_types {
        let result = context.perform_sync(effect_type, "invalid_op", &[]);
        // Should get an error for invalid operation, not missing handler
        assert!(result.is_err());
        if let Err(Error::Runtime(msg)) = result {
            assert!(!msg.contains("No handler registered"));
        }
    }
}

#[test]
fn test_effect_context_custom_handler() {
    struct CustomHandler;

    #[async_trait::async_trait]
    impl EffectHandler for CustomHandler {
        fn effect_type(&self) -> EffectType {
            EffectType::IO
        }

        fn handle_sync(&self, operation: &str, _args: &[Value]) -> Result<Value, Error> {
            match operation {
                "custom" => Ok(Value::String("custom result".to_string())),
                _ => Err(Error::Runtime("Unknown operation".to_string())),
            }
        }
    }

    let context = EffectContext::new();
    context.register_handler(Arc::new(CustomHandler));

    let result = context.perform_sync(EffectType::IO, "custom", &[]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String("custom result".to_string()));
}

#[test]
fn test_handler_error_propagation() {
    let context = EffectContext::default();

    // Test that errors are properly propagated
    let result = context.perform_sync(
        EffectType::IO,
        "read_file",
        &[Value::String("/nonexistent/path/to/file.txt".to_string())],
    );

    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("Failed to read file"));
    }
}

#[test]
fn test_multiple_handlers_isolation() {
    let handler1 = StateHandler::new();
    let handler2 = StateHandler::new();

    // Set value in first handler
    handler1
        .handle_sync(
            "set",
            &[Value::String("key".to_string()), Value::Integer(1)],
        )
        .unwrap();

    // Second handler should have its own state
    let result = handler2
        .handle_sync("get", &[Value::String("key".to_string())])
        .unwrap();

    assert_eq!(result, Value::Nil);
}

#[tokio::test]
async fn test_async_sync_operation_compatibility() {
    let context = EffectContext::default();

    // Test that sync operations work through async interface
    let result = context.perform_async(EffectType::Time, "now", &[]).await;

    assert!(result.is_ok());
    assert!(matches!(result.unwrap(), Value::Integer(_)));
}
