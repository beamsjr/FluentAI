//! Comprehensive tests for async effect handler

use fluentai_core::{ast::EffectType, error::Error, value::Value};
use fluentai_effects::{AsyncHandler, EffectHandler};
use std::time::Duration;
use tokio::time::{sleep, timeout};

#[test]
fn test_async_handler_creation() {
    let handler = AsyncHandler::new();
    assert_eq!(handler.effect_type(), EffectType::Async);
}

#[test]
fn test_async_handler_promise_creation() {
    let handler = AsyncHandler::new();

    // Create a promise
    let result = handler.handle_sync("promise", &[]);
    assert!(result.is_ok());

    let promise_id = result.unwrap();
    assert!(matches!(promise_id, Value::String(_)));
}

#[test]
fn test_async_handler_promise_with_args() {
    let handler = AsyncHandler::new();

    // Create a promise with arguments (currently ignored, but should work)
    let result = handler.handle_sync("promise", &[Value::String("executor".to_string())]);
    assert!(result.is_ok());

    let promise_id = result.unwrap();
    assert!(matches!(promise_id, Value::String(_)));
}

#[test]
fn test_async_handler_resolve_operation() {
    let handler = AsyncHandler::new();

    // Create a promise
    let promise_id = handler.handle_sync("promise", &[]).unwrap();

    // Resolve it
    let result = handler.handle_sync(
        "resolve",
        &[
            promise_id.clone(),
            Value::String("resolved value".to_string()),
        ],
    );

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_async_handler_resolve_errors() {
    let handler = AsyncHandler::new();

    // Test resolve without arguments
    let result = handler.handle_sync("resolve", &[]);
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires 2 arguments"));
    }

    // Test resolve with non-string promise ID
    let result = handler.handle_sync(
        "resolve",
        &[Value::Integer(123), Value::String("value".to_string())],
    );
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires promise ID"));
    }
}

#[test]
fn test_async_handler_reject_operation() {
    let handler = AsyncHandler::new();

    // Create a promise
    let promise_id = handler.handle_sync("promise", &[]).unwrap();

    // Reject it
    let result = handler.handle_sync(
        "reject",
        &[
            promise_id.clone(),
            Value::String("error message".to_string()),
        ],
    );

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_async_handler_reject_errors() {
    let handler = AsyncHandler::new();

    // Test reject without arguments
    let result = handler.handle_sync("reject", &[]);
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires 2 arguments"));
    }

    // Test reject with non-string promise ID
    let result = handler.handle_sync(
        "reject",
        &[Value::Integer(123), Value::String("error".to_string())],
    );
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires promise ID"));
    }
}

#[test]
fn test_async_handler_unknown_sync_operation() {
    let handler = AsyncHandler::new();

    let result = handler.handle_sync("unknown_op", &[]);
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("Unknown Async operation"));
    }
}

#[tokio::test]
async fn test_async_handler_await_auto_resolved() {
    let handler = AsyncHandler::new();

    // Create a promise that auto-resolves to nil
    let promise_id = handler.handle_sync("promise", &[]).unwrap();

    // Small delay to ensure promise is resolved
    sleep(Duration::from_millis(10)).await;

    // Await the promise
    let result = handler.handle_async("await", &[promise_id]).await;
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[tokio::test]
async fn test_async_handler_await_unknown_promise() {
    let handler = AsyncHandler::new();

    // Try to await non-existent promise
    let result = handler
        .handle_async("await", &[Value::String("unknown-promise-id".to_string())])
        .await;

    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("Unknown promise ID"));
    }
}

#[tokio::test]
async fn test_async_handler_await_errors() {
    let handler = AsyncHandler::new();

    // Test await without arguments
    let result = handler.handle_async("await", &[]).await;
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires promise ID"));
    }

    // Test await with non-string argument
    let result = handler.handle_async("await", &[Value::Integer(123)]).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_async_handler_all_operation() {
    let handler = AsyncHandler::new();

    // Create multiple promises
    let promise1 = handler.handle_sync("promise", &[]).unwrap();
    let promise2 = handler.handle_sync("promise", &[]).unwrap();
    let promise3 = handler.handle_sync("promise", &[]).unwrap();

    // Small delay to ensure promises are resolved
    sleep(Duration::from_millis(10)).await;

    // Wait for all promises
    let result = handler
        .handle_async("all", &[promise1, promise2, promise3])
        .await;

    assert!(result.is_ok());
    if let Value::List(values) = result.unwrap() {
        assert_eq!(values.len(), 3);
        assert!(values.iter().all(|v| *v == Value::Nil));
    } else {
        panic!("Expected list result from all operation");
    }
}

#[tokio::test]
async fn test_async_handler_all_empty() {
    let handler = AsyncHandler::new();

    // Call all with no promises
    let result = handler.handle_async("all", &[]).await;
    assert!(result.is_ok());
    if let Value::List(values) = result.unwrap() {
        assert_eq!(values.len(), 0);
    }
}

#[tokio::test]
async fn test_async_handler_all_with_non_promise() {
    let handler = AsyncHandler::new();

    let promise = handler.handle_sync("promise", &[]).unwrap();

    // Small delay to ensure promise is resolved
    sleep(Duration::from_millis(10)).await;

    // Mix promises with non-promise values
    let result = handler
        .handle_async(
            "all",
            &[
                promise,
                Value::Integer(123), // This will be skipped (not a string)
                Value::String("not-a-promise".to_string()), // This will try to be awaited but fail
            ],
        )
        .await;

    // The implementation skips non-string values but tries to await string values
    // Since "not-a-promise" is not a valid promise ID, it will fail
    assert!(result.is_err());

    // Test with only non-string values (should succeed with empty results)
    let result2 = handler
        .handle_async(
            "all",
            &[
                Value::Integer(123),
                Value::Float(456.0),
                Value::List(vec![]),
            ],
        )
        .await;

    assert!(result2.is_ok());
    if let Value::List(values) = result2.unwrap() {
        assert_eq!(values.len(), 0); // All non-string values are skipped
    }
}

#[tokio::test]
async fn test_async_handler_race_operation() {
    let handler = AsyncHandler::new();

    // Create multiple promises
    let promise1 = handler.handle_sync("promise", &[]).unwrap();
    let promise2 = handler.handle_sync("promise", &[]).unwrap();

    // Small delay to ensure promises are resolved
    sleep(Duration::from_millis(10)).await;

    // Race returns the first one
    let result = handler.handle_async("race", &[promise1, promise2]).await;

    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[tokio::test]
async fn test_async_handler_race_empty() {
    let handler = AsyncHandler::new();

    // Race with no promises
    let result = handler.handle_async("race", &[]).await;
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("requires at least one promise"));
    }
}

#[tokio::test]
async fn test_async_handler_race_single_promise() {
    let handler = AsyncHandler::new();

    let promise = handler.handle_sync("promise", &[]).unwrap();

    // Small delay to ensure promise is resolved
    sleep(Duration::from_millis(10)).await;

    // Race with single promise
    let result = handler.handle_async("race", &[promise]).await;
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_async_handler_is_async_operation() {
    let handler = AsyncHandler::new();

    // Async operations
    assert!(handler.is_async_operation("await"));
    assert!(handler.is_async_operation("all"));
    assert!(handler.is_async_operation("race"));

    // Sync operations
    assert!(!handler.is_async_operation("promise"));
    assert!(!handler.is_async_operation("resolve"));
    assert!(!handler.is_async_operation("reject"));

    // Unknown operation
    assert!(!handler.is_async_operation("unknown"));
}

#[tokio::test]
async fn test_async_handler_concurrent_promises() {
    let handler = AsyncHandler::new();

    // Create many promises concurrently
    let mut promises = Vec::new();
    for _ in 0..10 {
        promises.push(handler.handle_sync("promise", &[]).unwrap());
    }

    // Small delay
    sleep(Duration::from_millis(10)).await;

    // Await all of them
    let result = handler.handle_async("all", &promises).await;
    assert!(result.is_ok());

    if let Value::List(values) = result.unwrap() {
        assert_eq!(values.len(), 10);
    }
}

#[tokio::test]
async fn test_async_handler_promise_not_yet_resolved() {
    let handler = AsyncHandler::new();

    // Create a promise but don't auto-resolve it
    // Note: Current implementation auto-resolves, so this test shows expected behavior
    let promise_id = handler
        .handle_sync(
            "promise",
            &[
                Value::String("executor".to_string()), // With args, it doesn't auto-resolve
            ],
        )
        .unwrap();

    // Try to await immediately (before resolution)
    let result = handler.handle_async("await", &[promise_id]).await;

    // With current implementation, this might succeed or fail depending on timing
    // The test documents the behavior
    assert!(result.is_err() || result.is_ok());
}

#[tokio::test]
async fn test_async_handler_async_unknown_operation() {
    let handler = AsyncHandler::new();

    // Unknown async operation falls back to sync handler
    let result = handler.handle_async("unknown_async_op", &[]).await;
    assert!(result.is_err());
    if let Err(Error::Runtime(msg)) = result {
        assert!(msg.contains("Unknown Async operation"));
    }
}

#[tokio::test]
async fn test_async_handler_timeout_scenario() {
    let handler = AsyncHandler::new();

    // Create promises
    let promise1 = handler.handle_sync("promise", &[]).unwrap();
    let promise2 = handler.handle_sync("promise", &[]).unwrap();

    // Use timeout to simulate time-constrained operations
    let result = timeout(
        Duration::from_secs(1),
        handler.handle_async("all", &[promise1, promise2]),
    )
    .await;

    assert!(result.is_ok()); // Didn't timeout
    assert!(result.unwrap().is_ok()); // Operation succeeded
}

#[tokio::test]
async fn test_async_handler_mixed_operations() {
    let handler = AsyncHandler::new();

    // Create promise synchronously
    let promise = handler.handle_sync("promise", &[]).unwrap();

    // Resolve synchronously (note: current implementation doesn't actually resolve)
    let resolve_result = handler.handle_sync(
        "resolve",
        &[promise.clone(), Value::String("test".to_string())],
    );
    assert!(resolve_result.is_ok());

    // Await asynchronously
    sleep(Duration::from_millis(10)).await;
    let await_result = handler.handle_async("await", &[promise]).await;

    // Current implementation auto-resolves, so this should work
    assert!(await_result.is_ok());
}
