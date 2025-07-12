//! Integration tests for NetworkHandler HTTP server functionality

use fluentai_core::{error::Error, value::Value};
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::time::sleep;
use std::time::Duration;

/// Test helper to create a simple echo handler
fn create_echo_handler() -> Value {
    Value::NativeFunction {
        name: "echo_handler".to_string(),
        arity: 1,
        function: Arc::new(|args| {
            if let Some(Value::Map(request)) = args.first() {
                let mut response = FxHashMap::default();
                response.insert("status".to_string(), Value::Integer(200));
                
                // Echo back the request method and path
                if let Some(method) = request.get("method") {
                    response.insert("method".to_string(), method.clone());
                }
                if let Some(path) = request.get("path") {
                    response.insert("path".to_string(), path.clone());
                }
                
                // Echo back the body
                let body = request.get("body").cloned().unwrap_or(Value::Nil);
                response.insert("body".to_string(), body);
                
                Ok(Value::Map(response))
            } else {
                Err(fluentai_core::value::ValueError::TypeError {
                    expected: "Map",
                    actual: "other",
                })
            }
        }),
    }
}

#[tokio::test]
async fn test_server_lifecycle() {
    let handler = NetworkHandler::new();
    
    // Start server with echo handler
    let echo_handler = create_echo_handler();
    let result = handler.handle_async("serve", &[
        Value::Integer(0), // Port 0 = auto-assign
        echo_handler
    ]).await;
    
    assert!(result.is_ok());
    let server_id = match result.unwrap() {
        Value::String(id) => id,
        _ => panic!("Expected server ID string"),
    };
    
    assert!(server_id.starts_with("server_"));
    
    // Give server time to start
    sleep(Duration::from_millis(100)).await;
    
    // Stop the server
    let stop_result = handler.handle_async("stop", &[Value::String(server_id)]).await;
    assert!(stop_result.is_ok());
}

#[tokio::test]
async fn test_server_with_config() {
    let handler = NetworkHandler::new();
    
    let mut config = FxHashMap::default();
    config.insert("host".to_string(), Value::String("127.0.0.1".to_string()));
    config.insert("port".to_string(), Value::Integer(0));
    
    let echo_handler = create_echo_handler();
    let result = handler.handle_async("serve", &[
        Value::Map(config),
        echo_handler
    ]).await;
    
    assert!(result.is_ok());
    let server_id = match result.unwrap() {
        Value::String(id) => id,
        _ => panic!("Expected server ID string"),
    };
    
    // Clean up
    let _ = handler.handle_async("stop", &[Value::String(server_id)]).await;
}

#[tokio::test]
async fn test_server_request_handling() {
    let handler = NetworkHandler::new();
    
    // Start server
    let echo_handler = create_echo_handler();
    let server_result = handler.handle_async("serve", &[
        Value::Integer(0),
        echo_handler
    ]).await.unwrap();
    
    let server_id = match server_result {
        Value::String(id) => id,
        _ => panic!("Expected server ID"),
    };
    
    // Give server time to start
    sleep(Duration::from_millis(100)).await;
    
    // TODO: Once we have the actual port from the server,
    // we can make HTTP requests to test the echo functionality
    
    // For now, just verify the server started and can be stopped
    let stop_result = handler.handle_async("stop", &[Value::String(server_id)]).await;
    assert!(stop_result.is_ok());
}

#[tokio::test]
async fn test_multiple_servers() {
    let handler = NetworkHandler::new();
    
    // Start first server
    let echo_handler1 = create_echo_handler();
    let result1 = handler.handle_async("serve", &[
        Value::Integer(0),
        echo_handler1
    ]).await.unwrap();
    
    let server_id1 = match result1 {
        Value::String(id) => id,
        _ => panic!("Expected server ID"),
    };
    
    // Start second server
    let echo_handler2 = create_echo_handler();
    let result2 = handler.handle_async("serve", &[
        Value::Integer(0),
        echo_handler2
    ]).await.unwrap();
    
    let server_id2 = match result2 {
        Value::String(id) => id,
        _ => panic!("Expected server ID"),
    };
    
    // Verify they have different IDs
    assert_ne!(server_id1, server_id2);
    
    // Stop both servers
    let _ = handler.handle_async("stop", &[Value::String(server_id1)]).await;
    let _ = handler.handle_async("stop", &[Value::String(server_id2)]).await;
}

#[tokio::test]
async fn test_stop_nonexistent_server() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("stop", &[
        Value::String("nonexistent_server".to_string())
    ]).await;
    
    assert!(result.is_err());
    match result {
        Err(Error::Runtime(msg)) => assert!(msg.contains("not found")),
        _ => panic!("Expected Runtime error"),
    }
}

#[tokio::test]
async fn test_server_with_invalid_handler() {
    let handler = NetworkHandler::new();
    
    // Try to start server with non-function handler
    let result = handler.handle_async("serve", &[
        Value::Integer(0),
        Value::String("not a function".to_string())
    ]).await;
    
    assert!(result.is_err());
}