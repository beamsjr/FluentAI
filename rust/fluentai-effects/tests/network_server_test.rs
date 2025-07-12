//! Tests for NetworkHandler HTTP server functionality

use fluentai_core::{error::Error, value::Value};
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use rustc_hash::FxHashMap;
use std::sync::Arc;

#[tokio::test]
async fn test_simple_server_start_stop() {
    let handler = NetworkHandler::new();
    
    // Start a simple server on port 0 (let OS assign)
    let dummy_handler = Value::NativeFunction {
        name: "dummy".to_string(),
        arity: 1,
        function: Arc::new(|_args| Ok(Value::Nil)),
    };
    let result = handler.handle_async("serve", &[
        Value::Integer(0), // Port 0 = auto-assign
        dummy_handler
    ]).await;
    
    assert!(result.is_ok());
    let server_id = match result.unwrap() {
        Value::String(id) => id,
        _ => panic!("Expected server ID string"),
    };
    
    // Server should be running
    assert!(server_id.starts_with("server_"));
    
    // Stop the server
    let stop_result = handler.handle_async("stop", &[Value::String(server_id)]).await;
    assert!(stop_result.is_ok());
}

#[tokio::test]
async fn test_server_with_config() {
    let handler = NetworkHandler::new();
    
    let mut config = FxHashMap::default();
    config.insert("host".to_string(), Value::String("127.0.0.1".to_string()));
    config.insert("port".to_string(), Value::Integer(0)); // Auto-assign port
    
    let dummy_handler = Value::NativeFunction {
        name: "dummy".to_string(),
        arity: 1,
        function: Arc::new(|_args| Ok(Value::Nil)),
    };
    
    let result = handler.handle_async("serve", &[
        Value::Map(config),
        dummy_handler
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
async fn test_add_route() {
    let handler = NetworkHandler::new();
    
    // Start server
    let dummy_handler = Value::NativeFunction {
        name: "dummy".to_string(),
        arity: 1,
        function: Arc::new(|_args| Ok(Value::Nil)),
    };
    let result = handler.handle_async("serve", &[
        Value::Integer(0),
        dummy_handler.clone()
    ]).await.unwrap();
    
    let server_id = match result {
        Value::String(id) => id,
        _ => panic!("Expected server ID"),
    };
    
    // Add a route
    let route_result = handler.handle_async("route", &[
        Value::String(server_id.clone()),
        Value::String("GET".to_string()),
        Value::String("/test".to_string()),
        dummy_handler
    ]).await;
    
    assert!(route_result.is_ok());
    
    // Clean up
    let _ = handler.handle_async("stop", &[Value::String(server_id)]).await;
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
async fn test_server_missing_port() {
    let handler = NetworkHandler::new();
    
    let mut config = FxHashMap::default();
    config.insert("host".to_string(), Value::String("127.0.0.1".to_string()));
    // Missing port!
    
    let dummy_handler = Value::NativeFunction {
        name: "dummy".to_string(),
        arity: 1,
        function: Arc::new(|_args| Ok(Value::Nil)),
    };
    
    let result = handler.handle_async("serve", &[
        Value::Map(config),
        dummy_handler
    ]).await;
    
    assert!(result.is_err());
    match result {
        Err(Error::Runtime(msg)) => assert!(msg.contains("requires 'port'")),
        _ => panic!("Expected Runtime error about missing port"),
    }
}

#[tokio::test]
async fn test_invalid_route_method() {
    let handler = NetworkHandler::new();
    
    // Start server
    let dummy_handler = Value::NativeFunction {
        name: "dummy".to_string(),
        arity: 1,
        function: Arc::new(|_args| Ok(Value::Nil)),
    };
    let result = handler.handle_async("serve", &[
        Value::Integer(0),
        dummy_handler.clone()
    ]).await.unwrap();
    
    let server_id = match result {
        Value::String(id) => id,
        _ => panic!("Expected server ID"),
    };
    
    // Try to add route with invalid method
    let route_result = handler.handle_async("route", &[
        Value::String(server_id.clone()),
        Value::String("INVALID".to_string()),
        Value::String("/test".to_string()),
        dummy_handler
    ]).await;
    
    assert!(route_result.is_err());
    
    // Clean up
    let _ = handler.handle_async("stop", &[Value::String(server_id)]).await;
}