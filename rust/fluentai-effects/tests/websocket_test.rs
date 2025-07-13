//! Tests for WebSocket functionality

use fluentai_effects::{handlers::{HttpServerHandler, WebSocketHandler}, EffectHandler};
use fluentai_core::value::Value;

#[test]
fn test_websocket_handler_creation() {
    let handler = WebSocketHandler::new();
    assert_eq!(handler.effect_type(), fluentai_core::ast::EffectType::WebSocket);
}

#[test]
fn test_websocket_connections_empty() {
    let handler = WebSocketHandler::new();
    
    let result = handler.handle_sync("connections", &[]);
    assert!(result.is_ok());
    
    if let Ok(Value::List(connections)) = result {
        assert_eq!(connections.len(), 0);
    } else {
        panic!("Expected empty connections list");
    }
}

#[test]
fn test_websocket_connection_count() {
    let handler = WebSocketHandler::new();
    
    let result = handler.handle_sync("connection_count", &[]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(0));
}

#[test]
fn test_websocket_add_connection() {
    let handler = WebSocketHandler::new();
    
    // Add a connection
    let _rx = handler.add_connection("test_conn_1".to_string());
    
    // Check connection count
    let count_result = handler.handle_sync("connection_count", &[]);
    assert_eq!(count_result.unwrap(), Value::Integer(1));
    
    // Check connections list
    let list_result = handler.handle_sync("connections", &[]);
    if let Ok(Value::List(connections)) = list_result {
        assert_eq!(connections.len(), 1);
        assert_eq!(connections[0], Value::String("test_conn_1".to_string()));
    } else {
        panic!("Expected connections list");
    }
}

#[test]
fn test_websocket_remove_connection() {
    let handler = WebSocketHandler::new();
    
    // Add connections
    let _rx1 = handler.add_connection("conn1".to_string());
    let _rx2 = handler.add_connection("conn2".to_string());
    
    // Verify we have 2 connections
    let count_result = handler.handle_sync("connection_count", &[]);
    assert_eq!(count_result.unwrap(), Value::Integer(2));
    
    // Remove one connection
    handler.remove_connection("conn1");
    
    // Verify we have 1 connection
    let count_result = handler.handle_sync("connection_count", &[]);
    assert_eq!(count_result.unwrap(), Value::Integer(1));
    
    // Verify the right connection remains
    let list_result = handler.handle_sync("connections", &[]);
    if let Ok(Value::List(connections)) = list_result {
        assert_eq!(connections.len(), 1);
        assert_eq!(connections[0], Value::String("conn2".to_string()));
    }
}

#[tokio::test]
async fn test_websocket_send_to_nonexistent() {
    let handler = WebSocketHandler::new();
    
    let result = handler.handle_async(
        "send",
        &[
            Value::String("nonexistent".to_string()),
            Value::String("Hello".to_string()),
        ],
    ).await;
    
    assert!(result.is_err());
    if let Err(fluentai_core::error::Error::Runtime(msg)) = result {
        assert!(msg.contains("not found"));
    }
}

#[tokio::test]
async fn test_websocket_broadcast_empty() {
    let handler = WebSocketHandler::new();
    
    let result = handler.handle_async(
        "broadcast",
        &[Value::String("Hello everyone".to_string())],
    ).await;
    
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(0));
}

#[tokio::test]
async fn test_websocket_send_and_receive() {
    let handler = WebSocketHandler::new();
    
    // Add a connection
    let mut rx = handler.add_connection("test_conn".to_string());
    
    // Send a message
    let send_result = handler.handle_async(
        "send",
        &[
            Value::String("test_conn".to_string()),
            Value::String("Test message".to_string()),
        ],
    ).await;
    
    assert!(send_result.is_ok());
    assert_eq!(send_result.unwrap(), Value::Boolean(true));
    
    // Receive the message
    if let Some(msg) = rx.recv().await {
        assert_eq!(msg, "Test message");
    } else {
        panic!("Expected to receive message");
    }
}

#[tokio::test]
async fn test_websocket_broadcast() {
    let handler = WebSocketHandler::new();
    
    // Add multiple connections
    let mut rx1 = handler.add_connection("conn1".to_string());
    let mut rx2 = handler.add_connection("conn2".to_string());
    let mut rx3 = handler.add_connection("conn3".to_string());
    
    // Broadcast a message
    let broadcast_result = handler.handle_async(
        "broadcast",
        &[Value::String("Broadcast message".to_string())],
    ).await;
    
    assert!(broadcast_result.is_ok());
    assert_eq!(broadcast_result.unwrap(), Value::Integer(3));
    
    // All connections should receive the message
    assert_eq!(rx1.recv().await.unwrap(), "Broadcast message");
    assert_eq!(rx2.recv().await.unwrap(), "Broadcast message");
    assert_eq!(rx3.recv().await.unwrap(), "Broadcast message");
}

#[tokio::test]
async fn test_websocket_close_connection() {
    let handler = WebSocketHandler::new();
    
    // Add a connection
    let _rx = handler.add_connection("conn_to_close".to_string());
    
    // Verify it exists
    let count_result = handler.handle_sync("connection_count", &[]);
    assert_eq!(count_result.unwrap(), Value::Integer(1));
    
    // Close it
    let close_result = handler.handle_async(
        "close",
        &[Value::String("conn_to_close".to_string())],
    ).await;
    
    assert!(close_result.is_ok());
    assert_eq!(close_result.unwrap(), Value::Boolean(true));
    
    // Verify it's gone
    let count_result = handler.handle_sync("connection_count", &[]);
    assert_eq!(count_result.unwrap(), Value::Integer(0));
}

#[tokio::test]
async fn test_http_server_with_websocket() {
    let handler = HttpServerHandler::new();
    
    // Start server with WebSocket support (second parameter = true)
    let result = handler.handle_async(
        "listen",
        &[Value::Integer(0), Value::Boolean(true)],
    ).await;
    
    assert!(result.is_ok());
    
    if let Ok(Value::Map(server_info)) = result {
        assert!(server_info.contains_key("id"));
        
        // Get WebSocket connections for this server
        if let Some(Value::String(server_id)) = server_info.get("id") {
            let ws_result = handler.handle_sync(
                "ws_connections",
                &[Value::String(server_id.clone())],
            );
            
            assert!(ws_result.is_ok());
            if let Ok(Value::List(connections)) = ws_result {
                assert_eq!(connections.len(), 0);
            }
            
            // Stop the server
            let stop_result = handler.handle_async(
                "stop",
                &[Value::String(server_id.clone())],
            ).await;
            
            assert!(stop_result.is_ok());
        }
    }
}

#[test]
fn test_websocket_operations_are_async() {
    let handler = WebSocketHandler::new();
    
    assert!(!handler.is_async_operation("connections"));
    assert!(!handler.is_async_operation("connection_count"));
    assert!(handler.is_async_operation("send"));
    assert!(handler.is_async_operation("broadcast"));
    assert!(handler.is_async_operation("close"));
}