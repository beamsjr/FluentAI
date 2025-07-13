//! WebSocket handler for bidirectional communication
//!
//! Provides WebSocket support for real-time, bidirectional communication
//! between client and server.

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use tokio::sync::{mpsc, oneshot};

/// WebSocket connection information
pub struct WebSocketConnection {
    pub id: String,
    pub sender: mpsc::Sender<String>,
    pub close_tx: oneshot::Sender<()>,
}

/// WebSocket handler
pub struct WebSocketHandler {
    connections: Arc<RwLock<FxHashMap<String, WebSocketConnection>>>,
}

impl WebSocketHandler {
    pub fn new() -> Self {
        Self {
            connections: Arc::new(RwLock::new(FxHashMap::default())),
        }
    }

    /// Add a new WebSocket connection
    pub fn add_connection(&self, connection_id: String) -> mpsc::Receiver<String> {
        let (tx, rx) = mpsc::channel::<String>(100);
        let (close_tx, _close_rx) = oneshot::channel::<()>();
        
        let mut connections = self.connections.write().unwrap();
        connections.insert(
            connection_id.clone(),
            WebSocketConnection {
                id: connection_id,
                sender: tx,
                close_tx,
            },
        );
        
        rx
    }
    
    /// Remove a WebSocket connection
    pub fn remove_connection(&self, connection_id: &str) {
        let mut connections = self.connections.write().unwrap();
        if let Some(conn) = connections.remove(connection_id) {
            let _ = conn.close_tx.send(());
        }
    }
}

/// WebSocket message type
#[derive(Debug, Clone)]
pub enum MessageType {
    Text,
    Binary,
}

/// WebSocket message
#[derive(Debug, Clone)]
pub struct WebSocketMessage {
    pub connection_id: String,
    pub message_type: MessageType,
    pub data: String,
}

#[async_trait]
impl EffectHandler for WebSocketHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::WebSocket
    }

    fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
        match operation {
            "connections" => {
                // Get list of active connections
                let connections = self.connections.read().unwrap();
                let conn_ids: Vec<Value> = connections
                    .keys()
                    .map(|id| Value::String(id.clone()))
                    .collect();
                Ok(Value::List(conn_ids))
            }
            "connection_count" => {
                // Get number of active connections
                let count = self.connections.read().unwrap().len() as i64;
                Ok(Value::Integer(count))
            }
            _ => Err(Error::Runtime(format!(
                "WebSocket operation '{}' must be called asynchronously",
                operation
            ))),
        }
    }

    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "send" => {
                // Send message to specific connection
                if args.len() >= 2 {
                    if let (Some(Value::String(conn_id)), Some(Value::String(message))) =
                        (args.get(0), args.get(1))
                    {
                        let sender = {
                            let connections = self.connections.read().unwrap();
                            connections.get(conn_id).map(|conn| conn.sender.clone())
                        };
                        
                        if let Some(sender) = sender {
                            if sender.send(message.clone()).await.is_ok() {
                                Ok(Value::Boolean(true))
                            } else {
                                Err(Error::Runtime("Failed to send message".to_string()))
                            }
                        } else {
                            Err(Error::Runtime(format!("Connection '{}' not found", conn_id)))
                        }
                    } else {
                        Err(Error::Runtime("send requires connection ID and message".to_string()))
                    }
                } else {
                    Err(Error::Runtime("send requires 2 arguments".to_string()))
                }
            }
            "broadcast" => {
                // Send message to all connections
                if let Some(Value::String(message)) = args.first() {
                    let senders: Vec<_> = {
                        let connections = self.connections.read().unwrap();
                        connections.values().map(|conn| conn.sender.clone()).collect()
                    };
                    
                    let mut sent = 0;
                    for sender in senders {
                        if sender.send(message.clone()).await.is_ok() {
                            sent += 1;
                        }
                    }
                    
                    Ok(Value::Integer(sent))
                } else {
                    Err(Error::Runtime("broadcast requires a message".to_string()))
                }
            }
            "close" => {
                // Close specific connection
                if let Some(Value::String(conn_id)) = args.first() {
                    let mut connections = self.connections.write().unwrap();
                    if let Some(conn) = connections.remove(conn_id) {
                        let _ = conn.close_tx.send(());
                        Ok(Value::Boolean(true))
                    } else {
                        Err(Error::Runtime(format!("Connection '{}' not found", conn_id)))
                    }
                } else {
                    Err(Error::Runtime("close requires a connection ID".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!(
                "Unknown WebSocket operation: {}",
                operation
            ))),
        }
    }

    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, "send" | "broadcast" | "close")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_websocket_handler_creation() {
        let handler = WebSocketHandler::new();
        assert_eq!(handler.effect_type(), EffectType::WebSocket);
    }

    #[test]
    fn test_connection_count() {
        let handler = WebSocketHandler::new();
        let result = handler.handle_sync("connection_count", &[]);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Integer(0));
    }

    #[test]
    fn test_connections_list() {
        let handler = WebSocketHandler::new();
        let result = handler.handle_sync("connections", &[]);
        assert!(result.is_ok());
        if let Ok(Value::List(conns)) = result {
            assert!(conns.is_empty());
        } else {
            panic!("Expected list of connections");
        }
    }

    #[tokio::test]
    async fn test_send_without_connection() {
        let handler = WebSocketHandler::new();
        let result = handler
            .handle_async(
                "send",
                &[
                    Value::String("conn123".to_string()),
                    Value::String("Hello".to_string()),
                ],
            )
            .await;
        
        assert!(result.is_err());
        if let Err(Error::Runtime(msg)) = result {
            assert!(msg.contains("not found"));
        }
    }

    #[tokio::test]
    async fn test_broadcast_empty() {
        let handler = WebSocketHandler::new();
        let result = handler
            .handle_async("broadcast", &[Value::String("Hello everyone".to_string())])
            .await;
        
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Integer(0));
    }
}