//! Transport abstraction for MCP server
//!
//! This module provides a common interface for different transport mechanisms
//! (stdio, HTTP/SSE) to support the Model Context Protocol.

use anyhow::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod http;
pub mod stdio;
#[cfg(test)]
pub mod test;

/// JSON-RPC 2.0 Request
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcRequest {
    pub jsonrpc: String,
    pub method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<JsonValue>,
    pub id: JsonValue,
}

/// JSON-RPC 2.0 Response
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcResponse {
    pub jsonrpc: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<JsonValue>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<JsonRpcError>,
    pub id: JsonValue,
}

/// JSON-RPC 2.0 Error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcError {
    pub code: i32,
    pub message: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub data: Option<JsonValue>,
}

/// JSON-RPC 2.0 Notification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JsonRpcNotification {
    pub jsonrpc: String,
    pub method: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub params: Option<JsonValue>,
}

/// Request handler for processing JSON-RPC requests
#[async_trait]
pub trait RequestHandler: Send + Sync + 'static {
    /// Handle a JSON-RPC request and return a response
    async fn handle_request(&self, request: JsonRpcRequest) -> JsonRpcResponse;
}

/// Transport trait for handling MCP communication
#[async_trait]
pub trait Transport: Send + Sync + 'static {
    /// Start the transport and begin listening for requests
    async fn start(&mut self, handler: Arc<dyn RequestHandler>) -> Result<()>;

    /// Send a response back to the client
    async fn send_response(&self, response: JsonRpcResponse) -> Result<()>;

    /// Send a notification to the client
    async fn send_notification(&self, notification: JsonRpcNotification) -> Result<()>;

    /// Receive the next request from the client
    async fn receive_request(&mut self) -> Result<Option<JsonRpcRequest>>;

    /// Check if the transport is still connected
    fn is_connected(&self) -> bool;

    /// Gracefully shutdown the transport
    async fn shutdown(&mut self) -> Result<()>;
}

/// Transport factory for creating transport instances
pub enum TransportType {
    Stdio,
    Http { port: u16 },
}

impl TransportType {
    /// Create a new transport instance based on the type
    pub async fn create(self) -> Result<Box<dyn Transport>> {
        match self {
            TransportType::Stdio => Ok(Box::new(stdio::StdioTransport::new())),
            TransportType::Http { port } => Ok(Box::new(http::HttpTransport::new(port))),
        }
    }
}

/// Session state that can be shared across transport mechanisms
#[derive(Debug, Clone)]
pub struct SessionState {
    pub session_id: String,
    pub initialized: bool,
    pub client_info: Option<ClientInfo>,
}

/// Client information from initialization
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClientInfo {
    pub name: String,
    pub version: String,
}

impl Default for SessionState {
    fn default() -> Self {
        Self {
            session_id: uuid::Uuid::new_v4().to_string(),
            initialized: false,
            client_info: None,
        }
    }
}

/// Shared session manager for all transports
pub type SessionManager = Arc<RwLock<std::collections::HashMap<String, SessionState>>>;

/// Create a new session manager
pub fn create_session_manager() -> SessionManager {
    Arc::new(RwLock::new(std::collections::HashMap::new()))
}
