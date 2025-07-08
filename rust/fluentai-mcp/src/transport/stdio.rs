//! Stdio transport implementation for MCP
//!
//! This transport communicates via stdin/stdout, which is the traditional
//! method for MCP servers.

use super::{Transport, JsonRpcRequest, JsonRpcResponse, JsonRpcNotification};
use async_trait::async_trait;
use anyhow::Result;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::sync::Mutex;
use std::sync::Arc;
use tracing::{debug, error};

/// Stdio transport for MCP communication
pub struct StdioTransport {
    stdout: Arc<Mutex<tokio::io::Stdout>>,
    stdin: Arc<Mutex<BufReader<tokio::io::Stdin>>>,
    connected: Arc<Mutex<bool>>,
}

impl StdioTransport {
    /// Create a new stdio transport
    pub fn new() -> Self {
        Self {
            stdout: Arc::new(Mutex::new(tokio::io::stdout())),
            stdin: Arc::new(Mutex::new(BufReader::new(tokio::io::stdin()))),
            connected: Arc::new(Mutex::new(false)),
        }
    }
}

#[async_trait]
impl Transport for StdioTransport {
    async fn start(&mut self, _handler: Arc<dyn super::RequestHandler>) -> Result<()> {
        *self.connected.lock().await = true;
        debug!("Stdio transport started");
        Ok(())
    }
    
    async fn send_response(&self, response: JsonRpcResponse) -> Result<()> {
        let json = serde_json::to_string(&response)?;
        let mut stdout = self.stdout.lock().await;
        stdout.write_all(json.as_bytes()).await?;
        stdout.write_all(b"\n").await?;
        stdout.flush().await?;
        debug!("Sent response: {}", json);
        Ok(())
    }
    
    async fn send_notification(&self, notification: JsonRpcNotification) -> Result<()> {
        let json = serde_json::to_string(&notification)?;
        let mut stdout = self.stdout.lock().await;
        stdout.write_all(json.as_bytes()).await?;
        stdout.write_all(b"\n").await?;
        stdout.flush().await?;
        debug!("Sent notification: {}", json);
        Ok(())
    }
    
    async fn receive_request(&mut self) -> Result<Option<JsonRpcRequest>> {
        let mut stdin = self.stdin.lock().await;
        let mut line = String::new();
        
        match stdin.read_line(&mut line).await {
            Ok(0) => {
                // EOF reached
                *self.connected.lock().await = false;
                Ok(None)
            }
            Ok(_) => {
                let line = line.trim();
                if line.is_empty() {
                    return Ok(None);
                }
                
                debug!("Received: {}", line);
                
                match serde_json::from_str::<JsonRpcRequest>(line) {
                    Ok(request) => Ok(Some(request)),
                    Err(e) => {
                        error!("Failed to parse JSON-RPC request: {}", e);
                        Err(e.into())
                    }
                }
            }
            Err(e) => {
                error!("Failed to read from stdin: {}", e);
                *self.connected.lock().await = false;
                Err(e.into())
            }
        }
    }
    
    fn is_connected(&self) -> bool {
        // This will block briefly but should be fine for stdio
        futures::executor::block_on(async {
            *self.connected.lock().await
        })
    }
    
    async fn shutdown(&mut self) -> Result<()> {
        *self.connected.lock().await = false;
        debug!("Stdio transport shutdown");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transport::RequestHandler;
    use serde_json::Value as JsonValue;
    
    #[tokio::test]
    async fn test_stdio_transport_lifecycle() {
        let mut transport = StdioTransport::new();
        
        // Initially not connected
        assert!(!transport.is_connected());
        
        // Create a mock handler
        struct MockHandler;
        
        #[async_trait]
        impl RequestHandler for MockHandler {
            async fn handle_request(&self, _request: JsonRpcRequest) -> JsonRpcResponse {
                JsonRpcResponse {
                    jsonrpc: "2.0".to_string(),
                    id: JsonValue::Number(1.into()),
                    result: Some(JsonValue::Null),
                    error: None,
                }
            }
        }
        
        // Start transport with handler
        let handler = Arc::new(MockHandler);
        let result = transport.start(handler).await;
        assert!(result.is_ok());
        assert!(transport.is_connected());
        
        // Shutdown transport
        let shutdown_result = transport.shutdown().await;
        assert!(shutdown_result.is_ok());
        assert!(!transport.is_connected());
    }
}