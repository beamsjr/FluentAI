//! Test transport for integration testing

use super::{Transport, JsonRpcRequest, JsonRpcResponse, JsonRpcNotification, RequestHandler};
use async_trait::async_trait;
use anyhow::Result;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

/// Test transport that directly connects requests to handlers
#[derive(Clone)]
pub struct TestTransport {
    /// Channel for sending requests to the server
    request_sender: mpsc::Sender<JsonRpcRequest>,
    request_receiver: Arc<Mutex<mpsc::Receiver<JsonRpcRequest>>>,
    
    /// Channel for test to receive responses
    test_response_sender: mpsc::Sender<JsonRpcResponse>,
    test_response_receiver: Arc<Mutex<mpsc::Receiver<JsonRpcResponse>>>,
    
    /// Channel for notifications
    notification_sender: mpsc::Sender<JsonRpcNotification>,
    notification_receiver: Arc<Mutex<mpsc::Receiver<JsonRpcNotification>>>,
    
    /// Connection status
    connected: Arc<Mutex<bool>>,
    
    /// Request handler - set when server starts
    handler: Arc<Mutex<Option<Arc<dyn RequestHandler>>>>,
}

impl TestTransport {
    pub fn new() -> Self {
        let (req_tx, req_rx) = mpsc::channel(100);
        let (test_resp_tx, test_resp_rx) = mpsc::channel(100);
        let (notif_tx, notif_rx) = mpsc::channel(100);
        
        Self {
            request_sender: req_tx,
            request_receiver: Arc::new(Mutex::new(req_rx)),
            test_response_sender: test_resp_tx,
            test_response_receiver: Arc::new(Mutex::new(test_resp_rx)),
            notification_sender: notif_tx,
            notification_receiver: Arc::new(Mutex::new(notif_rx)),
            connected: Arc::new(Mutex::new(true)),
            handler: Arc::new(Mutex::new(None)),
        }
    }
    
    /// Send a request and get response (for testing)
    /// This simulates a client sending a request
    pub async fn send_request_and_receive(&self, request: JsonRpcRequest) -> Result<JsonRpcResponse> {
        // Send request to server
        self.request_sender.send(request).await
            .map_err(|e| anyhow::anyhow!("Failed to send request: {}", e))?;
        
        // Wait for response
        let mut receiver = self.test_response_receiver.lock().await;
        receiver.recv().await
            .ok_or_else(|| anyhow::anyhow!("No response received"))
    }
    
    /// Receive a notification from the transport (for testing)
    pub async fn receive_notification(&self) -> Result<Option<JsonRpcNotification>> {
        let mut receiver = self.notification_receiver.lock().await;
        Ok(receiver.recv().await)
    }
}

#[async_trait]
impl Transport for TestTransport {
    async fn start(&mut self, handler: Arc<dyn RequestHandler>) -> Result<()> {
        *self.handler.lock().await = Some(handler);
        Ok(())
    }
    
    async fn send_response(&self, response: JsonRpcResponse) -> Result<()> {
        // When server sends a response, forward it to the test
        self.test_response_sender.send(response).await
            .map_err(|e| anyhow::anyhow!("Failed to send response: {}", e))
    }
    
    async fn send_notification(&self, notification: JsonRpcNotification) -> Result<()> {
        self.notification_sender.send(notification).await
            .map_err(|e| anyhow::anyhow!("Failed to send notification: {}", e))
    }
    
    async fn receive_request(&mut self) -> Result<Option<JsonRpcRequest>> {
        // Server receives requests from the test
        let mut receiver = self.request_receiver.lock().await;
        Ok(receiver.recv().await)
    }
    
    fn is_connected(&self) -> bool {
        // For tests, always return true until explicitly shutdown
        true
    }
    
    async fn shutdown(&mut self) -> Result<()> {
        *self.connected.lock().await = false;
        Ok(())
    }
}