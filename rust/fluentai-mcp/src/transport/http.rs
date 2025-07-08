//! HTTP transport implementation for MCP with SSE support
//!
//! This transport implements the MCP HTTP/SSE specification for web-based clients.

use super::{Transport, JsonRpcRequest, JsonRpcResponse, JsonRpcNotification, SessionManager, RequestHandler};
use async_trait::async_trait;
use anyhow::Result;
use axum::{
    extract::{Path, State},
    http::StatusCode,
    response::{
        sse::{Event, KeepAlive, Sse},
        Json, IntoResponse,
    },
    routing::{get, post},
    Router,
};
use dashmap::DashMap;
use serde::{Deserialize, Serialize};
use std::{sync::Arc, time::Duration};
use tokio::sync::{broadcast, mpsc};
use tower_http::cors::CorsLayer;
use tracing::{debug, error, info};
use uuid::Uuid;

/// HTTP transport for MCP communication
pub struct HttpTransport {
    port: u16,
    request_receiver: Option<mpsc::Receiver<JsonRpcRequest>>,
    request_handler: Option<Arc<dyn super::RequestHandler>>,
    response_channels: Arc<DashMap<String, broadcast::Sender<JsonRpcResponse>>>,
    notification_sender: Arc<broadcast::Sender<JsonRpcNotification>>,
    server_handle: Option<tokio::task::JoinHandle<()>>,
}

/// Shared state for the HTTP server
#[derive(Clone)]
struct HttpServerState {
    request_handler: Arc<dyn super::RequestHandler>,
    notification_sender: Arc<broadcast::Sender<JsonRpcNotification>>,
    sessions: SessionManager,
}

/// Session creation request
#[derive(Debug, Serialize, Deserialize)]
struct CreateSessionRequest {
    pub client_info: ClientInfo,
}

/// Client info from session creation
#[derive(Debug, Clone, Serialize, Deserialize)]
struct ClientInfo {
    pub name: String,
    pub version: String,
}

/// Session creation response
#[derive(Debug, Serialize, Deserialize)]
struct CreateSessionResponse {
    pub session_id: String,
    pub sse_endpoint: String,
}

impl HttpTransport {
    /// Create a new HTTP transport
    pub fn new(port: u16) -> Self {
        let response_channels = Arc::new(DashMap::new());
        let (notification_sender, _) = broadcast::channel(100);
        
        Self {
            port,
            request_receiver: None,
            request_handler: None,
            response_channels,
            notification_sender: Arc::new(notification_sender),
            server_handle: None,
        }
    }
    
    /// Start the HTTP server
    async fn start_server(
        &mut self,
        handler: Arc<dyn super::RequestHandler>,
    ) -> Result<()> {
        let state = HttpServerState {
            request_handler: handler,
            notification_sender: self.notification_sender.clone(),
            sessions: super::create_session_manager(),
        };
        
        let app = Router::new()
            .route("/sessions", post(create_session))
            .route("/sessions/:session_id/messages", post(handle_message))
            .route("/sessions/:session_id/sse", get(sse_handler))
            .layer(CorsLayer::permissive())
            .with_state(state);
        
        let addr = format!("0.0.0.0:{}", self.port);
        info!("Starting HTTP server on {}", addr);
        
        let listener = tokio::net::TcpListener::bind(&addr).await?;
        
        // Spawn the server task
        let server_handle = tokio::spawn(async move {
            axum::serve(listener, app)
                .await
                .expect("HTTP server failed");
        });
        
        self.server_handle = Some(server_handle);
        
        Ok(())
    }
}

#[async_trait]
impl Transport for HttpTransport {
    async fn start(&mut self, handler: Arc<dyn super::RequestHandler>) -> Result<()> {
        if self.request_handler.is_some() {
            return Err(anyhow::anyhow!("Transport already started"));
        }
        
        self.request_handler = Some(handler.clone());
        
        self.start_server(handler).await?;
        Ok(())
    }
    
    async fn send_response(&self, response: JsonRpcResponse) -> Result<()> {
        let id = match &response.id {
            serde_json::Value::String(s) => s.clone(),
            other => other.to_string(),
        };
        
        if let Some((_, sender)) = self.response_channels.remove(&id) {
            let _ = sender.send(response);
        }
        
        Ok(())
    }
    
    async fn send_notification(&self, notification: JsonRpcNotification) -> Result<()> {
        let _ = self.notification_sender.send(notification);
        Ok(())
    }
    
    async fn receive_request(&mut self) -> Result<Option<JsonRpcRequest>> {
        // For HTTP transport, requests are received through the web server
        // This method is not used in the traditional sense
        Ok(None)
    }
    
    fn is_connected(&self) -> bool {
        self.server_handle.is_some()
    }
    
    async fn shutdown(&mut self) -> Result<()> {
        if let Some(handle) = self.server_handle.take() {
            handle.abort();
        }
        Ok(())
    }
}

/// Handler for creating a new session
async fn create_session(
    State(state): State<HttpServerState>,
    Json(req): Json<CreateSessionRequest>,
) -> impl IntoResponse {
    let session_id = Uuid::new_v4().to_string();
    
    // Create new session
    {
        let mut sessions = state.sessions.write().await;
        let new_session = super::SessionState {
            session_id: session_id.clone(),
            initialized: false,
            client_info: Some(super::ClientInfo {
                name: req.client_info.name,
                version: req.client_info.version,
            }),
        };
        sessions.insert(session_id.clone(), new_session);
    }
    
    let response = CreateSessionResponse {
        session_id: session_id.clone(),
        sse_endpoint: format!("/sessions/{}/sse", session_id),
    };
    
    Json(response)
}

/// Handler for JSON-RPC messages
async fn handle_message(
    State(state): State<HttpServerState>,
    Path(session_id): Path<String>,
    Json(request): Json<JsonRpcRequest>,
) -> impl IntoResponse {
    // Verify session exists
    {
        let sessions = state.sessions.read().await;
        if !sessions.contains_key(&session_id) {
            return (StatusCode::NOT_FOUND, "Session not found").into_response();
        }
    }
    
    // Handle the request directly
    let response = state.request_handler.handle_request(request).await;
    Json(response).into_response()
}

/// SSE handler for server-sent events
async fn sse_handler(
    State(state): State<HttpServerState>,
    Path(session_id): Path<String>,
) -> impl IntoResponse {
    // Verify session exists
    let valid_session = {
        let sessions = state.sessions.read().await;
        sessions.contains_key(&session_id)
    };
    
    if !valid_session {
        // Return empty stream for invalid session
        let stream = futures::stream::empty::<Result<Event, std::convert::Infallible>>();
        return Sse::new(stream).keep_alive(KeepAlive::default()).into_response();
    }
    
    let mut notification_receiver = state.notification_sender.subscribe();
    
    let stream = async_stream::stream! {
        loop {
            match notification_receiver.recv().await {
                Ok(notification) => {
                    let data = serde_json::to_string(&notification).unwrap_or_default();
                    yield Ok(Event::default().data(data)) as Result<Event, std::convert::Infallible>;
                }
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    debug!("SSE receiver lagged by {} messages", n);
                    continue;
                }
                Err(broadcast::error::RecvError::Closed) => {
                    debug!("SSE notification channel closed");
                    break;
                }
            }
        }
    };
    
    Sse::new(stream).keep_alive(KeepAlive::default()).into_response()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::transport::SessionState;
    use serde_json::Value as JsonValue;
    
    #[test]
    fn test_http_transport_creation() {
        let transport = HttpTransport::new(3000);
        assert!(!transport.is_connected());
    }
    
    #[tokio::test]
    async fn test_http_transport_double_start() {
        let mut transport = HttpTransport::new(0);
        
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
        
        let handler = Arc::new(MockHandler);
        
        // First start should succeed
        let result1 = transport.start(handler.clone()).await;
        assert!(result1.is_ok());
        
        // Second start should fail
        let result2 = transport.start(handler).await;
        assert!(result2.is_err());
        assert!(result2.unwrap_err().to_string().contains("already started"));
    }
    
    #[test]
    fn test_session_state_default() {
        let session = SessionState::default();
        assert!(!session.session_id.is_empty());
        assert!(!session.initialized);
        assert!(session.client_info.is_none());
    }
}