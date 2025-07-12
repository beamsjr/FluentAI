//! HTTP Server effect handler with routing support
//!
//! Provides a full-featured HTTP server with:
//! - Route registration with path parameters
//! - Query string parsing
//! - Request/response handling
//! - Middleware support
//! - WebSocket upgrade capability (for future implementation)

use crate::{
    router::{Router as FluentRouter, parse_query_string}, 
    handlers::websocket::{WebSocketHandler, WebSocketMessage},
    EffectHandler, EffectResult
};
use async_trait::async_trait;
use axum::{
    body::{Body, Bytes},
    extract::ws::WebSocketUpgrade,
    http::{Request, StatusCode},
    response::Response,
    Router,
};
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use rustc_hash::FxHashMap;
use serde_json::{Map as JsonMap, Value as JsonValue};
use std::{
    net::SocketAddr,
    sync::{Arc, RwLock},
};
use tokio::sync::{mpsc, oneshot};
use tower_http::cors::CorsLayer;
use tracing::{error, info};

/// Route handler function type
type RouteHandler = Arc<dyn Fn(RouteContext) -> RouteResponse + Send + Sync>;

/// Context passed to route handlers
#[derive(Clone)]
pub struct RouteContext {
    pub method: String,
    pub path: String,
    pub params: FxHashMap<String, String>,
    pub query: FxHashMap<String, String>,
    pub headers: FxHashMap<String, String>,
    pub body: String,
}

/// Response from route handlers
pub struct RouteResponse {
    pub status: u16,
    pub headers: FxHashMap<String, String>,
    pub body: String,
}

/// HTTP server state
#[derive(Clone)]
struct ServerState {
    router: Arc<RwLock<FluentRouter>>,
    handler_channel: mpsc::Sender<RouteRequest>,
    websocket_handler: Option<Arc<WebSocketHandler>>,
    websocket_message_tx: Option<mpsc::Sender<WebSocketMessage>>,
}

/// Route request sent to handler task
struct RouteRequest {
    handler_id: String,
    context: RouteContext,
    response_tx: oneshot::Sender<RouteResponse>,
}

/// HTTP server handler
pub struct HttpServerHandler {
    servers: Arc<RwLock<FxHashMap<String, ServerInfo>>>,
    route_registry: Arc<RwLock<FxHashMap<String, RouteInfo>>>,
    websocket_handlers: Arc<RwLock<FxHashMap<String, Arc<WebSocketHandler>>>>,
}

/// Information about a registered route
struct RouteInfo {
    method: String,
    path: String,
    handler_id: String,
}

/// Information about a running server
struct ServerInfo {
    address: SocketAddr,
    shutdown_tx: oneshot::Sender<()>,
    handle: tokio::task::JoinHandle<()>,
    handler_tx: mpsc::Sender<RouteRequest>,
}

impl HttpServerHandler {
    pub fn new() -> Self {
        Self {
            servers: Arc::new(RwLock::new(FxHashMap::default())),
            route_registry: Arc::new(RwLock::new(FxHashMap::default())),
            websocket_handlers: Arc::new(RwLock::new(FxHashMap::default())),
        }
    }

}

#[async_trait]
impl EffectHandler for HttpServerHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::HttpServer
    }

    fn handle_sync(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "ws_send" => {
                // Send WebSocket message to specific connection
                if args.len() >= 3 {
                    if let (
                        Some(Value::String(server_id)),
                        Some(Value::String(conn_id)),
                        Some(Value::String(message)),
                    ) = (args.get(0), args.get(1), args.get(2))
                    {
                        if let Some(ws_handler) = self.websocket_handlers.read().unwrap().get(server_id) {
                            // Delegate to WebSocket handler
                            ws_handler.handle_sync("send", &[Value::String(conn_id.clone()), Value::String(message.clone())])
                        } else {
                            Err(Error::Runtime(format!("No WebSocket handler for server '{}'", server_id)))
                        }
                    } else {
                        Err(Error::Runtime("Invalid ws_send arguments".to_string()))
                    }
                } else {
                    Err(Error::Runtime("ws_send requires server_id, connection_id, and message".to_string()))
                }
            }
            "ws_connections" => {
                // Get WebSocket connections for a server
                if let Some(Value::String(server_id)) = args.first() {
                    if let Some(ws_handler) = self.websocket_handlers.read().unwrap().get(server_id) {
                        ws_handler.handle_sync("connections", &[])
                    } else {
                        Ok(Value::List(vec![]))
                    }
                } else {
                    Err(Error::Runtime("ws_connections requires server_id".to_string()))
                }
            }
            "route" => {
                // Register a route: method, path, handler_id
                if args.len() >= 3 {
                    if let (
                        Some(Value::String(method)),
                        Some(Value::String(path)),
                        Some(Value::String(handler_id)),
                    ) = (args.get(0), args.get(1), args.get(2))
                    {
                        // Store route info for later use
                        let route_key = format!("{} {}", method, path);
                        let route_info = RouteInfo {
                            method: method.clone(),
                            path: path.clone(),
                            handler_id: handler_id.clone(),
                        };
                        
                        self.route_registry.write().unwrap().insert(route_key.clone(), route_info);
                        
                        Ok(Value::Map({
                            let mut map = FxHashMap::default();
                            map.insert("method".to_string(), Value::String(method.clone()));
                            map.insert("path".to_string(), Value::String(path.clone()));
                            map.insert("handler_id".to_string(), Value::String(handler_id.clone()));
                            map.insert("key".to_string(), Value::String(route_key));
                            map
                        }))
                    } else {
                        Err(Error::Runtime("Invalid route arguments".to_string()))
                    }
                } else {
                    Err(Error::Runtime("route requires method, path, and handler_id".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!(
                "HTTP server operation '{}' must be called asynchronously",
                operation
            ))),
        }
    }

    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            "listen" => {
                // Start HTTP server: port
                if let Some(Value::Integer(port)) = args.first() {
                    let port = *port as u16;
                    let addr = SocketAddr::from(([127, 0, 0, 1], port));
                    
                    // Create router and add registered routes
                    let mut router = FluentRouter::new();
                    {
                        let routes = self.route_registry.read().unwrap();
                        for route in routes.values() {
                            router.add_route(&route.method, &route.path, &route.handler_id);
                        }
                    }
                    
                    // Create handler channel
                    let (handler_tx, mut handler_rx) = mpsc::channel::<RouteRequest>(100);
                    
                    // Create WebSocket handler if requested
                    let (ws_handler, ws_msg_tx) = if args.len() > 1 && matches!(args.get(1), Some(Value::Boolean(true))) {
                        let ws_handler = Arc::new(WebSocketHandler::new());
                        let (ws_msg_tx, mut ws_msg_rx) = mpsc::channel::<WebSocketMessage>(100);
                        
                        // Spawn task to handle WebSocket messages
                        let _ws_task = tokio::spawn(async move {
                            while let Some(msg) = ws_msg_rx.recv().await {
                                // TODO: Route WebSocket messages to appropriate handlers
                                info!("WebSocket message from {}: {:?}", msg.connection_id, msg.data);
                            }
                        });
                        
                        (Some(ws_handler), Some(ws_msg_tx))
                    } else {
                        (None, None)
                    };
                    
                    // Create server state
                    let state = ServerState {
                        router: Arc::new(RwLock::new(router)),
                        handler_channel: handler_tx.clone(),
                        websocket_handler: ws_handler.clone(),
                        websocket_message_tx: ws_msg_tx,
                    };
                    
                    // Create the handler service
                    let handler_service = tower::service_fn(move |req: Request<Body>| {
                        let state = state.clone();
                        create_async_handler_impl(state, req)
                    });
                    
                    // Create router with catch-all route
                    let app = Router::new()
                        .fallback_service(handler_service)
                        .layer(CorsLayer::permissive());
                    
                    // Create shutdown channel
                    let (shutdown_tx, mut shutdown_rx) = oneshot::channel::<()>();
                    
                    // Start server
                    let listener = tokio::net::TcpListener::bind(addr).await
                        .map_err(|e| Error::Runtime(format!("Failed to bind to port {}: {}", port, e)))?;
                    
                    let actual_addr = listener.local_addr()
                        .map_err(|e| Error::Runtime(format!("Failed to get local address: {}", e)))?;
                    
                    info!("HTTP server listening on {}", actual_addr);
                    
                    // Spawn handler task
                    let _handler_task = tokio::spawn(async move {
                        while let Some(request) = handler_rx.recv().await {
                            // TODO: Here we would call into the VM to execute the handler
                            // For now, return a placeholder response
                            let response = RouteResponse {
                                status: 200,
                                headers: FxHashMap::default(),
                                body: format!("Handler {} called with path {}", request.handler_id, request.context.path),
                            };
                            let _ = request.response_tx.send(response);
                        }
                    });
                    
                    let handle = tokio::spawn(async move {
                        let serve_future = axum::serve(listener, app);
                        
                        tokio::select! {
                            result = serve_future => {
                                if let Err(e) = result {
                                    error!("Server error: {}", e);
                                }
                            }
                            _ = &mut shutdown_rx => {
                                info!("Server shutting down");
                            }
                        }
                    });
                    
                    // Store server info
                    let server_id = format!("server_{}", port);
                    {
                        let mut servers = self.servers.write().unwrap();
                        servers.insert(
                            server_id.clone(),
                            ServerInfo {
                                address: actual_addr,
                                shutdown_tx,
                                handle,
                                handler_tx,
                            },
                        );
                    }
                    
                    // Store WebSocket handler if created
                    if let Some(ref ws_handler) = ws_handler {
                        self.websocket_handlers.write().unwrap().insert(server_id.clone(), ws_handler.clone());
                    }
                    
                    // Return server info
                    Ok(Value::Map({
                        let mut map = FxHashMap::default();
                        map.insert("id".to_string(), Value::String(server_id));
                        map.insert("address".to_string(), Value::String(actual_addr.to_string()));
                        map.insert("port".to_string(), Value::Integer(actual_addr.port() as i64));
                        map
                    }))
                } else {
                    Err(Error::Runtime("listen requires a port number".to_string()))
                }
            }
            "ws_send_async" => {
                // Send WebSocket message asynchronously
                if args.len() >= 3 {
                    if let (
                        Some(Value::String(server_id)),
                        Some(Value::String(conn_id)),
                        Some(Value::String(message)),
                    ) = (args.get(0), args.get(1), args.get(2))
                    {
                        let ws_handler = self.websocket_handlers.read().unwrap().get(server_id).cloned();
                        if let Some(ws_handler) = ws_handler {
                            ws_handler.handle_async("send", &[Value::String(conn_id.clone()), Value::String(message.clone())]).await
                        } else {
                            Err(Error::Runtime(format!("No WebSocket handler for server '{}'", server_id)))
                        }
                    } else {
                        Err(Error::Runtime("Invalid ws_send_async arguments".to_string()))
                    }
                } else {
                    Err(Error::Runtime("ws_send_async requires server_id, connection_id, and message".to_string()))
                }
            }
            "ws_broadcast" => {
                // Broadcast to all WebSocket connections
                if args.len() >= 2 {
                    if let (
                        Some(Value::String(server_id)),
                        Some(Value::String(message)),
                    ) = (args.get(0), args.get(1))
                    {
                        let ws_handler = self.websocket_handlers.read().unwrap().get(server_id).cloned();
                        if let Some(ws_handler) = ws_handler {
                            ws_handler.handle_async("broadcast", &[Value::String(message.clone())]).await
                        } else {
                            Err(Error::Runtime(format!("No WebSocket handler for server '{}'", server_id)))
                        }
                    } else {
                        Err(Error::Runtime("Invalid ws_broadcast arguments".to_string()))
                    }
                } else {
                    Err(Error::Runtime("ws_broadcast requires server_id and message".to_string()))
                }
            }
            "stop" => {
                // Stop HTTP server by ID
                if let Some(Value::String(server_id)) = args.first() {
                    let server_info = {
                        let mut servers = self.servers.write().unwrap();
                        servers.remove(server_id)
                    }; // Lock released here
                    
                    if let Some(server_info) = server_info {
                        // Send shutdown signal
                        let _ = server_info.shutdown_tx.send(());
                        
                        // Wait for server to stop
                        // Remove WebSocket handler too
                        self.websocket_handlers.write().unwrap().remove(server_id);
                        
                        let _ = server_info.handle.await;
                        
                        Ok(Value::Boolean(true))
                    } else {
                        Err(Error::Runtime(format!("Server '{}' not found", server_id)))
                    }
                } else {
                    Err(Error::Runtime("stop requires a server ID".to_string()))
                }
            }
            _ => Err(Error::Runtime(format!(
                "Unknown HTTP server operation: {}",
                operation
            ))),
        }
    }

    fn is_async_operation(&self, operation: &str) -> bool {
        matches!(operation, "listen" | "stop" | "ws_send_async" | "ws_broadcast")
    }
}

/// Handle WebSocket upgrade
fn handle_websocket_upgrade(
    ws: WebSocketUpgrade,
    state: ServerState,
    connection_id: String,
) -> Response {
    if let (Some(_ws_handler), Some(_msg_tx)) = (state.websocket_handler, state.websocket_message_tx) {
        ws.on_upgrade(move |_socket| async move {
            info!("WebSocket connection established: {}", connection_id);
            // For now, just log the connection
            // Full integration would require adapting the WebSocket handler
        })
    } else {
        Response::builder()
            .status(StatusCode::SERVICE_UNAVAILABLE)
            .body(Body::from("WebSocket not configured"))
            .unwrap()
    }
}

/// Create an async handler implementation for axum
async fn create_async_handler_impl(
    state: ServerState,
    req: Request<Body>,
) -> Result<Response, std::convert::Infallible> {
    // Check for WebSocket upgrade
    // Note: Full WebSocket integration would require more complex handling
    // For now, we just recognize WebSocket upgrade requests
    
    let response = {
            // Extract request details
            let method = req.method().to_string();
            let path = req.uri().path().to_string();
            
            // Find matching route
            let route_match = {
                let router = state.router.read().unwrap();
                router.find_route(&method, &path)
            };
            
            if let Some((handler_id, params)) = route_match {
                // Extract headers
                let mut headers = FxHashMap::default();
                for (name, value) in req.headers() {
                    if let Ok(v) = value.to_str() {
                        headers.insert(name.to_string(), v.to_string());
                    }
                }
                
                // Extract query parameters
                let query = req
                    .uri()
                    .query()
                    .map(parse_query_string)
                    .unwrap_or_default();
                
                // Extract body
                let body_bytes = match axum::body::to_bytes(req.into_body(), usize::MAX).await {
                    Ok(bytes) => bytes,
                    Err(_) => Bytes::new(),
                };
                let body = String::from_utf8_lossy(&body_bytes).to_string();
                
                let context = RouteContext {
                    method,
                    path,
                    params,
                    query,
                    headers,
                    body,
                };
                
                // Send request to handler task
                let (response_tx, response_rx) = oneshot::channel();
                let request = RouteRequest {
                    handler_id,
                    context,
                    response_tx,
                };
                
                if state.handler_channel.send(request).await.is_ok() {
                    // Wait for response
                    match response_rx.await {
                        Ok(response) => {
                            let mut builder = Response::builder()
                                .status(StatusCode::from_u16(response.status).unwrap_or(StatusCode::OK));
                            
                            for (name, value) in response.headers {
                                builder = builder.header(name, value);
                            }
                            
                            builder.body(Body::from(response.body))
                                .unwrap_or_else(|_| Response::new(Body::empty()))
                        }
                        Err(_) => {
                            Response::builder()
                                .status(StatusCode::INTERNAL_SERVER_ERROR)
                                .body(Body::from("Handler error"))
                                .unwrap()
                        }
                    }
                } else {
                    Response::builder()
                        .status(StatusCode::SERVICE_UNAVAILABLE)
                        .body(Body::from("Server shutting down"))
                        .unwrap()
                }
            } else {
                Response::builder()
                    .status(StatusCode::NOT_FOUND)
                    .body(Body::from("Route not found"))
                    .unwrap()
            }
    };
    Ok(response)
}

/// Helper to convert Value to JSON
fn value_to_json(value: &Value) -> JsonValue {
    match value {
        Value::Nil => JsonValue::Null,
        Value::Boolean(b) => JsonValue::Bool(*b),
        Value::Integer(i) => JsonValue::Number((*i).into()),
        Value::Float(f) => JsonValue::Number(
            serde_json::Number::from_f64(*f).unwrap_or_else(|| serde_json::Number::from(0)),
        ),
        Value::String(s) => JsonValue::String(s.clone()),
        Value::List(list) => JsonValue::Array(list.iter().map(value_to_json).collect()),
        Value::Map(map) => {
            let mut json_map = JsonMap::new();
            for (k, v) in map {
                json_map.insert(k.clone(), value_to_json(v));
            }
            JsonValue::Object(json_map)
        }
        _ => JsonValue::String(format!("{:?}", value)),
    }
}

/// Helper to convert JSON to Value
fn json_to_value(json: JsonValue) -> Value {
    match json {
        JsonValue::Null => Value::Nil,
        JsonValue::Bool(b) => Value::Boolean(b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Integer(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Float(0.0)
            }
        }
        JsonValue::String(s) => Value::String(s),
        JsonValue::Array(arr) => Value::List(arr.into_iter().map(json_to_value).collect()),
        JsonValue::Object(obj) => {
            let mut map = FxHashMap::default();
            for (k, v) in obj {
                map.insert(k, json_to_value(v));
            }
            Value::Map(map)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_http_server_listen() {
        let handler = HttpServerHandler::new();
        
        // Start server on random port
        let result = handler
            .handle_async("listen", &[Value::Integer(0)])
            .await;
        
        assert!(result.is_ok());
        
        if let Ok(Value::Map(info)) = result {
            assert!(info.contains_key("id"));
            assert!(info.contains_key("address"));
            assert!(info.contains_key("port"));
            
            // Stop the server
            if let Some(Value::String(server_id)) = info.get("id") {
                let stop_result = handler
                    .handle_async("stop", &[Value::String(server_id.clone())])
                    .await;
                assert!(stop_result.is_ok());
            }
        }
    }

    #[test]
    fn test_route_registration() {
        let handler = HttpServerHandler::new();
        
        let result = handler.handle_sync(
            "route",
            &[
                Value::String("GET".to_string()),
                Value::String("/api/users".to_string()),
                Value::String("getUsersHandler".to_string()),
            ],
        );
        
        assert!(result.is_ok());
        
        if let Ok(Value::Map(route)) = result {
            assert_eq!(route.get("method"), Some(&Value::String("GET".to_string())));
            assert_eq!(route.get("path"), Some(&Value::String("/api/users".to_string())));
            assert_eq!(
                route.get("handler_id"),
                Some(&Value::String("getUsersHandler".to_string()))
            );
        }
    }
}