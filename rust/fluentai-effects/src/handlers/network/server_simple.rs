//! Simplified HTTP server implementation for Network effect

use axum::{
    Router,
    routing::any,
    response::{Json, IntoResponse},
    extract::{State, Query, Request},
    http::{StatusCode, Method},
    body::to_bytes,
};
use fluentai_core::{value::Value, error::Error};
use rustc_hash::FxHashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot, Mutex};
use tower_http::cors::CorsLayer;
use std::net::SocketAddr;

/// Server configuration
#[derive(Clone)]
pub struct ServerConfig {
    pub host: String,
    pub port: u16,
    pub routes: Arc<Mutex<Vec<RouteDefinition>>>,
    pub middleware: Vec<String>,
}

/// Route definition
#[derive(Clone)]
pub struct RouteDefinition {
    pub method: Method,
    pub path: String,
    pub handler_id: String,
}

/// Shared state for the server
#[derive(Clone)]
struct ServerState {
    request_sender: mpsc::Sender<(HttpRequest, oneshot::Sender<HttpResponse>)>,
    routes: Arc<Mutex<Vec<RouteDefinition>>>,
}

/// HTTP request representation
#[derive(Debug)]
pub struct HttpRequest {
    pub method: String,
    pub path: String,
    pub headers: FxHashMap<String, String>,
    pub query: FxHashMap<String, String>,
    pub body: Option<Value>,
    pub params: FxHashMap<String, String>,
}

/// HTTP response representation
#[derive(Debug)]
pub struct HttpResponse {
    pub status: u16,
    pub headers: FxHashMap<String, String>,
    pub body: Value,
}

impl Default for HttpResponse {
    fn default() -> Self {
        Self {
            status: 200,
            headers: FxHashMap::default(),
            body: Value::Nil,
        }
    }
}

/// Start an HTTP server
pub async fn start_server(
    config: ServerConfig,
    request_sender: mpsc::Sender<(HttpRequest, oneshot::Sender<HttpResponse>)>,
) -> Result<(), Error> {
    let state = ServerState { 
        request_sender,
        routes: config.routes.clone(),
    };
    
    // Build the router - use a catch-all handler
    let app = Router::new()
        .fallback(any(handle_any_request))
        .layer(CorsLayer::permissive())
        .with_state(state);
    
    // Create the server address
    let addr = format!("{}:{}", config.host, config.port).parse::<SocketAddr>()
        .map_err(|e| Error::Runtime(format!("Invalid server address: {}", e)))?;
    
    // Start the server
    let listener = tokio::net::TcpListener::bind(addr).await
        .map_err(|e| Error::Runtime(format!("Failed to bind to address: {}", e)))?;
    
    axum::serve(listener, app.into_make_service())
        .await
        .map_err(|e| Error::Runtime(format!("Server error: {}", e)))?;
    
    Ok(())
}

/// Handle any incoming HTTP request
async fn handle_any_request(
    State(state): State<ServerState>,
    Query(query): Query<FxHashMap<String, String>>,
    request: Request,
) -> impl IntoResponse {
    let method = request.method().clone();
    let path = request.uri().path().to_string();
    
    // Convert headers to HashMap
    let mut headers = FxHashMap::default();
    for (key, value) in request.headers() {
        if let Ok(v) = value.to_str() {
            headers.insert(key.as_str().to_string(), v.to_string());
        }
    }
    
    // Extract body
    let body_bytes = match to_bytes(request.into_body(), usize::MAX).await {
        Ok(bytes) => bytes,
        Err(_) => {
            return (
                StatusCode::BAD_REQUEST,
                Json(serde_json::json!({
                    "error": "Failed to read request body"
                }))
            ).into_response();
        }
    };
    
    let body_value = if !body_bytes.is_empty() {
        if let Ok(text) = String::from_utf8(body_bytes.to_vec()) {
            if let Ok(json) = serde_json::from_str::<serde_json::Value>(&text) {
                json_to_value(&json)
            } else {
                Some(Value::String(text))
            }
        } else {
            None
        }
    } else {
        None
    };
    
    // Create the request
    let request = HttpRequest {
        method: method.to_string(),
        path,
        headers,
        query,
        body: body_value,
        params: FxHashMap::default(),
    };
    
    // Send request to FluentAI runtime and wait for response
    let (response_tx, response_rx) = oneshot::channel();
    
    if let Err(_) = state.request_sender.send((request, response_tx)).await {
        return (
            StatusCode::INTERNAL_SERVER_ERROR,
            Json(serde_json::json!({
                "error": "Failed to process request"
            }))
        ).into_response();
    }
    
    // Wait for response from FluentAI
    match response_rx.await {
        Ok(response) => {
            let status = StatusCode::from_u16(response.status).unwrap_or(StatusCode::OK);
            let json_body = value_to_serde_json(&response.body);
            (status, Json(json_body)).into_response()
        }
        Err(_) => {
            (
                StatusCode::INTERNAL_SERVER_ERROR,
                Json(serde_json::json!({
                    "error": "Request timeout"
                }))
            ).into_response()
        }
    }
}

/// Convert serde_json::Value to fluentai Value
fn json_to_value(json: &serde_json::Value) -> Option<Value> {
    Some(match json {
        serde_json::Value::Null => Value::Nil,
        serde_json::Value::Bool(b) => Value::Boolean(*b),
        serde_json::Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Integer(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Float(0.0)
            }
        }
        serde_json::Value::String(s) => Value::String(s.clone()),
        serde_json::Value::Array(arr) => {
            let values: Vec<_> = arr
                .iter()
                .filter_map(json_to_value)
                .collect();
            Value::List(values)
        }
        serde_json::Value::Object(obj) => {
            let mut map = FxHashMap::default();
            for (k, v) in obj {
                if let Some(val) = json_to_value(v) {
                    map.insert(k.clone(), val);
                }
            }
            Value::Map(map)
        }
    })
}

/// Convert fluentai Value to serde_json::Value
fn value_to_serde_json(value: &Value) -> serde_json::Value {
    match value {
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Integer(i) => serde_json::Value::Number((*i).into()),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::Boolean(b) => serde_json::Value::Bool(*b),
        Value::Nil => serde_json::Value::Null,
        Value::List(items) => {
            let json_items: Vec<_> = items
                .iter()
                .map(value_to_serde_json)
                .collect();
            serde_json::Value::Array(json_items)
        }
        Value::Map(map) => {
            let mut json_map = serde_json::Map::new();
            for (k, v) in map {
                json_map.insert(k.clone(), value_to_serde_json(v));
            }
            serde_json::Value::Object(json_map)
        }
        _ => serde_json::Value::Null,
    }
}