//! HTTP server implementation for Network effect

use axum::{
    Router,
    routing::{get, post, put, delete, patch},
    response::{Json, Response, IntoResponse},
    extract::{Path, Query, State},
    http::{StatusCode, HeaderMap, Method},
    body::Body,
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
    pub handler_id: String, // ID of the handler function in FluentAI
}

/// Shared state for the server
#[derive(Clone)]
pub struct ServerState {
    /// Channel to send requests to the FluentAI runtime
    pub request_sender: mpsc::Sender<(HttpRequest, oneshot::Sender<HttpResponse>)>,
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
    let state = ServerState { request_sender };
    
    // Build the router with state
    let mut app = Router::new().with_state(state.clone());
    
    // Add routes dynamically based on configuration
    let routes = config.routes.lock().await;
    for route in routes.iter() {
        app = add_route(app, route.clone(), state.clone());
    }
    
    // Add middleware
    app = app.layer(CorsLayer::permissive());
    
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

/// Add a route to the router
fn add_route(
    mut router: Router,
    route: RouteDefinition,
    state: ServerState,
) -> Router {
    // For now, use a simplified approach without path parameters
    let state_clone = state.clone();
    
    match route.method {
        Method::GET => {
            router = router.route(&route.path, get(move |
                State(state): State<ServerState>,
                headers: HeaderMap,
                Query(query): Query<FxHashMap<String, String>>,
            | async move {
                let params = FxHashMap::default();
                let path = Path(vec![]);
                handle_request(Method::GET, path, Query(query), headers, String::new(), state).await
            }).with_state(state_clone));
        }
        Method::POST => {
            router = router.route(&route.path, post(move |
                State(state): State<ServerState>,
                headers: HeaderMap,
                Query(query): Query<FxHashMap<String, String>>,
                body: String,
            | async move {
                let params = FxHashMap::default();
                let path = Path(vec![]);
                handle_request(Method::POST, path, Query(query), headers, body, state).await
            }).with_state(state_clone));
        }
        Method::PUT => {
            router = router.route(&route.path, put(move |
                State(state): State<ServerState>,
                headers: HeaderMap,
                Query(query): Query<FxHashMap<String, String>>,
                body: String,
            | async move {
                let params = FxHashMap::default();
                let path = Path(vec![]);
                handle_request(Method::PUT, path, Query(query), headers, body, state).await
            }).with_state(state_clone));
        }
        Method::DELETE => {
            router = router.route(&route.path, delete(move |
                State(state): State<ServerState>,
                headers: HeaderMap,
                Query(query): Query<FxHashMap<String, String>>,
            | async move {
                let params = FxHashMap::default();
                let path = Path(vec![]);
                handle_request(Method::DELETE, path, Query(query), headers, String::new(), state).await
            }).with_state(state_clone));
        }
        Method::PATCH => {
            router = router.route(&route.path, patch(move |
                State(state): State<ServerState>,
                headers: HeaderMap,
                Query(query): Query<FxHashMap<String, String>>,
                body: String,
            | async move {
                let params = FxHashMap::default();
                let path = Path(vec![]);
                handle_request(Method::PATCH, path, Query(query), headers, body, state).await
            }).with_state(state_clone));
        }
        _ => {} // Unsupported method, skip
    }
    
    router
}

/// Handle an incoming HTTP request
async fn handle_request(
    method: Method,
    Path(params): Path<Vec<(String, String)>>,
    Query(query): Query<FxHashMap<String, String>>,
    headers: HeaderMap,
    body: String,
    state: ServerState,
) -> impl IntoResponse {
    // Convert headers to HashMap
    let mut header_map = FxHashMap::default();
    for (key, value) in headers.iter() {
        if let Ok(v) = value.to_str() {
            header_map.insert(key.as_str().to_string(), v.to_string());
        }
    }
    
    // Parse body if it's JSON
    let body_value = if !body.is_empty() {
        if let Ok(json) = serde_json::from_str::<serde_json::Value>(&body) {
            json_to_value(&json)
        } else {
            Some(Value::String(body))
        }
    } else {
        None
    };
    
    // Convert params to HashMap
    let param_map: FxHashMap<String, String> = params.into_iter().collect();
    
    // Create the request
    let request = HttpRequest {
        method: method.to_string(),
        path: headers.get("x-original-path")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("/")
            .to_string(),
        headers: header_map,
        query,
        body: body_value,
        params: param_map,
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
            // Build axum response
            let mut builder = Response::builder()
                .status(response.status);
            
            // Add headers
            for (key, value) in response.headers {
                builder = builder.header(key, value);
            }
            
            // Convert body to JSON
            let json_body = value_to_serde_json(&response.body);
            
            builder
                .body(Body::from(serde_json::to_string(&json_body).unwrap_or_default()))
                .unwrap_or_else(|_| {
                    Response::builder()
                        .status(StatusCode::INTERNAL_SERVER_ERROR)
                        .body(Body::empty())
                        .unwrap()
                })
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