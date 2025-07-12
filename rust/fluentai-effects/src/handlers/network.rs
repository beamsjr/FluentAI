//! Network effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use reqwest::{self, Method};
use rustc_hash::FxHashMap;
use std::time::Duration;
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot, Mutex};
use axum::http::Method as AxumMethod;
use uuid::Uuid;

mod server_simple;
use server_simple::{ServerConfig, RouteDefinition, HttpRequest, HttpResponse};

pub struct NetworkHandler {
    client: reqwest::Client,
    servers: Arc<Mutex<FxHashMap<String, ServerInfo>>>,
}

struct ServerInfo {
    config: ServerConfig,
    sender: mpsc::Sender<(HttpRequest, oneshot::Sender<HttpResponse>)>,
    handle: Option<tokio::task::JoinHandle<()>>,
    request_handle: Option<tokio::task::JoinHandle<()>>,
}

impl NetworkHandler {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::builder()
                .timeout(Duration::from_secs(30))
                .build()
                .unwrap_or_else(|_| reqwest::Client::new()),
            servers: Arc::new(Mutex::new(FxHashMap::default())),
        }
    }
    
    /// Parse request options from a Value::Map
    fn parse_request_options(options: &Value) -> Result<RequestOptions, Error> {
        let mut opts = RequestOptions::default();
        
        if let Value::Map(map) = options {
            // Parse headers
            if let Some(Value::Map(headers)) = map.get("headers") {
                for (key, value) in headers {
                    if let Value::String(val) = value {
                        opts.headers.insert(key.clone(), val.clone());
                    }
                }
            }
            
            // Parse body
            if let Some(body) = map.get("body") {
                opts.body = Some(body.clone());
            }
            
            // Parse timeout
            if let Some(Value::Integer(timeout)) = map.get("timeout") {
                opts.timeout = Some(Duration::from_millis(*timeout as u64));
            }
            
            // Parse follow_redirects
            if let Some(Value::Boolean(follow)) = map.get("follow_redirects") {
                opts.follow_redirects = *follow;
            }
            
            // Parse stream option
            if let Some(Value::Boolean(stream)) = map.get("stream") {
                opts.stream = *stream;
            }
            
            // Parse auth
            if let Some(Value::Map(auth)) = map.get("auth") {
                if let (Some(Value::String(auth_type)), Some(token)) = 
                    (auth.get("type"), auth.get("token")) {
                    match auth_type.as_str() {
                        "bearer" => {
                            if let Value::String(t) = token {
                                opts.auth = Some(Auth::Bearer(t.clone()));
                            }
                        }
                        "basic" => {
                            if let Value::Map(creds) = token {
                                if let (Some(Value::String(user)), Some(Value::String(pass))) = 
                                    (creds.get("username"), creds.get("password")) {
                                    opts.auth = Some(Auth::Basic(user.clone(), pass.clone()));
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        
        Ok(opts)
    }
    
    /// Build a request with options
    async fn execute_request(
        &self,
        method: Method,
        url: &str,
        options: RequestOptions,
    ) -> Result<Value, Error> {
        let mut request = self.client.request(method, url);
        
        // Add headers
        for (key, value) in options.headers {
            request = request.header(&key, &value);
        }
        
        // Add body
        if let Some(body) = options.body {
            match body {
                Value::String(s) => request = request.body(s),
                Value::Map(_) => {
                    // Serialize as JSON
                    let json_str = self.value_to_json(&body)?;
                    request = request
                        .header("Content-Type", "application/json")
                        .body(json_str);
                }
                _ => return Err(Error::Runtime("Invalid body type".to_string())),
            }
        }
        
        // Set timeout
        if let Some(timeout) = options.timeout {
            request = request.timeout(timeout);
        }
        
        // Set auth
        match options.auth {
            Some(Auth::Bearer(token)) => {
                request = request.bearer_auth(token);
            }
            Some(Auth::Basic(user, pass)) => {
                request = request.basic_auth(user, Some(pass));
            }
            None => {}
        }
        
        // Execute request
        let response = request
            .send()
            .await
            .map_err(|e| Error::Runtime(format!("Network error: {}", e)))?;
        
        // Build response map
        let status = response.status().as_u16() as i64;
        let status_text = response.status().canonical_reason()
            .unwrap_or("Unknown")
            .to_string();
        
        // Convert headers to a more structured format
        let mut headers_map = FxHashMap::default();
        for (key, value) in response.headers() {
            let key_str = key.as_str();
            let val_str = value.to_str().unwrap_or("").to_string();
            
            // Group multiple header values
            headers_map.entry(key_str.to_string())
                .or_insert_with(Vec::new)
                .push(val_str);
        }
        
        // Convert header values: single value as String, multiple as List
        let headers = headers_map.into_iter()
            .map(|(k, values)| {
                let v = if values.len() == 1 {
                    Value::String(values.into_iter().next().unwrap())
                } else {
                    Value::List(values.into_iter().map(Value::String).collect())
                };
                (k, v)
            })
            .collect::<FxHashMap<_, _>>();
        
        // Handle response body based on content type
        let content_type = response.headers()
            .get("content-type")
            .and_then(|v| v.to_str().ok())
            .unwrap_or("")
            .to_string();
        
        let body = if options.stream {
            // Stream response - return a channel ID that can be used to read chunks
            Value::String("[streaming not yet implemented]".to_string())
        } else {
            // Read full body
            let text = response
                .text()
                .await
                .map_err(|e| Error::Runtime(format!("Failed to read response: {}", e)))?;
            
            // Try to parse JSON if content type indicates it
            if content_type.contains("application/json") {
                match self.json_to_value(&text) {
                    Ok(json_value) => json_value,
                    Err(_) => Value::String(text), // Fall back to string if parsing fails
                }
            } else {
                Value::String(text)
            }
        };
        
        let mut result = FxHashMap::default();
        result.insert("status".to_string(), Value::Integer(status));
        result.insert("status_text".to_string(), Value::String(status_text));
        result.insert("body".to_string(), body);
        result.insert("headers".to_string(), Value::Map(headers));
        result.insert("ok".to_string(), Value::Boolean(status >= 200 && status < 300));
        result.insert("url".to_string(), Value::String(url.to_string()));
        
        Ok(Value::Map(result))
    }
    
    /// Convert a Value to JSON string
    fn value_to_json(&self, value: &Value) -> Result<String, Error> {
        match value {
            Value::Map(map) => {
                let mut json_map = serde_json::Map::new();
                for (k, v) in map {
                    json_map.insert(k.clone(), self.value_to_serde_json(v)?);
                }
                serde_json::to_string(&json_map)
                    .map_err(|e| Error::Runtime(format!("JSON serialization error: {}", e)))
            }
            _ => Err(Error::Runtime("Can only serialize maps to JSON".to_string()))
        }
    }
    
    /// Convert Value to serde_json::Value
    fn value_to_serde_json(&self, value: &Value) -> Result<serde_json::Value, Error> {
        Ok(match value {
            Value::String(s) => serde_json::Value::String(s.clone()),
            Value::Integer(i) => serde_json::Value::Number((*i).into()),
            Value::Float(f) => serde_json::Number::from_f64(*f)
                .map(serde_json::Value::Number)
                .unwrap_or(serde_json::Value::Null),
            Value::Boolean(b) => serde_json::Value::Bool(*b),
            Value::Nil => serde_json::Value::Null,
            Value::List(items) => {
                let json_items: Result<Vec<_>, _> = items
                    .iter()
                    .map(|v| self.value_to_serde_json(v))
                    .collect();
                serde_json::Value::Array(json_items?)
            }
            Value::Map(map) => {
                let mut json_map = serde_json::Map::new();
                for (k, v) in map {
                    json_map.insert(k.clone(), self.value_to_serde_json(v)?);
                }
                serde_json::Value::Object(json_map)
            }
            _ => serde_json::Value::Null,
        })
    }
    
    /// Convert JSON string to Value
    fn json_to_value(&self, json_str: &str) -> Result<Value, Error> {
        let json: serde_json::Value = serde_json::from_str(json_str)
            .map_err(|e| Error::Runtime(format!("JSON parse error: {}", e)))?;
        self.serde_json_to_value(&json)
    }
    
    /// Convert serde_json::Value to Value
    fn serde_json_to_value(&self, json: &serde_json::Value) -> Result<Value, Error> {
        Ok(match json {
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
                let values: Result<Vec<_>, _> = arr
                    .iter()
                    .map(|v| self.serde_json_to_value(v))
                    .collect();
                Value::List(values?)
            }
            serde_json::Value::Object(obj) => {
                let mut map = FxHashMap::default();
                for (k, v) in obj {
                    map.insert(k.clone(), self.serde_json_to_value(v)?);
                }
                Value::Map(map)
            }
        })
    }
    
    /// Convert HttpRequest to Value
    fn http_request_to_value(request: &HttpRequest) -> Value {
        let mut map = FxHashMap::default();
        
        map.insert("method".to_string(), Value::String(request.method.clone()));
        map.insert("path".to_string(), Value::String(request.path.clone()));
        
        // Convert headers
        let headers: FxHashMap<String, Value> = request.headers
            .iter()
            .map(|(k, v)| (k.clone(), Value::String(v.clone())))
            .collect();
        map.insert("headers".to_string(), Value::Map(headers));
        
        // Convert query parameters
        let query: FxHashMap<String, Value> = request.query
            .iter()
            .map(|(k, v)| (k.clone(), Value::String(v.clone())))
            .collect();
        map.insert("query".to_string(), Value::Map(query));
        
        // Convert path parameters
        let params: FxHashMap<String, Value> = request.params
            .iter()
            .map(|(k, v)| (k.clone(), Value::String(v.clone())))
            .collect();
        map.insert("params".to_string(), Value::Map(params));
        
        // Add body if present
        if let Some(body) = &request.body {
            map.insert("body".to_string(), body.clone());
        } else {
            map.insert("body".to_string(), Value::Nil);
        }
        
        Value::Map(map)
    }
    
    /// Convert Value to HttpResponse
    fn value_to_http_response(value: Value) -> HttpResponse {
        match value {
            Value::Map(mut map) => {
                let status = map.get("status")
                    .and_then(|v| if let Value::Integer(i) = v { Some(*i as u16) } else { None })
                    .unwrap_or(200);
                
                let headers = map.get("headers")
                    .and_then(|v| if let Value::Map(h) = v {
                        Some(h.iter()
                            .filter_map(|(k, v)| {
                                if let Value::String(s) = v {
                                    Some((k.clone(), s.clone()))
                                } else {
                                    None
                                }
                            })
                            .collect())
                    } else { None })
                    .unwrap_or_default();
                
                let body = map.remove("body").unwrap_or(Value::Nil);
                
                HttpResponse {
                    status,
                    headers,
                    body,
                }
            }
            // If response is just a string, treat it as the body
            Value::String(s) => HttpResponse {
                status: 200,
                headers: FxHashMap::default(),
                body: Value::String(s),
            },
            // For other types, convert to string representation
            other => HttpResponse {
                status: 200,
                headers: FxHashMap::default(),
                body: other,
            },
        }
    }
    
    /// Create an error response value
    fn error_response_value(status: u16, message: &str) -> Value {
        let mut map = FxHashMap::default();
        map.insert("status".to_string(), Value::Integer(status as i64));
        map.insert("body".to_string(), Value::Map({
            let mut error_map = FxHashMap::default();
            error_map.insert("error".to_string(), Value::String(message.to_string()));
            error_map
        }));
        Value::Map(map)
    }
    
    /// Start an HTTP server
    async fn start_server(&self, server_id: String, config: ServerConfig, handler: Value) -> Result<(), Error> {
        let (tx, mut rx) = mpsc::channel::<(HttpRequest, oneshot::Sender<HttpResponse>)>(100);
        
        // Clone for the spawned task
        let config_clone = config.clone();
        let tx_clone = tx.clone();
        
        // Spawn the server task
        let server_handle = tokio::spawn(async move {
            if let Err(e) = server_simple::start_server(config_clone, tx_clone).await {
                eprintln!("Server error: {}", e);
            }
        });
        
        // Spawn the request handler task
        let handler_clone = handler.clone();
        let request_handle = tokio::spawn(async move {
            while let Some((request, response_tx)) = rx.recv().await {
                // Convert request to FluentAI Value
                let request_value = Self::http_request_to_value(&request);
                
                // Call the handler function
                let response_value = match &handler_clone {
                    Value::NativeFunction { function, .. } => match function(&[request_value]) {
                        Ok(val) => val,
                        Err(e) => {
                            eprintln!("Handler error: {}", e);
                            Self::error_response_value(500, "Internal server error")
                        }
                    },
                    Value::Function { .. } => {
                        // VM functions need to be executed through the VM
                        eprintln!("VM function handlers not yet supported");
                        Self::error_response_value(500, "VM function handlers not yet supported")
                    },
                    _ => Self::error_response_value(500, "Handler is not a function"),
                };
                
                // Convert response value to HttpResponse
                let response = Self::value_to_http_response(response_value);
                
                // Send response back
                let _ = response_tx.send(response);
            }
        });
        
        // Store server info
        let mut servers = self.servers.lock().await;
        servers.insert(server_id, ServerInfo {
            config,
            sender: tx,
            handle: Some(server_handle),
            request_handle: Some(request_handle),
        });
        
        Ok(())
    }
    
    /// Add a route to a server
    async fn add_route(&self, server_id: &str, route: RouteDefinition, handler: Value) -> Result<(), Error> {
        let mut servers = self.servers.lock().await;
        
        if let Some(server_info) = servers.get_mut(server_id) {
            let mut routes = server_info.config.routes.lock().await;
            routes.push(route.clone());
            
            // Store the handler for this route
            // In a real implementation, we'd store handlers by route ID
            // For now, we'll use the main handler
            Ok(())
        } else {
            Err(Error::Runtime(format!("Server '{}' not found", server_id)))
        }
    }
    
    /// Stop a server
    async fn stop_server(&self, server_id: &str) -> Result<(), Error> {
        let mut servers = self.servers.lock().await;
        
        if let Some(mut server_info) = servers.remove(server_id) {
            // Cancel the server task
            if let Some(handle) = server_info.handle.take() {
                handle.abort();
            }
            // Cancel the request handler task
            if let Some(handle) = server_info.request_handle.take() {
                handle.abort();
            }
            Ok(())
        } else {
            Err(Error::Runtime(format!("Server '{}' not found", server_id)))
        }
    }
}

#[derive(Default)]
struct RequestOptions {
    headers: FxHashMap<String, String>,
    body: Option<Value>,
    timeout: Option<Duration>,
    follow_redirects: bool,
    auth: Option<Auth>,
    stream: bool,
}

enum Auth {
    Bearer(String),
    Basic(String, String),
}

#[async_trait]
impl EffectHandler for NetworkHandler {
    fn effect_type(&self) -> EffectType {
        EffectType::Network
    }

    fn handle_sync(&self, operation: &str, _args: &[Value]) -> EffectResult {
        Err(Error::Runtime(format!(
            "Network operation '{}' must be called asynchronously",
            operation
        )))
    }

    async fn handle_async(&self, operation: &str, args: &[Value]) -> EffectResult {
        match operation {
            // Simple GET request
            "fetch" | "get" => {
                if let Some(Value::String(url)) = args.first() {
                    // Check if there are options
                    let options = if let Some(opts) = args.get(1) {
                        Self::parse_request_options(opts)?
                    } else {
                        RequestOptions::default()
                    };
                    
                    self.execute_request(Method::GET, url, options).await
                } else {
                    Err(Error::Runtime("network:fetch requires a URL".to_string()))
                }
            }
            
            // POST request
            "post" => {
                match args {
                    [Value::String(url), body] => {
                        let mut options = RequestOptions::default();
                        options.body = Some(body.clone());
                        self.execute_request(Method::POST, url, options).await
                    }
                    [Value::String(url), body, opts] => {
                        let mut options = Self::parse_request_options(opts)?;
                        options.body = Some(body.clone());
                        self.execute_request(Method::POST, url, options).await
                    }
                    _ => Err(Error::Runtime("network:post requires URL and body".to_string()))
                }
            }
            
            // PUT request
            "put" => {
                match args {
                    [Value::String(url), body] => {
                        let mut options = RequestOptions::default();
                        options.body = Some(body.clone());
                        self.execute_request(Method::PUT, url, options).await
                    }
                    [Value::String(url), body, opts] => {
                        let mut options = Self::parse_request_options(opts)?;
                        options.body = Some(body.clone());
                        self.execute_request(Method::PUT, url, options).await
                    }
                    _ => Err(Error::Runtime("network:put requires URL and body".to_string()))
                }
            }
            
            // DELETE request
            "delete" => {
                if let Some(Value::String(url)) = args.first() {
                    let options = if let Some(opts) = args.get(1) {
                        Self::parse_request_options(opts)?
                    } else {
                        RequestOptions::default()
                    };
                    
                    self.execute_request(Method::DELETE, url, options).await
                } else {
                    Err(Error::Runtime("network:delete requires a URL".to_string()))
                }
            }
            
            // PATCH request
            "patch" => {
                match args {
                    [Value::String(url), body] => {
                        let mut options = RequestOptions::default();
                        options.body = Some(body.clone());
                        self.execute_request(Method::PATCH, url, options).await
                    }
                    [Value::String(url), body, opts] => {
                        let mut options = Self::parse_request_options(opts)?;
                        options.body = Some(body.clone());
                        self.execute_request(Method::PATCH, url, options).await
                    }
                    _ => Err(Error::Runtime("network:patch requires URL and body".to_string()))
                }
            }
            
            // HEAD request
            "head" => {
                if let Some(Value::String(url)) = args.first() {
                    let options = if let Some(opts) = args.get(1) {
                        Self::parse_request_options(opts)?
                    } else {
                        RequestOptions::default()
                    };
                    
                    self.execute_request(Method::HEAD, url, options).await
                } else {
                    Err(Error::Runtime("network:head requires a URL".to_string()))
                }
            }
            
            // OPTIONS request
            "options" => {
                if let Some(Value::String(url)) = args.first() {
                    let options = if let Some(opts) = args.get(1) {
                        Self::parse_request_options(opts)?
                    } else {
                        RequestOptions::default()
                    };
                    
                    self.execute_request(Method::OPTIONS, url, options).await
                } else {
                    Err(Error::Runtime("network:options requires a URL".to_string()))
                }
            }
            
            // Generic request method
            "request" => {
                match args {
                    [Value::String(method), Value::String(url)] => {
                        let method = Method::from_bytes(method.as_bytes())
                            .map_err(|_| Error::Runtime("Invalid HTTP method".to_string()))?;
                        self.execute_request(method, url, RequestOptions::default()).await
                    }
                    [Value::String(method), Value::String(url), opts] => {
                        let method = Method::from_bytes(method.as_bytes())
                            .map_err(|_| Error::Runtime("Invalid HTTP method".to_string()))?;
                        let options = Self::parse_request_options(opts)?;
                        self.execute_request(method, url, options).await
                    }
                    _ => Err(Error::Runtime("network:request requires method, URL, and optional options".to_string()))
                }
            }
            
            // Server operations
            "serve" => {
                // Start an HTTP server
                // Args: [port, handler_function] or [config_map, handler_function]
                match args {
                    [Value::Integer(port), handler] if matches!(handler, Value::NativeFunction { .. } | Value::Function { .. }) => {
                        // Simple server on port
                        let config = ServerConfig {
                            host: "0.0.0.0".to_string(),
                            port: *port as u16,
                            routes: Arc::new(Mutex::new(Vec::new())),
                            middleware: Vec::new(),
                        };
                        
                        let server_id = format!("server_{}", Uuid::new_v4());
                        self.start_server(server_id.clone(), config, handler.clone()).await?;
                        
                        Ok(Value::String(server_id))
                    }
                    [Value::Map(config_map), handler] if matches!(handler, Value::NativeFunction { .. } | Value::Function { .. }) => {
                        // Server with configuration
                        let host = config_map.get("host")
                            .and_then(|v| if let Value::String(s) = v { Some(s.clone()) } else { None })
                            .unwrap_or_else(|| "0.0.0.0".to_string());
                        
                        let port = config_map.get("port")
                            .and_then(|v| if let Value::Integer(p) = v { Some(*p as u16) } else { None })
                            .ok_or_else(|| Error::Runtime("Server config requires 'port'".to_string()))?;
                        
                        let config = ServerConfig {
                            host,
                            port,
                            routes: Arc::new(Mutex::new(Vec::new())),
                            middleware: Vec::new(),
                        };
                        
                        let server_id = format!("server_{}", Uuid::new_v4());
                        self.start_server(server_id.clone(), config, handler.clone()).await?;
                        
                        Ok(Value::String(server_id))
                    }
                    _ => Err(Error::Runtime("network:serve requires port/config and handler function".to_string()))
                }
            }
            
            "route" => {
                // Add a route to a server
                // Args: [server_id, method, path, handler]
                match args {
                    [Value::String(server_id), Value::String(method), Value::String(path), handler] => {
                        let method = AxumMethod::from_bytes(method.as_bytes())
                            .map_err(|_| Error::Runtime("Invalid HTTP method".to_string()))?;
                        
                        let route = RouteDefinition {
                            method,
                            path: path.clone(),
                            handler_id: format!("handler_{}", Uuid::new_v4()),
                        };
                        
                        self.add_route(server_id, route, handler.clone()).await?;
                        Ok(Value::Nil)
                    }
                    _ => Err(Error::Runtime("network:route requires server_id, method, path, and handler".to_string()))
                }
            }
            
            "stop" => {
                // Stop a server
                match args {
                    [Value::String(server_id)] => {
                        self.stop_server(server_id).await?;
                        Ok(Value::Nil)
                    }
                    _ => Err(Error::Runtime("network:stop requires server_id".to_string()))
                }
            }
            
            _ => Err(Error::Runtime(format!(
                "Unknown Network operation: {}",
                operation
            ))),
        }
    }

    fn is_async_operation(&self, _operation: &str) -> bool {
        true // All network operations are async
    }
}
