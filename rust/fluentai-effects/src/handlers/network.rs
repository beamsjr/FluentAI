//! Network effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use reqwest;
use rustc_hash::FxHashMap;
use serde_json;

/// Helper function to convert Value to serde_json::Value
fn value_to_json(value: &Value) -> serde_json::Value {
    match value {
        Value::Nil => serde_json::Value::Null,
        Value::Boolean(b) => serde_json::Value::Bool(*b),
        Value::Integer(i) => serde_json::Value::Number(serde_json::Number::from(*i)),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(serde_json::Value::Number)
            .unwrap_or(serde_json::Value::Null),
        Value::String(s) => serde_json::Value::String(s.clone()),
        Value::Symbol(s) => serde_json::Value::String(s.clone()),
        Value::List(list) => {
            let arr: Vec<serde_json::Value> = list.iter().map(|v| value_to_json(v)).collect();
            serde_json::Value::Array(arr)
        },
        Value::Map(map) => {
            let mut obj = serde_json::Map::new();
            for (k, v) in map {
                obj.insert(k.clone(), value_to_json(v));
            }
            serde_json::Value::Object(obj)
        },
        _ => serde_json::Value::String(value.to_string()),
    }
}

pub struct NetworkHandler {
    client: reqwest::Client,
}

impl NetworkHandler {
    pub fn new() -> Self {
        Self {
            client: reqwest::Client::new(),
        }
    }
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
            "request" => {
                // Handle full HTTP request with method, url, headers, body
                if let Some(Value::Map(request)) = args.first() {
                    let method = request.get("method")
                        .and_then(|v| if let Value::String(s) = v { Some(s.as_str()) } else { None })
                        .unwrap_or("GET");
                    
                    let url = request.get("url")
                        .and_then(|v| if let Value::String(s) = v { Some(s.as_str()) } else { None })
                        .ok_or_else(|| Error::Runtime("Request requires a 'url' field".to_string()))?;
                    
                    let mut req_builder = match method.to_uppercase().as_str() {
                        "GET" => self.client.get(url),
                        "POST" => self.client.post(url),
                        "PUT" => self.client.put(url),
                        "DELETE" => self.client.delete(url),
                        "PATCH" => self.client.patch(url),
                        "HEAD" => self.client.head(url),
                        "OPTIONS" => self.client.request(reqwest::Method::OPTIONS, url),
                        "TRACE" => self.client.request(reqwest::Method::TRACE, url),
                        _ => return Err(Error::Runtime(format!("Unsupported HTTP method: {}", method))),
                    };
                    
                    // Add headers if provided
                    if let Some(Value::Map(headers)) = request.get("headers") {
                        for (key, value) in headers {
                            if let Value::String(val) = value {
                                req_builder = req_builder.header(key, val);
                            }
                        }
                    }
                    
                    // Add body if provided
                    if let Some(body_value) = request.get("body") {
                        let (body_string, is_json) = match body_value {
                            Value::String(s) => (s.clone(), false),
                            Value::Map(map) => {
                                // Convert Map to JSON
                                let json_obj = serde_json::json!({});
                                let mut json_map = json_obj.as_object().unwrap().clone();
                                for (k, v) in map {
                                    json_map.insert(k.clone(), value_to_json(v));
                                }
                                (serde_json::Value::Object(json_map).to_string(), true)
                            },
                            Value::List(list) => {
                                // Convert List to JSON array
                                let json_array: Vec<serde_json::Value> = list.iter()
                                    .map(|v| value_to_json(v))
                                    .collect();
                                (serde_json::Value::Array(json_array).to_string(), true)
                            },
                            _ => (body_value.to_string(), false),
                        };
                        req_builder = req_builder.body(body_string);
                        
                        // Set content-type header if not already set and body is JSON
                        if is_json {
                            req_builder = req_builder.header("content-type", "application/json");
                        }
                    }
                    
                    let response = req_builder
                        .send()
                        .await
                        .map_err(|e| Error::Runtime(format!("Network error: {}", e)))?;

                    let status = response.status().as_u16() as i64;
                    let headers = response
                        .headers()
                        .iter()
                        .map(|(k, v)| {
                            (
                                k.as_str().to_string(),
                                Value::String(v.to_str().unwrap_or("").to_string()),
                            )
                        })
                        .collect::<FxHashMap<_, _>>();

                    let body = response
                        .text()
                        .await
                        .map_err(|e| Error::Runtime(format!("Failed to read response: {}", e)))?;

                    let mut result = FxHashMap::default();
                    result.insert("status".to_string(), Value::Integer(status));
                    result.insert("body".to_string(), Value::String(body));
                    result.insert("headers".to_string(), Value::Map(headers));

                    Ok(Value::Map(result))
                } else {
                    Err(Error::Runtime("network:request requires a request map".to_string()))
                }
            }
            "fetch" | "get" => {
                if let Some(Value::String(url)) = args.first() {
                    let response = self
                        .client
                        .get(url)
                        .send()
                        .await
                        .map_err(|e| Error::Runtime(format!("Network error: {}", e)))?;

                    let status = response.status().as_u16() as i64;
                    let headers = response
                        .headers()
                        .iter()
                        .map(|(k, v)| {
                            (
                                k.as_str().to_string(),
                                Value::String(v.to_str().unwrap_or("").to_string()),
                            )
                        })
                        .collect::<FxHashMap<_, _>>();

                    let body = response
                        .text()
                        .await
                        .map_err(|e| Error::Runtime(format!("Failed to read response: {}", e)))?;

                    let mut result = FxHashMap::default();
                    result.insert("status".to_string(), Value::Integer(status));
                    result.insert("body".to_string(), Value::String(body));
                    result.insert("headers".to_string(), Value::Map(headers));

                    Ok(Value::Map(result))
                } else {
                    Err(Error::Runtime("network:fetch requires a URL".to_string()))
                }
            }
            "post" => {
                if args.len() >= 2 {
                    if let (Some(Value::String(url)), Some(body)) = (args.get(0), args.get(1)) {
                        let mut req = self.client.post(url);
                        
                        // Convert body to appropriate format
                        let body_string = match body {
                            Value::String(s) => s.clone(),
                            Value::Map(_) | Value::List(_) => {
                                // Set content-type for JSON
                                req = req.header("content-type", "application/json");
                                value_to_json(body).to_string()
                            },
                            _ => body.to_string(),
                        };
                        
                        let response = req
                            .body(body_string)
                            .send()
                            .await
                            .map_err(|e| Error::Runtime(format!("Network error: {}", e)))?;

                        let status = response.status().as_u16() as i64;
                        let body = response.text().await.map_err(|e| {
                            Error::Runtime(format!("Failed to read response: {}", e))
                        })?;

                        let mut result = FxHashMap::default();
                        result.insert("status".to_string(), Value::Integer(status));
                        result.insert("body".to_string(), Value::String(body));

                        Ok(Value::Map(result))
                    } else {
                        Err(Error::Runtime(
                            "network:post requires URL and body".to_string(),
                        ))
                    }
                } else {
                    Err(Error::Runtime(
                        "network:post requires 2 arguments".to_string(),
                    ))
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
