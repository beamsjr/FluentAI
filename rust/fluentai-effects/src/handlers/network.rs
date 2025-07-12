//! Network effect handler

use crate::{EffectHandler, EffectResult};
use async_trait::async_trait;
use fluentai_core::{ast::EffectType, error::Error, value::Value};
use reqwest;
use rustc_hash::FxHashMap;

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
                        let body_string = match body_value {
                            Value::String(s) => s.clone(),
                            _ => body_value.to_string(),
                        };
                        req_builder = req_builder.body(body_string);
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
                        let response = self
                            .client
                            .post(url)
                            .body(body.to_string())
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
