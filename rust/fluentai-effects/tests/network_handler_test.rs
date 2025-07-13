//! Tests for the enhanced NetworkHandler

use fluentai_core::{error::Error, value::Value};
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use wiremock::{MockServer, Mock, ResponseTemplate};
use wiremock::matchers::{method, path, header, body_json};
use rustc_hash::FxHashMap;
use serde_json::json;

#[tokio::test]
async fn test_simple_get_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/users/1"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(json!({"id": 1, "name": "Alice"}))
                .insert_header("content-type", "application/json")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    // Check response structure
    match response {
        Value::Map(map) => {
            assert_eq!(map.get("status"), Some(&Value::Integer(200)));
            // Response body should be parsed JSON
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_get_with_headers() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/data"))
        .and(header("Authorization", "Bearer token123"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(json!({"data": "secret"}))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/data", mock_server.uri());
    
    let mut headers = FxHashMap::default();
    headers.insert("Authorization".to_string(), Value::String("Bearer token123".to_string()));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(headers),
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_post_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("POST"))
        .and(path("/api/users"))
        .and(body_json(json!({"name": "Bob", "age": 30})))
        .respond_with(
            ResponseTemplate::new(201)
                .set_body_json(json!({"id": 2, "name": "Bob", "age": 30}))
                .insert_header("Location", "/api/users/2")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users", mock_server.uri());
    
    let mut body = FxHashMap::default();
    body.insert("name".to_string(), Value::String("Bob".to_string()));
    body.insert("age".to_string(), Value::Integer(30));
    
    let result = handler.handle_async("post", &[
        Value::String(url),
        Value::Map(body),
    ]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    match response {
        Value::Map(map) => {
            assert_eq!(map.get("status"), Some(&Value::Integer(201)));
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_put_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("PUT"))
        .and(path("/api/users/1"))
        .respond_with(ResponseTemplate::new(200))
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", mock_server.uri());
    
    let mut body = FxHashMap::default();
    body.insert("name".to_string(), Value::String("Alice Updated".to_string()));
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("PUT".to_string()));
    request.insert("url".to_string(), Value::String(url));
    request.insert("body".to_string(), Value::Map(body));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_delete_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("DELETE"))
        .and(path("/api/users/1"))
        .respond_with(ResponseTemplate::new(204))
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", mock_server.uri());
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("DELETE".to_string()));
    request.insert("url".to_string(), Value::String(url));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    match response {
        Value::Map(map) => {
            assert_eq!(map.get("status"), Some(&Value::Integer(204)));
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_request_with_query_params() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/protected"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(json!({"protected": true}))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    // Query params can be part of the URL
    let url = format!("{}/api/protected?key=value&foo=bar", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_custom_headers() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/secure"))
        .and(header("X-API-Key", "secret123"))
        .and(header("X-Request-ID", "req-456"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(json!({"secure": true}))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/secure", mock_server.uri());
    
    let mut headers = FxHashMap::default();
    headers.insert("X-API-Key".to_string(), Value::String("secret123".to_string()));
    headers.insert("X-Request-ID".to_string(), Value::String("req-456".to_string()));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(headers),
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_error_response() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/error"))
        .respond_with(
            ResponseTemplate::new(500)
                .set_body_json(json!({"error": "Internal Server Error"}))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/error", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await;
    
    assert!(result.is_ok()); // Should not error, but return error status
    let response = result.unwrap();
    
    match response {
        Value::Map(map) => {
            assert_eq!(map.get("status"), Some(&Value::Integer(500)));
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_patch_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("PATCH"))
        .and(path("/api/users/1"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_json(json!({"id": 1, "name": "Patched"}))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", mock_server.uri());
    
    let mut body = FxHashMap::default();
    body.insert("name".to_string(), Value::String("Patched".to_string()));
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("PATCH".to_string()));
    request.insert("url".to_string(), Value::String(url));
    request.insert("body".to_string(), Value::Map(body));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_head_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("HEAD"))
        .and(path("/api/check"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("X-Resource-Exists", "true")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/check", mock_server.uri());
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("HEAD".to_string()));
    request.insert("url".to_string(), Value::String(url));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    match response {
        Value::Map(map) => {
            assert_eq!(map.get("status"), Some(&Value::Integer(200)));
            // HEAD requests don't have body
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_options_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("OPTIONS"))
        .and(path("/api/endpoint"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("Allow", "GET, POST, PUT, DELETE")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/endpoint", mock_server.uri());
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("OPTIONS".to_string()));
    request.insert("url".to_string(), Value::String(url));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_trace_request() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("TRACE"))
        .and(path("/api/trace"))
        .respond_with(ResponseTemplate::new(200))
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/trace", mock_server.uri());
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("TRACE".to_string()));
    request.insert("url".to_string(), Value::String(url));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_invalid_url() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("get", &[Value::String("not-a-url".to_string())]).await;
    
    assert!(result.is_err());
}

#[tokio::test]
async fn test_missing_arguments() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("get", &[]).await;
    
    assert!(result.is_err());
}

#[tokio::test]
async fn test_invalid_method() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("invalid", &[Value::String("http://example.com".to_string())]).await;
    
    assert!(result.is_err());
}