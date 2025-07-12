//! Tests for the enhanced NetworkHandler

use fluentai_core::{error::Error, value::Value};
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use mockito::{Server, ServerGuard, Matcher};
use rustc_hash::FxHashMap;
use serde_json::json;

#[tokio::test]
async fn test_simple_get_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/users/1")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{"id": 1, "name": "Alice"}"#)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    if let Value::Map(map) = response {
        assert_eq!(map.get("status"), Some(&Value::Integer(200)));
        assert_eq!(map.get("ok"), Some(&Value::Boolean(true)));
        assert!(map.contains_key("body"));
        assert!(map.contains_key("headers"));
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_get_with_headers() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/data")
        .match_header("authorization", "Bearer test-token")
        .match_header("x-custom", "custom-value")
        .with_status(200)
        .with_body("Success")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/data", server.url());
    
    let mut headers = FxHashMap::default();
    headers.insert("authorization".to_string(), Value::String("Bearer test-token".to_string()));
    headers.insert("x-custom".to_string(), Value::String("custom-value".to_string()));
    
    let mut options = FxHashMap::default();
    options.insert("headers".to_string(), Value::Map(headers));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(options)
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_post_with_json_body() {
    let mut server = Server::new_async().await;
    let _m = server.mock("POST", "/api/users")
        .match_header("content-type", "application/json")
        .match_body(Matcher::Json(json!({"name": "Bob", "age": 30})))
        .with_status(201)
        .with_body(r#"{"id": 2, "name": "Bob", "age": 30}"#)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users", server.url());
    
    let mut body = FxHashMap::default();
    body.insert("name".to_string(), Value::String("Bob".to_string()));
    body.insert("age".to_string(), Value::Integer(30));
    
    let result = handler.handle_async("post", &[
        Value::String(url),
        Value::Map(body)
    ]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    if let Value::Map(map) = response {
        assert_eq!(map.get("status"), Some(&Value::Integer(201)));
        assert_eq!(map.get("ok"), Some(&Value::Boolean(true)));
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_put_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("PUT", "/api/users/1")
        .match_body("Updated data")
        .with_status(200)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", server.url());
    
    let result = handler.handle_async("put", &[
        Value::String(url),
        Value::String("Updated data".to_string())
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_delete_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("DELETE", "/api/users/1")
        .with_status(204)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", server.url());
    
    let result = handler.handle_async("delete", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    if let Value::Map(map) = response {
        assert_eq!(map.get("status"), Some(&Value::Integer(204)));
        assert_eq!(map.get("ok"), Some(&Value::Boolean(true)));
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_bearer_auth() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/protected")
        .match_header("authorization", "Bearer secret-token")
        .with_status(200)
        .with_body("Protected data")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/protected", server.url());
    
    let mut auth = FxHashMap::default();
    auth.insert("type".to_string(), Value::String("bearer".to_string()));
    auth.insert("token".to_string(), Value::String("secret-token".to_string()));
    
    let mut options = FxHashMap::default();
    options.insert("auth".to_string(), Value::Map(auth));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(options)
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_basic_auth() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/secure")
        .match_header("authorization", Matcher::Regex(r"Basic \w+".to_string()))
        .with_status(200)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/secure", server.url());
    
    let mut creds = FxHashMap::default();
    creds.insert("username".to_string(), Value::String("user".to_string()));
    creds.insert("password".to_string(), Value::String("pass".to_string()));
    
    let mut auth = FxHashMap::default();
    auth.insert("type".to_string(), Value::String("basic".to_string()));
    auth.insert("token".to_string(), Value::Map(creds));
    
    let mut options = FxHashMap::default();
    options.insert("auth".to_string(), Value::Map(auth));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(options)
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_error_response() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/error")
        .with_status(404)
        .with_body("Not found")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/error", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    if let Value::Map(map) = response {
        assert_eq!(map.get("status"), Some(&Value::Integer(404)));
        assert_eq!(map.get("ok"), Some(&Value::Boolean(false)));
        assert_eq!(map.get("body"), Some(&Value::String("Not found".to_string())));
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_patch_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("PATCH", "/api/users/1")
        .match_body(r#"{"name":"Updated"}"#)
        .match_header("content-type", "application/json")
        .with_status(200)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/users/1", server.url());
    
    let mut body = FxHashMap::default();
    body.insert("name".to_string(), Value::String("Updated".to_string()));
    
    let result = handler.handle_async("patch", &[
        Value::String(url),
        Value::Map(body)
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_head_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("HEAD", "/api/check")
        .with_status(200)
        .with_header("x-custom", "value")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/check", server.url());
    
    let result = handler.handle_async("head", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
    let response = result.unwrap();
    
    if let Value::Map(map) = response {
        assert_eq!(map.get("status"), Some(&Value::Integer(200)));
        // HEAD requests don't have body
        assert_eq!(map.get("body"), Some(&Value::String("".to_string())));
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_options_request() {
    let mut server = Server::new_async().await;
    let _m = server.mock("OPTIONS", "/api/endpoint")
        .with_status(200)
        .with_header("allow", "GET, POST, PUT, DELETE")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/endpoint", server.url());
    
    let result = handler.handle_async("options", &[Value::String(url)]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_generic_request_method() {
    let mut server = Server::new_async().await;
    let _m = server.mock("TRACE", "/api/trace")
        .with_status(200)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/trace", server.url());
    
    let result = handler.handle_async("request", &[
        Value::String("TRACE".to_string()),
        Value::String(url)
    ]).await;
    
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_invalid_url() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("get", &[Value::String("not-a-url".to_string())]).await;
    
    assert!(result.is_err());
}

#[tokio::test]
async fn test_missing_url() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_async("get", &[]).await;
    
    assert!(result.is_err());
    match result {
        Err(Error::Runtime(msg)) => assert!(msg.contains("requires a URL")),
        _ => panic!("Expected Runtime error"),
    }
}

#[tokio::test]
async fn test_sync_operation_error() {
    let handler = NetworkHandler::new();
    
    let result = handler.handle_sync("get", &[Value::String("http://example.com".to_string())]);
    
    assert!(result.is_err());
    match result {
        Err(Error::Runtime(msg)) => assert!(msg.contains("must be called asynchronously")),
        _ => panic!("Expected Runtime error"),
    }
}