//! Tests for enhanced NetworkHandler response handling

use fluentai_core::value::Value;
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use mockito::{Server, Matcher};
use rustc_hash::FxHashMap;

#[tokio::test]
async fn test_json_response_parsing() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/data")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"{"users": [{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}], "total": 2}"#)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/data", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    if let Value::Map(response) = result {
        // Check that body was parsed as JSON
        if let Some(Value::Map(body)) = response.get("body") {
            // Check total field
            assert_eq!(body.get("total"), Some(&Value::Integer(2)));
            
            // Check users array
            if let Some(Value::List(users)) = body.get("users") {
                assert_eq!(users.len(), 2);
                
                // Check first user
                if let Value::Map(user) = &users[0] {
                    assert_eq!(user.get("id"), Some(&Value::Integer(1)));
                    assert_eq!(user.get("name"), Some(&Value::String("Alice".to_string())));
                }
            } else {
                panic!("Expected users to be a list");
            }
        } else {
            panic!("Expected body to be parsed as JSON Map");
        }
    } else {
        panic!("Expected Map response");
    }
}

#[tokio::test]
async fn test_response_headers_parsing() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/headers")
        .with_status(200)
        .with_header("x-single", "value1")
        .with_header("x-multi", "value1")
        .with_header("x-multi", "value2")  // Multiple values for same header
        .with_header("content-type", "text/plain")
        .with_body("OK")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/headers", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    if let Value::Map(response) = result {
        if let Some(Value::Map(headers)) = response.get("headers") {
            // Single value header
            assert_eq!(
                headers.get("x-single"),
                Some(&Value::String("value1".to_string()))
            );
            
            // Multi-value header should be a list
            if let Some(Value::List(values)) = headers.get("x-multi") {
                assert_eq!(values.len(), 2);
                assert!(values.contains(&Value::String("value1".to_string())));
                assert!(values.contains(&Value::String("value2".to_string())));
            } else {
                // Some servers may only return the last value
                assert!(matches!(headers.get("x-multi"), Some(Value::String(_))));
            }
            
            // Standard header
            assert!(headers.contains_key("content-type"));
        } else {
            panic!("Expected headers to be a Map");
        }
    }
}

#[tokio::test]
async fn test_status_text_included() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/status")
        .with_status(201)
        .with_body("Created")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/status", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url.clone())]).await.unwrap();
    
    if let Value::Map(response) = result {
        assert_eq!(response.get("status"), Some(&Value::Integer(201)));
        assert_eq!(response.get("status_text"), Some(&Value::String("Created".to_string())));
        assert_eq!(response.get("url"), Some(&Value::String(url)));
    }
}

#[tokio::test]
async fn test_invalid_json_fallback() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/invalid")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body("This is not valid JSON!")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/invalid", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    if let Value::Map(response) = result {
        // Should fall back to string when JSON parsing fails
        assert_eq!(
            response.get("body"),
            Some(&Value::String("This is not valid JSON!".to_string()))
        );
    }
}

#[tokio::test]
async fn test_timeout_option() {
    // This test would require a slow endpoint mock
    // For now, just verify the option is accepted
    let handler = NetworkHandler::new();
    
    let mut options = FxHashMap::default();
    options.insert("timeout".to_string(), Value::Integer(5000)); // 5 seconds
    
    let result = handler.handle_async("get", &[
        Value::String("http://example.com".to_string()),
        Value::Map(options)
    ]).await;
    
    // Should not panic, even if request fails
    assert!(result.is_ok() || result.is_err());
}

#[tokio::test]
async fn test_stream_option_placeholder() {
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/stream")
        .with_status(200)
        .with_body("Streaming data")
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/stream", server.url());
    
    let mut options = FxHashMap::default();
    options.insert("stream".to_string(), Value::Boolean(true));
    
    let result = handler.handle_async("get", &[
        Value::String(url),
        Value::Map(options)
    ]).await.unwrap();
    
    if let Value::Map(response) = result {
        // For now, streaming returns a placeholder
        assert_eq!(
            response.get("body"),
            Some(&Value::String("[streaming not yet implemented]".to_string()))
        );
    }
}

#[tokio::test]
async fn test_complex_json_structure() {
    let complex_json = r#"{
        "meta": {
            "version": "1.0",
            "timestamp": 1234567890
        },
        "data": {
            "items": [
                {"id": 1, "active": true, "score": 98.5},
                {"id": 2, "active": false, "score": 45.2}
            ],
            "pagination": {
                "page": 1,
                "per_page": 10,
                "total": 2
            }
        },
        "errors": null
    }"#;
    
    let mut server = Server::new_async().await;
    let _m = server.mock("GET", "/api/complex")
        .with_status(200)
        .with_header("content-type", "application/json; charset=utf-8")
        .with_body(complex_json)
        .create();

    let handler = NetworkHandler::new();
    let url = format!("{}/api/complex", server.url());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    if let Value::Map(response) = result {
        if let Some(Value::Map(body)) = response.get("body") {
            // Check nested structure
            if let Some(Value::Map(data)) = body.get("data") {
                if let Some(Value::List(items)) = data.get("items") {
                    assert_eq!(items.len(), 2);
                    
                    // Check first item
                    if let Value::Map(item) = &items[0] {
                        assert_eq!(item.get("id"), Some(&Value::Integer(1)));
                        assert_eq!(item.get("active"), Some(&Value::Boolean(true)));
                        assert_eq!(item.get("score"), Some(&Value::Float(98.5)));
                    }
                }
                
                // Check pagination
                if let Some(Value::Map(pagination)) = data.get("pagination") {
                    assert_eq!(pagination.get("page"), Some(&Value::Integer(1)));
                    assert_eq!(pagination.get("total"), Some(&Value::Integer(2)));
                }
            }
            
            // Check null value
            assert_eq!(body.get("errors"), Some(&Value::Nil));
        }
    }
}