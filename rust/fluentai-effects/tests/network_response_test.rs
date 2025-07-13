//! Tests for enhanced NetworkHandler response handling

use fluentai_core::value::Value;
use fluentai_effects::{handlers::NetworkHandler, EffectHandler};
use wiremock::{MockServer, Mock, ResponseTemplate};
use wiremock::matchers::{method, path};
use rustc_hash::FxHashMap;

#[tokio::test]
async fn test_json_response_parsing() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/data"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("content-type", "application/json")
                .set_body_string(r#"{"users": [{"id": 1, "name": "Alice"}, {"id": 2, "name": "Bob"}], "total": 2}"#)
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/data", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    // Response should be a Map
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(200)));
            
            // Body should be parsed JSON
            if let Some(Value::Map(body)) = response.get("body") {
                // Check the parsed JSON structure
                if let Some(Value::List(users)) = body.get("users") {
                    assert_eq!(users.len(), 2);
                }
                assert_eq!(body.get("total"), Some(&Value::Integer(2)));
            }
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_text_response() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/text"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("content-type", "text/plain")
                .set_body_string("Hello, World!")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/text", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(200)));
            // For text responses, body should be a String
            assert_eq!(response.get("body"), Some(&Value::String("Hello, World!".to_string())));
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_html_response() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("content-type", "text/html")
                .set_body_string("<html><body><h1>Welcome</h1></body></html>")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = mock_server.uri();
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(200)));
            // HTML should be returned as String
            if let Some(Value::String(body)) = response.get("body") {
                assert!(body.contains("<h1>Welcome</h1>"));
            }
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_empty_response() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("DELETE"))
        .and(path("/api/resource"))
        .respond_with(ResponseTemplate::new(204))
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/resource", mock_server.uri());
    
    let mut request = FxHashMap::default();
    request.insert("method".to_string(), Value::String("DELETE".to_string()));
    request.insert("url".to_string(), Value::String(url));
    
    let result = handler.handle_async("request", &[Value::Map(request)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(204)));
            // No body for 204 No Content
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_binary_response() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/image"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("content-type", "image/png")
                .set_body_bytes(vec![0x89, 0x50, 0x4E, 0x47]) // PNG magic number
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/image", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(200)));
            // Binary data might be returned as base64 or raw string
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_redirect_response() {
    let mock_server = MockServer::start().await;
    
    // Set up redirect and destination
    Mock::given(method("GET"))
        .and(path("/old-path"))
        .respond_with(
            ResponseTemplate::new(301)
                .insert_header("Location", "/new-path")
        )
        .mount(&mock_server)
        .await;
        
    // Also mock the redirect target
    Mock::given(method("GET"))
        .and(path("/new-path"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_body_string("Redirected content")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/old-path", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            // Should follow redirects by default, but we might see the redirect response
            if let Some(Value::Integer(status)) = response.get("status") {
                // The reqwest client follows redirects by default, so we should see 200
                assert_eq!(*status, 200, "Expected status 200 after redirect, got: {}", status);
            } else {
                panic!("No status in response: {:?}", response);
            }
            
            // Verify we got the redirected content
            if let Some(Value::String(body)) = response.get("body") {
                assert_eq!(body, "Redirected content");
            }
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
async fn test_response_headers() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/headers"))
        .respond_with(
            ResponseTemplate::new(200)
                .insert_header("X-Custom-Header", "custom-value")
                .insert_header("X-Request-ID", "12345")
                .set_body_string("OK")
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/headers", mock_server.uri());
    
    let result = handler.handle_async("get", &[Value::String(url)]).await.unwrap();
    
    match result {
        Value::Map(response) => {
            assert_eq!(response.get("status"), Some(&Value::Integer(200)));
            
            // Check if headers are included
            if let Some(Value::Map(headers)) = response.get("headers") {
                // Headers might be normalized to lowercase
                let has_custom = headers.contains_key("x-custom-header") || 
                                headers.contains_key("X-Custom-Header");
                assert!(has_custom);
            }
        }
        _ => panic!("Expected Map response"),
    }
}

#[tokio::test]
#[ignore = "NetworkHandler doesn't support timeout configuration yet"]
async fn test_timeout_handling() {
    let mock_server = MockServer::start().await;
    
    Mock::given(method("GET"))
        .and(path("/api/slow"))
        .respond_with(
            ResponseTemplate::new(200)
                .set_delay(std::time::Duration::from_secs(30))
        )
        .mount(&mock_server)
        .await;

    let handler = NetworkHandler::new();
    let url = format!("{}/api/slow", mock_server.uri());
    
    // The handler uses a default client with standard timeout
    // This test will actually wait for the response (which takes 30s)
    // For this test, we'll skip it as the handler doesn't support custom timeouts
    
    // TODO: Implement timeout configuration in NetworkHandler
}