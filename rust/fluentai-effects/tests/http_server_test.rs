//! Tests for HTTP server functionality

use fluentai_effects::{handlers::HttpServerHandler, EffectHandler};
use fluentai_core::value::Value;
use rustc_hash::FxHashMap;

#[tokio::test]
async fn test_http_server_route_registration() {
    let handler = HttpServerHandler::new();
    
    // Test route registration
    let result = handler.handle_sync(
        "route",
        &[
            Value::String("GET".to_string()),
            Value::String("/api/users/:id".to_string()),
            Value::String("getUserHandler".to_string()),
        ],
    );
    
    assert!(result.is_ok());
    
    if let Ok(Value::Map(route_info)) = result {
        assert_eq!(route_info.get("method"), Some(&Value::String("GET".to_string())));
        assert_eq!(route_info.get("path"), Some(&Value::String("/api/users/:id".to_string())));
        assert_eq!(route_info.get("handler_id"), Some(&Value::String("getUserHandler".to_string())));
        assert!(route_info.contains_key("key"));
    }
}

#[tokio::test]
async fn test_http_server_listen_and_stop() {
    let handler = HttpServerHandler::new();
    
    // Register a test route
    let _ = handler.handle_sync(
        "route",
        &[
            Value::String("GET".to_string()),
            Value::String("/test".to_string()),
            Value::String("testHandler".to_string()),
        ],
    );
    
    // Start server on random port
    let listen_result = handler
        .handle_async("listen", &[Value::Integer(0)])
        .await;
    
    assert!(listen_result.is_ok());
    
    if let Ok(Value::Map(server_info)) = listen_result {
        assert!(server_info.contains_key("id"));
        assert!(server_info.contains_key("address"));
        assert!(server_info.contains_key("port"));
        
        // Get server ID for stopping
        if let Some(Value::String(server_id)) = server_info.get("id") {
            // Stop the server
            let stop_result = handler
                .handle_async("stop", &[Value::String(server_id.clone())])
                .await;
            
            assert!(stop_result.is_ok());
            assert_eq!(stop_result.unwrap(), Value::Boolean(true));
        }
    }
}

#[tokio::test]
async fn test_multiple_route_registration() {
    let handler = HttpServerHandler::new();
    
    // Register multiple routes
    let routes = vec![
        ("GET", "/", "homeHandler"),
        ("GET", "/api/users", "listUsersHandler"),
        ("POST", "/api/users", "createUserHandler"),
        ("GET", "/api/users/:id", "getUserHandler"),
        ("PUT", "/api/users/:id", "updateUserHandler"),
        ("DELETE", "/api/users/:id", "deleteUserHandler"),
        ("*", "/", "notFoundHandler"),
    ];
    
    for (method, path, handler_id) in routes {
        let result = handler.handle_sync(
            "route",
            &[
                Value::String(method.to_string()),
                Value::String(path.to_string()),
                Value::String(handler_id.to_string()),
            ],
        );
        
        assert!(result.is_ok(), "Failed to register route {} {}", method, path);
    }
}

#[test]
fn test_route_sync_operations() {
    let handler = HttpServerHandler::new();
    
    // Test that async operations return proper error when called sync
    let result = handler.handle_sync("listen", &[Value::Integer(8080)]);
    assert!(result.is_err());
    
    if let Err(fluentai_core::error::Error::Runtime(msg)) = result {
        assert!(msg.contains("must be called asynchronously"));
    }
}

#[test]
fn test_is_async_operation() {
    let handler = HttpServerHandler::new();
    
    assert!(!handler.is_async_operation("route"));
    assert!(handler.is_async_operation("listen"));
    assert!(handler.is_async_operation("stop"));
}

#[test]
fn test_invalid_route_arguments() {
    let handler = HttpServerHandler::new();
    
    // Test with missing arguments
    let result = handler.handle_sync("route", &[Value::String("GET".to_string())]);
    assert!(result.is_err());
    
    // Test with wrong argument types
    let result = handler.handle_sync(
        "route",
        &[
            Value::Integer(123),
            Value::String("/path".to_string()),
            Value::String("handler".to_string()),
        ],
    );
    assert!(result.is_err());
}

#[tokio::test]
async fn test_server_not_found() {
    let handler = HttpServerHandler::new();
    
    // Try to stop non-existent server
    let result = handler
        .handle_async("stop", &[Value::String("non_existent_server".to_string())])
        .await;
    
    assert!(result.is_err());
    if let Err(fluentai_core::error::Error::Runtime(msg)) = result {
        assert!(msg.contains("not found"));
    }
}

#[tokio::test]
async fn test_invalid_port() {
    let handler = HttpServerHandler::new();
    
    // Test with non-integer port
    let result = handler
        .handle_async("listen", &[Value::String("not_a_port".to_string())])
        .await;
    
    assert!(result.is_err());
}

// Router tests
#[cfg(test)]
mod router_tests {
    use fluentai_effects::router::{Router, RoutePattern, parse_query_string};
    
    #[test]
    fn test_route_pattern_static() {
        let pattern = RoutePattern::new("/api/users");
        
        assert!(pattern.match_path("/api/users").is_some());
        assert!(pattern.match_path("/api/products").is_none());
    }
    
    #[test]
    fn test_route_pattern_with_params() {
        let pattern = RoutePattern::new("/api/users/:id/posts/:postId");
        
        if let Some(params) = pattern.match_path("/api/users/123/posts/456") {
            assert_eq!(params.get("id"), Some(&"123".to_string()));
            assert_eq!(params.get("postId"), Some(&"456".to_string()));
        } else {
            panic!("Expected pattern to match");
        }
    }
    
    #[test]
    fn test_route_pattern_wildcard() {
        let pattern = RoutePattern::new("/static/*");
        
        if let Some(params) = pattern.match_path("/static/css/main.css") {
            assert_eq!(params.get("*"), Some(&"css/main.css".to_string()));
        } else {
            panic!("Expected wildcard to match");
        }
    }
    
    #[test]
    fn test_router_routing() {
        let mut router = Router::new();
        
        router.add_route("GET", "/api/users", "listUsers");
        router.add_route("GET", "/api/users/:id", "getUser");
        router.add_route("POST", "/api/users", "createUser");
        
        // Test exact match
        if let Some((handler, params)) = router.find_route("GET", "/api/users") {
            assert_eq!(handler, "listUsers");
            assert!(params.is_empty());
        } else {
            panic!("Expected route match");
        }
        
        // Test param extraction
        if let Some((handler, params)) = router.find_route("GET", "/api/users/42") {
            assert_eq!(handler, "getUser");
            assert_eq!(params.get("id"), Some(&"42".to_string()));
        } else {
            panic!("Expected route match");
        }
        
        // Test method matching
        if let Some((handler, _)) = router.find_route("POST", "/api/users") {
            assert_eq!(handler, "createUser");
        } else {
            panic!("Expected route match");
        }
        
        // Test no match
        assert!(router.find_route("DELETE", "/api/users/42").is_none());
    }
    
    #[test]
    fn test_query_string_parsing() {
        let query = "page=2&limit=10&sort=name&filter=active";
        let params = parse_query_string(query);
        
        assert_eq!(params.get("page"), Some(&"2".to_string()));
        assert_eq!(params.get("limit"), Some(&"10".to_string()));
        assert_eq!(params.get("sort"), Some(&"name".to_string()));
        assert_eq!(params.get("filter"), Some(&"active".to_string()));
    }
}