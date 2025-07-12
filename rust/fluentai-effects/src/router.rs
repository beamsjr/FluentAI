//! HTTP router with dynamic path parameter support
//!
//! This router supports:
//! - Static path segments: /api/users
//! - Path parameters: /api/users/:id
//! - Wildcard segments: /api/users/*
//! - Query string extraction

use rustc_hash::FxHashMap;

/// A single segment in a route path
#[derive(Debug, Clone, PartialEq)]
enum RouteSegment {
    /// Static path segment that must match exactly
    Static(String),
    /// Dynamic parameter like :id
    Parameter(String),
    /// Wildcard that matches everything
    Wildcard,
}

/// A compiled route pattern
#[derive(Debug, Clone)]
pub struct RoutePattern {
    segments: Vec<RouteSegment>,
    original: String,
}

impl RoutePattern {
    /// Parse a route pattern string into segments
    pub fn new(pattern: &str) -> Self {
        let segments = pattern
            .split('/')
            .filter(|s| !s.is_empty())
            .map(|segment| {
                if segment == "*" {
                    RouteSegment::Wildcard
                } else if let Some(param) = segment.strip_prefix(':') {
                    RouteSegment::Parameter(param.to_string())
                } else {
                    RouteSegment::Static(segment.to_string())
                }
            })
            .collect();

        Self {
            segments,
            original: pattern.to_string(),
        }
    }

    /// Match a path against this pattern and extract parameters
    pub fn match_path(&self, path: &str) -> Option<FxHashMap<String, String>> {
        let path_segments: Vec<&str> = path
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        // Check if segment counts match (unless we have a wildcard)
        let has_wildcard = self.segments.iter().any(|s| matches!(s, RouteSegment::Wildcard));
        if !has_wildcard && path_segments.len() != self.segments.len() {
            return None;
        }

        let mut params = FxHashMap::default();
        
        for (i, (pattern_seg, path_seg)) in self.segments.iter().zip(path_segments.iter()).enumerate() {
            match pattern_seg {
                RouteSegment::Static(expected) => {
                    if expected != path_seg {
                        return None;
                    }
                }
                RouteSegment::Parameter(name) => {
                    params.insert(name.clone(), (*path_seg).to_string());
                }
                RouteSegment::Wildcard => {
                    // Capture the rest of the path
                    let rest = path_segments[i..].join("/");
                    params.insert("*".to_string(), rest);
                    break;
                }
            }
        }

        Some(params)
    }
}

/// Route handler information
#[derive(Clone)]
pub struct Route {
    pub method: String,
    pub pattern: RoutePattern,
    pub handler_id: String,
}

/// HTTP router that manages routes and matches requests
pub struct Router {
    routes: Vec<Route>,
}

impl Router {
    pub fn new() -> Self {
        Self {
            routes: Vec::new(),
        }
    }

    /// Add a route to the router
    pub fn add_route(&mut self, method: &str, path: &str, handler_id: &str) {
        let route = Route {
            method: method.to_uppercase(),
            pattern: RoutePattern::new(path),
            handler_id: handler_id.to_string(),
        };
        self.routes.push(route);
    }

    /// Find a matching route for the given method and path
    pub fn find_route(&self, method: &str, path: &str) -> Option<(String, FxHashMap<String, String>)> {
        let method = method.to_uppercase();
        
        for route in &self.routes {
            if route.method == method || route.method == "*" {
                if let Some(params) = route.pattern.match_path(path) {
                    return Some((route.handler_id.clone(), params));
                }
            }
        }
        
        None
    }

    /// Get all routes (for debugging/introspection)
    pub fn get_routes(&self) -> Vec<(String, String, String)> {
        self.routes
            .iter()
            .map(|r| (r.method.clone(), r.pattern.original.clone(), r.handler_id.clone()))
            .collect()
    }
}

/// Extract query parameters from a query string
pub fn parse_query_string(query: &str) -> FxHashMap<String, String> {
    url::form_urlencoded::parse(query.as_bytes())
        .map(|(k, v)| (k.to_string(), v.to_string()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_static_route_matching() {
        let pattern = RoutePattern::new("/api/users");
        
        assert!(pattern.match_path("/api/users").is_some());
        assert!(pattern.match_path("/api/users/").is_some());
        assert!(pattern.match_path("/api/products").is_none());
        assert!(pattern.match_path("/api").is_none());
    }

    #[test]
    fn test_parameter_route_matching() {
        let pattern = RoutePattern::new("/api/users/:id");
        
        if let Some(params) = pattern.match_path("/api/users/123") {
            assert_eq!(params.get("id"), Some(&"123".to_string()));
        } else {
            panic!("Expected match");
        }
        
        assert!(pattern.match_path("/api/users").is_none());
        assert!(pattern.match_path("/api/users/123/posts").is_none());
    }

    #[test]
    fn test_multiple_parameters() {
        let pattern = RoutePattern::new("/api/users/:userId/posts/:postId");
        
        if let Some(params) = pattern.match_path("/api/users/42/posts/99") {
            assert_eq!(params.get("userId"), Some(&"42".to_string()));
            assert_eq!(params.get("postId"), Some(&"99".to_string()));
        } else {
            panic!("Expected match");
        }
    }

    #[test]
    fn test_wildcard_route() {
        let pattern = RoutePattern::new("/api/*");
        
        if let Some(params) = pattern.match_path("/api/users/123/posts") {
            assert_eq!(params.get("*"), Some(&"users/123/posts".to_string()));
        } else {
            panic!("Expected match");
        }
    }

    #[test]
    fn test_router() {
        let mut router = Router::new();
        
        router.add_route("GET", "/api/users", "listUsers");
        router.add_route("GET", "/api/users/:id", "getUser");
        router.add_route("POST", "/api/users", "createUser");
        router.add_route("*", "/health", "healthCheck");
        
        // Test exact match
        if let Some((handler, params)) = router.find_route("GET", "/api/users") {
            assert_eq!(handler, "listUsers");
            assert!(params.is_empty());
        } else {
            panic!("Expected route match");
        }
        
        // Test parameter extraction
        if let Some((handler, params)) = router.find_route("GET", "/api/users/123") {
            assert_eq!(handler, "getUser");
            assert_eq!(params.get("id"), Some(&"123".to_string()));
        } else {
            panic!("Expected route match");
        }
        
        // Test method matching
        if let Some((handler, _)) = router.find_route("POST", "/api/users") {
            assert_eq!(handler, "createUser");
        } else {
            panic!("Expected route match");
        }
        
        // Test wildcard method
        if let Some((handler, _)) = router.find_route("PUT", "/health") {
            assert_eq!(handler, "healthCheck");
        } else {
            panic!("Expected route match");
        }
        
        // Test no match
        assert!(router.find_route("DELETE", "/api/products").is_none());
    }

    #[test]
    fn test_query_string_parsing() {
        let query = "page=2&limit=10&filter=active";
        let params = parse_query_string(query);
        
        assert_eq!(params.get("page"), Some(&"2".to_string()));
        assert_eq!(params.get("limit"), Some(&"10".to_string()));
        assert_eq!(params.get("filter"), Some(&"active".to_string()));
    }
}