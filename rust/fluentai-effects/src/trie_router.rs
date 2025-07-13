//! Trie-based router for efficient route matching
//!
//! This implementation provides O(log n) route matching using a trie data structure.
//! It supports static segments, dynamic parameters (:param), and wildcards (*).

use rustc_hash::FxHashMap;
use std::collections::HashMap;

/// Type of route segment
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum SegmentType {
    Static(String),
    Parameter(String),
    Wildcard,
}

/// A node in the route trie
#[derive(Debug)]
struct TrieNode {
    /// Routes that end at this node, keyed by HTTP method
    routes: HashMap<String, RouteData>,
    /// Child nodes for static segments
    static_children: HashMap<String, Box<TrieNode>>,
    /// Child node for parameter segments (if any)
    param_child: Option<(String, Box<TrieNode>)>,
    /// Child node for wildcard (if any)
    wildcard_child: Option<Box<TrieNode>>,
}

impl TrieNode {
    fn new() -> Self {
        Self {
            routes: HashMap::new(),
            static_children: HashMap::new(),
            param_child: None,
            wildcard_child: None,
        }
    }

    /// Insert a route into this node
    fn insert(&mut self, segments: &[&str], method: String, handler_id: String) {
        if segments.is_empty() {
            self.routes.insert(method, RouteData { handler_id });
            return;
        }

        let segment = segments[0];
        let remaining = &segments[1..];

        if segment == "*" {
            // Wildcard segment
            if self.wildcard_child.is_none() {
                self.wildcard_child = Some(Box::new(TrieNode::new()));
            }
            self.wildcard_child.as_mut().unwrap().insert(remaining, method, handler_id);
        } else if let Some(param_name) = segment.strip_prefix(':') {
            // Parameter segment
            match &mut self.param_child {
                Some((existing_name, child)) => {
                    if existing_name != param_name {
                        panic!("Conflicting parameter names: {} vs {}", existing_name, param_name);
                    }
                    child.insert(remaining, method, handler_id);
                }
                None => {
                    let mut child = Box::new(TrieNode::new());
                    child.insert(remaining, method, handler_id);
                    self.param_child = Some((param_name.to_string(), child));
                }
            }
        } else {
            // Static segment
            let child = self.static_children
                .entry(segment.to_string())
                .or_insert_with(|| Box::new(TrieNode::new()));
            child.insert(remaining, method, handler_id);
        }
    }

    /// Find a route in this node
    fn find(&self, segments: &[&str], method: &str, params: &mut FxHashMap<String, String>) -> Option<String> {
        if segments.is_empty() {
            // Check for exact method match first
            if let Some(route) = self.routes.get(method) {
                return Some(route.handler_id.clone());
            }
            // Check for wildcard method
            if let Some(route) = self.routes.get("*") {
                return Some(route.handler_id.clone());
            }
            return None;
        }

        let segment = segments[0];
        let remaining = &segments[1..];

        // Try static match first (highest priority)
        if let Some(child) = self.static_children.get(segment) {
            if let Some(handler) = child.find(remaining, method, params) {
                return Some(handler);
            }
        }

        // Try parameter match
        if let Some((param_name, child)) = &self.param_child {
            params.insert(param_name.clone(), segment.to_string());
            if let Some(handler) = child.find(remaining, method, params) {
                return Some(handler);
            }
            params.remove(param_name);
        }

        // Try wildcard match (lowest priority)
        if let Some(child) = &self.wildcard_child {
            let wildcard_path = segments.join("/");
            params.insert("*".to_string(), wildcard_path);
            // Wildcard consumes all remaining segments
            if let Some(handler) = child.find(&[], method, params) {
                return Some(handler);
            }
            params.remove("*");
        }

        None
    }
}

/// Route data stored in the trie
#[derive(Debug)]
struct RouteData {
    handler_id: String,
}

/// Trie-based router for efficient route matching
pub struct TrieRouter {
    root: TrieNode,
    /// Configuration for the router
    config: RouterConfig,
}

/// Router configuration
#[derive(Debug, Clone)]
pub struct RouterConfig {
    /// Maximum number of routes allowed
    pub max_routes: usize,
    /// Maximum depth of route paths
    pub max_path_depth: usize,
    /// Whether to allow duplicate routes
    pub allow_duplicates: bool,
}

impl Default for RouterConfig {
    fn default() -> Self {
        Self {
            max_routes: 10_000,
            max_path_depth: 20,
            allow_duplicates: false,
        }
    }
}

impl TrieRouter {
    /// Create a new trie router with default configuration
    pub fn new() -> Self {
        Self::with_config(RouterConfig::default())
    }

    /// Create a new trie router with custom configuration
    pub fn with_config(config: RouterConfig) -> Self {
        Self {
            root: TrieNode::new(),
            config,
        }
    }

    /// Add a route to the router
    pub fn add_route(&mut self, method: &str, path: &str, handler_id: &str) -> Result<(), String> {
        // Validate path
        if !path.starts_with('/') {
            return Err("Path must start with /".to_string());
        }

        // Split path into segments
        let segments: Vec<&str> = path
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        // Check path depth
        if segments.len() > self.config.max_path_depth {
            return Err(format!(
                "Path depth {} exceeds maximum allowed depth {}",
                segments.len(),
                self.config.max_path_depth
            ));
        }

        // Validate segments
        let mut param_names = std::collections::HashSet::new();
        let mut has_wildcard = false;
        
        for (i, segment) in segments.iter().enumerate() {
            if segment.is_empty() {
                return Err("Empty path segment".to_string());
            }
            
            if *segment == "*" {
                if has_wildcard {
                    return Err("Multiple wildcards in path".to_string());
                }
                if i != segments.len() - 1 {
                    return Err("Wildcard must be the last segment".to_string());
                }
                has_wildcard = true;
            } else if let Some(param) = segment.strip_prefix(':') {
                if param.is_empty() {
                    return Err("Empty parameter name".to_string());
                }
                if !param_names.insert(param) {
                    return Err(format!("Duplicate parameter name: {}", param));
                }
                if has_wildcard {
                    return Err("Parameters cannot appear after wildcard".to_string());
                }
            }
        }

        // Insert the route
        self.root.insert(&segments, method.to_uppercase(), handler_id.to_string());
        Ok(())
    }

    /// Find a matching route for the given method and path
    pub fn find_route(&self, method: &str, path: &str) -> Option<(String, FxHashMap<String, String>)> {
        let segments: Vec<&str> = path
            .split('/')
            .filter(|s| !s.is_empty())
            .collect();

        let mut params = FxHashMap::default();
        let handler = self.root.find(&segments, &method.to_uppercase(), &mut params)?;
        Some((handler, params))
    }

    /// Get statistics about the router
    pub fn stats(&self) -> RouterStats {
        let mut stats = RouterStats::default();
        self.collect_stats(&self.root, 0, &mut stats);
        stats
    }

    fn collect_stats(&self, node: &TrieNode, depth: usize, stats: &mut RouterStats) {
        stats.total_nodes += 1;
        stats.max_depth = stats.max_depth.max(depth);
        stats.total_routes += node.routes.len();

        for child in node.static_children.values() {
            self.collect_stats(child, depth + 1, stats);
        }

        if let Some((_, child)) = &node.param_child {
            stats.param_nodes += 1;
            self.collect_stats(child, depth + 1, stats);
        }

        if let Some(child) = &node.wildcard_child {
            stats.wildcard_nodes += 1;
            self.collect_stats(child, depth + 1, stats);
        }
    }
}

/// Router statistics
#[derive(Debug, Default)]
pub struct RouterStats {
    pub total_nodes: usize,
    pub total_routes: usize,
    pub param_nodes: usize,
    pub wildcard_nodes: usize,
    pub max_depth: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_static_routes() {
        let mut router = TrieRouter::new();
        router.add_route("GET", "/api/users", "listUsers").unwrap();
        router.add_route("POST", "/api/users", "createUser").unwrap();
        router.add_route("GET", "/api/products", "listProducts").unwrap();

        // Test exact matches
        let (handler, params) = router.find_route("GET", "/api/users").unwrap();
        assert_eq!(handler, "listUsers");
        assert!(params.is_empty());

        let (handler, params) = router.find_route("POST", "/api/users").unwrap();
        assert_eq!(handler, "createUser");
        assert!(params.is_empty());

        // Test no match
        assert!(router.find_route("DELETE", "/api/users").is_none());
        assert!(router.find_route("GET", "/api/orders").is_none());
    }

    #[test]
    fn test_parameter_routes() {
        let mut router = TrieRouter::new();
        router.add_route("GET", "/api/users/:id", "getUser").unwrap();
        router.add_route("GET", "/api/users/:id/posts/:postId", "getUserPost").unwrap();

        // Test parameter extraction
        let (handler, params) = router.find_route("GET", "/api/users/123").unwrap();
        assert_eq!(handler, "getUser");
        assert_eq!(params.get("id"), Some(&"123".to_string()));

        let (handler, params) = router.find_route("GET", "/api/users/456/posts/789").unwrap();
        assert_eq!(handler, "getUserPost");
        assert_eq!(params.get("id"), Some(&"456".to_string()));
        assert_eq!(params.get("postId"), Some(&"789".to_string()));
    }

    #[test]
    fn test_wildcard_routes() {
        let mut router = TrieRouter::new();
        router.add_route("GET", "/static/*", "serveStatic").unwrap();
        router.add_route("GET", "/api/*", "catchAll").unwrap();

        // Test wildcard matching
        let (handler, params) = router.find_route("GET", "/static/css/style.css").unwrap();
        assert_eq!(handler, "serveStatic");
        assert_eq!(params.get("*"), Some(&"css/style.css".to_string()));

        let (handler, params) = router.find_route("GET", "/api/v2/users/list").unwrap();
        assert_eq!(handler, "catchAll");
        assert_eq!(params.get("*"), Some(&"v2/users/list".to_string()));
    }

    #[test]
    fn test_route_priority() {
        let mut router = TrieRouter::new();
        router.add_route("GET", "/api/users/*", "wildcard").unwrap();
        router.add_route("GET", "/api/users/:id", "param").unwrap();
        router.add_route("GET", "/api/users/special", "static").unwrap();

        // Static routes have highest priority
        let (handler, _) = router.find_route("GET", "/api/users/special").unwrap();
        assert_eq!(handler, "static");

        // Parameter routes have higher priority than wildcards
        let (handler, params) = router.find_route("GET", "/api/users/123").unwrap();
        assert_eq!(handler, "param");
        assert_eq!(params.get("id"), Some(&"123".to_string()));

        // Wildcard catches everything else
        let (handler, params) = router.find_route("GET", "/api/users/123/posts").unwrap();
        assert_eq!(handler, "wildcard");
        assert_eq!(params.get("*"), Some(&"123/posts".to_string()));
    }

    #[test]
    fn test_wildcard_method() {
        let mut router = TrieRouter::new();
        router.add_route("*", "/health", "healthCheck").unwrap();
        router.add_route("GET", "/api/users", "getUsers").unwrap();

        // Wildcard method matches any HTTP method
        let (handler, _) = router.find_route("GET", "/health").unwrap();
        assert_eq!(handler, "healthCheck");

        let (handler, _) = router.find_route("POST", "/health").unwrap();
        assert_eq!(handler, "healthCheck");

        let (handler, _) = router.find_route("DELETE", "/health").unwrap();
        assert_eq!(handler, "healthCheck");

        // Specific methods still work
        let (handler, _) = router.find_route("GET", "/api/users").unwrap();
        assert_eq!(handler, "getUsers");
    }

    #[test]
    fn test_invalid_routes() {
        let mut router = TrieRouter::new();

        // Path must start with /
        assert!(router.add_route("GET", "api/users", "handler").is_err());

        // Empty parameter name
        assert!(router.add_route("GET", "/api/users/:", "handler").is_err());

        // Duplicate parameter names
        assert!(router.add_route("GET", "/api/:id/posts/:id", "handler").is_err());

        // Wildcard not at end
        assert!(router.add_route("GET", "/api/*/users", "handler").is_err());

        // Multiple wildcards
        assert!(router.add_route("GET", "/api/*/*", "handler").is_err());
    }

    #[test]
    fn test_router_stats() {
        let mut router = TrieRouter::new();
        router.add_route("GET", "/api/users", "listUsers").unwrap();
        router.add_route("GET", "/api/users/:id", "getUser").unwrap();
        router.add_route("GET", "/api/users/:id/posts", "getUserPosts").unwrap();
        router.add_route("GET", "/static/*", "serveStatic").unwrap();

        let stats = router.stats();
        assert!(stats.total_nodes > 0);
        assert!(stats.total_routes == 4);
        assert!(stats.param_nodes > 0);
        assert!(stats.wildcard_nodes == 1);
        assert!(stats.max_depth > 0);
    }
}