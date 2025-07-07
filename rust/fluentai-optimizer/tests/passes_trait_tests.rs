//! Tests for the OptimizationPass trait and general pass functionality

use fluentai_optimizer::passes::OptimizationPass;
use fluentai_core::ast::{Graph, Node, Literal};
use anyhow::Result;
use std::sync::{Arc, Mutex};

/// Mock optimization pass for testing trait functionality
struct MockPass {
    name: String,
    run_count: Arc<Mutex<usize>>,
    should_fail: bool,
    applicable: bool,
}

impl MockPass {
    fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            run_count: Arc::new(Mutex::new(0)),
            should_fail: false,
            applicable: true,
        }
    }
    
    fn with_failure(mut self) -> Self {
        self.should_fail = true;
        self
    }
    
    fn with_applicability(mut self, applicable: bool) -> Self {
        self.applicable = applicable;
        self
    }
    
    fn run_count(&self) -> usize {
        *self.run_count.lock().unwrap()
    }
}

impl OptimizationPass for MockPass {
    fn name(&self) -> &str {
        &self.name
    }
    
    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        *self.run_count.lock().unwrap() += 1;
        
        if self.should_fail {
            anyhow::bail!("Mock pass failed as requested");
        }
        
        // Simple transformation: increment all integer literals
        let mut result = Graph::new();
        let mut node_mapping = std::collections::HashMap::new();
        
        // Clone all nodes, incrementing integers
        for (id, node) in &graph.nodes {
            let new_node = match node {
                Node::Literal(Literal::Integer(n)) => Node::Literal(Literal::Integer(n + 1)),
                _ => node.clone(),
            };
            let new_id = result.add_node(new_node).expect("Failed to add node");
            node_mapping.insert(*id, new_id);
        }
        
        // Update root if present
        if let Some(root) = graph.root_id {
            result.root_id = node_mapping.get(&root).copied();
        }
        
        Ok(result)
    }
    
    fn is_applicable(&self, graph: &Graph) -> bool {
        self.applicable && !graph.nodes.is_empty()
    }
    
    fn stats(&self) -> String {
        format!("{} pass: ran {} times", self.name, self.run_count())
    }
}

#[test]
fn test_trait_basic_functionality() {
    let mut pass = MockPass::new("test-pass");
    
    assert_eq!(pass.name(), "test-pass");
    assert_eq!(pass.run_count(), 0);
    
    let mut graph = Graph::new();
    let node = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    graph.root_id = Some(node);
    
    let result = pass.run(&graph).unwrap();
    assert_eq!(pass.run_count(), 1);
    
    // Check transformation
    if let Some(root) = result.root_id {
        if let Some(Node::Literal(Literal::Integer(n))) = result.get_node(root) {
            assert_eq!(*n, 43);
        } else {
            panic!("Expected integer literal");
        }
    } else {
        panic!("Expected root node");
    }
}

#[test]
fn test_is_applicable_default() {
    let pass = MockPass::new("test");
    
    // Empty graph
    let empty_graph = Graph::new();
    assert!(!pass.is_applicable(&empty_graph));
    
    // Non-empty graph
    let mut graph = Graph::new();
    graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    assert!(pass.is_applicable(&graph));
}

#[test]
fn test_is_applicable_custom() {
    let pass = MockPass::new("test").with_applicability(false);
    
    let mut graph = Graph::new();
    graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    
    assert!(!pass.is_applicable(&graph));
}

#[test]
fn test_stats_default() {
    let mut pass = MockPass::new("optimization");
    
    assert_eq!(pass.stats(), "optimization pass: ran 0 times");
    
    let graph = Graph::new();
    let _ = pass.run(&graph);
    
    assert_eq!(pass.stats(), "optimization pass: ran 1 times");
}

#[test]
fn test_pass_failure() {
    let mut pass = MockPass::new("failing").with_failure();
    
    let graph = Graph::new();
    let result = pass.run(&graph);
    
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Mock pass failed"));
}

#[test]
fn test_send_sync_traits() {
    // Test that OptimizationPass can be sent between threads
    fn assert_send_sync<T: Send + Sync>() {}
    assert_send_sync::<MockPass>();
    
    // Test actual usage across threads
    let pass: Box<dyn OptimizationPass> = Box::new(MockPass::new("concurrent"));
    
    let handle = std::thread::spawn(move || {
        pass.name().to_string()
    });
    
    let name = handle.join().unwrap();
    assert_eq!(name, "concurrent");
}

#[test]
fn test_multiple_passes_sequential() {
    let mut passes: Vec<Box<dyn OptimizationPass>> = vec![
        Box::new(MockPass::new("pass1")),
        Box::new(MockPass::new("pass2")),
        Box::new(MockPass::new("pass3")),
    ];
    
    let mut graph = Graph::new();
    let node = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    graph.root_id = Some(node);
    
    // Run passes sequentially
    let mut current = graph;
    for pass in &mut passes {
        current = pass.run(&current).unwrap();
    }
    
    // Each pass increments by 1, so final value should be 3
    if let Some(root) = current.root_id {
        if let Some(Node::Literal(Literal::Integer(n))) = current.get_node(root) {
            assert_eq!(*n, 3);
        }
    }
}

#[test]
fn test_graph_preservation() {
    let mut pass = MockPass::new("preserving");
    
    let mut graph = Graph::new();
    let lit1 = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let lit2 = graph.add_node(Node::Literal(Literal::String("hello".to_string()))).expect("Failed to add node");
    let var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    let app = graph.add_node(Node::Application {
        function: var,
        args: vec![lit1, lit2],
    }).expect("Failed to add node");
    graph.root_id = Some(app);
    
    let result = pass.run(&graph).unwrap();
    
    // Check structure is preserved
    assert_eq!(result.nodes.len(), graph.nodes.len());
    assert!(result.root_id.is_some());
    
    // Find and check the modified nodes
    let mut found_int = false;
    let mut found_string = false;
    
    for (_, node) in &result.nodes {
        match node {
            Node::Literal(Literal::Integer(n)) => {
                assert_eq!(*n, 2);
                found_int = true;
            }
            Node::Literal(Literal::String(s)) => {
                assert_eq!(s, "hello");
                found_string = true;
            }
            _ => {}
        }
    }
    
    assert!(found_int);
    assert!(found_string);
}

/// Test a pass that does nothing
struct NoOpPass;

impl OptimizationPass for NoOpPass {
    fn name(&self) -> &str {
        "no-op"
    }
    
    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        Ok(graph.clone())
    }
}

#[test]
fn test_noop_pass() {
    let mut pass = NoOpPass;
    
    let mut graph = Graph::new();
    let node = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    graph.root_id = Some(node);
    
    let result = pass.run(&graph).unwrap();
    
    // Graph should be unchanged
    assert_eq!(result.nodes.len(), graph.nodes.len());
    if let Some(root) = result.root_id {
        if let Some(Node::Literal(Literal::Integer(n))) = result.get_node(root) {
            assert_eq!(*n, 42);
        }
    }
}

/// Test a pass that removes nodes
struct RemovalPass;

impl OptimizationPass for RemovalPass {
    fn name(&self) -> &str {
        "removal"
    }
    
    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        let mut result = Graph::new();
        
        // Only keep the root node
        if let Some(root) = graph.root_id {
            if let Some(node) = graph.get_node(root) {
                let new_root = result.add_node(node.clone()).expect("Failed to add node");
                result.root_id = Some(new_root);
            }
        }
        
        Ok(result)
    }
}

#[test]
fn test_removal_pass() {
    let mut pass = RemovalPass;
    
    let mut graph = Graph::new();
    let _unused1 = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let _unused2 = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let root = graph.add_node(Node::Literal(Literal::Integer(3))).expect("Failed to add node");
    graph.root_id = Some(root);
    
    let result = pass.run(&graph).unwrap();
    
    // Should only have root node
    assert_eq!(result.nodes.len(), 1);
    assert!(result.root_id.is_some());
}

#[test]
fn test_pass_with_empty_graph() {
    let mut pass = MockPass::new("empty-handler");
    
    let graph = Graph::new();
    let result = pass.run(&graph).unwrap();
    
    assert!(result.nodes.is_empty());
    assert!(result.root_id.is_none());
}

#[test]
fn test_dynamic_dispatch() {
    let passes: Vec<Box<dyn OptimizationPass>> = vec![
        Box::new(MockPass::new("mock")),
        Box::new(NoOpPass),
        Box::new(RemovalPass),
    ];
    
    let names: Vec<&str> = passes.iter().map(|p| p.name()).collect();
    assert_eq!(names, vec!["mock", "no-op", "removal"]);
    
    for pass in passes {
        assert!(pass.stats().contains(pass.name()));
    }
}