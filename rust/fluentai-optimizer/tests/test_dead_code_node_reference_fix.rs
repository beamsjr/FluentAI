use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use fluentai_optimizer::GraphOptimizer;

#[test]
fn test_dead_code_elimination_invalid_node_reference() {
    // This test reproduces the bug where dead code elimination creates invalid node references
    // The issue: When a Let node has a body that references a non-reachable node,
    // copy_with_mapping returns the original NodeId which doesn't exist in the optimized graph
    
    let mut graph = Graph::new();
    
    // Create nodes that won't be reachable
    let unused_var1 = graph.add_node(Node::Variable { name: "unused1".to_string() }).expect("Failed to add node");
    let unused_var2 = graph.add_node(Node::Variable { name: "unused2".to_string() }).expect("Failed to add node");
    let unused_var3 = graph.add_node(Node::Variable { name: "unused3".to_string() }).expect("Failed to add node");
    
    // Create an application node that won't be reachable
    let unreachable_app = graph.add_node(Node::Application {
        function: unused_var1,
        args: vec![unused_var2, unused_var3],
    }).expect("Failed to add node");
    
    // Create a literal that will be used as the binding value
    let lit_42 = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    
    // Create a Let node where:
    // - The binding "x" is not used in the body
    // - The body references nodes that won't be reachable
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), lit_42)], // "x" is not used
        body: unreachable_app, // This references nodes that won't be reachable!
    }).expect("Failed to add node");
    
    // Set root to let_node
    // This makes let_node reachable, but since "x" is not used in the body,
    // and the body (unreachable_app) doesn't have effects, it won't be marked as reachable
    graph.root_id = Some(let_node);
    
    println!("Original graph:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Run optimizer - with our fix, it should now succeed
    let mut optimizer = GraphOptimizer::new();
    match optimizer.optimize(&graph) {
        Ok(optimized) => {
            // If optimization succeeds, verify all node references are valid
            println!("\nOptimized graph:");
            for (id, node) in &optimized.nodes {
                println!("  {:?}: {:?}", id, node);
                
                // Verify all referenced nodes exist
                verify_node_references(&optimized, node);
            }
            
            // Check that the Let node has no bindings (since "x" was unused)
            let has_empty_bindings = optimized.nodes.values().any(|node| {
                matches!(node, Node::Let { bindings, .. } if bindings.is_empty())
            });
            assert!(has_empty_bindings, "Let node should have empty bindings after optimization");
            
            println!("\nOptimization succeeded correctly - unused bindings were removed");
        }
        Err(e) => {
            panic!("Optimization should succeed now with our fix, but failed: {}", e);
        }
    }
}

fn verify_node_references(graph: &Graph, node: &Node) {
    match node {
        Node::Application { function, args } => {
            assert!(graph.get_node(*function).is_some(), "Function node {:?} doesn't exist", function);
            for arg in args {
                assert!(graph.get_node(*arg).is_some(), "Arg node {:?} doesn't exist", arg);
            }
        }
        Node::Let { bindings, body } => {
            for (_, value_id) in bindings {
                assert!(graph.get_node(*value_id).is_some(), "Binding value node {:?} doesn't exist", value_id);
            }
            assert!(graph.get_node(*body).is_some(), "Let body node {:?} doesn't exist", body);
        }
        Node::Lambda { body, .. } => {
            assert!(graph.get_node(*body).is_some(), "Lambda body node {:?} doesn't exist", body);
        }
        Node::If { condition, then_branch, else_branch } => {
            assert!(graph.get_node(*condition).is_some(), "Condition node {:?} doesn't exist", condition);
            assert!(graph.get_node(*then_branch).is_some(), "Then branch node {:?} doesn't exist", then_branch);
            assert!(graph.get_node(*else_branch).is_some(), "Else branch node {:?} doesn't exist", else_branch);
        }
        _ => {}
    }
}

#[test]
fn test_dead_code_elimination_with_used_binding() {
    // Test the case where the binding IS used and everything should work correctly
    let mut graph = Graph::new();
    
    // Create nodes
    let add_var = graph.add_node(Node::Variable { name: "add".to_string() }).expect("Failed to add node");
    let x_lit = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let y_lit = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    
    // Create an application that uses the "add" variable
    let app = graph.add_node(Node::Application {
        function: add_var,
        args: vec![x_lit, y_lit],
    }).expect("Failed to add node");
    
    // Create a function node for the binding
    let add_fn = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    
    // Create a Let node where the binding IS used in the body
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("add".to_string(), add_fn)],
        body: app, // This uses "add" so everything should be reachable
    }).expect("Failed to add node");
    
    graph.root_id = Some(let_node);
    
    // Run optimizer - this should work correctly
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).expect("Optimization should succeed");
    
    // Verify all nodes are present since they're all used
    assert_eq!(graph.nodes.len(), optimized.nodes.len(), "No nodes should be eliminated");
    
    // Verify all references are valid
    for (_, node) in &optimized.nodes {
        verify_node_references(&optimized, node);
    }
}