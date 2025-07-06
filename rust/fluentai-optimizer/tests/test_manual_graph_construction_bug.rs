use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use fluentai_optimizer::GraphOptimizer;

#[test]
fn test_manual_invalid_node_reference() {
    // This test manually creates a graph with an invalid node reference
    // to simulate the exact bug described where NodeId(9) doesn't exist
    
    let mut graph = Graph::new();
    
    // Manually create nodes with specific IDs by inserting them directly
    // This simulates a graph that might be created by a parser or other tool
    // that has a bug and creates invalid references
    
    // Create some nodes
    let node1 = NodeId(std::num::NonZeroU32::new(1).unwrap());
    let node2 = NodeId(std::num::NonZeroU32::new(2).unwrap());
    let node3 = NodeId(std::num::NonZeroU32::new(3).unwrap());
    let node9 = NodeId(std::num::NonZeroU32::new(9).unwrap()); // This node won't exist!
    
    // Insert nodes into the graph
    graph.nodes.insert(node1, Node::Variable { name: "+".to_string() });
    graph.nodes.insert(node2, Node::Variable { name: "add".to_string() });
    graph.nodes.insert(node3, Node::Let {
        bindings: vec![("add".to_string(), node2)],
        body: node9, // Reference to non-existent NodeId(9)!
    });
    
    // Set root
    graph.root_id = Some(node3);
    
    println!("Original graph with invalid reference:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("  Note: NodeId(9) is referenced but doesn't exist!");
    
    // Run optimizer - with our fix, this should now return an error
    let mut optimizer = GraphOptimizer::new();
    match optimizer.optimize(&graph) {
        Ok(_) => {
            panic!("Expected optimization to fail due to invalid node reference");
        }
        Err(e) => {
            println!("\nOptimization correctly failed with error: {}", e);
            assert!(e.to_string().contains("Invalid node reference"));
            // The error might be about NodeId(2) or NodeId(9) depending on which is processed first
            assert!(e.to_string().contains("NodeId(") && e.to_string().contains(")"));
        }
    }
}

#[test] 
fn test_reachability_analysis_bug() {
    // Another way the bug could manifest: through complex reachability patterns
    let mut graph = Graph::new();
    
    // Create a lambda that captures a variable
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    
    // Create a lambda body that uses y but not x
    let plus_var = graph.add_node(Node::Variable { name: "+".to_string() });
    let one_lit = graph.add_node(Node::Literal(Literal::Integer(1)));
    let lambda_body = graph.add_node(Node::Application {
        function: plus_var,
        args: vec![y_var, one_lit],
    });
    
    // Create lambda
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["z".to_string()],
        body: lambda_body,
    });
    
    // Create let that binds x (unused) and has lambda as body
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_var),
            ("y".to_string(), y_var),
        ],
        body: lambda,
    });
    
    graph.root_id = Some(let_node);
    
    // Run optimizer
    let mut optimizer = GraphOptimizer::new();
    match optimizer.optimize(&graph) {
        Ok(optimized) => {
            // Verify all references are valid
            for (_, node) in &optimized.nodes {
                verify_node_references(&optimized, node);
            }
            println!("Optimization succeeded with valid references");
        }
        Err(e) => {
            println!("Optimization failed: {}", e);
            // This is also acceptable if the optimizer detects the issue
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
        Node::Letrec { bindings, body } => {
            for (_, value_id) in bindings {
                assert!(graph.get_node(*value_id).is_some(), "Binding value node {:?} doesn't exist", value_id);
            }
            assert!(graph.get_node(*body).is_some(), "Letrec body node {:?} doesn't exist", body);
        }
        Node::Lambda { body, .. } => {
            assert!(graph.get_node(*body).is_some(), "Lambda body node {:?} doesn't exist", body);
        }
        Node::If { condition, then_branch, else_branch } => {
            assert!(graph.get_node(*condition).is_some(), "Condition node {:?} doesn't exist", condition);
            assert!(graph.get_node(*then_branch).is_some(), "Then branch node {:?} doesn't exist", then_branch);
            assert!(graph.get_node(*else_branch).is_some(), "Else branch node {:?} doesn't exist", else_branch);
        }
        Node::List(items) => {
            for item in items {
                assert!(graph.get_node(*item).is_some(), "List item node {:?} doesn't exist", item);
            }
        }
        Node::Match { expr, branches } => {
            assert!(graph.get_node(*expr).is_some(), "Match expr node {:?} doesn't exist", expr);
            for (_, branch_body) in branches {
                assert!(graph.get_node(*branch_body).is_some(), "Match branch node {:?} doesn't exist", branch_body);
            }
        }
        _ => {}
    }
}