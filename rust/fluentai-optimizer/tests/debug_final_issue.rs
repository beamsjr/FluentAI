use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse_flc;
use std::num::NonZeroU32;

#[test]
fn debug_final_issue() {
    // Minimal test case
    let code = "{ let x = 5; x }";
    let ast = parse_flc(code).unwrap();

    println!("=== Original AST ===");
    let mut node_list: Vec<_> = ast.nodes.iter().collect();
    node_list.sort_by_key(|(id, _)| id.0);
    for (id, node) in node_list {
        println!("{:?}: {:?}", id, node);
    }

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    println!("\n=== Optimized AST ===");
    let mut opt_list: Vec<_> = optimized.nodes.iter().collect();
    opt_list.sort_by_key(|(id, _)| id.0);
    for (id, node) in opt_list {
        println!("{:?}: {:?}", id, node);
    }

    // Check all NodeIds from 1 to 10
    println!("\n=== NodeId Existence Check ===");
    for i in 1..=10 {
        if let Some(n) = NonZeroU32::new(i) {
            let node_id = NodeId(n);
            let exists = optimized.nodes.contains_key(&node_id);
            if exists {
                println!("NodeId({}) exists: {:?}", i, optimized.get_node(node_id));
            } else if i <= 5 {
                println!("NodeId({}) MISSING", i);
            }
        }
    }

    // The real test - can we validate all references?
    println!("\n=== Reference Validation ===");
    let mut all_valid = true;
    for (id, node) in &optimized.nodes {
        match node {
            Node::Let { bindings, body } => {
                for (name, value_id) in bindings {
                    if optimized.get_node(*value_id).is_none() {
                        println!(
                            "ERROR: Let at {:?} has binding '{}' -> {:?} which doesn't exist!",
                            id, name, value_id
                        );
                        all_valid = false;
                    }
                }
                if optimized.get_node(*body).is_none() {
                    println!(
                        "ERROR: Let at {:?} has body -> {:?} which doesn't exist!",
                        id, body
                    );
                    all_valid = false;
                }
            }
            _ => {}
        }
    }

    assert!(all_valid, "Found invalid node references!");
}
