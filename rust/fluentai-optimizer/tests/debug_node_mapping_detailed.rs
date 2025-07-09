use fluentai_core::ast::{Node, NodeId};
use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;

#[test]
fn debug_node_mapping_detailed() {
    let code = "(let ((x 5)) x)"; // Simpler case
    let ast = parse(code).unwrap();

    println!("=== Original AST ===");
    for (id, node) in &ast.nodes {
        println!("{:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", ast.root_id);

    // Create a custom optimizer to trace the issue
    let mut optimizer = AdvancedOptimizer::new();

    // Hack: use reflection or direct field access to get internal state
    // Since we can't, let's just optimize and check
    let optimized = optimizer.optimize(&ast).unwrap();

    println!("=== Optimized AST ===");
    for (id, node) in &optimized.nodes {
        println!("{:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", optimized.root_id);

    // Find the Let node and check its bindings
    for (id, node) in &optimized.nodes {
        if let Node::Let { bindings, body } = node {
            println!("Found Let node at {:?}", id);
            for (name, value_id) in bindings {
                println!("  Binding '{}' -> {:?}", name, value_id);
                match optimized.get_node(*value_id) {
                    Some(n) => println!("    Points to: {:?}", n),
                    None => println!("    ERROR: Points to non-existent node!"),
                }
            }
            println!("  Body -> {:?}", body);
            match optimized.get_node(*body) {
                Some(n) => println!("    Points to: {:?}", n),
                None => println!("    ERROR: Body points to non-existent node!"),
            }
        }
    }
}
