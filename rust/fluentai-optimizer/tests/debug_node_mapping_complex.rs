use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;
use fluentai_core::ast::{Node, NodeId};

#[test]
fn debug_node_mapping_complex() {
    // The exact test case that's failing
    let code = "(let ((x 5) (y (+ x 2))) (* y 3))";
    let ast = parse(code).unwrap();
    
    println!("=== Original AST ===");
    for (id, node) in &ast.nodes {
        println!("{:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", ast.root_id);
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    println!("=== Optimized AST ===");
    for (id, node) in &optimized.nodes {
        println!("{:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", optimized.root_id);
    
    // Check all node references
    for (id, node) in &optimized.nodes {
        match node {
            Node::Let { bindings, body } => {
                println!("\nValidating Let node at {:?}", id);
                for (name, value_id) in bindings {
                    print!("  Binding '{}' -> {:?}", name, value_id);
                    match optimized.get_node(*value_id) {
                        Some(n) => println!(" ✓ (points to {:?})", n),
                        None => println!(" ✗ ERROR: Points to non-existent node!"),
                    }
                }
                print!("  Body -> {:?}", body);
                match optimized.get_node(*body) {
                    Some(n) => println!(" ✓ (points to {:?})", n),
                    None => println!(" ✗ ERROR: Body points to non-existent node!"),
                }
            }
            Node::Application { function, args } => {
                println!("\nValidating Application node at {:?}", id);
                print!("  Function -> {:?}", function);
                match optimized.get_node(*function) {
                    Some(n) => println!(" ✓ (points to {:?})", n),
                    None => println!(" ✗ ERROR: Function points to non-existent node!"),
                }
                for (i, arg) in args.iter().enumerate() {
                    print!("  Arg[{}] -> {:?}", i, arg);
                    match optimized.get_node(*arg) {
                        Some(n) => println!(" ✓ (points to {:?})", n),
                        None => println!(" ✗ ERROR: Arg points to non-existent node!"),
                    }
                }
            }
            _ => {}
        }
    }
}