use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;

#[test]
fn debug_node_id_mapping() {
    let code = "(let ((x 5) (y (+ x 2))) (* y 3))";
    let ast = parse(code).unwrap();
    
    println!("Original AST ({} nodes):", ast.nodes.len());
    for (id, node) in &ast.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", ast.root_id);
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    println!("Optimized AST ({} nodes):", optimized.nodes.len());
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", optimized.root_id);
    
    // Check all node references
    for (id, node) in &optimized.nodes {
        match node {
            fluentai_core::ast::Node::Let { bindings, body } => {
                println!("\nChecking Let node {:?}:", id);
                for (name, value_id) in bindings {
                    println!("  Binding '{}' -> {:?}", name, value_id);
                    if optimized.get_node(*value_id).is_none() {
                        println!("    ERROR: Node {:?} not found!", value_id);
                    }
                }
                println!("  Body -> {:?}", body);
                if optimized.get_node(*body).is_none() {
                    println!("    ERROR: Body node {:?} not found!", body);
                }
            }
            _ => {}
        }
    }
}