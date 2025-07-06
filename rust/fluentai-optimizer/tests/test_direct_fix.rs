use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;
use fluentai_core::ast::{Graph, Node, NodeId};

#[test] 
fn test_direct_fix() {
    // Test the exact failing case
    let code = "(let ((x 5) (y (+ x 2))) (* y 3))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Validate all node references
    let mut errors = Vec::new();
    for (id, node) in &optimized.nodes {
        match node {
            Node::Let { bindings, body } => {
                for (name, value_id) in bindings {
                    if optimized.get_node(*value_id).is_none() {
                        errors.push(format!("Let at {:?}: binding '{}' -> {:?} (doesn't exist)", id, name, value_id));
                    }
                }
                if optimized.get_node(*body).is_none() {
                    errors.push(format!("Let at {:?}: body -> {:?} (doesn't exist)", id, body));
                }
            }
            Node::Application { function, args } => {
                if optimized.get_node(*function).is_none() {
                    errors.push(format!("App at {:?}: function -> {:?} (doesn't exist)", id, function));
                }
                for arg in args {
                    if optimized.get_node(*arg).is_none() {
                        errors.push(format!("App at {:?}: arg -> {:?} (doesn't exist)", id, arg));
                    }
                }
            }
            Node::Lambda { body, .. } => {
                if optimized.get_node(*body).is_none() {
                    errors.push(format!("Lambda at {:?}: body -> {:?} (doesn't exist)", id, body));
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                if optimized.get_node(*condition).is_none() {
                    errors.push(format!("If at {:?}: condition -> {:?} (doesn't exist)", id, condition));
                }
                if optimized.get_node(*then_branch).is_none() {
                    errors.push(format!("If at {:?}: then -> {:?} (doesn't exist)", id, then_branch));
                }
                if optimized.get_node(*else_branch).is_none() {
                    errors.push(format!("If at {:?}: else -> {:?} (doesn't exist)", id, else_branch));
                }
            }
            Node::Letrec { bindings, body } => {
                for (name, value_id) in bindings {
                    if optimized.get_node(*value_id).is_none() {
                        errors.push(format!("Letrec at {:?}: binding '{}' -> {:?} (doesn't exist)", id, name, value_id));
                    }
                }
                if optimized.get_node(*body).is_none() {
                    errors.push(format!("Letrec at {:?}: body -> {:?} (doesn't exist)", id, body));
                }
            }
            _ => {}
        }
    }
    
    if !errors.is_empty() {
        println!("Found {} invalid references:", errors.len());
        for error in &errors {
            println!("  {}", error);
        }
        panic!("Invalid node references found");
    }
}