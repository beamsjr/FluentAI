use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_optimizer::GraphOptimizer;
use fluentai_parser::parse_flc;

#[test]
fn test_define_optimization() {
    // Test simple define
    let code = "(define x 42)";
    let graph = parse_flc(code).unwrap();
    
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Should preserve the define node and its value
    assert_eq!(optimized.nodes.len(), 2); // Define node + literal value
    assert!(optimized.root_id.is_some());
    
    // Test define with expression
    let code2 = "(define y (+ 2 3))";
    let graph2 = parse_flc(code2).unwrap();
    
    let mut optimizer2 = GraphOptimizer::new();
    let optimized2 = optimizer2.optimize(&graph2).unwrap();
    
    // Should fold the expression to 5
    assert!(optimizer2.stats().constant_folded > 0);
    
    // Test define with effects
    let code3 = r#"(define z (effect io:print "hello"))"#;
    let graph3 = parse_flc(code3).unwrap();
    
    let mut optimizer3 = GraphOptimizer::new();
    let optimized3 = optimizer3.optimize(&graph3).unwrap();
    
    // Should preserve the effect
    assert!(optimized3.nodes.len() >= 2); // At least Define + effect
}

#[test]
fn test_define_no_stack_overflow() {
    // This is the specific case that was causing stack overflow
    let code = "(define x 42)";
    let graph = parse_flc(code).unwrap();
    
    // This should not cause stack overflow
    let mut optimizer = GraphOptimizer::new();
    let result = optimizer.optimize(&graph);
    
    assert!(result.is_ok(), "Optimization should not fail");
    let optimized = result.unwrap();
    
    // Verify the structure is preserved
    assert!(optimized.root_id.is_some());
    if let Some(root_id) = optimized.root_id {
        if let Some(node) = optimized.get_node(root_id) {
            match node {
                Node::Define { name, value } => {
                    assert_eq!(name, "x");
                    // The value should point to a literal 42
                    if let Some(Node::Literal(Literal::Integer(42))) = optimized.get_node(*value) {
                        // Success
                    } else {
                        panic!("Expected literal 42 as value");
                    }
                }
                _ => panic!("Expected Define node as root"),
            }
        }
    }
}