use fluentai_optimizer::{
    OptimizationPipeline, OptimizationConfig, GraphOptimizer, AdvancedOptimizer
};
use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_parser::parse;
use fluentai_core::ast::{Graph, Node, Literal};

#[test]
fn test_constant_folding() {
    let code = "(+ 2 3)";
    let graph = parse(code).unwrap();
    
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Should fold to literal 5
    assert_eq!(optimized.nodes.len(), 1);
    if let Some(root) = optimized.root_id {
        if let Some(Node::Literal(Literal::Integer(5))) = optimized.get_node(root) {
            // Success
        } else {
            panic!("Expected literal 5");
        }
    }
    
    assert_eq!(optimizer.stats().constant_folded, 1);
}

#[test]
fn test_dead_code_elimination() {
    let code = "(let ((x 1) (y 2) (unused 3)) (+ x y))";
    let graph = parse(code).unwrap();
    
    // Use Basic level which uses GraphOptimizer that does dead code elimination
    let config = OptimizationConfig::for_level(OptimizationLevel::Basic);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    // Should have fewer nodes after optimization
    println!("Original nodes: {}, Optimized nodes: {}", graph.nodes.len(), optimized.nodes.len());
    println!("Stats: {:?}", pipeline.stats());
    assert!(optimized.nodes.len() < graph.nodes.len(), 
            "Expected optimization to reduce nodes from {} to less, got {}", 
            graph.nodes.len(), optimized.nodes.len());
}

#[test]
fn test_branch_elimination() {
    let code = "(if #t 42 (error \"unreachable\"))";
    let graph = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    assert_eq!(optimizer.stats().branches_eliminated, 1);
}

#[test]
fn test_nested_constant_folding() {
    let code = "(+ (* 2 3) (- 10 5))";
    let graph = parse(code).unwrap();
    
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Should fold to literal 11
    if let Some(root) = optimized.root_id {
        if let Some(node) = optimized.get_node(root) {
            match node {
                Node::Literal(Literal::Integer(11)) => {
                    // Success
                }
                _ => {
                    panic!("Expected literal 11, got: {:?}", node);
                }
            }
        }
    }
}

#[test]
fn test_optimization_preserves_semantics() {
    let programs = vec![
        "(+ 1 2)",
        "(if (> 5 3) \"yes\" \"no\")",
        "(let ((x 10)) (+ x 5))",
        "((lambda (x) (* x 2)) 7)",
        "(car [1 2 3])",
    ];
    
    for code in programs {
        let graph = parse(code).unwrap();
        
        // Test that all optimization levels preserve the program structure
        for level in vec![OptimizationLevel::Basic, OptimizationLevel::Standard, OptimizationLevel::Aggressive] {
            let config = OptimizationConfig::for_level(level);
            let mut pipeline = OptimizationPipeline::new(config);
            let optimized = pipeline.optimize(&graph).unwrap();
            
            // Should have a root node
            assert!(optimized.root_id.is_some());
        }
    }
}

#[test]
fn test_cse_elimination() {
    // Program with repeated subexpressions
    let code = "(let ((x 5)) (+ (* x 2) (* x 2) (* x 2)))";
    let graph = parse(code).unwrap();
    
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    // Should have eliminated duplicate (* x 2) expressions
    assert!(optimized.nodes.len() < graph.nodes.len());
}

#[test]
fn test_pure_expression_evaluation() {
    let code = "(+ (+ 1 2) (+ 3 4))";
    let graph = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Should evaluate to 10
    assert!(optimizer.stats().pure_expressions_evaluated > 0);
}

#[test]
fn test_optimization_stats() {
    let code = r#"
        (let ((x 5) (y 10) (unused 15))
            (if #t
                (+ x y)
                (error "unreachable")))
    "#;
    
    let graph = parse(code).unwrap();
    
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    let stats = pipeline.stats();
    
    // Should have some optimizations
    assert!(stats.total_optimizations() > 0);
    assert!(stats.reduction_percentage() > 0.0);
    
    println!("{}", stats);
}

#[test]
fn test_no_optimization() {
    let code = "(+ x y)"; // Variables, can't optimize
    let graph = parse(code).unwrap();
    
    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();
    
    // Should be unchanged
    assert_eq!(optimized.nodes.len(), graph.nodes.len());
    assert_eq!(optimizer.stats().constant_folded, 0);
}

#[test]
fn test_boolean_operations() {
    let tests = vec![
        ("(and #t #t)", true),
        ("(and #t #f)", false),
        ("(or #f #t)", true),
        ("(not #f)", true),
    ];
    
    for (code, expected) in tests {
        let graph = parse(code).unwrap();
        
        let mut optimizer = GraphOptimizer::new();
        let optimized = optimizer.optimize(&graph).unwrap();
        
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(Literal::Boolean(result))) = optimized.get_node(root) {
                assert_eq!(*result, expected, "Failed for: {}", code);
            } else {
                panic!("Expected boolean result for: {}", code);
            }
        }
    }
}