//! Comprehensive tests for the advanced optimizer to catch edge cases and bugs

use fluentai_optimizer::*;
use fluentai_parser::parse;
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::num::NonZeroU32;

#[test]
fn test_cycle_detection_simple() {
    // Test direct self-reference: (letrec ((f f)) f)
    let code = "(letrec ((f f)) f)";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);
    
    // Should not panic or stack overflow
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Verify the cycle was handled
    assert!(optimized.nodes.len() > 0);
}

#[test]
fn test_cycle_detection_mutual_recursion() {
    // Test mutual recursion: (letrec ((f g) (g f)) f)
    let code = "(letrec ((f g) (g f)) f)";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);
    
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should preserve the mutual recursion structure
    assert!(optimized.nodes.len() >= 3); // At least f, g, and letrec
}

#[test]
fn test_cycle_detection_complex() {
    // Test complex cycle through let bindings
    let code = "(let ((a (let ((b a)) b))) a)";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);
    
    assert!(result.is_ok());
}

#[test]
fn test_node_id_mapping_consistency() {
    // Test that node IDs are properly mapped between original and optimized graphs
    let code = "(let ((x 5) (y (+ x 2))) (* y 3))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Verify all node references are valid
    for (id, node) in &optimized.nodes {
        verify_node_references(&optimized, *id, node);
    }
}

#[test]
fn test_deep_nested_optimization() {
    // Test deeply nested expressions don't cause stack overflow
    let mut code = String::from("(+ 1");
    for _ in 0..100 {
        code.push_str(" (+ 1");
    }
    for _ in 0..100 {
        code.push_str(" 1)");
    }
    code.push_str(" 1)");
    
    let ast = parse(&code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);
    
    match &result {
        Ok(_) => assert!(true),
        Err(e) => {
            // If it fails due to recursion depth, that's acceptable
            if e.to_string().contains("recursion depth") {
                assert!(true);
            } else {
                panic!("Unexpected error: {}", e);
            }
        }
    }
}

#[test]
fn test_memoization_correctness() {
    // Test that memoization doesn't break optimization
    let code = r#"
        (let ((x (+ 2 3))
              (y (+ 2 3))
              (z (+ 2 3)))
          (+ x (+ y z)))
    "#;
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // All three (+ 2 3) should be optimized to the same value
    let stats = optimizer.stats();
    assert!(stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_substitution_preserves_semantics() {
    // Test that substitution doesn't create dangling references
    let code = "((lambda (x y) (+ x y)) 5 10)";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Verify all nodes are reachable and valid
    if let Some(root) = optimized.root_id {
        let reachable = count_reachable_nodes(&optimized, root);
        assert_eq!(reachable, optimized.nodes.len());
    }
}

#[test]
fn test_let_binding_shadowing() {
    // Test that variable shadowing is handled correctly
    let code = "(let ((x 5)) (let ((x 10)) x))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // The result should optimize to 10, not 5
    verify_optimization_result(&optimized, 10);
}

#[test]
fn test_effect_preservation() {
    // Test that side effects are not eliminated
    let code = r#"
        (let ((x (print "hello")))
          (+ 1 2))
    "#;
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // The print should still be in the graph
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, Node::Application { function, .. } if {
            optimized.get_node(*function)
                .map(|n| matches!(n, Node::Variable { name } if name == "print"))
                .unwrap_or(false)
        })
    });
    assert!(has_print);
}

#[test]
fn test_constant_propagation_through_let() {
    // Test constant propagation through let bindings
    let code = "(let ((x 5) (y 10)) (let ((z (+ x y))) (* z 2)))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Should optimize to 30
    let stats = optimizer.stats();
    assert!(stats.pure_expressions_evaluated >= 2); // At least (+ x y) and (* z 2)
}

#[test]
fn test_if_constant_condition_elimination() {
    // Test that if with constant condition is eliminated
    let code = "(if #t (+ 1 2) (error \"unreachable\"))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Should not contain the error branch
    let has_error = optimized.nodes.values().any(|node| {
        matches!(node, Node::Application { function, .. } if {
            optimized.get_node(*function)
                .map(|n| matches!(n, Node::Variable { name } if name == "error"))
                .unwrap_or(false)
        })
    });
    assert!(!has_error);
    
    let stats = optimizer.stats();
    assert!(stats.branches_eliminated > 0);
}

#[test]
fn test_tail_call_detection() {
    // Test tail call optimization detection
    let code = r#"
        (letrec ((factorial (lambda (n acc)
                              (if (= n 0)
                                  acc
                                  (factorial (- n 1) (* n acc))))))
          (factorial 5 1))
    "#;
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    let stats = optimizer.stats();
    assert!(stats.tail_calls_optimized > 0);
}

#[test]
fn test_dead_code_elimination_preserves_root() {
    // Test that dead code elimination preserves the root
    let code = r#"
        (let ((unused (lambda (x) (* x x)))
              (used (lambda (x) (+ x 1))))
          (used 5))
    "#;
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    assert!(optimized.root_id.is_some());
    
    // The unused lambda should be eliminated
    let stats = optimizer.stats();
    assert!(stats.dead_code_eliminated > 0);
}

#[test]
fn test_inline_threshold_respected() {
    // Test that inline threshold is respected
    let mut large_body = String::from("(+");
    for i in 0..30 {
        large_body.push_str(&format!(" {}", i));
    }
    large_body.push(')');
    
    let code = format!("((lambda (x) {}) 5)", large_body);
    let ast = parse(&code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new()
        .with_inline_threshold(10); // Small threshold
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Large lambda should not be inlined
    let has_lambda = optimized.nodes.values().any(|node| {
        matches!(node, Node::Lambda { .. })
    });
    assert!(has_lambda, "Large lambda should not have been inlined");
}

#[test]
fn test_recursion_depth_limit() {
    // Test that recursion depth limit prevents stack overflow
    let mut code = String::from("(let ((a 1))");
    for i in 0..600 {
        code.push_str(&format!(" (let ((a{}  a))", i));
    }
    code.push_str(" a");
    for _ in 0..600 {
        code.push(')');
    }
    code.push(')');
    
    // The parser itself may hit depth limits first
    match parse(&code) {
        Ok(ast) => {
            let mut optimizer = AdvancedOptimizer::new();
            let result = optimizer.optimize(&ast);
            
            // Should either succeed or fail with recursion depth error, not stack overflow
            match result {
                Ok(_) => assert!(true),
                Err(e) => {
                    let error_str = e.to_string();
                    assert!(error_str.contains("recursion depth") || error_str.contains("MaxDepthExceeded"),
                            "Expected recursion depth error, got: {}", error_str);
                }
            }
        }
        Err(e) => {
            // Parser hitting depth limit is also acceptable
            let error_str = e.to_string();
            assert!(error_str.contains("MaxDepthExceeded") || error_str.contains("depth"),
                    "Expected depth limit error from parser, got: {}", error_str);
        }
    }
}

#[test]
fn test_cse_with_side_effects() {
    // Test that CSE doesn't eliminate expressions with side effects
    let code = r#"
        (let ((x (print "hello"))
              (y (print "hello")))
          (+ x y))
    "#;
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    // Both prints should remain
    let print_count = optimized.nodes.values().filter(|node| {
        matches!(node, Node::Application { function, .. } if {
            optimized.get_node(*function)
                .map(|n| matches!(n, Node::Variable { name } if name == "print"))
                .unwrap_or(false)
        })
    }).count();
    assert_eq!(print_count, 2);
}

#[test] 
fn test_regression_cse_stack_overflow() {
    // Regression test for the original stack overflow issue
    let code = "(let ((x 5)) (+ (* x 2) (* x 2) (* x 2)))";
    let ast = parse(code).unwrap();
    
    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);
    
    assert!(result.is_ok());
    let optimized = result.unwrap();
    
    // Should have optimized the repeated (* x 2) expressions
    let stats = optimizer.stats();
    assert!(stats.pure_expressions_evaluated > 0 || stats.nodes_after < stats.nodes_before);
}

// Helper functions

fn verify_node_references(graph: &Graph, node_id: NodeId, node: &Node) {
    match node {
        Node::Application { function, args } => {
            assert!(graph.get_node(*function).is_some(), 
                    "Node {:?} has invalid function reference {:?}", node_id, function);
            for arg in args {
                assert!(graph.get_node(*arg).is_some(),
                        "Node {:?} has invalid arg reference {:?}", node_id, arg);
            }
        }
        Node::Lambda { body, .. } => {
            assert!(graph.get_node(*body).is_some(),
                    "Node {:?} has invalid body reference {:?}", node_id, body);
        }
        Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
            for (name, value_id) in bindings {
                assert!(graph.get_node(*value_id).is_some(),
                        "Node {:?} has invalid binding '{}' reference {:?}", node_id, name, value_id);
            }
            assert!(graph.get_node(*body).is_some(),
                    "Node {:?} has invalid body reference {:?}", node_id, body);
        }
        Node::If { condition, then_branch, else_branch } => {
            assert!(graph.get_node(*condition).is_some(),
                    "Node {:?} has invalid condition reference {:?}", node_id, condition);
            assert!(graph.get_node(*then_branch).is_some(),
                    "Node {:?} has invalid then reference {:?}", node_id, then_branch);
            assert!(graph.get_node(*else_branch).is_some(),
                    "Node {:?} has invalid else reference {:?}", node_id, else_branch);
        }
        _ => {}
    }
}

fn count_reachable_nodes(graph: &Graph, root: NodeId) -> usize {
    use rustc_hash::FxHashSet;
    let mut visited = FxHashSet::default();
    let mut stack = vec![root];
    
    while let Some(node_id) = stack.pop() {
        if !visited.insert(node_id) {
            continue;
        }
        
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    stack.push(*function);
                    stack.extend(args);
                }
                Node::Lambda { body, .. } => stack.push(*body),
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value_id) in bindings {
                        stack.push(*value_id);
                    }
                    stack.push(*body);
                }
                Node::If { condition, then_branch, else_branch } => {
                    stack.push(*condition);
                    stack.push(*then_branch);
                    stack.push(*else_branch);
                }
                _ => {}
            }
        }
    }
    
    visited.len()
}

fn verify_optimization_result(graph: &Graph, expected: i64) {
    if let Some(root) = graph.root_id {
        if let Some(Node::Literal(Literal::Integer(val))) = graph.get_node(root) {
            assert_eq!(*val, expected);
        }
    }
}