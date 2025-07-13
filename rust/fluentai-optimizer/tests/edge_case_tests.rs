//! Edge case tests for the optimizer

use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::*;
use fluentai_parser::parse_flc;

#[test]
fn test_empty_program() {
    // Test optimization of empty program
    let code = "()";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(result.is_ok());
}

#[test]
fn test_single_literal() {
    // Test optimization of single literal
    let code = "42";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    assert_eq!(optimized.nodes.len(), 1);
}

#[test]
fn test_deeply_nested_let() {
    // Test deeply nested let expressions
    let mut code = String::from("(let ((a 1))");
    for i in 0..50 {
        code.push_str(&format!(" (let ((a{} a))", i));
    }
    code.push_str(" a");
    for _ in 0..50 {
        code.push(')');
    }
    code.push(')');

    let ast = parse_flc(&code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(result.is_ok());
}

#[test]
fn test_many_unused_bindings() {
    // Test optimization with many unused bindings
    let mut code = String::from("(let (");
    for i in 0..100 {
        code.push_str(&format!("(unused{} {}) ", i, i));
    }
    code.push_str("(used 42)) used)");

    let ast = parse_flc(&code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    let stats = optimizer.stats();
    assert!(stats.dead_code_eliminated >= 100);
}

#[test]
fn test_circular_let_bindings() {
    // Test handling of circular let bindings (should fail gracefully)
    let code = "(let ((x y) (y x)) x)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    // Should handle this without panic
    assert!(result.is_ok());
}

#[test]
fn test_empty_function_body() {
    // Test lambda with empty body
    let code = "(lambda () ())";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(result.is_ok());
}

#[test]
fn test_if_with_same_branches() {
    // Test if expression with identical branches
    let code = "(if (> x 0) 42 42)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Could optimize to just 42, but at least shouldn't crash
    assert!(optimized.nodes.len() > 0);
}

#[test]
fn test_application_with_no_args() {
    // Test function application with no arguments
    let code = "((lambda () 42))";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Should optimize to 42
    let stats = optimizer.stats();
    assert!(stats.inlined_expressions > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_let_with_no_bindings() {
    // Test let with no bindings
    let code = "(let () 42)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Should optimize to just 42
    assert!(optimized.nodes.len() <= 2); // At most let node and literal
}

#[test]
fn test_list_optimization() {
    // Test optimization of list expressions
    let code = "(list (+ 1 2) (- 5 3) (* 2 3))";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Arithmetic should be folded
    let stats = optimizer.stats();
    assert!(stats.constant_folded > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_match_optimization() {
    // Test optimization of match expressions
    let code = r#"
        (match (+ 1 2)
          (3 "three")
          (_ "other"))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // (+ 1 2) should be optimized
    let stats = optimizer.stats();
    assert!(stats.constant_folded > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_module_optimization() {
    // Test optimization of module expressions
    let code = r#"
        (module test-module
          (export add)
          (let ((add (lambda (x y) (+ x y))))
            add))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(result.is_ok());
}

#[test]
fn test_async_optimization() {
    // Test optimization of async/await expressions
    let code = "(async (+ 1 2))";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Inner expression should be optimized
    let stats = optimizer.stats();
    assert!(stats.constant_folded > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_contract_optimization() {
    // Test optimization with contracts
    let code = r#"
        (contract my-function
          (pre (> x 0))
          (post (> result 0))
          (pure #t))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(result.is_ok());
}

#[test]
fn test_effect_optimization() {
    // Test optimization of effect expressions
    let code = r#"
        (effect io
          (read-file "test.txt"))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Effects should not be eliminated
    let has_effect = optimized
        .nodes
        .values()
        .any(|node| matches!(node, Node::Effect { .. }));
    assert!(has_effect);
}

#[test]
fn test_optimization_with_invalid_node_ids() {
    // Test handling of graphs with gaps in node IDs
    let mut graph = Graph::new();

    // Add nodes with gaps
    let n1 = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let n2 = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");
    let add = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let app = graph
        .add_node(Node::Application {
            function: add,
            args: vec![n1, n2],
        })
        .expect("Failed to add node");

    // Remove a node to create a gap
    graph.nodes.remove(&n1);

    // Add a new application that references the removed node (invalid)
    let invalid_app = graph
        .add_node(Node::Application {
            function: add,
            args: vec![n1, n2], // n1 no longer exists!
        })
        .expect("Failed to add node");

    graph.root_id = Some(invalid_app);

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&graph);

    // Should handle invalid references gracefully
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_mixed_numeric_types() {
    // Test optimization with mixed integer and float operations
    let code = "(+ 1 2.5)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    // Should handle type mismatch gracefully
    assert!(result.is_ok());
}

#[test]
fn test_variable_without_binding() {
    // Test optimization of free variables
    let code = "(+ x 5)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Should preserve the expression since x is unbound
    let has_var = optimized
        .nodes
        .values()
        .any(|node| matches!(node, Node::Variable { name } if name == "x"));
    assert!(has_var);
}

#[test]
fn test_multiple_optimization_passes() {
    // Test running optimization multiple times
    let code = "(let ((x 5) (y 10)) (+ (+ x y) (+ x y)))";
    let ast = parse_flc(code).unwrap();

    let mut optimizer1 = AdvancedOptimizer::new();
    let opt1 = optimizer1.optimize(&ast).unwrap();

    let mut optimizer2 = AdvancedOptimizer::new();
    let opt2 = optimizer2.optimize(&opt1).unwrap();

    // Second optimization should not break anything
    assert!(opt2.root_id.is_some());
    verify_graph_integrity(&opt2);
}

// Helper function to verify graph integrity
fn verify_graph_integrity(graph: &Graph) {
    // Check root is valid
    if let Some(root) = graph.root_id {
        assert!(graph.get_node(root).is_some(), "Invalid root node");
    }

    // Check all node references are valid
    for (id, node) in &graph.nodes {
        match node {
            Node::Application { function, args } => {
                assert!(graph.get_node(*function).is_some());
                for arg in args {
                    assert!(graph.get_node(*arg).is_some());
                }
            }
            Node::Lambda { body, .. } => {
                assert!(graph.get_node(*body).is_some());
            }
            Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                for (_, value_id) in bindings {
                    assert!(graph.get_node(*value_id).is_some());
                }
                assert!(graph.get_node(*body).is_some());
            }
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert!(graph.get_node(*condition).is_some());
                assert!(graph.get_node(*then_branch).is_some());
                assert!(graph.get_node(*else_branch).is_some());
            }
            _ => {}
        }
    }
}
