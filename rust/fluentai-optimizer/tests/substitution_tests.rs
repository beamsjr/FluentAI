//! Tests specifically for substitution operations and node ID mapping

use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::*;
use fluentai_parser::parse_flc;
use rustc_hash::FxHashMap;
use std::num::NonZeroU32;

#[test]
fn test_basic_substitution() {
    // Test basic variable substitution
    let code = "((lambda (x) (+ x x)) 5)";
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        beta_reduction: true,
        inline: true,
        ..OptimizationConfig::for_level(OptimizationLevel::Standard)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Should reduce to (+ 5 5)
    verify_all_references_valid(&optimized);
}

#[test]
fn test_nested_substitution() {
    // Test substitution in nested expressions
    let code = "((lambda (x) ((lambda (y) (+ x y)) x)) 3)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_substitution_with_shadowing() {
    // Test that substitution respects variable shadowing
    let code = "((lambda (x) ((lambda (x) x) 10)) 5)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
    // Inner lambda should return 10, not 5
}

#[test]
fn test_substitution_in_let() {
    // Test substitution within let bindings
    let code = "((lambda (x) (let ((y x)) (+ y x))) 7)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_substitution_preserves_application_args() {
    // Test that substitution properly handles function applications
    let code = "((lambda (f x) (f x x)) + 5)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_node_id_mapping_after_substitution() {
    // Test that node IDs are properly mapped after substitution
    let code = r#"
        (let ((f (lambda (x) (* x 2))))
          ((lambda (g) (+ (g 3) (g 4))) f))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // All nodes should have valid IDs
    assert!(optimized.root_id.is_some());
    verify_all_references_valid(&optimized);

    // No orphaned nodes
    let reachable = count_all_reachable(&optimized);
    assert_eq!(reachable, optimized.nodes.len());
}

#[test]
fn test_deep_copy_with_substitution() {
    // Test the deep_copy_with_substitution method indirectly
    let code = r#"
        ((lambda (x y) 
          (let ((a (+ x y))
                (b (* x y)))
            (- a b)))
         10 5)
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_multiple_substitutions() {
    // Test multiple simultaneous substitutions
    let code = "((lambda (x y z) (+ x (+ y z))) 1 2 3)";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_substitution_in_recursive_functions() {
    // Test substitution doesn't break recursive functions
    let code = r#"
        (letrec ((fact (lambda (n)
                         (if (= n 0)
                             1
                             (* n (fact (- n 1)))))))
          ((lambda (f) (f 5)) fact))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

#[test]
fn test_substitution_edge_case_self_application() {
    // Test edge case: ((lambda (x) (x x)) (lambda (y) y))
    let code = "((lambda (x) (x x)) (lambda (y) y))";
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    verify_all_references_valid(&optimized);
}

// Helper functions

fn verify_all_references_valid(graph: &Graph) {
    for (id, node) in &graph.nodes {
        verify_node_references_recursive(graph, *id, node);
    }

    // Verify root is valid
    if let Some(root) = graph.root_id {
        assert!(graph.get_node(root).is_some(), "Invalid root reference");
    }
}

fn verify_node_references_recursive(graph: &Graph, node_id: NodeId, node: &Node) {
    match node {
        Node::Application { function, args } => {
            assert!(
                graph.get_node(*function).is_some(),
                "Node {:?} has dangling function reference {:?}",
                node_id,
                function
            );
            for arg in args {
                assert!(
                    graph.get_node(*arg).is_some(),
                    "Node {:?} has dangling arg reference {:?}",
                    node_id,
                    arg
                );
            }
        }
        Node::Lambda { body, .. } => {
            assert!(
                graph.get_node(*body).is_some(),
                "Node {:?} has dangling body reference {:?}",
                node_id,
                body
            );
        }
        Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
            for (name, value_id) in bindings {
                assert!(
                    graph.get_node(*value_id).is_some(),
                    "Node {:?} has dangling binding '{}' reference {:?}",
                    node_id,
                    name,
                    value_id
                );
            }
            assert!(
                graph.get_node(*body).is_some(),
                "Node {:?} has dangling body reference {:?}",
                node_id,
                body
            );
        }
        Node::If {
            condition,
            then_branch,
            else_branch,
        } => {
            assert!(
                graph.get_node(*condition).is_some(),
                "Node {:?} has dangling condition reference {:?}",
                node_id,
                condition
            );
            assert!(
                graph.get_node(*then_branch).is_some(),
                "Node {:?} has dangling then reference {:?}",
                node_id,
                then_branch
            );
            assert!(
                graph.get_node(*else_branch).is_some(),
                "Node {:?} has dangling else reference {:?}",
                node_id,
                else_branch
            );
        }
        Node::List(items) => {
            for item in items {
                assert!(
                    graph.get_node(*item).is_some(),
                    "Node {:?} has dangling list item reference {:?}",
                    node_id,
                    item
                );
            }
        }
        Node::Match { expr, branches } => {
            assert!(
                graph.get_node(*expr).is_some(),
                "Node {:?} has dangling expr reference {:?}",
                node_id,
                expr
            );
            for (_, branch) in branches {
                assert!(
                    graph.get_node(*branch).is_some(),
                    "Node {:?} has dangling branch reference {:?}",
                    node_id,
                    branch
                );
            }
        }
        Node::Module { body, .. } => {
            assert!(
                graph.get_node(*body).is_some(),
                "Node {:?} has dangling module body reference {:?}",
                node_id,
                body
            );
        }
        Node::Async { body } => {
            assert!(
                graph.get_node(*body).is_some(),
                "Node {:?} has dangling async body reference {:?}",
                node_id,
                body
            );
        }
        Node::Await { expr } => {
            assert!(
                graph.get_node(*expr).is_some(),
                "Node {:?} has dangling await expr reference {:?}",
                node_id,
                expr
            );
        }
        Node::Spawn { expr } => {
            assert!(
                graph.get_node(*expr).is_some(),
                "Node {:?} has dangling spawn expr reference {:?}",
                node_id,
                expr
            );
        }
        Node::Send { channel, value } => {
            assert!(
                graph.get_node(*channel).is_some(),
                "Node {:?} has dangling channel reference {:?}",
                node_id,
                channel
            );
            assert!(
                graph.get_node(*value).is_some(),
                "Node {:?} has dangling value reference {:?}",
                node_id,
                value
            );
        }
        Node::Receive { channel } => {
            assert!(
                graph.get_node(*channel).is_some(),
                "Node {:?} has dangling channel reference {:?}",
                node_id,
                channel
            );
        }
        Node::Contract {
            preconditions,
            postconditions,
            invariants,
            ..
        } => {
            for pre in preconditions {
                assert!(
                    graph.get_node(*pre).is_some(),
                    "Node {:?} has dangling precondition reference {:?}",
                    node_id,
                    pre
                );
            }
            for post in postconditions {
                assert!(
                    graph.get_node(*post).is_some(),
                    "Node {:?} has dangling postcondition reference {:?}",
                    node_id,
                    post
                );
            }
            for inv in invariants {
                assert!(
                    graph.get_node(*inv).is_some(),
                    "Node {:?} has dangling invariant reference {:?}",
                    node_id,
                    inv
                );
            }
        }
        Node::Effect { args, .. } => {
            for arg in args {
                assert!(
                    graph.get_node(*arg).is_some(),
                    "Node {:?} has dangling effect arg reference {:?}",
                    node_id,
                    arg
                );
            }
        }
        _ => {} // Literals, Variables, etc. have no references
    }
}

fn count_all_reachable(graph: &Graph) -> usize {
    use rustc_hash::FxHashSet;
    let mut visited = FxHashSet::default();

    if let Some(root) = graph.root_id {
        let mut stack = vec![root];

        while let Some(node_id) = stack.pop() {
            if !visited.insert(node_id) {
                continue;
            }

            if let Some(node) = graph.get_node(node_id) {
                collect_node_references(node, &mut stack);
            }
        }
    }

    visited.len()
}

fn collect_node_references(node: &Node, stack: &mut Vec<NodeId>) {
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
        Node::If {
            condition,
            then_branch,
            else_branch,
        } => {
            stack.push(*condition);
            stack.push(*then_branch);
            stack.push(*else_branch);
        }
        Node::List(items) => stack.extend(items),
        Node::Match { expr, branches } => {
            stack.push(*expr);
            for (_, branch) in branches {
                stack.push(*branch);
            }
        }
        Node::Module { body, .. } => stack.push(*body),
        Node::Async { body } => stack.push(*body),
        Node::Await { expr } => stack.push(*expr),
        Node::Spawn { expr } => stack.push(*expr),
        Node::Send { channel, value } => {
            stack.push(*channel);
            stack.push(*value);
        }
        Node::Receive { channel } => stack.push(*channel),
        Node::Contract {
            preconditions,
            postconditions,
            invariants,
            ..
        } => {
            stack.extend(preconditions);
            stack.extend(postconditions);
            stack.extend(invariants);
        }
        Node::Effect { args, .. } => stack.extend(args),
        _ => {}
    }
}
