//! Tests to ensure optimizations preserve effects correctly

use fluentai_core::ast::{EffectType, Graph, Node};
use fluentai_optimizer::*;
use fluentai_parser::parse_flc;
use std::cell::RefCell;

thread_local! {
    static TEST_OUTPUT: RefCell<Vec<String>> = RefCell::new(Vec::new());
}

/// Mock print function that captures output for testing
fn mock_print(msg: &str) {
    TEST_OUTPUT.with(|output| {
        output.borrow_mut().push(msg.to_string());
    });
}

#[test]
fn test_preserve_io_effects() {
    // Test that IO effects are not eliminated
    let code = r#"(effect io:print "effect1")"#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Print statement should still exist
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 1, "Print operation should be preserved");
}

#[test]
fn test_preserve_effect_order() {
    // Test that effect order is preserved
    let code = r#"
        (let ((a (effect io:print "1"))
              (b (effect io:print "2"))
              (c (effect io:print "3")))
          42)
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // All three prints should exist
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 3, "All print operations should be preserved");
}

#[test]
fn test_dead_code_elimination_preserves_effects() {
    // Test that DCE doesn't remove effectful bindings
    let code = r#"
        (let ((unused-pure 42)
              (unused-effect (effect io:print "keep me"))
              (used 10))
          used)
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        dead_code_elimination: true,
        ..OptimizationConfig::for_level(OptimizationLevel::Standard)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // The print should still exist
    let print_count = count_operations(&optimized, "print");
    assert_eq!(
        print_count, 1,
        "Effect should be preserved despite binding being unused"
    );

    let stats = pipeline.stats();
    assert!(
        stats.dead_code_eliminated >= 1,
        "Pure unused binding should be eliminated"
    );
}

#[test]
fn test_cse_doesnt_merge_effects() {
    // Test that CSE doesn't merge effectful expressions
    let code = r#"
        (let ((a (effect io:print "effect1"))
              (b (effect io:print "effect1")))
          (list a b))
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        cse: true,
        ..OptimizationConfig::for_level(OptimizationLevel::Standard)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Both prints should remain
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 2, "CSE should not merge effectful operations");
}

#[test]
fn test_constant_folding_preserves_surrounding_effects() {
    // Test that constant folding doesn't eliminate surrounding effects
    let code = r#"
        (let ((x (effect io:print "before")))
          (let ((result (+ 2 3))
                (_ (effect io:print "after")))
            result))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Both prints should exist
    let print_count = count_operations(&optimized, "print");
    assert_eq!(
        print_count, 2,
        "Effects surrounding folded constants should be preserved"
    );
}

#[test]
fn test_inline_preserves_effects() {
    // Test that inlining preserves effects in the inlined function
    let code = r#"
        ((lambda (x) 
          (let ((_ (effect io:print x)))
            (+ x 1)))
         10)
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        inline: true,
        inline_threshold: 20,
        ..OptimizationConfig::for_level(OptimizationLevel::Standard)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Print should still exist after inlining
    let print_count = count_operations(&optimized, "print");
    assert_eq!(
        print_count, 1,
        "Effect in inlined function should be preserved"
    );
}

#[test]
fn test_if_elimination_preserves_condition_effects() {
    // Test that if elimination preserves effects in condition
    let code = r#"
        (if (let ((x (effect io:print "condition effect"))) #t)
            42
            99)
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Condition effect should be preserved
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 1, "Effect in if condition should be preserved");
}

#[test]
fn test_unused_branch_effects_eliminated() {
    // Test that effects in eliminated branches are removed
    let code = r#"
        (if #t
            42
            (effect io:print "unreachable"))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Unreachable print should be eliminated
    let print_count = count_operations(&optimized, "print");
    assert_eq!(
        print_count, 0,
        "Effect in unreachable branch should be eliminated"
    );

    let stats = optimizer.stats();
    assert!(stats.branches_eliminated > 0);
}

#[test]
fn test_effect_types_preserved() {
    // Test that different effect types are preserved
    let mut graph = Graph::new();

    // Create different effect nodes
    let io_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::IO,
            operation: "print".to_string(),
            args: vec![],
        })
        .expect("Failed to add IO effect node");

    let state_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::State,
            operation: "set".to_string(),
            args: vec![],
        })
        .expect("Failed to add State effect node");

    let time_effect = graph
        .add_node(Node::Effect {
            effect_type: EffectType::Time,
            operation: "now".to_string(),
            args: vec![],
        })
        .expect("Failed to add Time effect node");

    // Create a sequence
    let list = graph
        .add_node(Node::List(vec![io_effect, state_effect, time_effect]))
        .expect("Failed to add list node");
    graph.root_id = Some(list);

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();

    // All effects should be preserved
    let effect_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, Node::Effect { .. }))
        .count();

    assert_eq!(effect_count, 3, "All effect types should be preserved");
}

#[test]
fn test_concurrent_effects_preserved() {
    // Test that concurrent/async effects are preserved
    let code = r#"
        (let ((task (spawn (effect io:print "concurrent"))))
          (await task))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Debug: print all nodes
    println!("Optimized graph nodes:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }

    // Print within spawn should be preserved
    let print_count = count_operations(&optimized, "print");
    println!("Found {} print operations", print_count);
    assert_eq!(print_count, 1, "Effect in spawned task should be preserved");

    // Spawn and await nodes should exist
    let spawn_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, Node::Spawn { .. }))
        .count();

    let await_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, Node::Await { .. }))
        .count();

    assert!(spawn_count >= 1, "Spawn should be preserved");
    assert!(await_count >= 1, "Await should be preserved");
}

#[test]
fn test_effect_preservation_correctness() {
    // Test that optimization preserves program behavior with effects
    let code = r#"
        (lambda () 
          (effect state:set "counter" 1))
    "#;

    // This test would require a full VM with effect support
    // For now, just ensure the code parses and optimizes without error
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let result = optimizer.optimize(&ast);

    assert!(
        result.is_ok(),
        "Should optimize effectful code without error"
    );
}

#[test]
fn test_pure_subexpression_extraction() {
    // Test that pure subexpressions can be optimized within effectful contexts
    let code = r#"
        (let ((x (effect io:print "effect")))
          (+ (* 2 3) (* 4 5)))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Print should be preserved
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 1);

    // But arithmetic should be optimized
    let stats = optimizer.stats();
    assert!(stats.constant_folded > 0 || stats.pure_expressions_evaluated > 0);
}

#[test]
fn test_effect_binding_dependency() {
    // Test that bindings with effect dependencies are handled correctly
    let code = r#"
        (let ((x (effect io:print "first")))
          (let ((y (+ x 1))
                (_ (effect io:print "second")))
            y))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Both prints should exist
    let print_count = count_operations(&optimized, "print");
    assert_eq!(print_count, 2, "All effects should be preserved");
}

#[test]
fn test_effect_free_optimization() {
    // Test that effect-free code is aggressively optimized
    let code = r#"
        (let ((square (lambda (x) (* x x)))
              (a 3)
              (b 4))
          (+ (square a) (square b)))
    "#;
    let ast = parse_flc(code).unwrap();

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    // Debug output
    println!("Original AST nodes: {}", ast.nodes.len());
    println!("Optimized nodes:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }

    // Should have no effects
    let effect_count = optimized
        .nodes
        .values()
        .filter(|node| matches!(node, Node::Effect { .. }))
        .count();

    assert_eq!(effect_count, 0, "No effects should be present");

    // Should be heavily optimized
    let stats = optimizer.stats();
    println!(
        "Effect-free optimization: nodes_before={}, nodes_after={}",
        stats.nodes_before, stats.nodes_after
    );
    println!(
        "Stats: pure_expressions_evaluated={}, inlined_expressions={}",
        stats.pure_expressions_evaluated, stats.inlined_expressions
    );
    assert!(
        stats.nodes_after < stats.nodes_before,
        "Effect-free code should be optimized"
    );
}

// Helper function to count operations
fn count_operations(graph: &Graph, op_name: &str) -> usize {
    graph
        .nodes
        .values()
        .filter(|node| match node {
            Node::Application { function, .. } => {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    name == op_name
                } else {
                    false
                }
            }
            Node::Effect { operation, .. } => operation == op_name,
            _ => false,
        })
        .count()
}
