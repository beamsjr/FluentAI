//! Tests for specific optimization passes in isolation

use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::*;
use fluentai_parser::parse_flc;
use fluentai_vm::{Compiler, CompilerOptions, VM};

#[test]
fn test_constant_folding_arithmetic() {
    // Test arithmetic constant folding
    let cases = vec![
        ("(+ 2 3)", "5"),
        ("(- 10 4)", "6"),
        ("(* 3 7)", "21"),
        ("(/ 20 4)", "5"),
        ("(mod 17 5)", "2"),
    ];

    for (code, expected) in cases {
        let ast = parse_flc(code).unwrap();
        let config = OptimizationConfig {
            constant_folding: true,
            ..OptimizationConfig::for_level(OptimizationLevel::None)
        };

        let mut pipeline = OptimizationPipeline::new(config);
        let optimized = pipeline.optimize(&ast).unwrap();

        println!(
            "Testing {}: {} nodes, root={:?}",
            code,
            optimized.nodes.len(),
            optimized.root_id
        );
        for (id, node) in &optimized.nodes {
            println!("  {:?}: {:?}", id, node);
        }

        // Should be folded to a single literal
        assert_eq!(optimized.nodes.len(), 1, "Failed to fold {}", code);
        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(lit)) = optimized.get_node(root) {
                assert_eq!(format!("{}", lit), expected);
            } else {
                panic!("Expected literal node for {}", code);
            }
        }
    }
}

#[test]
fn test_constant_folding_boolean() {
    // Test boolean constant folding
    let cases = vec![
        ("(and #t #t)", "true"),
        ("(and #t #f)", "false"),
        ("(or #f #t)", "true"),
        ("(not #f)", "true"),
        ("(< 3 5)", "true"),
        ("(> 3 5)", "false"),
        ("(= 5 5)", "true"),
    ];

    for (code, expected) in cases {
        let ast = parse_flc(code).unwrap();
        let config = OptimizationConfig {
            constant_folding: true,
            ..OptimizationConfig::for_level(OptimizationLevel::None)
        };

        let mut pipeline = OptimizationPipeline::new(config);
        let optimized = pipeline.optimize(&ast).unwrap();

        if let Some(root) = optimized.root_id {
            if let Some(Node::Literal(lit)) = optimized.get_node(root) {
                let lit_str = format!("{}", lit);
                println!("  Result: {} (expected: {})", lit_str, expected);
                assert_eq!(lit_str, expected, "Failed for {}", code);
            }
        }
    }
}

#[test]
fn test_constant_folding_string() {
    // Test string operation constant folding
    let code = r#"(str-concat "hello" " world")"#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        constant_folding: true,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    if let Some(root) = optimized.root_id {
        if let Some(Node::Literal(Literal::String(s))) = optimized.get_node(root) {
            assert_eq!(s, "hello world");
        }
    }
}

#[test]
fn test_dead_code_elimination_unused_let() {
    // Test elimination of unused let bindings
    let code = r#"
        (let ((unused1 (+ 1 2))
              (unused2 (* 3 4))
              (used (- 10 5)))
          used)
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        dead_code_elimination: true,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Should have eliminated unused bindings
    let stats = pipeline.stats();
    println!(
        "Dead code elimination: eliminated {} nodes",
        stats.dead_code_eliminated
    );
    println!(
        "Optimized graph has {} nodes (from {})",
        optimized.nodes.len(),
        ast.nodes.len()
    );
    assert!(
        stats.dead_code_eliminated >= 2,
        "Expected at least 2 nodes eliminated, got {}",
        stats.dead_code_eliminated
    );
}

#[test]
fn test_dead_code_elimination_preserves_effects() {
    // Test that dead code elimination preserves side effects
    let code = r#"
        (let ((x (print "side effect"))
              (y 10))
          y)
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        dead_code_elimination: true,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // The print should not be eliminated
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
fn test_cse_basic() {
    // Test basic common subexpression elimination
    let code = r#"
        (let ((a (+ 1 2))
              (b (+ 1 2)))
          (* a b))
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        cse: true,
        constant_folding: false, // Disable to test CSE specifically
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Should have eliminated duplicate (+ 1 2)
    let add_count = optimized
        .nodes
        .values()
        .filter(|node| {
            matches!(node, Node::Application { function, .. } if {
                optimized.get_node(*function)
                    .map(|n| matches!(n, Node::Variable { name } if name == "+"))
                    .unwrap_or(false)
            })
        })
        .count();

    assert_eq!(add_count, 1, "Should have only one addition after CSE");
}

#[test]
fn test_inline_simple_function() {
    // Test inlining of simple functions
    let code = "((lambda (x) (+ x 1)) 5)";
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        inline: true,
        inline_threshold: 10,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Lambda should be inlined
    let has_lambda = optimized
        .nodes
        .values()
        .any(|node| matches!(node, Node::Lambda { .. }));
    assert!(!has_lambda, "Lambda should have been inlined");
}

#[test]
fn test_inline_respects_threshold() {
    // Test that inlining respects the threshold
    let code = r#"
        ((lambda (x) 
          (+ x x x x x x x x x x x x x x x x x x x x)) 
         5)
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        inline: true,
        inline_threshold: 5, // Small threshold
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Lambda should NOT be inlined (too large)
    let has_lambda = optimized
        .nodes
        .values()
        .any(|node| matches!(node, Node::Lambda { .. }));
    assert!(has_lambda, "Large lambda should not have been inlined");
}

#[test]
fn test_tail_call_optimization_simple() {
    // Test simple tail call optimization
    let code = r#"
        (letrec ((sum (lambda (n acc)
                        (if (= n 0)
                            acc
                            (sum (- n 1) (+ acc n))))))
          (sum 10 0))
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        tail_call_optimization: true,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    let stats = pipeline.stats();
    println!(
        "Tail call optimization: {} tail calls optimized",
        stats.tail_calls_optimized
    );
    assert!(
        stats.tail_calls_optimized > 0,
        "Expected at least 1 tail call optimized, got {}",
        stats.tail_calls_optimized
    );
}

#[test]
fn test_partial_evaluation_if() {
    // Test partial evaluation of if expressions
    let code = "(if #t 42 (error \"unreachable\"))";
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig {
        partial_evaluation: true,
        ..OptimizationConfig::for_level(OptimizationLevel::None)
    };

    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    // Should have eliminated the if and the error branch
    let has_error = optimized.nodes.values().any(|node| {
        matches!(node, Node::Application { function, .. } if {
            optimized.get_node(*function)
                .map(|n| matches!(n, Node::Variable { name } if name == "error"))
                .unwrap_or(false)
        })
    });
    assert!(!has_error);
}

#[test]
fn test_optimization_preserves_correctness() {
    // Test that optimizations preserve program correctness
    let test_cases = vec![
        // Basic arithmetic
        "(+ (* 2 3) (/ 10 2))", // 6 + 5 = 11
        // Let bindings with dependencies
        "(let ((x 5) (y (* x 2))) (+ x y))", // 5 + 10 = 15
        // Conditional expressions
        "(if (> 10 5) (* 3 4) (/ 8 2))", // 12
        // Function application
        "((lambda (x y) (- (* x 2) y)) 10 3)", // 20 - 3 = 17
        // Nested expressions
        "(let ((f (lambda (x) (* x x)))) (+ (f 3) (f 4)))", // 9 + 16 = 25
    ];

    for code in test_cases {
        let ast = parse_flc(code).unwrap();

        // Compile without optimization
        let unopt_compiler = Compiler::with_options(CompilerOptions {
            optimization_level: OptimizationLevel::None,
            debug_info: false,
        });
        let unopt_bytecode = unopt_compiler.compile(&ast).unwrap();
        let mut unopt_vm = VM::new(unopt_bytecode);
        let unopt_result = unopt_vm.run().unwrap();

        // Compile with aggressive optimization
        let opt_config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
        let mut pipeline = OptimizationPipeline::new(opt_config);
        let optimized = pipeline.optimize(&ast).unwrap();

        let opt_compiler = Compiler::with_options(CompilerOptions {
            optimization_level: OptimizationLevel::None, // Already optimized
            debug_info: false,
        });
        let opt_bytecode = opt_compiler.compile(&optimized).unwrap();
        let mut opt_vm = VM::new(opt_bytecode);
        let opt_result = opt_vm.run().unwrap();

        // Results should be identical
        assert_eq!(
            format!("{}", unopt_result),
            format!("{}", opt_result),
            "Optimization changed result for: {}",
            code
        );
    }
}

#[test]
fn test_optimization_combination() {
    // Test that multiple optimizations work together correctly
    let code = r#"
        (let ((x 5)
              (y 10)
              (unused 42))
          (if #t
              (+ (+ x y) (+ x y))
              (error "unreachable")))
    "#;
    let ast = parse_flc(code).unwrap();

    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();

    let stats = pipeline.stats();

    // Should have:
    // - Eliminated unused binding
    // - Eliminated unreachable branch
    // - Folded constants or eliminated common subexpressions
    assert!(stats.dead_code_eliminated > 0);
    assert!(stats.branches_eliminated > 0);
    assert!(stats.nodes_after < stats.nodes_before);
}
