//! Demonstrates the loop-invariant code motion optimization pass

use anyhow::Result;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

fn main() -> Result<()> {
    println!("=== Loop-Invariant Code Motion Optimization Demo ===\n");
    
    // Example 1: Simple tail-recursive loop with invariant computation
    println!("Example 1: Tail-recursive loop with invariant");
    let code1 = r#"
    letrec {
        factorial = (n) => {
            if (n <= 1) {
                1
            } else {
                let const_expr = 10 * 20;  // This is loop-invariant
                n * factorial(n - 1)
            }
        }
    } {
        factorial(5)
    }
    "#;
    
    demonstrate_optimization(code1, "Tail-recursive factorial")?;
    
    // Example 2: Map with invariant computation
    println!("\nExample 2: Map with invariant computation");
    let code2 = r#"{
        let data = [1, 2, 3, 4, 5];
        let multiplier = 100;
        data.map(x => {
            let invariant = multiplier * 2;  // This doesn't depend on x
            x * invariant
        })
    }"#;
    
    demonstrate_optimization(code2, "Map with invariant")?;
    
    // Example 3: Complex loop with multiple invariants
    println!("\nExample 3: Complex loop with multiple invariants");
    let code3 = r#"
    letrec {
        sum_with_constants = (list, acc) => {
            if (list.is_empty()) {
                acc
            } else {
                let a = 5 * 10;      // Invariant 1
                let b = a + 20;      // Invariant 2
                let c = b * 2;       // Invariant 3
                let head = list.head();
                let tail = list.tail();
                sum_with_constants(tail, acc + head + c)
            }
        }
    } {
        sum_with_constants([1, 2, 3, 4, 5], 0)
    }
    "#;
    
    demonstrate_optimization(code3, "Multiple invariants")?;
    
    // Example 4: Loop where computation depends on loop variable
    println!("\nExample 4: Loop-dependent computation (not invariant)");
    let code4 = r#"
    letrec {
        power_sum = (n, sum) => {
            if (n <= 0) {
                sum
            } else {
                let square = n * n;  // Depends on n, not invariant
                power_sum(n - 1, sum + square)
            }
        }
    } {
        power_sum(10, 0)
    }
    "#;
    
    demonstrate_optimization(code4, "Loop-dependent")?;
    
    Ok(())
}

fn demonstrate_optimization(code: &str, name: &str) -> Result<()> {
    let ast = parse_flc(code)?;
    
    // First, run without loop-invariant code motion
    let config_without = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 20,
        tail_call_optimization: true,
        loop_optimization: true,
        beta_reduction: true,
        partial_evaluation: true,
        strength_reduction: true,
        algebraic_simplification: true,
        loop_invariant_code_motion: false, // Disabled
        max_iterations: 3,
        debug_mode: false,
    };
    
    let mut pipeline_without = OptimizationPipeline::new(config_without);
    let result_without = pipeline_without.optimize(&ast)?;
    
    // Then, run with loop-invariant code motion
    let config_with = OptimizationConfig {
        level: OptimizationLevel::Aggressive,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 20,
        tail_call_optimization: true,
        loop_optimization: true,
        beta_reduction: true,
        partial_evaluation: true,
        strength_reduction: true,
        algebraic_simplification: true,
        loop_invariant_code_motion: true, // Enabled
        max_iterations: 3,
        debug_mode: false,
    };
    
    let mut pipeline_with = OptimizationPipeline::new(config_with);
    let result_with = pipeline_with.optimize(&ast)?;
    
    println!("  {} optimization:", name);
    println!("    Original nodes: {}", ast.nodes.len());
    println!("    Without LICM: {} nodes", result_without.nodes.len());
    println!("    With LICM: {} nodes", result_with.nodes.len());
    
    // Get stats from the pipeline
    let stats = pipeline_with.stats();
    println!("    Optimization stats: {:?}", stats);
    
    // Count let bindings to see if hoisting occurred
    let let_count_before = count_let_nodes(&ast);
    let let_count_after = count_let_nodes(&result_with);
    
    if let_count_after != let_count_before {
        println!("    Let nodes: {} -> {} (hoisting detected)", let_count_before, let_count_after);
    }
    
    Ok(())
}

fn count_let_nodes(graph: &fluentai_core::ast::Graph) -> usize {
    let mut count = 0;
    for node in graph.nodes.values() {
        if matches!(node, Node::Let { .. }) {
            count += 1;
        }
    }
    count
}