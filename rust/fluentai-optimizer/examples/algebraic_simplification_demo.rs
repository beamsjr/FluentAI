//! Demonstrates the algebraic simplification optimization pass

use anyhow::Result;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

fn main() -> Result<()> {
    println!("=== Algebraic Simplification Optimization Demo ===\n");
    
    // Example 1: Addition identities
    println!("Example 1: Addition identities");
    let code1 = r#"{
        let a = 10;
        let b = a + 0;   // Should become a
        let c = 0 + a;   // Should become a
        b + c
    }"#;
    
    demonstrate_optimization(code1, "Addition")?;
    
    // Example 2: Subtraction identities
    println!("\nExample 2: Subtraction identities");
    let code2 = r#"{
        let x = 100;
        let y = x - 0;   // Should become x
        let z = x - x;   // Should become 0
        y + z
    }"#;
    
    demonstrate_optimization(code2, "Subtraction")?;
    
    // Example 3: Boolean identities
    println!("\nExample 3: Boolean identities");
    let code3 = r#"{
        let p = true;
        let q = false;
        let a = p && true;  // Should become p
        let b = q || false; // Should become q
        let c = p || true;  // Should become true
        let d = q && false; // Should become false
        let e = !true;      // Should become false
        let f = !false;     // Should become true
        a && b || c && d || e || f
    }"#;
    
    demonstrate_optimization(code3, "Boolean")?;
    
    // Example 4: Complex expression with multiple simplifications
    println!("\nExample 4: Complex expression");
    let code4 = r#"{
        let x = 10;
        let y = 20;
        // Multiple algebraic simplifications
        let a = x + 0;          // Should become x
        let b = y - 0;          // Should become y
        let c = x - x;          // Should become 0
        let d = true && a > 0;  // Should simplify true &&
        let e = false || b < 0; // Should simplify false ||
        let result = (a + b + c) * (if d { 1 } else { 0 });
        result
    }"#;
    
    demonstrate_optimization(code4, "Complex")?;
    
    // Example 5: Division identity
    println!("\nExample 5: Division identity");
    let code5 = r#"{
        let n = 42;
        let m = n / n;  // Should become 1
        m
    }"#;
    
    demonstrate_optimization(code5, "Division")?;
    
    Ok(())
}

fn demonstrate_optimization(code: &str, name: &str) -> Result<()> {
    let ast = parse_flc(code)?;
    
    // First, run without algebraic simplification
    let config_without = OptimizationConfig {
        level: OptimizationLevel::Standard,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 10,
        tail_call_optimization: true,
        loop_optimization: false,
        beta_reduction: true,
        partial_evaluation: false,
        strength_reduction: false,
        algebraic_simplification: false, // Disabled
        loop_invariant_code_motion: false,
        max_iterations: 2,
        debug_mode: false,
    };
    
    let mut pipeline_without = OptimizationPipeline::new(config_without);
    let result_without = pipeline_without.optimize(&ast)?;
    
    // Then, run with algebraic simplification
    let config_with = OptimizationConfig {
        level: OptimizationLevel::Standard,
        constant_folding: true,
        dead_code_elimination: true,
        cse: true,
        inline: true,
        inline_threshold: 10,
        tail_call_optimization: true,
        loop_optimization: false,
        beta_reduction: true,
        partial_evaluation: false,
        strength_reduction: false,
        algebraic_simplification: true, // Enabled
        loop_invariant_code_motion: false,
        max_iterations: 2,
        debug_mode: false,
    };
    
    let mut pipeline_with = OptimizationPipeline::new(config_with);
    let result_with = pipeline_with.optimize(&ast)?;
    
    println!("  {} optimization:", name);
    println!("    Original nodes: {}", ast.nodes.len());
    println!("    Without algebraic simplification: {} nodes", result_without.nodes.len());
    println!("    With algebraic simplification: {} nodes", result_with.nodes.len());
    
    // Count specific operation types
    let add_count_before = count_operations(&ast, "+");
    let add_count_after = count_operations(&result_with, "+");
    let sub_count_before = count_operations(&ast, "-");
    let sub_count_after = count_operations(&result_with, "-");
    let and_count_before = count_operations(&ast, "&&") + count_operations(&ast, "and");
    let and_count_after = count_operations(&result_with, "&&") + count_operations(&result_with, "and");
    let or_count_before = count_operations(&ast, "||") + count_operations(&ast, "or");
    let or_count_after = count_operations(&result_with, "||") + count_operations(&result_with, "or");
    
    if add_count_before > add_count_after {
        println!("    Addition operations: {} -> {}", add_count_before, add_count_after);
    }
    if sub_count_before > sub_count_after {
        println!("    Subtraction operations: {} -> {}", sub_count_before, sub_count_after);
    }
    if and_count_before > and_count_after {
        println!("    AND operations: {} -> {}", and_count_before, and_count_after);
    }
    if or_count_before > or_count_after {
        println!("    OR operations: {} -> {}", or_count_before, or_count_after);
    }
    
    // Get stats from the pipeline
    let stats = pipeline_with.stats();
    println!("    Optimization stats: {:?}", stats);
    
    Ok(())
}

fn count_operations(graph: &fluentai_core::ast::Graph, op: &str) -> usize {
    let mut count = 0;
    for node in graph.nodes.values() {
        if let Node::Application { function, .. } = node {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                if name == op {
                    count += 1;
                }
            }
        }
    }
    count
}