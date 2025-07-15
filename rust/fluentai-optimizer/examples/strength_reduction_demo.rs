//! Demonstrates the strength reduction optimization pass

use anyhow::Result;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

fn main() -> Result<()> {
    println!("=== Strength Reduction Optimization Demo ===\n");
    
    // Example 1: Multiplication optimizations
    println!("Example 1: Multiplication optimizations");
    let code1 = r#"{
        let a = 10;
        let b = a * 2;   // Should become a + a
        let c = a * 1;   // Should become a
        let d = a * 0;   // Should become 0
        b + c + d
    }"#;
    
    demonstrate_optimization(code1, "Multiplication")?;
    
    // Example 2: Division optimizations
    println!("\nExample 2: Division optimizations");
    let code2 = r#"{
        let x = 100;
        let y = x / 1;   // Should become x
        y
    }"#;
    
    demonstrate_optimization(code2, "Division")?;
    
    // Example 3: Exponentiation optimizations
    println!("\nExample 3: Exponentiation optimizations");
    let code3 = r#"{
        let n = 5;
        let a = n ** 0;  // Should become 1
        let b = n ** 1;  // Should become n
        let c = n ** 2;  // Should become n * n
        a + b + c
    }"#;
    
    demonstrate_optimization(code3, "Exponentiation")?;
    
    // Example 4: Complex expression with multiple optimizations
    println!("\nExample 4: Complex expression");
    let code4 = r#"{
        let x = 10;
        let y = 20;
        let result = (x * 2) + (y * 1) + (x ** 2) + (y / 1);
        result
    }"#;
    
    demonstrate_optimization(code4, "Complex")?;
    
    Ok(())
}

fn demonstrate_optimization(code: &str, name: &str) -> Result<()> {
    let ast = parse_flc(code)?;
    
    // First, run without strength reduction
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
        strength_reduction: false, // Disabled
        algebraic_simplification: false,
        loop_invariant_code_motion: false,
        function_specialization: false,
        max_iterations: 2,
        debug_mode: false,
    };
    
    let mut pipeline_without = OptimizationPipeline::new(config_without);
    let result_without = pipeline_without.optimize(&ast)?;
    
    // Then, run with strength reduction
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
        strength_reduction: true, // Enabled
        algebraic_simplification: false,
        loop_invariant_code_motion: false,
        function_specialization: false,
        max_iterations: 2,
        debug_mode: false,
    };
    
    let mut pipeline_with = OptimizationPipeline::new(config_with);
    let result_with = pipeline_with.optimize(&ast)?;
    
    println!("  {} optimization:", name);
    println!("    Original nodes: {}", ast.nodes.len());
    println!("    Without strength reduction: {} nodes", result_without.nodes.len());
    println!("    With strength reduction: {} nodes", result_with.nodes.len());
    
    // Count specific operation types
    let mul_count_before = count_operations(&ast, "*");
    let mul_count_after = count_operations(&result_with, "*");
    let div_count_before = count_operations(&ast, "/");
    let div_count_after = count_operations(&result_with, "/");
    let exp_count_before = count_operations(&ast, "**");
    let exp_count_after = count_operations(&result_with, "**");
    
    if mul_count_before > mul_count_after {
        println!("    Multiplication operations: {} -> {}", mul_count_before, mul_count_after);
    }
    if div_count_before > div_count_after {
        println!("    Division operations: {} -> {}", div_count_before, div_count_after);
    }
    if exp_count_before > exp_count_after {
        println!("    Exponentiation operations: {} -> {}", exp_count_before, exp_count_after);
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