//! Test for assignment optimization issue

use fluentai_parser::parse;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};

#[test]
fn test_assignment_optimization() {
    let code = "let x = 10; x := 42";
    
    eprintln!("Parsing code: {}", code);
    let graph = parse(code).expect("Parse failed");
    
    eprintln!("\nOriginal graph:");
    eprintln!("  Root: {:?}", graph.root_id);
    for (id, node) in &graph.nodes {
        eprintln!("  {:?}: {:?}", id, node);
    }
    
    // Optimize with standard level
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config.clone());
    
    eprintln!("\nOptimizing...");
    eprintln!("Optimization level: {:?}", config.level);
    
    // Add step-by-step debugging
    eprintln!("\nExpected mapping:");
    eprintln!("  NodeId(2) Variable 'x' should map to some new ID");
    eprintln!("  NodeId(3) Literal(42) should map to some new ID");
    
    let optimized = pipeline.optimize(&graph).expect("Optimization failed");
    
    eprintln!("\nOptimized graph:");
    eprintln!("  Root: {:?}", optimized.root_id);
    for (id, node) in &optimized.nodes {
        eprintln!("  {:?}: {:?}", id, node);
    }
    
    // Check if all referenced nodes exist
    eprintln!("\nChecking node references:");
    let mut all_valid = true;
    for (id, node) in &optimized.nodes {
        match node {
            fluentai_core::ast::Node::Assignment { target, value } => {
                eprintln!("  Assignment at {:?}:", id);
                let target_exists = optimized.nodes.contains_key(target);
                let value_exists = optimized.nodes.contains_key(value);
                eprintln!("    target: {:?} -> exists: {}", target, target_exists);
                eprintln!("    value: {:?} -> exists: {}", value, value_exists);
                
                if !target_exists || !value_exists {
                    all_valid = false;
                }
            }
            _ => {}
        }
    }
    
    assert!(all_valid, "Assignment nodes reference non-existent nodes");
}