//! Test dead code elimination

use fluentai_optimizer::{GraphOptimizer, AdvancedOptimizer};
use fluentai_parser::parse;

fn main() {
    let code = "(let ((x 1) (y 2) (unused 3)) (+ x y))";
    println!("Original code: {}", code);
    
    let graph = parse(code).unwrap();
    println!("Original graph nodes: {}", graph.nodes.len());
    
    // Test with GraphOptimizer
    let mut basic_opt = GraphOptimizer::new();
    let basic_result = basic_opt.optimize(&graph).unwrap();
    println!("\nBasic optimizer:");
    println!("  Nodes: {}", basic_result.nodes.len());
    println!("  Stats: {:?}", basic_opt.stats());
    
    // Test with AdvancedOptimizer
    let mut adv_opt = AdvancedOptimizer::new();
    let adv_result = adv_opt.optimize(&graph).unwrap();
    println!("\nAdvanced optimizer:");
    println!("  Nodes: {}", adv_result.nodes.len());
    println!("  Stats: {:?}", adv_opt.stats());
}