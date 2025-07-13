//! Debug stack overflow issue

use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_optimizer::{OptimizationConfig, OptimizationPipeline};
use fluentai_parser::parse_flc;

fn main() {
    let code = r#"
        (let ((x 5) (y 10) (unused 15))
            (if #t
                (+ x y)
                (error "unreachable")))
    "#;

    println!("Parsing code...");
    let graph = parse_flc(code).unwrap();
    println!("Graph has {} nodes", graph.nodes.len());

    // First try with AdvancedOptimizer directly
    println!("\nTrying AdvancedOptimizer directly...");
    let mut adv_opt = fluentai_optimizer::AdvancedOptimizer::new();
    match adv_opt.optimize(&graph) {
        Ok(result) => {
            println!("AdvancedOptimizer succeeded: {} nodes", result.nodes.len());
        }
        Err(e) => {
            println!("AdvancedOptimizer failed: {}", e);
        }
    }

    println!("\nTrying Aggressive optimization pipeline...");
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);

    println!("Starting optimization...");
    match pipeline.optimize(&graph) {
        Ok(optimized) => {
            println!("Success! Optimized to {} nodes", optimized.nodes.len());
            println!("Stats: {:?}", pipeline.stats());
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}
