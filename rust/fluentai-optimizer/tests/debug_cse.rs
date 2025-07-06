use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig, AdvancedOptimizer};
use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_parser::parse;

#[test]
fn debug_cse() {
    // Program with repeated subexpressions
    let code = "(let ((x 5)) (+ (* x 2) (* x 2) (* x 2)))";
    let graph = parse(code).unwrap();
    
    println!("Original AST ({} nodes):", graph.nodes.len());
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", graph.root_id);
    
    // First try with AdvancedOptimizer directly
    println!("Testing AdvancedOptimizer:");
    let mut advanced = AdvancedOptimizer::new();
    let optimized_advanced = advanced.optimize(&graph).unwrap();
    
    println!("AdvancedOptimizer result ({} nodes):", optimized_advanced.nodes.len());
    for (id, node) in &optimized_advanced.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", optimized_advanced.root_id);
    println!("Stats: {:?}\n", advanced.stats());
    
    // Now try with OptimizationPipeline
    println!("Testing OptimizationPipeline with Aggressive level:");
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    println!("Config: cse={}, inline={}, beta_reduction={}", 
             config.cse, config.inline, config.beta_reduction);
    
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized_pipeline = pipeline.optimize(&graph).unwrap();
    
    println!("Pipeline result ({} nodes):", optimized_pipeline.nodes.len());
    for (id, node) in &optimized_pipeline.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", optimized_pipeline.root_id);
    println!("Stats: {:?}", pipeline.stats());
}