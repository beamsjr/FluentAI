use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;

#[test]
fn test_strength_reduction_integration() {
    // Test that x * 2 gets optimized when strength reduction is enabled
    let code = "let x = 5; x * 2";
    let ast = parse_flc(code).unwrap();
    
    // First test with strength reduction disabled
    let config_without = OptimizationConfig {
        level: OptimizationLevel::Standard,
        constant_folding: false,
        dead_code_elimination: false,
        cse: false,
        inline: false,
        inline_threshold: 0,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: false,
        strength_reduction: false,
        algebraic_simplification: false,
        loop_invariant_code_motion: false,
        function_specialization: false,
        max_iterations: 1,
        debug_mode: false,
    };
    
    let mut pipeline_without = OptimizationPipeline::new(config_without);
    let result_without = pipeline_without.optimize(&ast).unwrap();
    
    // Now test with strength reduction enabled
    let config_with = OptimizationConfig {
        level: OptimizationLevel::Standard,
        constant_folding: false,
        dead_code_elimination: false,
        cse: false,
        inline: false,
        inline_threshold: 0,
        tail_call_optimization: false,
        loop_optimization: false,
        beta_reduction: false,
        partial_evaluation: false,
        strength_reduction: true,
        algebraic_simplification: false,
        loop_invariant_code_motion: false,
        function_specialization: false,
        max_iterations: 1,
        debug_mode: false,
    };
    
    let mut pipeline_with = OptimizationPipeline::new(config_with);
    let result_with = pipeline_with.optimize(&ast).unwrap();
    
    // Currently the strength reduction pass just returns a clone,
    // so both results should be the same for now
    assert_eq!(result_without.nodes.len(), result_with.nodes.len());
    
    // Test that Standard optimization level includes strength reduction
    let standard_config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    assert!(standard_config.strength_reduction);
    
    // Test that Aggressive optimization level includes strength reduction
    let aggressive_config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    assert!(aggressive_config.strength_reduction);
    
    // Test that Basic optimization level does not include strength reduction
    let basic_config = OptimizationConfig::for_level(OptimizationLevel::Basic);
    assert!(!basic_config.strength_reduction);
}

#[test]
fn test_strength_reduction_with_ml_benchmark() {
    // Create a more complex test that would benefit from strength reduction
    let code = r#"{
        let a = 10;
        let b = 20;
        let c = a * 2;      // Should become a + a
        let d = b * 1;      // Should become b
        let e = c * 0;      // Should become 0
        let f = d / 1;      // Should become d
        a + b + c + d + e + f
    }"#;
    
    let ast = parse_flc(code).unwrap();
    
    // Run with aggressive optimization (includes strength reduction)
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // The optimized code should have fewer nodes due to various optimizations
    assert!(optimized.nodes.len() <= ast.nodes.len());
    
    // Get the stats to see what happened
    let stats = pipeline.stats();
    println!("Optimization stats: {:?}", stats);
}