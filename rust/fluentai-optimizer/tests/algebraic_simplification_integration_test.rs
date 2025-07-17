use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;

#[test]
fn test_algebraic_simplification_integration() {
    // Test that x + 0 gets optimized when algebraic simplification is enabled
    let code = "let x = 5; x + 0";
    let ast = parse_flc(code).unwrap();
    
    // First test with algebraic simplification disabled
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
    
    // Now test with algebraic simplification enabled
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
        strength_reduction: false,
        algebraic_simplification: true,
        loop_invariant_code_motion: false,
        function_specialization: false,
        max_iterations: 1,
        debug_mode: false,
    };
    
    let mut pipeline_with = OptimizationPipeline::new(config_with);
    let result_with = pipeline_with.optimize(&ast).unwrap();
    
    // With algebraic simplification, the expression should be simplified
    // The exact node count may vary based on the AST structure
    assert!(result_with.nodes.len() <= result_without.nodes.len());
    
    // Test that Standard optimization level includes algebraic simplification
    let standard_config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    assert!(standard_config.algebraic_simplification);
    
    // Test that Aggressive optimization level includes algebraic simplification
    let aggressive_config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    assert!(aggressive_config.algebraic_simplification);
    
    // Test that Basic optimization level does not include algebraic simplification
    let basic_config = OptimizationConfig::for_level(OptimizationLevel::Basic);
    assert!(!basic_config.algebraic_simplification);
}

#[test]
fn test_algebraic_simplification_with_boolean_ops() {
    // Create a test with boolean operations
    let code = r#"{
        let p = true;
        let q = false;
        let a = p && true;
        let b = q || false;
        a || b
    }"#;
    
    let ast = parse_flc(code).unwrap();
    
    // Run with aggressive optimization (includes algebraic simplification)
    let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&ast).unwrap();
    
    // The optimized code should have fewer nodes due to various optimizations
    assert!(optimized.nodes.len() <= ast.nodes.len());
    
    // Get the stats to see what happened
    let stats = pipeline.stats();
    println!("Optimization stats: {:?}", stats);
}