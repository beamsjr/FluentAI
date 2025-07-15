//! Demo of function specialization optimization
//!
//! This example shows how the optimizer creates specialized versions of functions
//! when they're frequently called with constant arguments.

use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_parser::parse_flc;

fn main() {
    // Example: A math function that's often called with specific constants
    let code = r#"
    // Function to calculate polynomial: a*x^2 + b*x + c
    let poly = (x, a, b, c) => {
        let x2 = x * x;
        let term1 = a * x2;
        let term2 = b * x;
        term1 + term2 + c
    };
    
    // Many calls with the same coefficients (a=1, b=2, c=3)
    let y1 = poly(5, 1, 2, 3);
    let y2 = poly(10, 1, 2, 3);
    let y3 = poly(15, 1, 2, 3);
    let y4 = poly(20, 1, 2, 3);
    
    // A few calls with different coefficients
    let z1 = poly(5, 2, 3, 4);
    
    [y1, y2, y3, y4, z1]
    "#;
    
    println!("=== Function Specialization Demo ===\n");
    println!("Original code:");
    println!("{}", code);
    
    // Parse the code
    let ast = parse_flc(code).expect("Failed to parse");
    
    // Run with optimization disabled
    let config_no_opt = OptimizationConfig {
        level: OptimizationLevel::None,
        function_specialization: false,
        ..Default::default()
    };
    
    let mut pipeline_no_opt = OptimizationPipeline::new(config_no_opt);
    let result_no_opt = pipeline_no_opt.optimize(&ast).expect("Optimization failed");
    
    println!("\n=== Without function specialization ===");
    println!("Total nodes: {}", result_no_opt.nodes.len());
    
    // Run with function specialization enabled
    let config_with_spec = OptimizationConfig {
        level: OptimizationLevel::None,
        function_specialization: true,
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
        max_iterations: 1,
        debug_mode: false,
    };
    
    let mut pipeline_with_spec = OptimizationPipeline::new(config_with_spec);
    let result_with_spec = pipeline_with_spec.optimize(&ast).expect("Optimization failed");
    
    println!("\n=== With function specialization ===");
    println!("Total nodes: {}", result_with_spec.nodes.len());
    println!("{}", pipeline_with_spec.stats());
    
    // More complex example with nested functions
    let complex_code = r#"
    // Configuration processor with many constant config values
    let process_data = (data, config_type, debug_mode) => {
        let multiplier = if (config_type == "production") { 1.0 } else { 0.5 };
        let result = data * multiplier;
        
        if (debug_mode) {
            // In debug mode, add extra validation
            if (result < 0) { 0 } else { result }
        } else {
            result
        }
    };
    
    // Production calls (should be specialized)
    let p1 = process_data(100, "production", false);
    let p2 = process_data(200, "production", false);
    let p3 = process_data(300, "production", false);
    let p4 = process_data(400, "production", false);
    
    // Development calls
    let d1 = process_data(100, "development", true);
    
    [p1, p2, p3, p4, d1]
    "#;
    
    println!("\n\n=== Complex Example: Configuration Processing ===\n");
    println!("Code with configuration-dependent processing...");
    
    let complex_ast = parse_flc(complex_code).expect("Failed to parse");
    
    // Create a new config for the complex example
    let config_complex = OptimizationConfig {
        level: OptimizationLevel::None,
        function_specialization: true,
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
        max_iterations: 1,
        debug_mode: false,
    };
    
    let mut complex_pipeline = OptimizationPipeline::new(config_complex);
    let _complex_result = complex_pipeline.optimize(&complex_ast).expect("Optimization failed");
    
    println!("\nOptimization results:");
    println!("{}", complex_pipeline.stats());
    
    println!("\n=== Benefits of Function Specialization ===");
    println!("1. Constant parameters are baked into specialized versions");
    println!("2. Conditional branches based on constants can be eliminated");
    println!("3. Further optimizations become possible within specialized functions");
    println!("4. Reduces runtime overhead for frequently-used parameter combinations");
}