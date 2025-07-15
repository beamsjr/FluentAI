//! Demonstrates the ML-based optimization hints system

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_optimizer::ml_hints::{MLOptimizationHints, OptimizationHint};
use fluentai_parser::parse_flc;

fn main() -> Result<()> {
    println!("=== ML Optimization Hints Demo ===\n");
    
    // Create ML hints generator
    let ml_hints = MLOptimizationHints::new();
    
    // Test 1: Simple arithmetic operations
    println!("Test 1: Simple arithmetic operations");
    let code1 = "2 + 3 * 4 - 5";
    let ast1 = parse_flc(code1)?;
    let features1 = ml_hints.extract_features(&ast1);
    println!("  Arithmetic ops: {}", features1.arithmetic_ops);
    println!("  Node count: {}", features1.node_count);
    println!("  Depth: {}", features1.depth);
    let hints1 = ml_hints.generate_hints(&ast1);
    println!("  Generated hints: {:?}\n", hints1);
    
    // Test 2: Loop pattern
    println!("Test 2: Loop pattern (recursive function)");
    let code2 = r#"
    {
        let fact = (n) => {
            if (n <= 1) { 1 }
            else { n * fact(n - 1) }
        };
        fact(10)
    }
    "#;
    let ast2 = parse_flc(code2)?;
    let features2 = ml_hints.extract_features(&ast2);
    println!("  Has recursion: {}", features2.has_recursion);
    println!("  Has loops: {}", features2.has_loops);
    println!("  Function calls: {}", features2.function_calls);
    let hints2 = ml_hints.generate_hints(&ast2);
    println!("  Generated hints: {:?}\n", hints2);
    
    // Test 3: Map pattern
    println!("Test 3: Map pattern");
    let code3 = "[1, 2, 3, 4, 5].map(x => x * x)";
    let ast3 = parse_flc(code3)?;
    let features3 = ml_hints.extract_features(&ast3);
    println!("  Has map pattern: {}", features3.has_map_pattern);
    println!("  Uses lists: {}", features3.uses_lists);
    println!("  Uses higher order: {}", features3.uses_higher_order);
    let hints3 = ml_hints.generate_hints(&ast3);
    println!("  Generated hints: {:?}\n", hints3);
    
    // Test 4: Complex computation suitable for multiple optimizations
    println!("Test 4: Complex computation");
    let code4 = r#"
    {
        let sum = 0;
        let product = 1;
        let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        
        // Multiple arithmetic operations that could benefit from strength reduction
        let x = 10 * 2;  // Could become 10 + 10
        let y = 20 * 1;  // Could become 20
        let z = 30 * 0;  // Could become 0
        
        // Function that could be inlined
        let add = (a, b) => a + b;
        let mul = (a, b) => a * b;
        
        // Use the functions multiple times
        add(x, y) + mul(y, z) + add(z, x)
    }
    "#;
    let ast4 = parse_flc(code4)?;
    let features4 = ml_hints.extract_features(&ast4);
    println!("  Node count: {}", features4.node_count);
    println!("  Arithmetic ops: {}", features4.arithmetic_ops);
    println!("  Function calls: {}", features4.function_calls);
    println!("  Data dependencies: {}", features4.data_dependencies);
    println!("  Register pressure: {:.2}", features4.register_pressure);
    let hints4 = ml_hints.generate_hints(&ast4);
    println!("  Generated hints: {:?}\n", hints4);
    
    // Test 5: Use SimplePredictionModel
    println!("Test 5: Simple prediction model");
    use fluentai_optimizer::ml_hints::SimplePredictionModel;
    let model = SimplePredictionModel::new();
    
    // Create features for a program that should trigger various hints
    let mut test_features = fluentai_optimizer::ml_hints::ProgramFeatures::default();
    test_features.has_loops = true;
    test_features.arithmetic_ops = 25;
    test_features.data_dependencies = 5;
    test_features.function_calls = 8;
    test_features.node_count = 40;
    test_features.has_recursion = true;
    test_features.estimated_iterations = Some(200);
    
    let predictions = model.predict(&test_features);
    println!("  Predictions: {:?}\n", predictions);
    
    // Show feature vector
    println!("Feature vector representation:");
    let feature_vec = features4.to_vector();
    println!("  Vector length: {}", feature_vec.len());
    println!("  First 5 values: {:?}", &feature_vec[..5.min(feature_vec.len())]);
    
    Ok(())
}