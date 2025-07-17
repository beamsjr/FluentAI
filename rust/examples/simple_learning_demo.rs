//! Simple demonstration of VM Learning Mode
//!
//! This example shows the VM learning optimal optimization strategies at runtime.

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, LearningModeConfig};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI Learning Mode Demo ===\n");
    
    // Create a simple program that can benefit from optimizations
    let source = r#"
        // Function with constant folding opportunities
        private function compute_with_constants(n) {
            (10 + 20) * n + (5 * 6)
        }
        
        // Function with dead code (always returns first branch)
        private function always_positive(x) {
            if (true) {
                x * 2
            } else {
                x * -1  // Dead code
            }
        }
        
        // Hot function that will trigger optimization
        private function hot_function(n) {
            let sum = 0;
            let i = 0;
            while (i < n) {
                sum := sum + compute_with_constants(i);
                i := i + 1;
            }
            sum
        }
        
        // Main function
        private function main() {
            $("Starting learning mode demo...").print();
            
            // Warm up functions
            let result1 = compute_with_constants(5);
            let result2 = always_positive(10);
            
            $("Warm-up complete. Now running hot function...").print();
            
            // Run hot function many times to trigger learning
            let total = 0;
            let iter = 0;
            while (iter < 200) {  // Run enough to become "hot"
                total := total + hot_function(5);
                iter := iter + 1;
            }
            
            $("Hot function executions complete!").print();
            $("Total computed: " + total).print();
        }
        
        main()
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Program has {} AST nodes\n", graph.nodes.len());
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with learning mode
    println!("Creating VM with learning mode enabled...");
    let mut vm = VM::new(bytecode);
    
    // Configure learning mode
    let learning_config = LearningModeConfig {
        hot_threshold: 50,  // Lower threshold for demo
        max_strategies_per_function: 3,
        save_learned_data: true,
        model_path: Some("simple_learning_model.bin".to_string()),
        exploration_rate: 0.3,
        use_rl_agent: false,
    };
    
    vm.enable_learning_mode_with_config(learning_config);
    
    println!("Running program with learning mode...\n");
    
    // Run the program
    let start = Instant::now();
    match vm.run() {
        Ok(result) => {
            let elapsed = start.elapsed();
            println!("\nProgram completed in {:?}", elapsed);
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    // Get learning statistics
    if let Some(stats) = vm.get_learning_statistics() {
        println!("\n=== Learning Mode Statistics ===");
        println!("Functions analyzed: {}", stats.functions_analyzed);
        println!("Hot functions found: {}", stats.hot_functions);
        println!("Functions with optimizations explored: {}", stats.functions_explored);
        println!("Total optimization variants created: {}", stats.total_variants);
        println!("Functions currently being explored: {}", stats.exploring_now);
    }
    
    println!("\n=== Demo Complete ===");
    println!("The VM has learned optimization strategies for hot functions.");
    
    Ok(())
}