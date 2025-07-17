//! Minimal demonstration of VM Learning Mode
//!
//! This example shows the VM learning optimization strategies at runtime.

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, LearningModeConfig};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI Learning Mode Demo ===\n");
    
    // Create a very simple program with optimization opportunities
    let source = r#"
        // Function with constant expressions that can be folded
        private function compute(x) {
            x + 10 + 20 + 30  // Can be optimized to x + 60
        }
        
        // Function that calls compute many times
        private function hot_loop() {
            let result = 0;
            let i = 0;
            while (i < 1000) {
                result := result + compute(i);
                i := i + 1;
            }
            result
        }
        
        // Main entry point
        $("Starting learning mode demo...").print();
        
        // Warm up
        let warmup = compute(5);
        $("Warmup result: " + warmup).print();
        
        // Run hot function to trigger learning
        $("Running hot loop...").print();
        let result = hot_loop();
        $("Result: " + result).print();
        
        $("Demo complete!").print();
        
        0  // Return value
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
        hot_threshold: 100,  // Function becomes hot after 100 executions
        max_strategies_per_function: 3,
        save_learned_data: false,
        model_path: None,
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
            println!("Final result: {:?}", result);
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
    }
    
    println!("\n=== Demo Complete ===");
    
    Ok(())
}