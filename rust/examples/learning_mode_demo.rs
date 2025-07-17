//! Demonstrates VM Learning Mode
//!
//! This example shows how the VM can learn optimal optimization strategies
//! for different functions during runtime.

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, LearningModeConfig};
use std::time::Instant;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FluentAI Learning Mode Demo ===\n");
    
    // Create a program with functions that benefit from different optimizations
    let source = r#"
        // Function with constant folding opportunities
        private function compute_constants(n) {
            let a = 10 + 20;       // = 30
            let b = 5 * 6;         // = 30
            let c = 100 / 2;       // = 50
            let d = a + b - c;     // = 10
            
            // Use the constants in computation
            n * d + (a * b / c)
        }
        
        // Function with dead code
        private function process_with_branches(x) {
            if (false) {
                // This whole branch is dead code
                let expensive = compute_constants(999);
                $(("Never executed: " + expensive)).print();
            }
            
            if (x > 0) {
                x * 2
            } else {
                if (x < -10) {
                    x * -1
                } else {
                    0
                }
            }
        }
        
        // Function with common subexpressions
        private function calculate_distance(x1, y1, x2, y2) {
            let dx = x2 - x1;
            let dy = y2 - y1;
            
            // These are the same computation
            let dist_squared1 = dx * dx + dy * dy;
            let dist_squared2 = dx * dx + dy * dy;
            
            // Redundant computation
            if (dist_squared1 == dist_squared2) {
                dist_squared1
            } else {
                0  // Never happens
            }
        }
        
        // Hot function that will trigger optimization
        private function hot_loop(n) {
            let sum = 0;
            let i = 0;
            while (i < n) {
                sum = sum + compute_constants(i);
                i = i + 1;
            }
            sum
        }
        
        // Main test function
        private function main() {
            $("Starting learning mode demo...").print();
            
            // Call functions to warm them up
            let result1 = compute_constants(5);
            let result2 = process_with_branches(10);
            let result3 = calculate_distance(0, 0, 3, 4);
            
            $(("Initial results: " + result1 + ", " + result2 + ", " + result3)).print();
            
            // Now run the hot function many times
            // This should trigger learning mode optimization
            let total = 0;
            let iter = 0;
            while (iter < 150) {  // Run enough times to become "hot"
                total = total + hot_loop(10);
                iter = iter + 1;
            }
            
            $(("Hot loop total: " + total)).print();
            $("Learning mode demo complete!").print();
        }
        
        main()
    "#;
    
    // Parse and compile the program
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Program has {} AST nodes\n", graph.nodes.len());
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with learning mode enabled
    println!("Creating VM with learning mode enabled...");
    let mut vm = VM::new(bytecode);
    
    // Configure learning mode
    let learning_config = LearningModeConfig {
        hot_threshold: 100,  // Function becomes hot after 100 executions
        max_strategies_per_function: 4,
        save_learned_data: true,
        model_path: Some("learning_demo_model.bin".to_string()),
        exploration_rate: 0.3,
        use_rl_agent: false,  // Start without RL agent
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
    
    // Save learned optimization data
    if let Err(e) = vm.save_learned_data("learning_demo_optimizations.dat") {
        eprintln!("Failed to save learned data: {}", e);
    } else {
        println!("\nLearned optimization data saved!");
    }
    
    println!("\n=== Demo Complete ===");
    println!("The VM has learned which optimizations work best for each function.");
    println!("Future runs can load this data for improved performance!");
    
    Ok(())
}