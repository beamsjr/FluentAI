//! Test learning mode functionality

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Learning Mode Test ===\n");
    
    // Create a program with a function that will be called multiple times
    let source = r#"
        {
            // Define a function that does some computation
            let compute = (n) => {
                let sum = 0;
                let i = 0;
                while (i < n) {
                    sum := sum + i;
                    i := i + 1
                };
                sum
            };
            
            // Call it multiple times to make it "hot"
            let count = 0;
            let j = 0;
            while (j < 15) {
                compute(10);
                count := count + 1;
                j := j + 1
            };
            
            "Functions called: " + count
        }
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with learning mode enabled
    println!("\nCreating VM with learning mode...");
    let mut vm = VM::new(bytecode);
    vm.enable_learning_mode();
    vm.set_ast_graph(std::sync::Arc::new(graph));
    
    // Run the program
    println!("Running program...\n");
    match vm.run() {
        Ok(result) => {
            println!("\nSuccess! Result: {:?}", result);
            
            // Print learning mode statistics
            if let Some(stats) = vm.get_learning_statistics() {
                println!("\nLearning Mode Statistics:");
                println!("  Functions analyzed: {}", stats.functions_analyzed);
                println!("  Hot functions: {}", stats.hot_functions);
                println!("  Functions explored: {}", stats.functions_explored);
                println!("  Total variants: {}", stats.total_variants);
                println!("  Currently exploring: {}", stats.exploring_now);
            }
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}