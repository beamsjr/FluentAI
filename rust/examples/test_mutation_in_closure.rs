//! Test mutable variables in closures (where cells should be used)

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Mutation in Closure Test ===\n");
    
    // Test mutable variables captured by closures
    let source = r#"
        // Create a counter that uses mutable state
        let make_counter = () => {
            let count = 0;
            
            // Return a closure that captures and mutates count
            () => {
                // This should create a cell for count
                count := count + 1;
                count
            }
        };
        
        let counter = make_counter();
        let a = counter();  // Should be 1
        let b = counter();  // Should be 2
        let c = counter();  // Should be 3
        
        "Results: " + a + ", " + b + ", " + c
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Program has {} AST nodes", graph.nodes.len());
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    println!("Bytecode info:");
    println!("  Number of chunks: {}", bytecode.chunks.len());
    
    // Create VM
    println!("\nCreating VM...");
    let mut vm = VM::new(bytecode);
    
    println!("Running program...\n");
    
    // Run the program
    match vm.run() {
        Ok(result) => {
            println!("Success! Result: {:?}", result);
            if let fluentai_core::value::Value::String(s) = &result {
                if s == "Results: 1, 2, 3" {
                    println!("Test PASSED!");
                } else {
                    println!("Test FAILED! Expected 'Results: 1, 2, 3', got '{}'", s);
                }
            }
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}