//! Test proper cell usage for mutable variables

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Cell Test ===\n");
    
    // Test with proper mutable variable usage in closures
    let source = r#"
        // Create a counter using a closure with mutable state
        let make_counter = () => {
            let count = 0;  // This will be captured by the closure
            () => {
                count := count + 1;  // This should create a cell
                count
            }
        };
        
        let counter = make_counter();
        let a = counter();  // Should return 1
        let b = counter();  // Should return 2
        let c = counter();  // Should return 3
        
        a + b + c  // Should return 6
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
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}