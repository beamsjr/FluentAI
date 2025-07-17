//! Test simple variable assignment within let body

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Simple Assignment Test ===\n");
    
    // Test assignment within a block
    let source = r#"
        // Use a block with let statements
        {
            let x = 10;
            let y = 20;
            
            // Now we can assign to x and y
            x := 15;
            y := x + y;
            "x=" + x + ", y=" + y
        }
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
                if s == "x=15, y=35" {
                    println!("Test PASSED!");
                } else {
                    println!("Test FAILED! Expected 'x=15, y=35', got '{}'", s);
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