//! Test string concatenation with numbers

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== String Concatenation Test ===\n");
    
    // Test string concatenation with numbers
    let source = r#"
        let msg = "Result: ";
        let num = 42;
        msg + num
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Program has {} AST nodes", graph.nodes.len());
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM
    println!("\nCreating VM...");
    let mut vm = VM::new(bytecode);
    
    println!("Running program...\n");
    
    // Run the program
    match vm.run() {
        Ok(result) => {
            println!("Success! Result: {:?}", result);
            if let fluentai_core::value::Value::String(s) = &result {
                if s == "Result: 42" {
                    println!("Test PASSED!");
                } else {
                    println!("Test FAILED! Expected 'Result: 42', got '{}'", s);
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