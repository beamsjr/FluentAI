//! Test without loops

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== No Loop Test ===\n");
    
    // Simple program without loops
    let source = r#"
        {
            let x = 10;
            let y = 20;
            x := x + 5;
            y := y + x;
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
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}