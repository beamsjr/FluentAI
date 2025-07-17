//! Test simple while loop that should work

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Simple While Loop Test ===\n");
    
    // Use a pattern that doesn't require mutable captured variables
    let source = r#"
        {
            let rec loop_fn = (i) => {
                if (i < 3) {
                    perform IO.println("i = " + i);
                    loop_fn(i + 1)
                } else {
                    "Done!"
                }
            };
            loop_fn(0)
        }
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and run
    println!("\nRunning program...\n");
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(result) => {
            println!("\nSuccess! Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}