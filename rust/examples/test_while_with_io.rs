//! Test while loop with I/O effects

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== While Loop with I/O Test ===\n");
    
    // While loop that prints values
    let source = r#"
        {
            let i = 0;
            while (i < 3) {
                perform IO.println("Count: " + i);
                i := i + 1
            };
            "Done!"
        }
    "#;
    
    // Parse and compile
    let graph = parse_flc(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and run
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(result) => {
            println!("\nResult: {:?}", result);
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}