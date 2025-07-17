use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() {
    println!("=== Testing Printable Function ===");
    
    let source = include_str!("test_printable.flc");
    
    // Parse
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            return;
        }
    };
    
    println!("Parsed successfully");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Compile error: {:?}", e);
            return;
        }
    };
    
    println!("Compiled successfully");
    
    // Run
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => {
            println!("✅ Execution succeeded");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            println!("❌ Runtime error: {:?}", e);
        }
    }
}