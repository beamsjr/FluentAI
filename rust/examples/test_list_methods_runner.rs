use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() {
    println!("=== Testing List Methods ===");
    
    let source = include_str!("test_list_methods.flc");
    
    // Parse
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            return;
        }
    };
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Compile error: {:?}", e);
            return;
        }
    };
    
    // Run
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => {
            println!("\n✅ Execution succeeded");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            println!("\n❌ Runtime error: {:?}", e);
        }
    }
}