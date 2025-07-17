use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() {
    let source = r#"
// Simple add function
private function add(x: int, y: int) -> int {
    x + y
}

// Test
add(5, 3)
    "#;
    
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
    
    println!("=== BYTECODE ===");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("\nChunk {} ({}):", i, chunk.name.as_deref().unwrap_or("unnamed"));
        println!("Instructions:");
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("  {:3}: {:?}", j, inst);
        }
    }
    
    // Run and trace key points
    let mut vm = VM::new(bytecode);
    
    println!("\n=== EXECUTION ===");
    match vm.run() {
        Ok(result) => {
            println!("Result: {:?}", result);
        }
        Err(e) => {
            println!("Error: {:?}", e);
        }
    }
}