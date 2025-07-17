use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() {
    println!("=== Testing Simple Effect ===");
    
    let source = include_str!("test_simple_effect.flc");
    
    // Parse
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            return;
        }
    };
    
    println!("AST nodes: {}", graph.nodes.len());
    for (id, node) in &graph.nodes {
        println!("{:?}: {:?}", id, node);
    }
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Compile error: {:?}", e);
            return;
        }
    };
    
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("{:3}: {:?}", i, inst);
    }
    
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