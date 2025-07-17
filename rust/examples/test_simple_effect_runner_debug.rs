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
    
    println!("\nBytecode chunks: {}", bytecode.chunks.len());
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("\n=== Chunk {} ===", i);
        println!("Constants: {:?}", chunk.constants);
        println!("Instructions:");
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("{:3}: {:?}", j, inst);
        }
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