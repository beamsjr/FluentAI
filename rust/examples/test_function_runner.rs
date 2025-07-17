use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler};

fn main() {
    println!("=== Testing Function Definition ===");
    
    let source = include_str!("test_function_def.flc");
    
    // Parse
    let graph = match parse_flc(source) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            return;
        }
    };
    
    println!("Parsed successfully. AST nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!();
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("Compile error: {:?}", e);
            return;
        }
    };
    
    println!("Compiled successfully. Instructions:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}:", i);
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("  {:04}: {:?}", j, inst);
        }
        println!("  Constants:");
        for (j, c) in chunk.constants.iter().enumerate() {
            println!("    [{}]: {:?}", j, c);
        }
    }
    println!();
    
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