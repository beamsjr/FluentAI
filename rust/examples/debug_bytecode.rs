use fluentai_parser::parse_flc;
use fluentai_vm::Compiler;

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
    
    println!("=== AST ===");
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
    
    println!("\n=== BYTECODE ===");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("\nChunk {} ({}):", i, chunk.name.as_deref().unwrap_or("unnamed"));
        println!("Constants:");
        for (j, constant) in chunk.constants.iter().enumerate() {
            println!("  [{}]: {:?}", j, constant);
        }
        println!("Instructions:");
        for (j, instruction) in chunk.instructions.iter().enumerate() {
            println!("  {:3}: {:?}", j, instruction);
        }
    }
}