//! Debug while loop bytecode generation

use fluentai_parser::parse_flc;
use fluentai_vm::{Compiler, CompilerOptions};
use fluentai_core::traits::OptimizationLevel;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Debug While Loop Bytecode ===\n");
    
    // Test 1: Simple while loop without outer let
    let source1 = r#"
        while (false) {
            perform IO.println("Should not print")
        }
    "#;
    
    // Test 2: While loop with cell variable
    let source2 = r#"
        {
            let rec i = 0;
            while (i < 3) {
                perform IO.println("i = " + i);
                i := i + 1
            }
        }
    "#;
    
    let source = r#"
        {
            let i = 0;
            while (i < 3) {
                perform IO.println("i = " + i);
                i := i + 1
            }
        }
    "#; // Back to original test
    
    // Parse
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    
    // Print the AST to understand structure
    println!("\nAST Structure:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Find the while loop node (should be a Letrec)
    println!("\nLooking for Letrec nodes:");
    for (id, node) in &graph.nodes {
        if let fluentai_core::ast::Node::Letrec { bindings, body } = node {
            println!("Found Letrec at {:?}:", id);
            println!("  Bindings:");
            for (name, node_id) in bindings {
                println!("    {} = {:?}", name, node_id);
            }
            println!("  Body: {:?}", body);
        }
    }
    
    // Compile without optimization
    println!("\nCompiling...");
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    
    match compiler.compile(&graph) {
        Ok(bytecode) => {
            println!("\nCompilation succeeded!");
            println!("Generated {} chunks", bytecode.chunks.len());
            
            // Print bytecode for each chunk
            for (i, chunk) in bytecode.chunks.iter().enumerate() {
                println!("\nChunk {} ({}):", i, chunk.name.as_deref().unwrap_or("unnamed"));
                for (j, instr) in chunk.instructions.iter().enumerate() {
                    println!("  {:04}: {:?}", j, instr);
                }
            }
        }
        Err(e) => {
            println!("\nCompilation error: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}