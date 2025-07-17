//! Test simple letrec

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, CompilerOptions};
use fluentai_core::traits::OptimizationLevel;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Test Simple Letrec ===\n");
    
    // Simple recursive function
    let source = r#"
        {
            let rec f = (n) => {
                if (n <= 0) {
                    0
                } else {
                    n + f(n - 1)
                }
            };
            f(3)
        }
    "#;
    
    // Parse
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Parsed {} nodes", graph.nodes.len());
    
    // Print AST
    println!("\nAST nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Compile without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM and run
    println!("\nRunning program...");
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(val) => {
            println!("\nSuccess! Result: {:?}", val);
        }
        Err(e) => {
            println!("\nError: {:?}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}