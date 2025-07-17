//! Most basic test of VM Learning Mode

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, LearningModeConfig};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Basic Learning Mode Test ===\n");
    
    // Program with block and assignments that now work!
    let source = r#"
        {
            let x = 5;
            let result = 0;
            let i = 0;
            
            // Simple loop using assignments - reduced iterations
            while (i < 10) {
                result := result + (x + 10 + 20);  // Constant folding opportunity: x + 10 + 20 = 35
                i := i + 1;
            }
            
            "Result: " + result  // Should be 35 * 10 = 350
        }
    "#;
    
    // Parse and compile
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    println!("Program has {} AST nodes", graph.nodes.len());
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Debug: Print bytecode info
    println!("Bytecode info:");
    println!("  Number of chunks: {}", bytecode.chunks.len());
    println!("  Main chunk ID: {}", bytecode.main_chunk);
    
    // Create VM with learning mode
    println!("\nCreating VM with learning mode...");
    let mut vm = VM::new(bytecode);
    
    // Enable basic learning mode
    // vm.enable_learning_mode();  // DISABLED for now
    
    println!("Running program WITHOUT learning mode...\n");
    
    // Run the program
    match vm.run() {
        Ok(result) => {
            println!("\nProgram completed successfully!");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
            return Err(e.into());
        }
    }
    
    // Get learning statistics
    if let Some(stats) = vm.get_learning_statistics() {
        println!("\n=== Learning Mode Statistics ===");
        println!("Functions analyzed: {}", stats.functions_analyzed);
        println!("Hot functions found: {}", stats.hot_functions);
    }
    
    Ok(())
}