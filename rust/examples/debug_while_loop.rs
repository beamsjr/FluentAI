//! Debug while loop execution

use fluentai_parser::parse_flc;
use fluentai_vm::{VM, Compiler, CompilerOptions};
use fluentai_core::traits::OptimizationLevel;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Debug While Loop ===\n");
    
    // Very simple while loop
    let source = r#"
        {
            let i = 0;
            while (i < 3) {
                perform IO.println("i = " + i);
                i := i + 1;
            }
            "Done, i = " + i
        }
    "#;
    
    // Parse
    println!("Parsing program...");
    let graph = parse_flc(source)?;
    
    // Compile without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Print bytecode
    println!("\nBytecode for main chunk:");
    if let Some(chunk) = bytecode.chunks.get(0) {
        for (i, instr) in chunk.instructions.iter().enumerate() {
            println!("  {:04}: {:?}", i, instr);
        }
    }
    
    // Create VM
    println!("\nRunning program...\n");
    let mut vm = VM::new(bytecode);
    
    // Run with timeout
    use std::time::{Duration, Instant};
    let start = Instant::now();
    let timeout = Duration::from_secs(2);
    
    // Set up a simple execution loop with timeout
    let result = std::thread::spawn(move || {
        vm.run()
    });
    
    // Wait for result or timeout
    loop {
        if start.elapsed() > timeout {
            println!("Execution timed out!");
            return Err("Execution timed out".into());
        }
        
        if result.is_finished() {
            match result.join().unwrap() {
                Ok(val) => {
                    println!("\nSuccess! Result: {:?}", val);
                    return Ok(());
                }
                Err(e) => {
                    println!("\nError: {:?}", e);
                    return Err(e.into());
                }
            }
        }
        
        std::thread::sleep(Duration::from_millis(10));
    }
}