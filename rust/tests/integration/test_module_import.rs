//! Test module import functionality

use anyhow::Result;
use fluentai_vm::{compiler::Compiler, VM};

fn main() -> Result<()> {
    // Test simple import and qualified variable
    let source = r#"
        (import "math" (sin cos))
        (sin 1.5)
    "#;
    
    println!("Testing module import...");
    
    // Parse
    let graph = parse(source)?;
    println!("Parsed successfully");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    println!("Compiled successfully");
    
    // Execute
    let mut vm = VM::new(bytecode);
    vm.enable_trace();
    
    let result = vm.run()?;
    println!("Result: {:?}", result);
    
    Ok(())
}