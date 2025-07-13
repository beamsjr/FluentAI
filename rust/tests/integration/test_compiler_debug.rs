use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    // Simple test to understand variable positions
    let code = r#"
        (let ((x 10))
          x)
    "#;
    
    println!("Test 1: Simple variable lookup");
    println!("Code: {}", code);
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("Bytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    // Test 2: Nested let
    let code2 = r#"
        (let ((x 10))
          (let ((y 20))
            x))
    "#;
    
    println!("\nTest 2: Looking up outer variable from inner let");
    println!("Code: {}", code2);
    
    let ast2 = parse(code2)?;
    let compiler2 = Compiler::new();
    let bytecode2 = compiler2.compile(&ast2)?;
    
    println!("Bytecode:");
    for (i, inst) in bytecode2.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    // Test 3: The problematic case
    let code3 = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) y)))
            add-x))
    "#;
    
    println!("\nTest 3: Looking up function in inner let");
    println!("Code: {}", code3);
    
    let ast3 = parse(code3)?;
    let compiler3 = Compiler::new();
    let bytecode3 = compiler3.compile(&ast3)?;
    
    println!("Bytecode:");
    for (i, inst) in bytecode3.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    println!("\nExpected stack positions:");
    println!("  After x=10: stack=[10], x at pos 0");
    println!("  After add-x=lambda: stack=[10, func], x at pos 0, add-x at pos 1");
    println!("  When looking up add-x: should generate Load 1");
    
    // Test 4: The actual failing case
    let code4 = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) y)))
            (add-x 5)))
    "#;
    
    println!("\nTest 4: Function application in inner let");
    println!("Code: {}", code4);
    
    let ast4 = parse(code4)?;
    let compiler4 = Compiler::new();
    let bytecode4 = compiler4.compile(&ast4)?;
    
    println!("Bytecode:");
    for (i, inst) in bytecode4.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    println!("\nStack trace during application (add-x 5):");
    println!("  1. Compile arg 5: stack=[10, func, 5]");
    println!("  2. Compile add-x: should load from pos 1, but loads from pos ?");
    
    Ok(())
}