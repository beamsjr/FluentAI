use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Load/Store Test ===");
    
    // Test 1: Simple let binding
    println!("\nTest 1: Simple let");
    let code = r#"(let ((x 42)) x)"#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Function with parameters
    println!("\n\nTest 2: Function with parameters");
    let code = r#"((lambda (x y) (+ x y)) 10 20)"#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nMain bytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    if bytecode.chunks.len() > 1 {
        println!("\nLambda bytecode:");
        for (i, inst) in bytecode.chunks[1].instructions.iter().enumerate() {
            println!("  {}: {:?}", i, inst);
        }
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}