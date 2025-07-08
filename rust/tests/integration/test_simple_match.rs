use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Simple Match Test ===");
    
    // Test 1: Single pattern
    println!("\nTest 1: Single literal pattern");
    let code = r#"(match 1 (1 "one"))"#;
    
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
    vm.enable_trace();
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    println!("\n\nTest 2: Variable pattern only");
    let code = r#"(match 42 (x x))"#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    Ok(())
}