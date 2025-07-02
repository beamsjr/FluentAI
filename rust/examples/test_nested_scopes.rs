use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Nested Scopes Test ===");
    
    // Test 1: Simple let inside let
    println!("\nTest 1: Let inside let");
    let code = r#"
        (let ((x 10))
          (let ((y 20))
            (+ x y)))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 30)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Let wrapping letrec
    println!("\n\nTest 2: Let wrapping letrec");
    let code = r#"
        (let ((x 10))
          (letrec ((f (lambda () x)))
            (f)))
    "#;
    
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
        Ok(result) => println!("\nResult: {:?} (expected 10)", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    Ok(())
}