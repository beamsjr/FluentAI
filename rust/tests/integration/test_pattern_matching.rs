use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Pattern Matching Test ===");
    
    // Test 1: Simple literal matching
    println!("\nTest 1: Literal patterns");
    let code = r#"
        (match 2
          (1 "one")
          (2 "two")
          (3 "three"))
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
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected \"two\")", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Variable pattern
    println!("\n\nTest 2: Variable pattern");
    let code = r#"
        (match 42
          (x (+ x 1)))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 43)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: Wildcard pattern
    println!("\n\nTest 3: Wildcard pattern");
    let code = r#"
        (match "hello"
          (1 "number")
          ("hello" "greeting")
          (_ "other"))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected \"greeting\")", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 4: Boolean patterns
    println!("\n\nTest 4: Boolean patterns");
    let code = r#"
        (match #t
          (#f "false")
          (#t "true"))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected \"true\")", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}