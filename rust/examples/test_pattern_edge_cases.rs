use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Pattern Matching Edge Cases ===");
    
    // Test 1: No matching pattern (should return nil)
    println!("\nTest 1: No matching pattern");
    let code = r#"
        (match 4
          (1 "one")
          (2 "two")
          (3 "three"))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected Nil)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Variable pattern with expression
    println!("\n\nTest 2: Variable pattern with expression");
    let code = r#"
        (match (+ 40 2)
          (x (* x 2)))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 84)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: Nested match
    println!("\n\nTest 3: Nested match");
    let code = r#"
        (match 1
          (1 (match 2
               (2 "nested match"))))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected \"nested match\")", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 4: Multiple variable bindings
    println!("\n\nTest 4: Match with let");
    let code = r#"
        (let ((a 10))
          (match 5
            (x (+ a x))))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 15)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}