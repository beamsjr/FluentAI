use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== List Operations Test ===");
    
    // Test 1: car/head
    println!("\nTest 1: car/head");
    let code = r#"(car (list 1 2 3))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 1)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: cdr/tail
    println!("\nTest 2: cdr/tail");
    let code = r#"(cdr (list 1 2 3))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected [2, 3])", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: cons
    println!("\nTest 3: cons");
    let code = r#"(cons 0 (list 1 2 3))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected [0, 1, 2, 3])", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 4: Nested operations
    println!("\nTest 4: Nested operations");
    let code = r#"(car (cdr (cons 0 (list 1 2 3))))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 1)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 5: Alternative names
    println!("\nTest 5: Alternative names (head/tail/first/rest)");
    let code = r#"(head (tail (list 1 2 3)))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 2)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 6: Empty list error handling
    println!("\nTest 6: Empty list error handling");
    let code = r#"(car (list))"#;
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    match vm.run() {
        Ok(result) => println!("Result: {:?} (unexpected success)", result),
        Err(e) => println!("Error (expected): {}", e),
    }
    
    Ok(())
}