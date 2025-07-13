use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Simple Recursion Test ===");
    
    // For now, test that letrec at least handles non-recursive cases
    println!("\nTest 1: Non-recursive letrec");
    let code = r#"
        (letrec ((x 10)
                 (y 20))
          (+ x y))
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
        Ok(result) => println!("Result: {:?} (expected 30)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Simple function in letrec (not recursive)
    println!("\n\nTest 2: Non-recursive function in letrec");
    let code = r#"
        (letrec ((add-ten (lambda (n) (+ n 10))))
          (add-ten 5))
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