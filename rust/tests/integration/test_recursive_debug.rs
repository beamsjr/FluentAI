use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Recursive Debug ===");
    
    // Test 1: Self-reference without capture
    println!("\nTest 1: Direct self-reference");
    let code = r#"
        (letrec ((f (lambda (n)
                      (if (= n 0)
                          0
                          (f (- n 1))))))
          (f 3))
    "#;
    
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
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?} (expected 0)", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    // Test 2: Factorial
    println!("\n\nTest 2: Factorial");
    let code = r#"
        (letrec ((fact (lambda (n)
                         (if (= n 0)
                             1
                             (* n (fact (- n 1)))))))
          (fact 5))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    Ok(())
}