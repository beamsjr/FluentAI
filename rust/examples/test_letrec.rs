use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    println!("=== Letrec Test ===");
    
    // Test 1: Simple recursive factorial
    println!("\nTest 1: Factorial");
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
    
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 120)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Mutually recursive even/odd
    println!("\n\nTest 2: Mutually recursive even/odd");
    let code = r#"
        (letrec ((even? (lambda (n)
                          (if (= n 0)
                              #t
                              (odd? (- n 1)))))
                 (odd? (lambda (n)
                         (if (= n 0)
                             #f
                             (even? (- n 1))))))
          (even? 4))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context.clone());
    vm.set_effect_runtime(runtime.clone());
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected true)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: Recursive with closure
    println!("\n\nTest 3: Recursive with closure");
    let code = r#"
        (let ((x 10))
          (letrec ((countdown (lambda (n)
                                (if (= n 0)
                                    x
                                    (countdown (- n 1))))))
            (countdown 5)))
    "#;
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("Result: {:?} (expected 10)", result),
        Err(e) => println!("Error: {}", e),
    }
    
    Ok(())
}