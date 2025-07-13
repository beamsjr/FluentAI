use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Test let binding positions
    let code = r#"
        (let ((a 1))
          (let ((b 2))
            (let ((c 3))
              (+ (+ a b) c))))
    "#;
    
    println!("Testing let binding positions");
    println!("Code: {}", code);
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    println!("\nExpected positions:");
    println!("  After (let ((a 1)): stack=[1], a at pos 0");
    println!("  After (let ((b 2)): stack=[1, 2], a at pos 0, b at pos 1");  
    println!("  After (let ((c 3)): stack=[1, 2, 3], a at pos 0, b at pos 1, c at pos 2");
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    vm.enable_trace();
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    Ok(())
}