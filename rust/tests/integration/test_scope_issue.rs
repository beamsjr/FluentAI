use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    // Test looking up variable from outer scope
    let code = r#"
        (let ((x 10))
          (let ((y 20))
            x))
    "#;
    
    println!("Testing lookup of outer scope variable");
    
    // Parse
    let ast = parse(code)?;
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Print bytecode
    println!("\nBytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    println!("\nExpected behavior:");
    println!("  Stack after outer let: [10]");
    println!("  Stack after inner let: [10, 20]");
    println!("  Looking up 'x' should Load from position 0");
    
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Run
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