use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    // Even simpler test
    let code = r#"
        (let ((x 10))
          (let ((y 20))
            y))
    "#;
    
    println!("Testing nested let with simple variable lookup");
    
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