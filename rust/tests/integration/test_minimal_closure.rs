use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Minimal closure test - no outer variables
    let code = r#"
        (let ((make-adder (lambda (n) (lambda (m) (+ n m)))))
          make-adder)
    "#;
    
    println!("Testing minimal closure - just return the function");
    println!("Code: {}", code);
    
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nMain chunk bytecode:");
    for (i, inst) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("  {}: {:?}", i, inst);
    }
    
    if bytecode.chunks.len() > 1 {
        println!("\nLambda chunks:");
        for (chunk_idx, chunk) in bytecode.chunks.iter().enumerate().skip(1) {
            println!("  Chunk {}:", chunk_idx);
            for (i, inst) in chunk.instructions.iter().enumerate() {
                println!("    {}: {:?}", i, inst);
            }
        }
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    Ok(())
}