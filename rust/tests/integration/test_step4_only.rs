use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    let code = r#"
        (let ((x 10))
          (let ((make-adder (lambda (n)
                              (lambda (m) (+ (+ n m) x)))))
            (let ((add5 (make-adder 5)))
              (add5 7))))
    "#;
    
    println!("Testing Step 4 with outer variable x");
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("\nBytecode:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}:", i);
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, inst);
        }
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    match vm.run() {
        Ok(result) => println!("\nResult: {:?}", result),
        Err(e) => println!("\nError: {}", e),
    }
    
    println!("\nExpected: 22 (5 + 7 + 10)");
    
    Ok(())
}