use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM, bytecode::Instruction};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Simple closure test
    let code = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) (+ x y))))
            (add-x 5)))
    "#;
    
    println!("Code: {}", code);
    
    // Parse
    let ast = parse(code)?;
    println!("AST: {:#?}", ast);
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Print bytecode
    println!("\nBytecode:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {} ({}):", i, chunk.name.as_ref().unwrap_or(&"unnamed".to_string()));
        println!("  Constants: {:?}", chunk.constants);
        println!("  Instructions:");
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("    {}: {:?}", j, inst);
        }
    }
    
    // Trace compilation by re-compiling with debug
    println!("\nTracing compilation:");
    let mut debug_compiler = Compiler::new();
    println!("Initial stack depth: 0");
    
    // Step through main chunk compilation
    println!("  PushIntSmall 10 -> stack depth: 1 (x=10)");
    println!("  Load 0 -> stack depth: 2 (x=10, x=10)"); 
    println!("  MakeClosure -> stack depth: 2 (x=10, func)");
    println!("  Now in inner let: x at pos 0, add-x at pos 1");
    println!("  PushIntSmall 5 -> stack depth: 3 (x=10, func, 5)");
    println!("  Load ? should load add-x which is at position 1");
    
    // Create VM and run
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