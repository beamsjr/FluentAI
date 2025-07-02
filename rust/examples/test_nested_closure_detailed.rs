use claudelang_parser::parse;
use claudelang_vm::{compiler::Compiler, vm::VM};
use claudelang_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Test the nested closure case step by step
    println!("=== Testing Nested Closures ===\n");
    
    // Step 1: Simple function that returns a function
    let code1 = r#"
        (let ((make-adder (lambda (n) (lambda (m) (+ n m)))))
          (make-adder 5))
    "#;
    
    println!("Step 1: make-adder returns a function");
    println!("Code: {}", code1);
    test_code(code1, context.clone(), runtime.clone())?;
    
    // Step 2: Call the returned function
    let code2 = r#"
        (let ((make-adder (lambda (n) (lambda (m) (+ n m)))))
          (let ((add5 (make-adder 5)))
            add5))
    "#;
    
    println!("\nStep 2: Store returned function in add5");
    println!("Code: {}", code2);
    test_code(code2, context.clone(), runtime.clone())?;
    
    // Step 3: Actually call add5
    let code3 = r#"
        (let ((make-adder (lambda (n) (lambda (m) (+ n m)))))
          (let ((add5 (make-adder 5)))
            (add5 7)))
    "#;
    
    println!("\nStep 3: Call add5 with 7");
    println!("Code: {}", code3);
    test_code(code3, context.clone(), runtime.clone())?;
    
    // Step 4: The full test with x from outer scope
    let code4 = r#"
        (let ((x 10))
          (let ((make-adder (lambda (n)
                              (lambda (m) (+ (+ n m) x)))))
            (let ((add5 (make-adder 5)))
              (add5 7))))
    "#;
    
    println!("\nStep 4: Full test with outer variable x");
    println!("Code: {}", code4);
    test_code(code4, context.clone(), runtime.clone())?;
    
    Ok(())
}

fn test_code(code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    let ast = parse(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    println!("Bytecode:");
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("  Chunk {}: {}", i, chunk.name.as_ref().unwrap_or(&"unnamed".to_string()));
        for (j, inst) in chunk.instructions.iter().enumerate() {
            println!("    {}: {:?}", j, inst);
        }
    }
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    vm.enable_trace();
    
    match vm.run() {
        Ok(result) => println!("Result: {:?}\n", result),
        Err(e) => println!("Error: {}\n", e),
    }
    
    Ok(())
}