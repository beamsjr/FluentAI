use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Basic Effect System Test ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Test without any effects first
    println!("Test 1: Basic computation without effects");
    let code = "(+ (* 2 3) (- 10 5))";
    run_test("No effects", code, context.clone(), runtime.clone())?;
    
    // Test with let bindings
    println!("\nTest 2: Let bindings");
    let code = r#"
        (let ((x 5)
              (y 10))
          (* x y))
    "#;
    run_test("Let bindings", code, context.clone(), runtime.clone())?;
    
    // Test with lambda
    println!("\nTest 3: Lambda function creation");
    let code = r#"
        (lambda (x y) (+ x y))
    "#;
    run_test("Lambda creation", code, context.clone(), runtime.clone())?;
    
    // Test with lambda call
    println!("\nTest 4: Lambda function call");
    let code = r#"
        ((lambda (x y) (+ x y)) 3 4)
    "#;
    run_test("Lambda call", code, context.clone(), runtime.clone())?;
    
    // Test with simple let + lambda call
    println!("\nTest 5: Let binding with lambda call");
    let code = r#"
        (let ((add (lambda (x y) (+ x y))))
          (add 10 20))
    "#;
    run_test("Let + lambda", code, context.clone(), runtime.clone())?;
    
    println!("\n=== All tests completed successfully! ===");
    Ok(())
}

fn run_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    println!("Running: {}", name);
    
    // Parse
    let ast = parse(code)?;
    println!("  ✓ Parsed");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    let total_instructions: usize = bytecode.chunks.iter().map(|c| c.instructions.len()).sum();
    println!("  ✓ Compiled ({} instructions total)", total_instructions);
    
    // Create VM
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    if name == "Let + lambda" {
        vm.enable_trace(); // Enable for debugging
    }
    
    // Run
    match vm.run() {
        Ok(result) => {
            println!("  ✓ Result: {:?}", result);
        }
        Err(e) => {
            println!("  ✗ Error: {}", e);
            return Err(e);
        }
    }
    
    Ok(())
}