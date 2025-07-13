use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime, handlers::IOHandler};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Basic Async Test ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register basic handlers
    context.register_handler(Arc::new(IOHandler::new()));
    
    // Test 1: Simple expression
    println!("Test 1: Simple expression");
    let code = "42";
    run_test("Simple", code, context.clone(), runtime.clone())?;
    
    // Test 2: Basic arithmetic
    println!("\nTest 2: Basic arithmetic");
    let code = "1 + 2";
    run_test("Arithmetic", code, context.clone(), runtime.clone())?;
    
    // Test 3: Let binding
    println!("\nTest 3: Let binding");
    let code = "{ let x = 10; let y = 20; x + y }";
    run_test("Let binding", code, context.clone(), runtime.clone())?;
    
    println!("\n=== All tests completed! ===");
    Ok(())
}

fn run_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    // Parse
    let ast = parse(code)?;
    println!("  Parsed: ✓");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    println!("  Compiled: ✓ ({} instructions)", bytecode.chunks[0].instructions.len());
    
    // Create VM
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    // Run
    match vm.run() {
        Ok(result) => {
            println!("  Result: {:?}", result);
            println!("✓ {} test passed", name);
        }
        Err(e) => {
            println!("✗ {} test failed: {}", name, e);
            return Err(e.into());
        }
    }
    
    Ok(())
}