use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime, handlers::*};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Working Effects Demo ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register all handlers
    context.register_handler(Arc::new(IOHandler::new()));
    context.register_handler(Arc::new(TimeHandler::new()));
    context.register_handler(Arc::new(StateHandler::new()));
    context.register_handler(Arc::new(RandomHandler::new()));
    
    // Test 1: Print effect (defaults to IO)
    println!("Test 1: Print Effect");
    let code = r#"(effect println "Hello from FluentAi!")"#;
    run_test(code, context.clone(), runtime.clone())?;
    
    // Test 2: Effects in sequence  
    println!("\nTest 2: Effects in Sequence");
    let code = r#"
        (do
          (effect println "First message")
          (effect println "Second message")
          42)
    "#;
    run_test(code, context.clone(), runtime.clone())?;
    
    // Test 3: Simple lambda
    println!("\nTest 3: Simple Lambda");
    let code = r#"
        (let ((add (lambda (x y) (+ x y))))
          (add 10 32))
    "#;
    run_test(code, context.clone(), runtime.clone())?;
    
    println!("\n=== Effects system is working! ===");
    Ok(())
}

fn run_test(code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    // Parse
    let ast = parse(code)?;
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Create VM
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    // Run
    match vm.run() {
        Ok(result) => {
            println!("  Result: {:?}", result);
        }
        Err(e) => {
            println!("  Error: {}", e);
            return Err(e.into());
        }
    }
    
    Ok(())
}