use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime, handlers::*};
use std::sync::Arc;
use tokio;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("=== Testing Spawn Implementation ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register handlers
    context.register_handler(Arc::new(IOHandler::new()));
    context.register_handler(Arc::new(TimeHandler::new()));
    context.register_handler(Arc::new(AsyncHandler::new()));
    context.register_handler(Arc::new(ConcurrentHandler::new()));

    // Test 1: Basic spawn with lambda
    println!("Test 1: Basic spawn with lambda");
    let code = r#"
        (let ((promise (spawn (lambda () (+ 1 2)))))
          (await promise))
    "#;
    run_test("Basic spawn", code, context.clone(), runtime.clone()).await?;

    // Test 2: Spawn with channel communication
    println!("\nTest 2: Spawn with channel communication");
    let code = r#"
        (let ((ch (chan 1)))
          (spawn (lambda () (send! ch 42)))
          (receive! ch))
    "#;
    run_test("Spawn with channel", code, context.clone(), runtime.clone()).await?;

    // Test 3: Multiple spawns
    println!("\nTest 3: Multiple spawns");
    let code = r#"
        (let ((p1 (spawn (lambda () (* 2 3))))
              (p2 (spawn (lambda () (+ 10 5))))
              (p3 (spawn (lambda () (- 20 8)))))
          (list (await p1) (await p2) (await p3)))
    "#;
    run_test("Multiple spawns", code, context.clone(), runtime.clone()).await?;

    // Test 4: Spawn with async effect
    println!("\nTest 4: Spawn with async effect");
    let code = r#"
        (let ((promise (spawn (lambda () (effect async:sleep 10)))))
          (await promise))
    "#;
    run_test("Spawn with async effect", code, context.clone(), runtime.clone()).await?;

    println!("\n=== All spawn tests completed successfully! ===");
    Ok(())
}

async fn run_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    let start = std::time::Instant::now();
    
    // Parse
    let ast = parse(code)?;
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast)?;
    
    // Create VM with effect context and runtime
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    
    // Run
    match vm.run() {
        Ok(result) => {
            let elapsed = start.elapsed();
            println!("✓ {}: {:?} (took {:?})", name, result, elapsed);
        }
        Err(e) => {
            println!("✗ {} failed: {}", name, e);
            return Err(e.into());
        }
    }
    
    Ok(())
}