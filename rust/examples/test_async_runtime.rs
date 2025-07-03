use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime, handlers::*};
use std::sync::Arc;
use tokio;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("=== Async Runtime Tests ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register handlers
    context.register_handler(Arc::new(IOHandler::new()));
    context.register_handler(Arc::new(TimeHandler::new()));
    context.register_handler(Arc::new(NetworkHandler::new()));
    context.register_handler(Arc::new(AsyncHandler::new()));
    context.register_handler(Arc::new(ConcurrentHandler::new()));

    // Test 1: Basic async operation
    println!("Test 1: Basic async effect");
    let code = r#"
        (effect async:sleep 100)
    "#;
    run_async_test("Basic async", code, context.clone(), runtime.clone()).await?;

    // Test 2: Await a promise
    println!("\nTest 2: Await promise");
    let code = r#"
        (let ((p (effect async:sleep 50)))
          (await p))
    "#;
    run_async_test("Await promise", code, context.clone(), runtime.clone()).await?;

    // Test 3: Multiple async operations
    println!("\nTest 3: Multiple async operations");
    let code = r#"
        (let ((p1 (effect async:sleep 20))
              (p2 (effect async:sleep 30))
              (p3 (effect async:sleep 10)))
          (list (await p1) (await p2) (await p3)))
    "#;
    run_async_test("Multiple async", code, context.clone(), runtime.clone()).await?;

    // Test 4: Concurrent channels
    println!("\nTest 4: Channel communication");
    let code = r#"
        (let ((ch (chan 10)))
          (spawn (lambda () 
            (send! ch 42)
            (send! ch 84)))
          (list (receive! ch) (receive! ch)))
    "#;
    run_async_test("Channels", code, context.clone(), runtime.clone()).await?;

    // Test 5: Select statement
    println!("\nTest 5: Select statement (simulated)");
    let code = r#"
        (let ((ch1 (chan 1))
              (ch2 (chan 1)))
          (spawn (lambda () (send! ch1 "from-ch1")))
          (spawn (lambda () (send! ch2 "from-ch2")))
          (receive! ch1))
    "#;
    run_async_test("Select", code, context.clone(), runtime.clone()).await?;

    // Test 6: Error handling in async
    println!("\nTest 6: Async error handling");
    let code = r#"
        (handle
          (effect async:error "Async operation failed")
          ((error e) (str "Caught: " e)))
    "#;
    run_async_test("Async errors", code, context.clone(), runtime.clone()).await?;

    // Test 7: Network simulation
    println!("\nTest 7: Network request simulation");
    let code = r#"
        (let ((response (effect network:get "https://api.example.com/data")))
          (await response))
    "#;
    run_async_test("Network", code, context.clone(), runtime.clone()).await?;

    // Test 8: Complex async workflow
    println!("\nTest 8: Complex async workflow");
    let code = r#"
        (let ((fetch-user (lambda (id)
                           (effect network:get (str "https://api.example.com/user/" id))))
              (fetch-posts (lambda (user-id)
                            (effect network:get (str "https://api.example.com/posts?user=" user-id)))))
          (let ((user (await (fetch-user 123))))
            (await (fetch-posts (get user :id)))))
    "#;
    run_async_test("Complex workflow", code, context.clone(), runtime.clone()).await?;

    // Test 9: Spawn multiple tasks
    println!("\nTest 9: Spawn multiple concurrent tasks");
    let code = r#"
        (let ((results (chan 3)))
          (spawn (lambda () (send! results (+ 1 2))))
          (spawn (lambda () (send! results (* 3 4))))
          (spawn (lambda () (send! results (- 10 5))))
          (list (receive! results) (receive! results) (receive! results)))
    "#;
    run_async_test("Multiple spawns", code, context.clone(), runtime.clone()).await?;

    // Test 10: Promise chaining
    println!("\nTest 10: Promise chaining");
    let code = r#"
        (let ((p1 (effect async:sleep 10))
              (process (lambda (v) (* v 2))))
          (await (then p1 process)))
    "#;
    run_async_test("Promise chaining", code, context.clone(), runtime.clone()).await?;

    println!("\n=== All async tests completed successfully! ===");
    Ok(())
}

async fn run_async_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
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