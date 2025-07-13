use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime, handlers::*};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Effects System Demo ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Register all handlers
    context.register_handler(Arc::new(IOHandler::new()));
    context.register_handler(Arc::new(TimeHandler::new()));
    context.register_handler(Arc::new(StateHandler::new()));
    context.register_handler(Arc::new(RandomHandler::new()));
    
    // Test 1: IO Effect
    println!("Test 1: IO Effect");
    let code = r#"(effect io:println "Hello from FluentAi effects!")"#;
    run_test("IO", code, context.clone(), runtime.clone())?;
    
    // Test 2: Time Effect
    println!("\nTest 2: Time Effect");
    let code = r#"(effect time:now)"#;
    run_test("Time", code, context.clone(), runtime.clone())?;
    
    // Test 3: State Effect
    println!("\nTest 3: State Effect");
    let code = r#"
        (let ((result1 (effect state:get "counter"))
              (result2 (effect state:set "counter" 42))
              (result3 (effect state:get "counter")))
          result3)
    "#;
    run_test("State", code, context.clone(), runtime.clone())?;
    
    // Test 4: Random Effect
    println!("\nTest 4: Random Effect");
    let code = r#"(effect random:int 1 100)"#;
    run_test("Random", code, context.clone(), runtime.clone())?;
    
    // Test 5: Combined Effects
    println!("\nTest 5: Combined Effects with Lambda");
    let code = r#"
        (let ((roll-dice (lambda ()
                          (+ (effect random:int 1 6)
                             (effect random:int 1 6)))))
          (let ((result (roll-dice)))
            (effect io:println (str "Rolled: " result))
            result))
    "#;
    run_test("Combined", code, context.clone(), runtime.clone())?;
    
    println!("\n=== All effect tests completed! ===");
    Ok(())
}

fn run_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    println!("Running: {}", name);
    
    // Parse
    let ast = match parse(code) {
        Ok(ast) => ast,
        Err(e) => {
            println!("  ✗ Parse error: {}", e);
            return Err(e.into());
        }
    };
    println!("  ✓ Parsed");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = match compiler.compile(&ast) {
        Ok(bc) => bc,
        Err(e) => {
            println!("  ✗ Compile error: {}", e);
            return Err(anyhow::anyhow!("Compile error: {}", e));
        }
    };
    println!("  ✓ Compiled");
    
    // Create VM
    let mut vm = VM::new(bytecode);
    vm.set_effect_context(context);
    vm.set_effect_runtime(runtime);
    if name == "IO" {
        vm.enable_trace(); // Debug first test
    }
    
    // Run
    match vm.run() {
        Ok(result) => {
            println!("  ✓ Result: {:?}", result);
        }
        Err(e) => {
            println!("  ✗ Runtime error: {}", e);
            return Err(anyhow::anyhow!("Runtime error: {}", e));
        }
    }
    
    Ok(())
}