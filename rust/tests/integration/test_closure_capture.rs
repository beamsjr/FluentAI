use fluentai_vm::{compiler::Compiler, vm::VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    println!("=== Closure Capture Test ===\n");

    // Initialize effect context and runtime
    let context = Arc::new(EffectContext::new());
    let runtime = Arc::new(EffectRuntime::new()?);
    
    // Test 1: Simple closure
    println!("Test 1: Simple closure");
    let code = r#"
        (let ((x 10))
          (let ((add-x (lambda (y) (+ x y))))
            (add-x 5)))
    "#;
    run_test("Simple closure", code, context.clone(), runtime.clone())?;
    
    // Test 2: Nested closures
    println!("\nTest 2: Nested closures");
    let code = r#"
        (let ((x 10))
          (let ((make-adder (lambda (n)
                              (lambda (m) (+ (+ n m) x)))))
            (let ((add5 (make-adder 5)))
              (add5 7))))
    "#;
    run_test("Nested closures", code, context.clone(), runtime.clone())?;
    
    // Test 3: Multiple captures
    println!("\nTest 3: Multiple captures");
    let code = r#"
        (let ((a 1)
              (b 2)
              (c 3))
          (let ((sum-all (lambda (d) (+ (+ (+ a b) c) d))))
            (sum-all 4)))
    "#;
    run_test("Multiple captures", code, context.clone(), runtime.clone())?;
    
    // Test 4: Closure mutation (if we had set!)
    println!("\nTest 4: Counter closure");
    let code = r#"
        (let ((counter 0))
          (let ((inc (lambda () (+ counter 1))))
            (inc)))
    "#;
    run_test("Counter closure", code, context.clone(), runtime.clone())?;
    
    println!("\n=== Closure tests completed ===");
    Ok(())
}

fn run_test(name: &str, code: &str, context: Arc<EffectContext>, runtime: Arc<EffectRuntime>) -> anyhow::Result<()> {
    println!("Running: {}", name);
    
    // Parse
    match parse(code) {
        Ok(ast) => {
            println!("  ✓ Parsed");
            
            // Compile
            let compiler = Compiler::new();
            match compiler.compile(&ast) {
                Ok(bytecode) => {
                    println!("  ✓ Compiled");
                    
                    // Create VM
                    let mut vm = VM::new(bytecode);
                    vm.set_effect_context(context);
                    vm.set_effect_runtime(runtime);
                    vm.enable_trace();
                    
                    // Run
                    match vm.run() {
                        Ok(result) => {
                            println!("  ✓ Result: {:?}", result);
                        }
                        Err(e) => {
                            println!("  ✗ Runtime error: {}", e);
                            // Don't propagate error - we expect some tests to fail
                        }
                    }
                }
                Err(e) => {
                    println!("  ✗ Compile error: {}", e);
                }
            }
        }
        Err(e) => {
            println!("  ✗ Parse error: {}", e);
        }
    }
    
    Ok(())
}