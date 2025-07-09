use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

fn main() {
    // Test 1: Simple print
    println!("Test 1: Simple print function");
    let code1 = r#"(print "Hello from print function!")"#;
    test_code(code1);
    
    // Test 2: Print with variable
    println!("\nTest 2: Print with variable");
    let code2 = r#"
(define x 42)
(print x)
"#;
    test_code(code2);
    
    // Test 3: Print with expression
    println!("\nTest 3: Print with expression");
    let code3 = r#"(print (+ 1 2 3))"#;
    test_code(code3);
    
    // Test 4: Multiple prints
    println!("\nTest 4: Multiple prints");
    let code4 = r#"
(print "First")
(print "Second")
(print "Third")
"#;
    test_code(code4);
    
    // Test 5: Compare with effect io:print
    println!("\nTest 5: Compare with effect io:print");
    let code5 = r#"(effect io:print "Hello from effect!")"#;
    test_code(code5);
}

fn test_code(code: &str) {
    // Parse
    let graph = match parse(code) {
        Ok(g) => g,
        Err(e) => {
            println!("Parse error: {:?}", e);
            return;
        }
    };
    
    // Compile without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    
    let bytecode = match compiler.compile(&graph) {
        Ok(b) => b,
        Err(e) => {
            println!("Compilation error: {:?}", e);
            return;
        }
    };
    
    // Run
    let mut vm = VM::new(bytecode);
    
    // The VM should already have stdlib functions available
    // Let's check if print is in the stdlib
    println!("Checking if 'print' is in stdlib...");
    
    match vm.run() {
        Ok(result) => {
            println!("Result: {:?}", result);
        }
        Err(e) => {
            println!("Runtime error: {:?}", e);
        }
    }
}