use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

pub fn test_readme_features() {
    println!("\n=== Testing README Status Items ===");
    
    // Test 1: Recursion (factorial)
    println!("\nTest 1: Recursion (factorial)");
    let recursion_test = r#"
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 5)
"#;
    run_test("Factorial", recursion_test);
    
    // Test 2: Pattern Matching with Cons/Nil
    println!("\nTest 2: Pattern Matching with Cons/Nil");
    let pattern_test = r#"
(define lst (list 1 2 3))
(match lst
  (Nil "empty")
  ((Cons x xs) x))
"#;
    run_test("Pattern matching", pattern_test);
    
    // Test 3: Custom Effect Handlers
    println!("\nTest 3: Custom Effect Handlers");
    let handler_test = r#"
(handler ((error (lambda (e) 99)))
  (effect error:raise "test error"))
"#;
    run_test("Custom handler", handler_test);
    
    // Test 4: Module System
    println!("\nTest 4: Module System");
    let module_test = r#"
(module test-module
  (export foo)
  (define (foo x) (* x 2)))
"#;
    run_test("Module definition", module_test);
    
    // Test 5: Multiple expressions in let body
    println!("\nTest 5: Multiple expressions in let body");
    let let_body_test = r#"
(let ((x 10))
  (print-line "First expression")
  (print-line "Second expression")
  x)
"#;
    run_test("Let with multiple expressions", let_body_test);
}

fn run_test(name: &str, code: &str) {
    match parse(code) {
        Ok(graph) => {
            let options = CompilerOptions {
                optimization_level: OptimizationLevel::None,
                ..Default::default()
            };
            let compiler = Compiler::with_options(options);
            
            match compiler.compile(&graph) {
                Ok(bytecode) => {
                    let mut vm = VM::new(bytecode);
                    match vm.run() {
                        Ok(result) => println!("{}: SUCCESS - {:?}", name, result),
                        Err(e) => println!("{}: RUNTIME ERROR - {:?}", name, e),
                    }
                }
                Err(e) => println!("{}: COMPILE ERROR - {:?}", name, e),
            }
        }
        Err(e) => println!("{}: PARSE ERROR - {:?}", name, e),
    }
}