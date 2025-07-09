use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

mod test_print;
mod test_readme_features;

fn main() {
    // Test 1: Multiple defines at top level
    println!("Test 1: Multiple top-level defines");
    let code1 = r#"
(define x 10)
(define y 20)
(define sum (+ x y))
sum
"#;
    run_test(code1);
    
    // Test 2: Define functions
    println!("\nTest 2: Define functions");
    let code2 = r#"
(define (square n) (* n n))
(define (cube n) (* n n n))
(+ (square 5) (cube 3))
"#;
    run_test(code2);
    
    // Test 3: Mixed defines and expressions
    println!("\nTest 3: Mixed defines and expressions");
    let code3 = r#"
(define pi 3.14159)
(define (area r) (* pi (* r r)))
(define radius 5.0)
(area radius)
"#;
    run_test(code3);
    
    // Test 4: Using begin explicitly
    println!("\nTest 4: Using begin explicitly");
    let code4 = r#"
(begin
  (define a 1)
  (define b 2)
  (define c 3)
  (+ a b c))
"#;
    run_test(code4);
    
    // Test print functions
    test_print::test_print_functions();
    
    // Test README features
    test_readme_features::test_readme_features();
}

fn run_test(code: &str) {
    // Parse
    let graph = match parse(code) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
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
            eprintln!("Compilation error: {:?}", e);
            return;
        }
    };
    
    // Run
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(result) => {
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("Runtime error: {:?}", e);
        }
    }
}