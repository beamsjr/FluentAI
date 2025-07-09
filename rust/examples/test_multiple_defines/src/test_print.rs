use fluentai_parser::parse;
use fluentai_vm::{VM, compiler::{Compiler, CompilerOptions}};
use fluentai_optimizer::OptimizationLevel;

pub fn test_print_functions() {
    println!("\n=== Testing print and print-line functions ===");
    
    // Test print (no newline)
    println!("\nTest 1: print function (no newline)");
    let code1 = r#"
(print "Hello")
(print " ")
(print "World")
"#;
    run_test(code1);
    
    // Test print-line (with newline)
    println!("\nTest 2: print-line function (with newline)");
    let code2 = r#"
(print-line "Hello")
(print-line "World")
(print-line (+ 1 2 3))
"#;
    run_test(code2);
    
    // Test mixed
    println!("\nTest 3: Mixed print and print-line");
    let code3 = r#"
(print "Name: ")
(print-line "FluentAI")
(print "Version: ")
(print-line "0.1.0")
"#;
    run_test(code3);
}

fn run_test(code: &str) {
    let graph = parse(code).expect("Parse failed");
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        ..Default::default()
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).expect("Compile failed");
    
    let mut vm = VM::new(bytecode);
    match vm.run() {
        Ok(result) => println!("Result: {:?}", result),
        Err(e) => println!("Error: {:?}", e),
    }
}