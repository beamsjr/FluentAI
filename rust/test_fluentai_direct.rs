// Direct test of FluentAI components
use fluentai_parser::parse;
use fluentai_vm::{Compiler, CompilerOptions, VM};
use fluentai_optimizer::OptimizationLevel;

fn main() {
    // Test 1: Simple expression
    let code = "(+ 1 2)";
    println!("Testing: {}", code);
    
    match test_code(code) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 2: Define and print
    let code2 = r#"
        (define x 42)
        (print x)
        x
    "#;
    println!("\nTesting define:");
    
    match test_code(code2) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
    
    // Test 3: List operations
    let code3 = r#"
        (define lst [1 2 3 4 5])
        (map (lambda (x) (* x 2)) lst)
    "#;
    println!("\nTesting map:");
    
    match test_code(code3) {
        Ok(result) => println!("Result: {}", result),
        Err(e) => println!("Error: {}", e),
    }
}

fn test_code(code: &str) -> anyhow::Result<fluentai_vm::bytecode::Value> {
    // Parse
    let ast = parse(code)?;
    println!("Parsed AST: {:?}", ast);
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&ast)?;
    println!("Compiled bytecode");
    
    // Execute
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    Ok(result)
}