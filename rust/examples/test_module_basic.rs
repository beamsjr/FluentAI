//! Test basic module functionality

use anyhow::Result;
use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, VM};

fn main() -> Result<()> {
    // Test basic module declaration and export
    let source = r#"
        (module math (add multiply)
            (define add (lambda (a b)
                (+ a b)))
            
            (define multiply (lambda (a b)
                (* a b)))
            
            (define internal-helper (lambda (x)
                (* x x))))
    "#;
    
    println!("Testing module declaration...");
    
    // Parse
    let graph = parse(source)?;
    println!("Parsed successfully");
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    println!("Compiled successfully");
    
    // Execute
    let mut vm = VM::new(bytecode);
    vm.enable_trace();
    
    let result = vm.run()?;
    println!("Result: {:?}", result);
    
    Ok(())
}