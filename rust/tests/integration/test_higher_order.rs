//! Test higher-order functions with stdlib integration

use anyhow::Result;
use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM};

fn test_expr(expr: &str, desc: &str) -> Result<()> {
    println!("\n{}: {}", desc, expr);
    
    let graph = parse(expr)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    println!("Result: {}", result);
    Ok(())
}

fn main() -> Result<()> {
    println!("Testing Higher-Order Functions in FluentAi");
    println!("===========================================");
    
    // Test map
    test_expr(
        "(map (lambda (x) (* x 2)) [1 2 3 4 5])",
        "Map: double each element"
    )?;
    
    // Test filter
    test_expr(
        "(filter (lambda (x) (> x 3)) [1 2 3 4 5 6])",
        "Filter: keep elements > 3"
    )?;
    
    // Test fold with lambda for now (+ is a builtin, not a function)
    test_expr(
        "(fold (lambda (acc x) (+ acc x)) 0 [1 2 3 4 5])",
        "Fold: sum of list"
    )?;
    
    // Test fold with lambda
    test_expr(
        "(fold (lambda (acc x) (+ acc (* x x))) 0 [1 2 3 4])",
        "Fold: sum of squares"
    )?;
    
    // Test nested higher-order functions
    test_expr(
        "(map (lambda (x) (* x x)) (filter (lambda (x) (even? x)) [1 2 3 4 5 6 7 8]))",
        "Nested: square of even numbers"
    )?;
    
    // Test with string operations
    test_expr(
        "(map string-upcase [\"hello\" \"world\" \"test\"])",
        "Map strings to uppercase"
    )?;
    
    // Test with more complex lambda
    test_expr(
        "(filter (lambda (x) (and (> x 10) (< x 50))) [5 15 25 35 45 55 65])",
        "Filter: numbers between 10 and 50"
    )?;
    
    println!("\nAll higher-order function tests completed!");
    Ok(())
}