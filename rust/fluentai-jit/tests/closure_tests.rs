//! Tests for JIT closure support

use fluentai_jit::JitCompiler;
use fluentai_parser::parse;
use fluentai_vm::Compiler;
use fluentai_core::value::Value;

#[test]
fn test_simple_function() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test a simple function definition and call
    let source = "((lambda (x) (+ x 1)) 5)";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    // Since CALL returns dummy value 42 for now
    match result {
        Value::Integer(n) => assert_eq!(n, 42, "Expected dummy result"),
        _ => panic!("Expected integer result"),
    }
}

#[test]
fn test_closure_creation() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test closure that captures a variable
    let source = "(let ((y 10)) (lambda (x) (+ x y)))";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    // This should create a closure
    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Integer(_) => {
            // For now, we return a chunk_id as integer
            // In a real implementation, this would be a Function value
        }
        _ => panic!("Expected closure to be represented as integer for now"),
    }
}

#[test]
fn test_higher_order_function() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test function that returns a function
    let source = "(define (make-adder n) (lambda (x) (+ x n)))";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    // This defines a function
    let result = jit.compile_and_run(&bytecode).unwrap();
    // For now, just check it doesn't crash
    assert!(matches!(result, Value::Nil | Value::Integer(_)));
}