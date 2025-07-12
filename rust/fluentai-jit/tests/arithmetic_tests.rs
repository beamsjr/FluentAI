//! Tests for JIT arithmetic operations

use fluentai_jit::JitCompiler;
use fluentai_parser::parse;
use fluentai_vm::Compiler;
use fluentai_core::value::Value;

#[test]
fn test_multiplication() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    let test_cases = vec![
        ("(* 3 4)", 12),
        ("(* -5 7)", -35),
        ("(* 0 100)", 0),
        ("(* (* 2 3) (* 4 5))", 120),
    ];

    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Integer(n) => assert_eq!(n, expected, "Failed for: {}", source),
            _ => panic!("Expected integer result for: {}", source),
        }
    }
}

#[test]
fn test_division() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    let test_cases = vec![
        ("(/ 12 3)", 4),
        ("(/ 100 -5)", -20),
        ("(/ 15 2)", 7), // Integer division
    ];

    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Integer(n) => assert_eq!(n, expected, "Failed for: {}", source),
            _ => panic!("Expected integer result for: {}", source),
        }
    }
}

#[test]
fn test_modulo() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    let test_cases = vec![
        ("(% 10 3)", 1),
        ("(% 15 4)", 3),
        ("(% 20 5)", 0),
    ];

    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Integer(n) => assert_eq!(n, expected, "Failed for: {}", source),
            _ => panic!("Expected integer result for: {}", source),
        }
    }
}

#[test]
fn test_comparison_operations() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    let test_cases = vec![
        ("(< 3 5)", true),
        ("(< 5 3)", false),
        ("(<= 3 3)", true),
        ("(<= 3 2)", false),
        ("(> 5 3)", true),
        ("(> 3 5)", false),
        ("(>= 3 3)", true),
        ("(>= 2 3)", false),
        ("(= 5 5)", true),
        ("(= 5 6)", false),
        ("(!= 5 6)", true),
        ("(!= 5 5)", false),
    ];

    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Integer(n) => {
                let bool_result = n != 0;
                assert_eq!(bool_result, expected, "Failed for: {}", source);
            }
            Value::Boolean(b) => assert_eq!(b, expected, "Failed for: {}", source),
            _ => panic!("Expected boolean result for: {}", source),
        }
    }
}

#[test]
fn test_boolean_operations() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test AND
    let test_cases = vec![
        ("(and true true)", true),
        ("(and true false)", false),
        ("(and false true)", false),
        ("(and false false)", false),
        ("(and 1 1)", true),
        ("(and 1 0)", false),
        ("(and 0 0)", false),
    ];

    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Integer(n) => {
                let bool_result = n != 0;
                assert_eq!(bool_result, expected, "Failed for: {}", source);
            }
            Value::Boolean(b) => assert_eq!(b, expected, "Failed for: {}", source),
            _ => panic!("Expected boolean result for: {}", source),
        }
    }
}

#[test]
fn test_complex_arithmetic() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Complex expression: (+ (* 3 4) (- 10 (/ 20 5)))
    // = (+ 12 (- 10 4))
    // = (+ 12 6)
    // = 18
    let source = "(+ (* 3 4) (- 10 (/ 20 5)))";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Integer(n) => assert_eq!(n, 18),
        _ => panic!("Expected integer result"),
    }
}