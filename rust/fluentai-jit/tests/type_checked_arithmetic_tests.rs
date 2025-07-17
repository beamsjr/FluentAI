//! Tests for type-checked arithmetic operations in the JIT

use fluentai_jit::JitCompiler;
use fluentai_parser::parse_flc;
use fluentai_vm::Compiler;
use fluentai_core::value::Value;

#[test]
fn test_integer_arithmetic() {
    let test_cases = vec![
        ("10 + 5", Value::Integer(15)),
        ("10 - 5", Value::Integer(5)),
        ("10 * 5", Value::Integer(50)),
        ("10 / 5", Value::Integer(2)),
        ("10 % 3", Value::Integer(1)),
    ];

    for (source, expected) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        assert_eq!(result, expected, "Failed for: {}", source);
    }
}

#[test]
fn test_float_arithmetic() {
    let test_cases = vec![
        ("10.5 + 5.5", Value::Float(16.0)),
        ("10.5 - 5.5", Value::Float(5.0)),
        ("10.5 * 2.0", Value::Float(21.0)),
        ("10.5 / 2.0", Value::Float(5.25)),
    ];

    for (source, expected) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match (result, expected) {
            (Value::Float(a), Value::Float(b)) => {
                assert!((a - b).abs() < 0.0001, "Failed for: {} (expected {}, got {})", source, b, a);
            }
            _ => panic!("Expected float result for: {}", source),
        }
    }
}

#[test]
fn test_mixed_type_arithmetic() {
    let test_cases = vec![
        ("10 + 5.5", Value::Float(15.5)),
        ("10.5 + 5", Value::Float(15.5)),
        ("10 - 2.5", Value::Float(7.5)),
        ("10.5 - 2", Value::Float(8.5)),
        ("10 * 2.5", Value::Float(25.0)),
        ("10.5 * 2", Value::Float(21.0)),
        ("10 / 2.5", Value::Float(4.0)),
        ("10.5 / 2", Value::Float(5.25)),
    ];

    for (source, expected) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match (result, expected) {
            (Value::Float(a), Value::Float(b)) => {
                assert!((a - b).abs() < 0.0001, "Failed for: {} (expected {}, got {})", source, b, a);
            }
            _ => panic!("Expected float result for: {}", source),
        }
    }
}

#[test]
fn test_string_addition() {
    let test_cases = vec![
        (r#""Hello, " + "World!""#, Value::String("Hello, World!".to_string())),
        (r#""foo" + "bar""#, Value::String("foobar".to_string())),
    ];

    for (source, expected) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        assert_eq!(result, expected, "Failed for: {}", source);
    }
}

#[test]
fn test_division_by_zero() {
    let test_cases = vec![
        "10 / 0",
        "10.5 / 0.0",
        "10 / 0.0",
        "10.0 / 0",
    ];

    for source in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Error { kind, message, .. } => {
                assert_eq!(kind, "ArithmeticError");
                assert!(message.contains("Division by zero"), "Expected division by zero error for: {}", source);
            }
            _ => panic!("Expected error for division by zero: {}", source),
        }
    }
}

#[test]
fn test_modulo_by_zero() {
    let test_cases = vec![
        "10 % 0",
        "10.5 % 0.0",
    ];

    for source in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Error { kind, message, .. } => {
                assert_eq!(kind, "ArithmeticError");
                assert!(message.contains("Modulo by zero"), "Expected modulo by zero error for: {}", source);
            }
            _ => panic!("Expected error for modulo by zero: {}", source),
        }
    }
}

#[test]
fn test_type_errors() {
    let test_cases = vec![
        (r#""hello" - 5"#, "Subtraction requires numeric types"),
        (r#"5 - "hello""#, "Subtraction requires numeric types"),
        (r#""hello" * 5"#, "Multiplication requires numeric types"),
        (r#""hello" / 5"#, "Division requires numeric types"),
        (r#""hello" % 5"#, "Modulo requires numeric types"),
    ];

    for (source, expected_msg) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Error { kind, message, .. } => {
                assert_eq!(kind, "TypeError");
                assert!(message.contains(expected_msg), "Expected error message '{}' for: {}", expected_msg, source);
            }
            _ => panic!("Expected type error for: {}", source),
        }
    }
}

#[test]
fn test_overflow_detection() {
    // Test integer overflow
    let source = "9223372036854775807 + 1"; // i64::MAX + 1
    
    let mut jit = JitCompiler::new().unwrap();
    let ast = parse_flc(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "ArithmeticError");
            assert!(message.contains("overflow"), "Expected overflow error");
        }
        _ => panic!("Expected overflow error"),
    }
}

#[test]
fn test_negation() {
    let test_cases = vec![
        ("-5", Value::Integer(-5)),
        ("-10.5", Value::Float(-10.5)),
        ("-(-5)", Value::Integer(5)),
        ("-(-10.5)", Value::Float(10.5)),
    ];

    for (source, expected) in test_cases {
        let mut jit = JitCompiler::new().unwrap();
        let ast = parse_flc(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();

        let result = jit.compile_and_run(&bytecode).unwrap();
        match (result, expected) {
            (Value::Float(a), Value::Float(b)) => {
                assert!((a - b).abs() < 0.0001, "Failed for: {} (expected {}, got {})", source, b, a);
            }
            (a, b) => assert_eq!(a, b, "Failed for: {}", source),
        }
    }
}

#[test]
fn test_negation_type_error() {
    let source = r#"-"hello""#;
    
    let mut jit = JitCompiler::new().unwrap();
    let ast = parse_flc(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Error { kind, message, .. } => {
            assert_eq!(kind, "TypeError");
            assert!(message.contains("Negation requires numeric type"));
        }
        _ => panic!("Expected type error for string negation"),
    }
}