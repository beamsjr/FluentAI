//! Tests for JIT string operations

use fluentai_jit::JitCompiler;
use fluentai_parser::parse;
use fluentai_vm::Compiler;
use fluentai_core::value::Value;

#[test]
#[ignore = "String operations are not yet supported in FLC parser"]
fn test_string_length() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test string length using FLC syntax
    let source = r#""hello".length()"#;
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Integer(n) => assert_eq!(n, 5, "Expected string length 5"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}

#[test]
fn test_string_concat() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test string concatenation
    let source = r#"(str-concat "hello" " world")"#;
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::String(s) => assert_eq!(s, "hello world", "Expected concatenated string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_string_upper() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test string to uppercase
    let source = r#"(str-upper "hello")"#;
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::String(s) => assert_eq!(s, "HELLO", "Expected uppercase string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_string_lower() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test string to lowercase
    let source = r#"(str-lower "HELLO")"#;
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::String(s) => assert_eq!(s, "hello", "Expected lowercase string"),
        _ => panic!("Expected string result, got {:?}", result),
    }
}

#[test]
fn test_string_operations_combined() {
    if cfg!(not(target_arch = "x86_64")) {
        return;
    }

    let mut jit = JitCompiler::new().unwrap();
    
    // Test combined string operations
    let source = r#"(strlen (str-upper (str-concat "hello" " world")))"#;
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();

    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Integer(n) => assert_eq!(n, 11, "Expected length 11 for 'HELLO WORLD'"),
        _ => panic!("Expected integer result, got {:?}", result),
    }
}