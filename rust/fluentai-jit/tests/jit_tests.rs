//! Integration tests for JIT compiler

use fluentai_parser::parse;
use fluentai_vm::compiler::Compiler;
use fluentai_vm::bytecode::Value;
use fluentai_jit::JitCompiler;

#[test]
fn test_simple_arithmetic() {
    // Skip on non-x86_64 platforms due to Cranelift PLT limitations
    if cfg!(not(target_arch = "x86_64")) {
        eprintln!("Skipping JIT test on non-x86_64 platform");
        return;
    }
    
    let mut jit = JitCompiler::new().unwrap();
    
    let test_cases = vec![
        ("42", 42),
        ("(+ 1 2)", 3),
        ("(* 5 7)", 35),
        ("(- 10 3)", 7),
        ("(+ (* 2 3) (- 5 1))", 10),
    ];
    
    for (source, expected) in test_cases {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();
        
        let result = jit.compile_and_run(&bytecode).unwrap();
        match result {
            Value::Int(n) => assert_eq!(n, expected, "Failed for: {}", source),
            _ => panic!("Expected integer result for: {}", source),
        }
    }
}

#[test]
fn test_local_variables() {
    // Skip on non-x86_64 platforms due to Cranelift PLT limitations
    if cfg!(not(target_arch = "x86_64")) {
        eprintln!("Skipping JIT test on non-x86_64 platform");
        return;
    }
    
    let mut jit = JitCompiler::new().unwrap();
    
    let source = "(let ((x 5) (y 3)) (+ x y))";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let result = jit.compile_and_run(&bytecode).unwrap();
    match result {
        Value::Int(n) => assert_eq!(n, 8),
        _ => panic!("Expected integer result"),
    }
}

#[test]
fn test_compilation_cache() {
    // Skip on non-x86_64 platforms due to Cranelift PLT limitations
    if cfg!(not(target_arch = "x86_64")) {
        eprintln!("Skipping JIT test on non-x86_64 platform");
        return;
    }
    
    let mut jit = JitCompiler::new().unwrap();
    
    let source = "(+ 1 2)";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    // First compilation
    let func1 = jit.compile(&bytecode, 0).unwrap();
    
    // Second compilation should return cached function
    let func2 = jit.compile(&bytecode, 0).unwrap();
    
    // Function pointers should be identical
    assert_eq!(func1 as usize, func2 as usize);
    
    // Stats should show only one compilation
    assert_eq!(jit.stats().functions_compiled, 1);
}

#[test]
fn test_jit_stats() {
    // Skip on non-x86_64 platforms due to Cranelift PLT limitations
    if cfg!(not(target_arch = "x86_64")) {
        eprintln!("Skipping JIT test on non-x86_64 platform");
        return;
    }
    
    let mut jit = JitCompiler::new().unwrap();
    
    let sources = vec![
        "(+ 1 2)",
        "(* 5 7)",
        "(- 10 3)",
    ];
    
    for source in sources {
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();
        jit.compile_and_run(&bytecode).unwrap();
    }
    
    let stats = jit.stats();
    assert_eq!(stats.functions_compiled, 3);
    assert!(stats.total_instructions > 0);
    assert!(stats.codegen_time_ms > 0.0);
}