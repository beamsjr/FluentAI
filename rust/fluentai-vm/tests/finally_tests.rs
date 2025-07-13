//! Tests for try-catch-finally blocks

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn run_test(source: &str) -> Result<Value> {
    // Parse the source code
    let graph = fluentai_parser::parse_flc(source)
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

    // Compile to bytecode without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;

    // Create VM with effect runtime
    let runtime = Arc::new(EffectRuntime::new()?);
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    // Run the VM
    Ok(vm.run()?)
}

#[test]
fn test_finally_executes_on_normal_path() {
    let code = r#"
        (try
            42
            (finally
                (+ 1 1)))
    "#;
    
    let result = run_test(code).unwrap();
    // Should return 42, with finally executing but not affecting result
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_finally_executes_on_exception_path() {
    let code = r#"
        (try
            (do
                (throw "error")
                999)
            (catch e
                123)
            (finally
                (+ 2 2)))
    "#;
    
    let result = run_test(code).unwrap();
    // Should return 123 from catch, with finally executing
    assert_eq!(result, Value::Integer(123));
}

#[test]
fn test_finally_without_catch() {
    let code = r#"
        (try
            42
            (finally
                (+ 3 3)))
    "#;
    
    let result = run_test(code).unwrap();
    // Should return 42, finally executes but doesn't affect result
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_finally_rethrows_uncaught_exception() {
    let code = r#"
        (try
            (try
                (throw "inner error")
                (finally
                    (+ 4 4)))
            (catch e
                456))
    "#;
    
    let result = run_test(code).unwrap();
    // Inner finally executes, then outer catch handles the error
    assert_eq!(result, Value::Integer(456));
}

#[test]
fn test_nested_finally_blocks() {
    let code = r#"
        (try
            (try
                100
                (finally
                    (+ 5 5)))
            (finally
                (+ 6 6)))
    "#;
    
    let result = run_test(code).unwrap();
    // Both finally blocks execute, result is 100
    assert_eq!(result, Value::Integer(100));
}

#[test]
fn test_finally_preserves_return_value() {
    let code = r#"
        (try
            42
            (finally
                (+ 1 1)))  ; Side effect in finally shouldn't affect return value
    "#;
    
    let result = run_test(code).unwrap();
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_finally_preserves_exception() {
    let code = r#"
        (try
            (try
                (throw "original error")
                (finally
                    (+ 1 1)))  ; Side effect in finally
            (catch e e))
    "#;
    
    let result = run_test(code).unwrap();
    assert_eq!(result, Value::String("original error".to_string()));
}

#[test]
fn test_finally_with_multiple_catch_branches() {
    let code = r#"
        (try
            (throw "test-error")
            (catch "other-error" 1)
            (catch "test-error" 2)
            (catch _ 3)
            (finally
                (+ 7 7)))
    "#;
    
    let result = run_test(code).unwrap();
    // Should match second catch branch and return 2
    assert_eq!(result, Value::Integer(2));
}

#[test]
fn test_deeply_nested_finally_blocks() {
    // Test that nested finally blocks don't corrupt the stack
    let code = r#"
        (try
            (try
                (try
                    (try
                        999
                        (finally 1))
                    (finally 2))
                (finally 3))
            (finally 4))
    "#;
    
    let result = run_test(code).unwrap();
    // Should return the original value 999
    assert_eq!(result, Value::Integer(999));
}