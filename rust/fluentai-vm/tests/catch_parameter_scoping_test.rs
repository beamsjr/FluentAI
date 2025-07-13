//! Tests for catch parameter scoping and local variable indexing

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse_flc;

fn compile_and_run(source: &str) -> Result<Value> {
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
fn test_catch_parameter_with_single_local() {
    // Test that catch parameter doesn't interfere with a single local variable
    let result = compile_and_run(
        r#"
        (let ((x 42))
          (try
            (throw "error")
            (catch (e) x)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_catch_parameter_with_multiple_locals() {
    // Test that catch parameter doesn't interfere with multiple local variables
    let result = compile_and_run(
        r#"
        (let ((x 10)
              (y 20)
              (z 30))
          (try
            (throw "error")
            (catch (e) (+ x (+ y z)))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(60));
}

#[test]
fn test_catch_parameter_access() {
    // Test that catch parameter itself is accessible
    let result = compile_and_run(
        r#"
        (let ((x "local"))
          (try
            (throw "caught")
            (catch (e) e)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("caught".to_string()));
}

#[test]
fn test_catch_parameter_and_local_access() {
    // Test accessing both catch parameter and local variables
    let result = compile_and_run(
        r#"
        (let ((prefix "Error: "))
          (try
            (throw "something went wrong")
            (catch (e) (list prefix e))))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("Error: ".to_string()));
            assert_eq!(items[1], Value::String("something went wrong".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_nested_let_in_catch() {
    // Test let binding inside catch block
    let result = compile_and_run(
        r#"
        (let ((outer 100))
          (try
            (throw 99)
            (catch (e)
              (let ((inner 50))
                (+ outer inner)))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(150));
}

#[test]
fn test_nested_try_catch_scoping() {
    // Test nested try-catch blocks with different scopes
    let result = compile_and_run(
        r#"
        (let ((x 1))
          (try
            (let ((y 2))
              (try
                (throw "inner")
                (catch (e1) (+ x y))))
            (catch (e2) x)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
fn test_catch_parameter_shadowing() {
    // Test that catch parameter can shadow outer variables
    let result = compile_and_run(
        r#"
        (let ((e "outer"))
          (try
            (throw "inner")
            (catch (e) e)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("inner".to_string()));
}

#[test]
fn test_catch_parameter_not_used() {
    // Test catch with unused parameter (underscore pattern)
    let result = compile_and_run(
        r#"
        (let ((x 42))
          (try
            (throw "ignored")
            (catch (_) x)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_multiple_catch_branches_with_locals() {
    // Test multiple catch branches accessing locals
    let result = compile_and_run(
        r#"
        (let ((default 999))
          (try
            (throw 42)
            (catch "string" default)
            (catch e e)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_catch_with_function_call() {
    // Test catch block calling a function with locals
    let result = compile_and_run(
        r#"
        (let ((f (lambda (x) (* x 2)))
              (base 21))
          (try
            (throw "error")
            (catch (e) (f base))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}


#[test]
fn test_deep_nesting_with_catch() {
    // Test deeply nested scopes with catch
    let result = compile_and_run(
        r#"
        (let ((a 1))
          (let ((b 2))
            (let ((c 3))
              (let ((d 4))
                (try
                  (throw "error")
                  (catch (e) (+ a (+ b (+ c d)))))))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(10));
}