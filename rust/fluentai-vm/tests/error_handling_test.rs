//! Tests for async error handling

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

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
fn test_try_catch_basic() {
    let result = compile_and_run(
        r#"
        (try
          (throw (list "error" "test error"))
          (catch (err) err))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("error".to_string()));
            assert_eq!(items[1], Value::String("test error".to_string()));
        }
        _ => panic!("Expected error list, got {:?}", result),
    }
}

#[test]
fn test_try_no_error() {
    let result = compile_and_run(
        r#"
        (try
          (+ 1 2)
          (catch (err) "error caught"))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
#[ignore = "Finally blocks and set! not fully implemented yet"]
fn test_try_finally() {
    let result = compile_and_run(
        r#"
        (let ((x 0))
          (try
            (throw "error")
            (catch (err) 42)
            (finally (set! x 100)))
          x)
        "#
    ).unwrap();
    
    // Finally block should execute, setting x to 100
    assert_eq!(result, Value::Integer(100));
}

#[test]
#[ignore = "Promise operations not fully implemented yet"]
fn test_promise_basic() {
    let result = compile_and_run(
        r#"
        (await (promise (+ 1 2)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
#[ignore = "Promise operations not fully implemented yet"]
fn test_promise_all() {
    let result = compile_and_run(
        r#"
        (await
          (promise-all
            (promise 1)
            (promise 2)
            (promise 3)))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Integer(1));
            assert_eq!(items[1], Value::Integer(2));
            assert_eq!(items[2], Value::Integer(3));
        }
        _ => panic!("Expected list of results"),
    }
}

#[test]
#[ignore = "Promise operations not fully implemented yet"]
fn test_promise_race() {
    let result = compile_and_run(
        r#"
        (await
          (promise-race
            (promise (begin (sleep 100) "slow"))
            (promise "fast")))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("fast".to_string()));
}

#[test]
#[ignore = "Timeout not fully implemented yet"]
fn test_timeout() {
    let result = compile_and_run(
        r#"
        (timeout 100
          (promise (begin (sleep 1000) "slow"))
          "timeout")
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("timeout".to_string()));
}

#[test]
#[ignore = "Async error propagation not fully implemented yet"]
fn test_async_error_propagation() {
    let result = compile_and_run(
        r#"
        (try
          (await (spawn (throw "async error")))
          (catch (err) (str "caught: " err)))
        "#
    );
    
    match result {
        Ok(Value::String(s)) => assert!(s.contains("caught:")),
        _ => panic!("Expected caught error message"),
    }
}