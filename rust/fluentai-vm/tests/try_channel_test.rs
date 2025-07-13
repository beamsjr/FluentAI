//! Tests for non-blocking channel operations

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

    // Compile to bytecode without optimization due to optimizer bug
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
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
fn test_try_send_success() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 1)))
          (try-send! ch 42))
        "#
    ).unwrap();
    
    // try-send! should return true on success
    assert_eq!(result, Value::Boolean(true));
}

#[test]
fn test_try_send_full_buffer() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 1)))
          (begin
            (send! ch 1)      ; Fill the buffer
            (try-send! ch 2))) ; This should fail
        "#
    ).unwrap();
    
    // try-send! should return false when buffer is full
    assert_eq!(result, Value::Boolean(false));
}

#[test]
fn test_try_receive_empty() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (try-recv! ch))
        "#
    ).unwrap();
    
    // try-recv! should return [false, nil] when no value available
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Boolean(false));
            assert_eq!(items[1], Value::Nil);
        }
        _ => panic!("Expected list result from try-recv!"),
    }
}

#[test]
fn test_try_receive_success() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 1)))
          (begin
            (send! ch 42)
            (try-recv! ch)))
        "#
    ).unwrap();
    
    // try-recv! should return [true, value] on success
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Boolean(true));
            assert_eq!(items[1], Value::Integer(42));
        }
        _ => panic!("Expected list result from try-recv!"),
    }
}

#[test]
fn test_try_operations_in_condition() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 1)))
          (if (try-send! ch 100)
              "sent"
              "full"))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("sent".to_string()));
}

#[test]
fn test_try_recv_pattern_matching() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 1)))
          (let ((_ (send! ch 99)))
            (let ((result (try-recv! ch)))
              (if (car result)
                  (car (cdr result))  ; Extract the value
                  -1))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(99));
}

#[test]
fn test_multiple_try_sends() {
    let result = compile_and_run(
        r#"
        (let ((ch (chan 2)))  ; Buffer size 2
          (list
            (try-send! ch 1)
            (try-send! ch 2)
            (try-send! ch 3))) ; This one should fail
        "#
    ).unwrap();
    
    match result {
        Value::List(ref items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::Boolean(true));  // First send succeeds
            assert_eq!(items[1], Value::Boolean(true));  // Second send succeeds
            assert_eq!(items[2], Value::Boolean(false)); // Third send fails (buffer full)
        }
        _ => panic!("Expected list result"),
    }
}