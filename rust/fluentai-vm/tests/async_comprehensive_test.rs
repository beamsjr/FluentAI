//! Comprehensive tests for async/concurrent features

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run(source: &str) -> Result<Value> {
    // Parse the source code
    let graph = fluentai_parser::parse(source)
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

    // Compile to bytecode without optimization
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
fn test_channel_send_receive() {
    // Test basic channel communication
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (send! ch 42)
          (recv! ch))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_buffered_channel() {
    // Test buffered channel
    let result = compile_and_run(
        r#"
        (let ((ch (chan 2)))
          (send! ch 1)
          (send! ch 2)
          (+ (recv! ch) (recv! ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
fn test_try_channel_operations() {
    // Test non-blocking channel operations
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (let ((res (try-recv! ch)))
            (if (car res)
                "received"
                "empty")))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("empty".to_string()));
}

#[test]
fn test_spawn_with_channel() {
    // Test spawning a task that sends to a channel
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (spawn (send! ch 100))
          (recv! ch))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(100));
}

#[test]
fn test_async_await_simple() {
    // Test basic async/await
    let result = compile_and_run(
        r#"
        (await (async (+ 1 2)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(3));
}

#[test]
fn test_error_handling_with_channel() {
    // Test error handling with channels
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (try
            (begin
              (send! ch "data")
              (throw "error")
              (send! ch "never"))
            (catch (err) (recv! ch))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("data".to_string()));
}

#[test]
fn test_multiple_spawn() {
    // Test multiple spawned tasks
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan))
              (ch2 (chan)))
          (spawn (send! ch1 10))
          (spawn (send! ch2 20))
          (+ (recv! ch1) (recv! ch2)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(30));
}

#[test]
fn test_actor_creation_and_messaging() {
    // Test basic actor functionality
    let result = compile_and_run(
        r#"
        (let ((counter (actor 0 (lambda (state msg) (+ state msg)))))
          (! counter 5)
          (! counter 3)
          "sent")
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("sent".to_string()));
}

#[test]
#[ignore = "Nested try-catch not fully working yet"]
fn test_try_catch_nested() {
    // Test nested try-catch blocks
    let result = compile_and_run(
        r#"
        (try
          (try
            (throw "inner")
            (catch (e1) (list "inner-catch" e1)))
          (catch (e2) (list "outer-catch" e2)))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("inner-catch".to_string()));
            assert_eq!(items[1], Value::String("inner".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
#[ignore = "Select not fully implemented yet"]
fn test_select_channels() {
    // Test select on multiple channels
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan))
              (ch2 (chan)))
          (send! ch2 "second")
          (select
            ((recv! ch1) v (list "from-ch1" v))
            ((recv! ch2) v (list "from-ch2" v))))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("from-ch2".to_string()));
            assert_eq!(items[1], Value::String("second".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
#[ignore = "Complex channel/error interaction"]
fn test_channel_in_error_handler() {
    // Test using channels in error handlers
    let result = compile_and_run(
        r#"
        (let ((ch (chan))
              (err-ch (chan)))
          (try
            (begin
              (send! ch "before")
              (throw "oops"))
            (catch (e) 
              (begin
                (send! err-ch e)
                (recv! ch))))
          (recv! err-ch))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("oops".to_string()));
}

#[test]
#[ignore = "Complex concurrent pattern"]
fn test_concurrent_counter() {
    // Test concurrent access to shared state (via channels)
    let result = compile_and_run(
        r#"
        (let ((ch (chan))
              (result-ch (chan)))
          (spawn (begin
                   (send! ch 1)
                   (send! ch 2)
                   (send! ch 3)))
          (spawn (let ((sum 0))
                   (define (loop n acc)
                     (if (= n 0)
                         (send! result-ch acc)
                         (loop (- n 1) (+ acc (recv! ch)))))
                   (loop 3 0)))
          (recv! result-ch))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::Integer(6));
}