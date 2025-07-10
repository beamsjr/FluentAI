//! Debug test for channel error handling

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
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;

    // Print bytecode for debugging
    println!("Bytecode for chunk 0:");
    for (i, instr) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("{}: {:?} {}", i, instr.opcode, instr.arg);
    }

    // Create VM with effect runtime
    let runtime = Arc::new(EffectRuntime::new()?);
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    // Run the VM
    Ok(vm.run()?)
}

#[test]
fn test_simple_channel_in_let() {
    // Test that channels work in let bindings
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (begin
            (send! ch "test")
            (recv! ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("test".to_string()));
}

#[test]
fn test_channel_survives_try_catch() {
    // Test that channels survive try-catch without exception
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (begin
            (try
              (send! ch "hello")
              (catch (e) "error"))
            (recv! ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("hello".to_string()));
}

#[test]
fn test_channel_in_catch_simple() {
    // Test using a channel in a catch block
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (try
            (throw "error")
            (catch (e) 
              (begin
                (send! ch e)
                (recv! ch)))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("error".to_string()));
}

#[test]
fn test_channel_preserved_across_exception() {
    // Test that let-bound channels are preserved across exceptions
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (begin
            (send! ch "before")
            (try
              (throw "error")
              (catch (e) 
                (recv! ch)))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("before".to_string()));
}

#[test]
fn test_two_channels_in_error_handler() {
    // Test the exact scenario from the failing test
    let result = compile_and_run(
        r#"
        (let ((ch (chan))
              (err-ch (chan)))
          (begin
            (try
              (begin
                (send! ch "before")
                (throw "oops"))
              (catch (e) 
                (begin
                  (send! err-ch "hardcoded")
                  (recv! ch))))
            (recv! err-ch)))
        "#
    ).unwrap();
    
    println!("Result: {:?}", result);
    // Should return "hardcoded" from err-ch
    assert_eq!(result, Value::String("hardcoded".to_string()));
}

#[test]
fn test_channel_after_catch() {
    // Test what the catch block returns
    let result = compile_and_run(
        r#"
        (let ((ch (chan))
              (err-ch (chan)))
          (begin
            (send! ch "data")
            (try
              (throw "error")
              (catch (e) 
                (begin
                  (send! err-ch e)
                  (recv! ch))))))
        "#
    ).unwrap();
    
    println!("Catch result: {:?}", result);
    assert_eq!(result, Value::String("data".to_string()));
}

#[test]
fn test_recv_empty_channel() {
    // Test what happens when we receive from an empty channel
    let result = compile_and_run(
        r#"
        (let ((ch (chan)))
          (recv! ch))
        "#
    ).unwrap();
    
    println!("Empty channel recv result: {:?}", result);
    // Should return Nil for non-blocking receive
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_receive_outside_try() {
    // Test receiving from err-ch outside the try block
    let result = compile_and_run(
        r#"
        (let ((err-ch (chan)))
          (begin
            (send! err-ch "test")
            (recv! err-ch)))
        "#
    ).unwrap();
    
    println!("Result: {:?}", result);
    assert_eq!(result, Value::String("test".to_string()));
}