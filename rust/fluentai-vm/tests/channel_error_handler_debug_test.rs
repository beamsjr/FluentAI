//! Debug test to understand channel-in-error-handler issue

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run_debug(source: &str) -> Result<Value> {
    println!("\n=== Source code ===");
    println!("{}", source);
    
    // Parse the source code
    let graph = fluentai_parser::parse(source)
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;
    
    println!("\n=== AST Graph ===");
    println!("{:#?}", graph);

    // Compile to bytecode without optimization
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    println!("\n=== Main Chunk Bytecode ===");
    if let Some(main_chunk) = bytecode.chunks.get(bytecode.main_chunk) {
        for (i, instr) in main_chunk.instructions.iter().enumerate() {
            println!("{:3}: {:?}", i, instr);
        }
        
        println!("\n=== Constants ===");
        for (i, constant) in main_chunk.constants.iter().enumerate() {
            println!("{}: {:?}", i, constant);
        }
    }

    // Create VM with effect runtime
    let runtime = Arc::new(EffectRuntime::new()?);
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    println!("\n=== Running VM ===");
    // Run the VM
    let result = vm.run()?;
    println!("\n=== Result ===");
    println!("{:?}", result);
    
    Ok(result)
}

#[test]
fn test_simple_channel_recv() {
    // Test just receiving from a channel
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan)))
          (begin
            (send! ch "hello")
            (recv! ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("hello".to_string()));
}

#[test]
fn test_channel_recv_in_outer_scope() {
    // Test receiving from a channel defined in outer scope
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan)))
          (begin
            (send! ch "data")
            (let ((x 42))
              (recv! ch))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("data".to_string()));
}

#[test]
fn test_channel_in_catch_minimal() {
    // Minimal test case that fails
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan)))
          (begin
            (send! ch "test")
            (try
              (throw "error")
              (catch (e) (recv! ch)))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("test".to_string()));
}

#[test]
fn test_channel_access_in_catch() {
    // Test just accessing the channel variable
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan)))
          (try
            (throw "error")
            (catch (e) ch)))
        "#
    ).unwrap();
    
    // This should return the channel, not String
    match result {
        Value::Channel(_) => {
            println!("Got channel as expected");
        }
        _ => panic!("Expected Channel, got {:?}", result),
    }
}

#[test]
fn test_two_channels_in_catch() {
    // Test with two channels to see variable indexing
    let result = compile_and_run_debug(
        r#"
        (let ((ch1 (chan))
              (ch2 (chan)))
          (begin
            (send! ch1 "from-ch1")
            (send! ch2 "from-ch2")
            (try
              (throw "error")
              (catch (e) 
                (list (recv! ch1) (recv! ch2))))))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("from-ch1".to_string()));
            assert_eq!(items[1], Value::String("from-ch2".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_failing_case_simplified() {
    // The actual failing test case but simplified
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan))
              (err-ch (chan)))
          (begin
            (send! err-ch "oops")
            (recv! err-ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("oops".to_string()));
}

#[test]
fn test_failing_case_exact() {
    // The exact failing test case
    let result = compile_and_run_debug(
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
                  (send! err-ch e)
                  (recv! ch))))
            (recv! err-ch)))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("oops".to_string()));
}