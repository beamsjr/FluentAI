//! Exact reproduction of the failing test

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

    // Run the VM
    let result = vm.run()?;
    println!("\n=== Result ===");
    println!("{:?}", result);
    
    Ok(result)
}

#[test]
fn test_exact_failing_case_simplified() {
    // Simplify to just the send operation
    let result = compile_and_run_debug(
        r#"
        (let ((ch (chan))
              (err-ch (chan)))
          (try
            (throw "error-value")
            (catch (e) 
              (send! err-ch e))))
        "#
    ).unwrap();
    
    // send! returns nil
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_two_channels_simple() {
    // Test just accessing two channels
    let result = compile_and_run_debug(
        r#"
        (let ((ch1 (chan))
              (ch2 (chan)))
          (list ch1 ch2))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            match (&items[0], &items[1]) {
                (Value::Channel(_), Value::Channel(_)) => {
                    println!("Got two channels as expected");
                }
                _ => panic!("Expected two channels"),
            }
        }
        _ => panic!("Expected list"),
    }
}