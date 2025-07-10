//! Trace test for finally blocks

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM, debug::{DebugConfig, StepMode}};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn run_test_trace(source: &str) -> Result<Value> {
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
    println!("\nBytecode:");
    for (i, instr) in bytecode.chunks[0].instructions.iter().enumerate() {
        println!("{:2}: {:?}", i, instr);
    }
    println!();

    // Create VM with effect runtime and debug
    let runtime = Arc::new(EffectRuntime::new()?);
    let debug_config = DebugConfig {
        step_mode: StepMode::StepInto,
        breakpoints: vec![],
        trace_execution: true,
        trace_stack: true,
        trace_calls: true,
    };
    
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);
    vm.set_debug_config(debug_config);

    // Run the VM
    Ok(vm.run()?)
}

#[test]
fn test_trace_exception_finally() {
    let code = r#"
        (try
            (throw "error")
            (catch e 123)
            (finally
                999))
    "#;
    
    match run_test_trace(code) {
        Ok(result) => {
            println!("\nFinal Result: {:?}", result);
            assert_eq!(result, Value::Integer(123));
        }
        Err(e) => {
            println!("\nError: {:?}", e);
            panic!("Test failed with error: {:?}", e);
        }
    }
}