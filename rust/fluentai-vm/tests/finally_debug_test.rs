//! Debug test for finally blocks

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;
use fluentai_parser::parse_flc;

fn run_test_debug(source: &str) -> Result<Value> {
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
fn test_simple_exception_finally() {
    let code = r#"
        (try
            (throw "error")
            (catch e 123)
            (finally
                (+ 2 2)))
    "#;
    
    match run_test_debug(code) {
        Ok(result) => {
            println!("Result: {:?}", result);
            assert_eq!(result, Value::Integer(123));
        }
        Err(e) => {
            println!("Error: {:?}", e);
            panic!("Test failed with error: {:?}", e);
        }
    }
}