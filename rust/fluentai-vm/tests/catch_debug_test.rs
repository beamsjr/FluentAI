//! Debug test for catch parameter issue

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

    // Compile to bytecode with debug info
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
fn test_simple_catch_with_let() {
    // Simplest test case
    let result = compile_and_run(
        r#"
        (let ((x 42))
          (try
            (throw "error")
            (catch (e) x)))
        "#
    ).unwrap();
    
    println!("Result: {:?}", result);
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_catch_param_and_two_locals() {
    // Test with two locals to see indexing
    let result = compile_and_run(
        r#"
        (let ((x 10)
              (y 20))
          (try
            (throw "error")
            (catch (e) y)))
        "#
    ).unwrap();
    
    println!("Result: {:?}", result);
    assert_eq!(result, Value::Integer(20));
}

#[test]
fn test_catch_accessing_both() {
    // Test accessing both parameter and local
    let result = compile_and_run(
        r#"
        (let ((x 42))
          (try
            (throw "caught")
            (catch (e) (list e x))))
        "#
    ).unwrap();
    
    println!("Result: {:?}", result);
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::String("caught".to_string()));
            assert_eq!(items[1], Value::Integer(42));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}