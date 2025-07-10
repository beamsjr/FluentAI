//! Debug test to understand catch parameter loading

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
fn test_catch_param_access() {
    // Test just accessing the catch parameter
    let result = compile_and_run_debug(
        r#"
        (try
          (throw "hello")
          (catch (e) e))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("hello".to_string()));
}

#[test]
fn test_catch_param_with_let() {
    // Test catch parameter with let binding
    let result = compile_and_run_debug(
        r#"
        (let ((x 42))
          (try
            (throw "error")
            (catch (e) (list x e))))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Integer(42));
            assert_eq!(items[1], Value::String("error".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}

#[test]
fn test_catch_param_list_construction() {
    // Test building a list with catch parameter
    let result = compile_and_run_debug(
        r#"
        (let ((a "first")
              (b "second"))
          (try
            (throw "caught")
            (catch (e) (list a b e))))
        "#
    ).unwrap();
    
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], Value::String("first".to_string()));
            assert_eq!(items[1], Value::String("second".to_string()));
            assert_eq!(items[2], Value::String("caught".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}