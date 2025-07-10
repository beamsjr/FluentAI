//! Test to trace variable scoping in catch blocks

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

#[test]
fn test_catch_scoping_detailed() {
    // Create a simple test case
    let source = r#"
        (let ((outer 42))
          (try
            (throw "error")
            (catch (e) (list outer e))))
    "#;
    
    // Parse
    let graph = fluentai_parser::parse(source).unwrap();
    
    // Compile 
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Print bytecode
    println!("\n=== Bytecode ===");
    if let Some(chunk) = bytecode.chunks.get(bytecode.main_chunk) {
        for (i, instr) in chunk.instructions.iter().enumerate() {
            println!("{:3}: {:?}", i, instr);
        }
    }
    
    // Run
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);
    let result = vm.run().unwrap();
    
    // Check result
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
fn test_catch_variable_positions() {
    // Test the exact positions of variables
    let source = r#"
        (let ((a "first")
              (b "second"))
          (try
            (throw "caught")
            (catch (e) (list a b e))))
    "#;
    
    // Parse
    let graph = fluentai_parser::parse(source).unwrap();
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Print bytecode
    println!("\n=== Bytecode for position test ===");
    if let Some(chunk) = bytecode.chunks.get(bytecode.main_chunk) {
        for (i, instr) in chunk.instructions.iter().enumerate() {
            println!("{:3}: {:?}", i, instr);
        }
    }
    
    // Run
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);
    let result = vm.run().unwrap();
    
    // Check result
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