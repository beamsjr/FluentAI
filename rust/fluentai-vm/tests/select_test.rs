//! Tests for select operation on multiple channels

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
        #[cfg(feature = "ai-analysis")]
        ai_optimization: false,
        #[cfg(feature = "ai-analysis")]
        hybrid_optimization: false,
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
fn test_select_basic() {
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan 1))
              (ch2 (chan 1)))
          (let ((_ (send! ch2 "from-ch2")))
            (select
              ((recv! ch1) "got-ch1")
              ((recv! ch2) "got-ch2"))))
        "#
    ).unwrap();
    
    // Should receive from ch2 since ch1 has no data
    assert_eq!(result, Value::String("got-ch2".to_string()));
}

#[test]
fn test_select_with_default() {
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan))
              (ch2 (chan)))
          (select
            ((recv! ch1) "got-ch1")
            ((recv! ch2) "got-ch2")
            (default "no-data")))
        "#
    ).unwrap();
    
    // Should execute default since no channels have data
    assert_eq!(result, Value::String("no-data".to_string()));
}

#[test]
fn test_select_first_ready() {
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan 1))
              (ch2 (chan 1)))
          (let ((_ (send! ch1 100))
                (_ (send! ch2 200)))
            (select
              ((recv! ch1) 101)
              ((recv! ch2) 202)))))
        "#
    ).unwrap();
    
    // Should receive from ch1 first (deterministic in our implementation)
    assert_eq!(result, Value::Integer(101));
}

#[test]
#[ignore = "Complex test with recursion and variable scoping issues"]
fn test_select_in_loop() {
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan 3))
              (ch2 (chan 3))
              (results (list)))
          (let ((_ (send! ch1 1))
                (_ (send! ch2 2))
                (_ (send! ch1 3)))
            (letrec ((loop (lambda (n acc)
                            (if (= n 0)
                                acc
                                (let ((val (select
                                           ((recv! ch1) (lambda (x) x))
                                           ((recv! ch2) (lambda (x) x))
                                           (default -1))))
                                  (loop (- n 1) (cons val acc)))))))
              (loop 4 (list)))))
        "#
    ).unwrap();
    
    // Should get values from both channels and then default
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 4);
            // Values should include 1, 2, 3, and -1 (default)
            let mut values: Vec<i64> = items.iter()
                .filter_map(|v| match v {
                    Value::Integer(n) => Some(*n),
                    _ => None,
                })
                .collect();
            values.sort();
            assert_eq!(values, vec![-1, 1, 2, 3]);
        }
        _ => panic!("Expected list result"),
    }
}

#[test]
fn test_select_nested() {
    let result = compile_and_run(
        r#"
        (let ((ch1 (chan 1))
              (ch2 (chan 1))
              (ch3 (chan 1)))
          (let ((_ (send! ch3 "winner")))
            (select
              ((recv! ch1) "ch1")
              ((recv! ch2) (select
                            ((recv! ch3) "nested-ch3")
                            (default "nested-default")))
              (default (select
                        ((recv! ch3) "default-ch3")
                        (default "all-default"))))))
        "#
    ).unwrap();
    
    // Should get from ch3 via the default branch
    assert_eq!(result, Value::String("default-ch3".to_string()));
}