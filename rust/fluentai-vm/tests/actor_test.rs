//! Tests for actor model primitives

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run(source: &str) -> Result<Value> {
    // Parse the source code using S-expression parser for tests
    let graph = fluentai_parser::sexp::parse_sexp(source)
        .map_err(|e| anyhow::anyhow!("Parse error: {:?}", e))?;

    // Compile to bytecode without optimization due to optimizer bug
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
fn test_create_actor() {
    let result = compile_and_run(
        r#"
        (actor 0 (lambda (state msg) state))
        "#
    ).unwrap();
    
    // Should return an actor ID
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
fn test_actor_send() {
    let result = compile_and_run(
        r#"
        (let ((counter (actor 0 (lambda (state msg) state))))
          (! counter "hello"))
        "#
    ).unwrap();
    
    // Send returns nil
    assert_eq!(result, Value::Nil);
}

#[test]
fn test_simple_counter_actor() {
    let result = compile_and_run(
        r#"
        (let ((counter (actor 0 (lambda (state msg) 
                                  (+ state 1)))))
          (let ((_ (! counter "inc"))
                (_ (! counter "inc"))
                (_ (! counter "inc")))
            counter))
        "#
    ).unwrap();
    
    // Should return the actor
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "Actor receive not fully implemented yet"]
fn test_actor_receive() {
    let result = compile_and_run(
        r#"
        (let ((echo (actor nil (lambda (state msg)
                                (receive
                                  ((ping) "pong")
                                  ((hello name) (str "Hello, " name))
                                  (_ "unknown"))))))
          (let ((_ (! echo (ping))))
            echo))
        "#
    ).unwrap();
    
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "Become not fully implemented yet"]
fn test_actor_become() {
    let result = compile_and_run(
        r#"
        (let ((stateful (actor "initial" 
                              (lambda (state msg)
                                (receive
                                  ((set new-state) (become new-state))
                                  ((get reply-to) (! reply-to state))
                                  (_ state))))))
          (let ((ch (chan)))
            (let ((_ (! stateful (set "changed")))
                  (_ (! stateful (get ch))))
              (recv! ch))))
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("changed".to_string()));
}