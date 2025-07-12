//! Tests for actor model primitives

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

fn compile_and_run(source: &str) -> Result<Value> {
    // Parse the source code using FLC parser
    let graph = fluentai_parser::parse(source)
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
#[ignore = "Actor syntax parsing not fully implemented"]
fn test_create_actor() {
    let result = compile_and_run(
        r#"
        // Simple actor that returns its state
        private actor Echo {
            state: int = 0;
            private handle message(msg: any) {
                self.state
            }
        }
        Echo
        "#
    ).unwrap();
    
    // Should return an actor ID
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "Actor send syntax not fully implemented"]
fn test_actor_send() {
    let result = compile_and_run(
        r#"
        private actor Counter {
            state: int = 0;
            private handle message(msg: any) {
                self.state
            }
        }
        let counter = Counter;
        counter.send("hello")
        "#
    ).unwrap();
    
    // Send returns nil
    assert_eq!(result, Value::Nil);
}

#[test]
#[ignore = "Actor message handling not fully implemented"]
fn test_simple_counter_actor() {
    let result = compile_and_run(
        r#"
        private actor Counter {
            state: int = 0;
            private handle increment() {
                self.state = self.state + 1
            }
        }
        let counter = Counter;
        counter.send(increment());
        counter.send(increment());
        counter.send(increment());
        counter
        "#
    ).unwrap();
    
    // Should return the actor
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "Actor syntax parsing not fully implemented"]
fn test_actor_receive() {
    let result = compile_and_run(
        r#"
        // Actor that pattern matches on messages
        private actor Echo {
            state: any = null;
            private handle message(msg: any) {
                receive {
                    case "ping" => "pong",
                    case ("hello", name) => f"Hello, {name}",
                    case _ => "unknown"
                }
            }
        }
        let echo = Echo;
        echo.send("ping");
        echo
        "#
    ).unwrap();
    
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "Actor syntax parsing not fully implemented"]
fn test_actor_become() {
    let result = compile_and_run(
        r#"
        // Actor that uses become to change state
        private actor Stateful {
            state: string = "initial";
            private handle message(msg: any) {
                receive {
                    case ("set", new_state) => {
                        become(new_state);
                        new_state
                    },
                    case ("get", reply_to) => {
                        reply_to.send(state);
                        state
                    },
                    case _ => state
                }
            }
        }
        let stateful = Stateful;
        let ch = channel();
        stateful.send(("set", "changed"));
        stateful.send(("get", ch));
        ch.receive()
        "#
    ).unwrap();
    
    assert_eq!(result, Value::String("changed".to_string()));
}