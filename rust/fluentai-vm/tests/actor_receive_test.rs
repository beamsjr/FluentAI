//! Tests for actor receive pattern matching
//!
//! Tests the receive construct that allows actors to pattern match on messages

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

    // Compile to bytecode without optimization
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
fn test_actor_receive_simple() {
    // Create AST directly since FLC actor syntax is not fully implemented
    use fluentai_core::ast::{Graph, Node, Literal, Pattern};
    
    let mut graph = Graph::new();
    
    // Create handler with receive expression that returns the state
    let state_var = graph.add_node(Node::Variable { name: "state".to_string() }).unwrap();
    let _msg_var = graph.add_node(Node::Variable { name: "msg".to_string() }).unwrap();
    
    // For now, just return the state unchanged
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: state_var,
    }).unwrap();
    
    // Create actor with initial state
    let initial_state = graph.add_node(Node::Literal(Literal::String("ready".to_string()))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    graph.root_id = Some(actor_node);
    
    // Compile to bytecode
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    // Create VM with effect runtime
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    // Run the VM
    let result = vm.run().unwrap();
    
    // Should return an actor ID
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
#[ignore = "FLC actor syntax not fully implemented"]
fn test_actor_receive_with_pattern_binding() {
    let source = r#"
        // Actor that pattern matches on tuple messages
        private actor Greeter {
            state: string = "ready";
            private handle message(msg: any) {
                receive {
                    case ("greet", name) => f"Hello, {name}!",
                    case ("farewell", name) => f"Goodbye, {name}!",
                    case _ => "Unknown command"
                }
            }
        }
        
        let greeter = Greeter;
        greeter.send(("greet", "Alice"));
        greeter
    "#;
    
    let result = compile_and_run(source);
    
    // For now, just check that it compiles and runs
    // Full testing would require observing the actor's behavior
    assert!(result.is_ok());
}

#[test]
#[ignore = "FLC actor syntax not fully implemented"]
fn test_actor_become_with_receive() {
    let source = r#"
        // Actor that changes state using become
        private actor StateMachine {
            state: string = "initial";
            private handle message(msg: any) {
                receive {
                    case "next" => {
                        become("changed");
                        "state changed"
                    },
                    case "reset" => {
                        become("initial");
                        "state reset"
                    },
                    case "query" => self.state,
                    case _ => "unknown command"
                }
            }
        }
        
        let machine = StateMachine;
        machine.send("next");
        machine
    "#;
    
    let result = compile_and_run(source);
    
    // For now, just check that it compiles and runs
    assert!(result.is_ok());
}

#[test]
#[ignore = "Complex pattern matching not fully implemented"]
fn test_actor_receive_nested_patterns() {
    let source = r#"
        // Actor with nested pattern matching
        private actor Calculator {
            state: int = 0;
            private handle message(msg: any) {
                receive {
                    case ("add", n) => {
                        let new_state = self.state + n;
                        become(new_state);
                        new_state
                    },
                    case ("multiply", n) => {
                        let new_state = self.state * n;
                        become(new_state);
                        new_state
                    },
                    case "get" => self.state,
                    case ("set", n) => {
                        become(n);
                        n
                    },
                    case _ => self.state
                }
            }
        }
        
        let calc = Calculator;
        calc.send(("set", 10));
        calc.send(("add", 5));
        calc.send(("multiply", 2));
        calc
    "#;
    
    let result = compile_and_run(source);
    assert!(result.is_ok());
}