//! Simple actor tests to verify basic functionality

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;
use fluentai_core::ast::{Graph, Node, NodeId, Literal};

fn create_simple_actor_test() -> Result<Graph> {
    let mut graph = Graph::new();
    
    // Create a simple handler that returns the state
    let state_var = graph.add_node(Node::Variable { name: "state".to_string() })?;
    let msg_var = graph.add_node(Node::Variable { name: "msg".to_string() })?;
    
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: state_var,
    })?;
    
    // Create actor with initial state 0
    let initial_state = graph.add_node(Node::Literal(Literal::Integer(0)))?;
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    })?;
    
    graph.root_id = Some(actor_node);
    Ok(graph)
}

fn compile_and_run(graph: &Graph) -> Result<Value> {
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
fn test_create_simple_actor() {
    let graph = create_simple_actor_test().unwrap();
    let result = compile_and_run(&graph).unwrap();
    
    // Should return an actor ID
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test]
fn test_actor_send_and_process() {
    let mut graph = Graph::new();
    
    // Create a counter handler that increments state
    let state_var = graph.add_node(Node::Variable { name: "state".to_string() }).unwrap();
    let msg_var = graph.add_node(Node::Variable { name: "msg".to_string() }).unwrap();
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
    
    // Create application node for state + 1
    let add_op = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
    let increment = graph.add_node(Node::Application {
        function: add_op,
        args: vec![state_var, one],
    }).unwrap();
    
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: increment,
    }).unwrap();
    
    // Create actor with initial state 0
    let initial_state = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    // Create message to send
    let message = graph.add_node(Node::Literal(Literal::String("inc".to_string()))).unwrap();
    
    // Send message to actor
    let send_node = graph.add_node(Node::ActorSend {
        actor: actor_node,
        message,
    }).unwrap();
    
    graph.root_id = Some(send_node);
    
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

    // Run should create actor and send message
    let result = vm.run().unwrap();
    
    // Send returns nil
    assert_eq!(result, Value::Nil);
    
    // Process actor messages
    vm.process_all_actor_messages().unwrap();
    
    // The actor's state should have been incremented
    // (We can't easily check this without exposing actor internals)
}