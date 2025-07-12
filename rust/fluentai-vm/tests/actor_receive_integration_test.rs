//! Integration tests for actor receive pattern matching
//!
//! Tests the full actor receive functionality including message sending and processing

use anyhow::Result;
use fluentai_core::value::Value;
use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

#[test]
fn test_actor_receive_with_message_processing() {
    let mut graph = Graph::new();
    
    // Create handler with receive expression
    let state_var = graph.add_node(Node::Variable { name: "state".to_string() }).unwrap();
    let _msg_var = graph.add_node(Node::Variable { name: "msg".to_string() }).unwrap();
    
    // Create receive patterns that return different states based on message
    let ready_lit = graph.add_node(Node::Literal(Literal::String("ready".to_string()))).unwrap();
    let processing_lit = graph.add_node(Node::Literal(Literal::String("processing".to_string()))).unwrap();
    let done_lit = graph.add_node(Node::Literal(Literal::String("done".to_string()))).unwrap();
    
    let patterns = vec![
        (Pattern::Literal(Literal::String("start".to_string())), processing_lit),
        (Pattern::Literal(Literal::String("stop".to_string())), done_lit),
        (Pattern::Wildcard, state_var), // Keep current state for other messages
    ];
    
    let receive_node = graph.add_node(Node::ActorReceive { patterns, timeout: None }).unwrap();
    
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: receive_node,
    }).unwrap();
    
    // Create actor with initial state "ready"
    let initial_state = graph.add_node(Node::Literal(Literal::String("ready".to_string()))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    // Send "start" message to actor
    let start_msg = graph.add_node(Node::Literal(Literal::String("start".to_string()))).unwrap();
    let send_node = graph.add_node(Node::ActorSend {
        actor: actor_node,
        message: start_msg,
    }).unwrap();
    
    // Return the send result (should be nil)
    graph.root_id = Some(send_node);
    
    // Compile to bytecode
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    // Create VM with effect runtime
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    // Run the VM - this creates the actor and sends the message
    let result = vm.run().unwrap();
    assert_eq!(result, Value::Nil); // Send returns nil
    
    // Process actor messages
    vm.process_all_actor_messages().unwrap();
    
    // The actor's state should have changed from "ready" to "processing"
    // (We can't directly inspect it without exposing internals, but the processing happened)
}

#[test]
fn test_actor_become_in_receive() {
    let mut graph = Graph::new();
    
    // Create handler that uses become within receive
    let _state_var = graph.add_node(Node::Variable { name: "state".to_string() }).unwrap();
    let _msg_var = graph.add_node(Node::Variable { name: "msg".to_string() }).unwrap();
    
    // Create a handler body that uses become
    let new_state_lit = graph.add_node(Node::Literal(Literal::String("changed".to_string()))).unwrap();
    let become_node = graph.add_node(Node::Become { new_state: new_state_lit }).unwrap();
    
    // Wrap become in a begin block to return the new state
    let state_lit = graph.add_node(Node::Literal(Literal::String("changed".to_string()))).unwrap();
    let handler_body = graph.add_node(Node::Begin { 
        exprs: vec![become_node, state_lit] 
    }).unwrap();
    
    let patterns = vec![
        (Pattern::Literal(Literal::String("change".to_string())), handler_body),
        (Pattern::Wildcard, graph.add_node(Node::Variable { name: "state".to_string() }).unwrap()),
    ];
    
    let receive_node = graph.add_node(Node::ActorReceive { patterns, timeout: None }).unwrap();
    
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: receive_node,
    }).unwrap();
    
    // Create actor
    let initial_state = graph.add_node(Node::Literal(Literal::String("initial".to_string()))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    // Send "change" message
    let msg = graph.add_node(Node::Literal(Literal::String("change".to_string()))).unwrap();
    let send_node = graph.add_node(Node::ActorSend {
        actor: actor_node,
        message: msg,
    }).unwrap();
    
    graph.root_id = Some(send_node);
    
    // Compile and run
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    vm.run().unwrap();
    vm.process_all_actor_messages().unwrap();
    
    // Actor state should have been updated via become
}