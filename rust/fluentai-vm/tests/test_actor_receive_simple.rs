//! Simple test for actor receive functionality

use fluentai_core::value::Value;
use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::{compiler::{Compiler, CompilerOptions}, VM};
use std::sync::Arc;
use fluentai_effects::EffectRuntime;
use fluentai_optimizer::OptimizationLevel;

#[test]
fn test_actor_receive_returns_value() {
    let mut graph = Graph::new();
    
    // Create a simple handler that uses receive to return different values
    let handler_body = {
        // Create literals for the return values
        let pong_lit = graph.add_node(Node::Literal(Literal::String("pong".to_string()))).unwrap();
        let world_lit = graph.add_node(Node::Literal(Literal::String("world".to_string()))).unwrap();
        let unknown_lit = graph.add_node(Node::Literal(Literal::String("unknown".to_string()))).unwrap();
        
        let patterns = vec![
            (Pattern::Literal(Literal::String("ping".to_string())), pong_lit),
            (Pattern::Literal(Literal::String("hello".to_string())), world_lit),
            (Pattern::Wildcard, unknown_lit),
        ];
        
        graph.add_node(Node::ActorReceive { patterns, timeout: None }).unwrap()
    };
    
    // Create the handler lambda
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: handler_body,
    }).unwrap();
    
    // Create actor with initial state
    let initial_state = graph.add_node(Node::Literal(Literal::String("ready".to_string()))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    graph.root_id = Some(actor_node);
    
    // Compile
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph).unwrap();

    // Create VM
    let runtime = Arc::new(EffectRuntime::new().unwrap());
    let mut vm = VM::new(bytecode);
    vm.set_effect_runtime(runtime);

    // Run - should create an actor
    let result = vm.run().unwrap();
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}

#[test] 
fn test_actor_with_become() {
    let mut graph = Graph::new();
    
    // Create a handler that uses become
    let new_state = graph.add_node(Node::Literal(Literal::String("new_state".to_string()))).unwrap();
    let become_node = graph.add_node(Node::Become { new_state }).unwrap();
    
    // Become doesn't return a value, so we need to return something explicitly
    let return_value = graph.add_node(Node::Literal(Literal::String("processed".to_string()))).unwrap();
    let handler_body = graph.add_node(Node::Begin { 
        exprs: vec![become_node, return_value] 
    }).unwrap();
    
    let handler = graph.add_node(Node::Lambda {
        params: vec!["state".to_string(), "msg".to_string()],
        body: handler_body,
    }).unwrap();
    
    // Create actor
    let initial_state = graph.add_node(Node::Literal(Literal::String("initial".to_string()))).unwrap();
    let actor_node = graph.add_node(Node::Actor {
        initial_state,
        handler,
    }).unwrap();
    
    graph.root_id = Some(actor_node);
    
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

    let result = vm.run().unwrap();
    match result {
        Value::Actor(_) => {},
        _ => panic!("Expected actor value, got {:?}", result),
    }
}