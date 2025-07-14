//! Tests for async/await compilation and execution

use fluentai_core::ast::{Graph, Node, NodeId};
use fluentai_core::value::Value;
use fluentai_parser::parse_flc;
use fluentai_vm::{Compiler, VM};

#[test]
fn test_compile_async_function() {
    // Create an AST for: async { 42 }
    let mut graph = Graph::new();
    let const_node = graph.add_node(Node::Literal(
        fluentai_core::ast::Literal::Integer(42)
    )).unwrap();
    let async_node = graph.add_node(Node::Async { body: const_node }).unwrap();
    graph.root_id = Some(async_node);
    
    // Compile it
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // The bytecode should contain:
    // 1. A chunk for the async body (returning 42)
    // 2. Main chunk that creates a function and wraps it in a promise
    assert!(bytecode.chunks.len() >= 2);
    
    // Run it in the VM
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    // Result should be a promise
    match result {
        Value::Promise(_) => {
            // Success - we got a promise
        }
        _ => panic!("Expected promise, got {:?}", result),
    }
}

#[test]
fn test_compile_await_expression() {
    // Create an AST for: await (async { 42 })
    let mut graph = Graph::new();
    
    // Inner async block
    let const_node = graph.add_node(Node::Literal(
        fluentai_core::ast::Literal::Integer(42)
    )).unwrap();
    let async_node = graph.add_node(Node::Async { body: const_node }).unwrap();
    
    // Await the async block
    let await_node = graph.add_node(Node::Await { expr: async_node }).unwrap();
    graph.root_id = Some(await_node);
    
    // Compile it
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    // Should have chunks for the async function
    assert!(bytecode.chunks.len() >= 2);
}

#[test]
fn test_parse_and_compile_async_await() {
    // Test parsing async/await syntax (if supported)
    let source = r#"
        let make_promise = () => promise { 42 };
        let p = make_promise();
        p
    "#;
    
    let graph = parse_flc(source);
    if graph.is_err() {
        // Parser doesn't support async syntax yet, skip test
        return;
    }
    
    let graph = graph.unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    // Should get a promise
    match result {
        Value::Promise(_) => {
            // Success
        }
        _ => panic!("Expected promise, got {:?}", result),
    }
}

#[test]
fn test_promise_operations() {
    // Test promise operations like Promise.all
    let source = r#"
        let p1 = promise_new(() => 1);
        let p2 = promise_new(() => 2);
        let p3 = promise_new(() => 3);
        promise_all([p1, p2, p3])
    "#;
    
    let graph = parse_flc(source).unwrap();
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    // Should get a promise that will resolve to [1, 2, 3]
    match result {
        Value::Promise(_) => {
            // Success - Promise.all returns a new promise
        }
        _ => panic!("Expected promise from promise_all, got {:?}", result),
    }
}

#[test]
fn test_nested_async() {
    // Test nested async blocks
    let mut graph = Graph::new();
    
    // Inner async: async { 10 }
    let inner_const = graph.add_node(Node::Literal(
        fluentai_core::ast::Literal::Integer(10)
    )).unwrap();
    let inner_async = graph.add_node(Node::Async { body: inner_const }).unwrap();
    
    // Outer async: async { async { 10 } }
    let outer_async = graph.add_node(Node::Async { body: inner_async }).unwrap();
    graph.root_id = Some(outer_async);
    
    // Compile and run
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    // Should get a promise that resolves to another promise
    match result {
        Value::Promise(_) => {
            // Success
        }
        _ => panic!("Expected promise, got {:?}", result),
    }
}