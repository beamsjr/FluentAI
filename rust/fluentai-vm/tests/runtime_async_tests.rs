//! Runtime tests for async functionality in FluentAI VM

use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_core::value::Value;
use fluentai_optimizer::OptimizationLevel;
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    VM,
};
use std::time::Duration;
use tokio::time::timeout;

async fn compile_and_run_async(graph: &Graph) -> Result<Value> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;

    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    Ok(result)
}

#[tokio::test]
async fn test_simple_async_block() -> Result<()> {
    let mut graph = Graph::new();

    // Create (async 42)
    let value = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");

    let async_node = graph
        .add_node(Node::Async { body: value })
        .expect("Failed to add node");
    graph.root_id = Some(async_node);

    let result = compile_and_run_async(&graph).await?;
    
    // The result should be a promise
    assert!(matches!(result, Value::Promise(_)));
    Ok(())
}

#[tokio::test]
async fn test_async_with_await() -> Result<()> {
    let mut graph = Graph::new();

    // Create (let ((promise (async 42))) (await promise))
    let value = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");

    let async_node = graph
        .add_node(Node::Async { body: value })
        .expect("Failed to add node");

    let promise_var = graph
        .add_node(Node::Variable {
            name: "promise".to_string(),
        })
        .expect("Failed to add node");

    let await_node = graph
        .add_node(Node::Await { expr: promise_var })
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![("promise".to_string(), async_node)],
            body: await_node,
        })
        .expect("Failed to add node");
    graph.root_id = Some(let_node);

    let result = timeout(Duration::from_secs(5), compile_and_run_async(&graph)).await??;
    
    // The result should be the awaited value
    assert_eq!(result, Value::Integer(42));
    Ok(())
}

#[tokio::test]
async fn test_async_with_computation() -> Result<()> {
    let mut graph = Graph::new();

    // Create (async (+ 1 2))
    let plus = graph
        .add_node(Node::Variable {
            name: "+".to_string(),
        })
        .expect("Failed to add node");
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");

    let add = graph
        .add_node(Node::Application {
            function: plus,
            args: vec![one, two],
        })
        .expect("Failed to add node");

    let async_node = graph
        .add_node(Node::Async { body: add })
        .expect("Failed to add node");
    graph.root_id = Some(async_node);

    let result = compile_and_run_async(&graph).await?;
    
    // The result should be a promise
    assert!(matches!(result, Value::Promise(_)));
    Ok(())
}

#[tokio::test]
async fn test_nested_async_await() -> Result<()> {
    let mut graph = Graph::new();

    // Create (async (let ((inner (async 10))) (await inner)))
    let ten = graph
        .add_node(Node::Literal(Literal::Integer(10)))
        .expect("Failed to add node");

    let inner_async = graph
        .add_node(Node::Async { body: ten })
        .expect("Failed to add node");

    let inner_var = graph
        .add_node(Node::Variable {
            name: "inner".to_string(),
        })
        .expect("Failed to add node");

    let await_inner = graph
        .add_node(Node::Await { expr: inner_var })
        .expect("Failed to add node");

    let inner_let = graph
        .add_node(Node::Let {
            bindings: vec![("inner".to_string(), inner_async)],
            body: await_inner,
        })
        .expect("Failed to add node");

    let outer_async = graph
        .add_node(Node::Async { body: inner_let })
        .expect("Failed to add node");

    let outer_var = graph
        .add_node(Node::Variable {
            name: "outer".to_string(),
        })
        .expect("Failed to add node");

    let await_outer = graph
        .add_node(Node::Await { expr: outer_var })
        .expect("Failed to add node");

    let outer_let = graph
        .add_node(Node::Let {
            bindings: vec![("outer".to_string(), outer_async)],
            body: await_outer,
        })
        .expect("Failed to add node");

    graph.root_id = Some(outer_let);

    let result = timeout(Duration::from_secs(5), compile_and_run_async(&graph)).await??;
    
    // The result should be the nested awaited value
    assert_eq!(result, Value::Integer(10));
    Ok(())
}

#[tokio::test]
async fn test_multiple_async_blocks() -> Result<()> {
    let mut graph = Graph::new();

    // Create (let ((p1 (async 1)) (p2 (async 2))) (list (await p1) (await p2)))
    let one = graph
        .add_node(Node::Literal(Literal::Integer(1)))
        .expect("Failed to add node");
    let two = graph
        .add_node(Node::Literal(Literal::Integer(2)))
        .expect("Failed to add node");

    let async1 = graph
        .add_node(Node::Async { body: one })
        .expect("Failed to add node");
    let async2 = graph
        .add_node(Node::Async { body: two })
        .expect("Failed to add node");

    let p1_var = graph
        .add_node(Node::Variable {
            name: "p1".to_string(),
        })
        .expect("Failed to add node");
    let p2_var = graph
        .add_node(Node::Variable {
            name: "p2".to_string(),
        })
        .expect("Failed to add node");

    let await1 = graph
        .add_node(Node::Await { expr: p1_var })
        .expect("Failed to add node");
    let await2 = graph
        .add_node(Node::Await { expr: p2_var })
        .expect("Failed to add node");

    let list_node = graph
        .add_node(Node::List(vec![await1, await2]))
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![
                ("p1".to_string(), async1),
                ("p2".to_string(), async2),
            ],
            body: list_node,
        })
        .expect("Failed to add node");

    graph.root_id = Some(let_node);

    let result = timeout(Duration::from_secs(5), compile_and_run_async(&graph)).await??;
    
    // The result should be a list with the awaited values
    if let Value::List(values) = result {
        assert_eq!(values.len(), 2);
        assert_eq!(values[0], Value::Integer(1));
        assert_eq!(values[1], Value::Integer(2));
    } else {
        panic!("Expected list result, got {:?}", result);
    }
    Ok(())
}

#[tokio::test]
async fn test_async_with_channel_operations() -> Result<()> {
    let mut graph = Graph::new();

    // Create (let ((ch (channel))) 
    //          (async (begin (send ch 42) (receive ch))))
    let channel_node = graph.add_node(Node::Channel { capacity: None }).expect("Failed to add node");

    let ch_var1 = graph
        .add_node(Node::Variable {
            name: "ch".to_string(),
        })
        .expect("Failed to add node");
    let ch_var2 = graph
        .add_node(Node::Variable {
            name: "ch".to_string(),
        })
        .expect("Failed to add node");

    let value = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");

    let send_node = graph
        .add_node(Node::Send {
            channel: ch_var1,
            value,
        })
        .expect("Failed to add node");

    let receive_node = graph
        .add_node(Node::Receive { channel: ch_var2 })
        .expect("Failed to add node");

    let sequence = graph
        .add_node(Node::List(vec![send_node, receive_node]))
        .expect("Failed to add node");

    let async_node = graph
        .add_node(Node::Async { body: sequence })
        .expect("Failed to add node");

    let async_var = graph
        .add_node(Node::Variable {
            name: "async_result".to_string(),
        })
        .expect("Failed to add node");

    let await_node = graph
        .add_node(Node::Await { expr: async_var })
        .expect("Failed to add node");

    let let_node = graph
        .add_node(Node::Let {
            bindings: vec![
                ("ch".to_string(), channel_node),
                ("async_result".to_string(), async_node),
            ],
            body: await_node,
        })
        .expect("Failed to add node");

    graph.root_id = Some(let_node);

    let result = timeout(Duration::from_secs(5), compile_and_run_async(&graph)).await??;
    
    // The result should be the value sent through the channel
    assert_eq!(result, Value::Integer(42));
    Ok(())
}