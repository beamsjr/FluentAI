//! Async and channel operation tests for FluentAI compiler

use fluentai_core::ast::{Graph, Node, Literal};
use fluentai_core::value::Value;
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    bytecode::Opcode,
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn compile_and_check_opcodes(graph: &Graph, expected_opcodes: &[Opcode]) -> Result<()> {
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(graph)?;
    
    let main_chunk = &bytecode.chunks[0];
    for opcode in expected_opcodes {
        assert!(
            main_chunk.instructions.iter().any(|instr| instr.opcode == *opcode),
            "Expected opcode {:?} not found", opcode
        );
    }
    
    Ok(())
}

#[test]
fn test_compile_channel() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (channel)
    let chan = graph.add_node(Node::Channel).expect("Failed to add node");
    graph.root_id = Some(chan);
    
    compile_and_check_opcodes(&graph, &[Opcode::Channel])?;
    Ok(())
}

#[test]
fn test_compile_send() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (send chan 42)
    let chan_var = graph.add_node(Node::Variable { name: "chan".to_string() }).expect("Failed to add node");
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    
    let send_node = graph.add_node(Node::Send {
        channel: chan_var,
        value,
    }).expect("Failed to add node");
    graph.root_id = Some(send_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Send])?;
    Ok(())
}

#[test]
fn test_compile_receive() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (receive chan)
    let chan_var = graph.add_node(Node::Variable { name: "chan".to_string() }).expect("Failed to add node");
    
    let recv_node = graph.add_node(Node::Receive {
        channel: chan_var,
    }).expect("Failed to add node");
    graph.root_id = Some(recv_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Receive])?;
    Ok(())
}

#[test]
fn test_compile_async() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (async (+ 1 2))
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    
    let add = graph.add_node(Node::Application {
        function: plus,
        args: vec![one, two],
    }).expect("Failed to add node");
    
    let async_node = graph.add_node(Node::Async {
        body: add,
    }).expect("Failed to add node");
    graph.root_id = Some(async_node);
    
    // Async should compile the body normally (no special opcode yet)
    compile_and_check_opcodes(&graph, &[Opcode::Add])?;
    Ok(())
}

#[test]
fn test_compile_await() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (await promise)
    let promise_var = graph.add_node(Node::Variable { name: "promise".to_string() }).expect("Failed to add node");
    
    let await_node = graph.add_node(Node::Await {
        expr: promise_var,
    }).expect("Failed to add node");
    graph.root_id = Some(await_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Await])?;
    Ok(())
}

#[test]
fn test_compile_spawn() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (spawn (worker))
    let worker_var = graph.add_node(Node::Variable { name: "worker".to_string() }).expect("Failed to add node");
    let call_worker = graph.add_node(Node::Application {
        function: worker_var,
        args: vec![],
    }).expect("Failed to add node");
    
    let spawn_node = graph.add_node(Node::Spawn {
        expr: call_worker,
    }).expect("Failed to add node");
    graph.root_id = Some(spawn_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Spawn])?;
    Ok(())
}

#[test]
fn test_compile_channel_operations_sequence() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (let ((ch (channel)))
    //          (send ch 10)
    //          (receive ch))
    
    let channel_node = graph.add_node(Node::Channel).expect("Failed to add node");
    
    let ch_var1 = graph.add_node(Node::Variable { name: "ch".to_string() }).expect("Failed to add node");
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).expect("Failed to add node");
    let send_node = graph.add_node(Node::Send {
        channel: ch_var1,
        value: ten,
    }).expect("Failed to add node");
    
    let ch_var2 = graph.add_node(Node::Variable { name: "ch".to_string() }).expect("Failed to add node");
    let recv_node = graph.add_node(Node::Receive {
        channel: ch_var2,
    }).expect("Failed to add node");
    
    // Create sequence of operations
    let seq = graph.add_node(Node::List(vec![send_node, recv_node])).expect("Failed to add node");
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel_node)],
        body: seq,
    }).expect("Failed to add node");
    graph.root_id = Some(let_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Channel, Opcode::Send, Opcode::Receive])?;
    Ok(())
}

#[test]
fn test_compile_async_with_await() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (async (let ((result (await (fetch url))))
    //                 (+ result 1)))
    
    let fetch_var = graph.add_node(Node::Variable { name: "fetch".to_string() }).expect("Failed to add node");
    let url_var = graph.add_node(Node::Variable { name: "url".to_string() }).expect("Failed to add node");
    let fetch_call = graph.add_node(Node::Application {
        function: fetch_var,
        args: vec![url_var],
    }).expect("Failed to add node");
    
    let await_node = graph.add_node(Node::Await {
        expr: fetch_call,
    }).expect("Failed to add node");
    
    let result_var = graph.add_node(Node::Variable { name: "result".to_string() }).expect("Failed to add node");
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    let add = graph.add_node(Node::Application {
        function: plus,
        args: vec![result_var, one],
    }).expect("Failed to add node");
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("result".to_string(), await_node)],
        body: add,
    }).expect("Failed to add node");
    
    let async_node = graph.add_node(Node::Async {
        body: let_node,
    }).expect("Failed to add node");
    graph.root_id = Some(async_node);
    
    compile_and_check_opcodes(&graph, &[Opcode::Await, Opcode::Add])?;
    Ok(())
}

#[test]
fn test_compile_spawn_multiple() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (list (spawn (worker 1)) (spawn (worker 2)))
    
    let worker_var1 = graph.add_node(Node::Variable { name: "worker".to_string() }).expect("Failed to add node");
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let call1 = graph.add_node(Node::Application {
        function: worker_var1,
        args: vec![one],
    }).expect("Failed to add node");
    let spawn1 = graph.add_node(Node::Spawn { expr: call1 }).expect("Failed to add node");
    
    let worker_var2 = graph.add_node(Node::Variable { name: "worker".to_string() }).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let call2 = graph.add_node(Node::Application {
        function: worker_var2,
        args: vec![two],
    }).expect("Failed to add node");
    let spawn2 = graph.add_node(Node::Spawn { expr: call2 }).expect("Failed to add node");
    
    let list_node = graph.add_node(Node::List(vec![spawn1, spawn2])).expect("Failed to add node");
    graph.root_id = Some(list_node);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Should have two spawn instructions
    let main_chunk = &bytecode.chunks[0];
    let spawn_count = main_chunk.instructions.iter()
        .filter(|instr| instr.opcode == Opcode::Spawn)
        .count();
    assert_eq!(spawn_count, 2);
    
    Ok(())
}

#[test]
fn test_compile_channel_in_closure() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create (lambda (msg) 
    //          (let ((ch (channel)))
    //            (send ch msg)
    //            ch))
    
    let channel_node = graph.add_node(Node::Channel).expect("Failed to add node");
    
    let ch_var1 = graph.add_node(Node::Variable { name: "ch".to_string() }).expect("Failed to add node");
    let msg_var = graph.add_node(Node::Variable { name: "msg".to_string() }).expect("Failed to add node");
    let send_node = graph.add_node(Node::Send {
        channel: ch_var1,
        value: msg_var,
    }).expect("Failed to add node");
    
    let ch_var2 = graph.add_node(Node::Variable { name: "ch".to_string() }).expect("Failed to add node");
    
    // Sequence: send then return channel
    let seq = graph.add_node(Node::List(vec![send_node, ch_var2])).expect("Failed to add node");
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel_node)],
        body: seq,
    }).expect("Failed to add node");
    
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["msg".to_string()],
        body: let_node,
    }).expect("Failed to add node");
    graph.root_id = Some(lambda);
    
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: false,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Lambda should create a new chunk
    assert!(bytecode.chunks.len() >= 2);
    
    Ok(())
}