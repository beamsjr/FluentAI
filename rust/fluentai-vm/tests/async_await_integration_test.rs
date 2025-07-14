//! Integration tests for async/await functionality

use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_core::value::Value;
use fluentai_vm::{Compiler, VM};

#[test]
fn test_async_creates_promise() {
    println!("\n=== Testing async creates promise ===");
    
    // Create AST for: async { 42 }
    let mut graph = Graph::new();
    let const_node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let async_node = graph.add_node(Node::Async { body: const_node }).unwrap();
    graph.root_id = Some(async_node);
    
    // Compile
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    
    println!("Bytecode has {} chunks", bytecode.chunks.len());
    println!("Main chunk: {}", bytecode.main_chunk);
    
    // Run
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    println!("Result: {:?}", result);
    
    // Should get a promise
    match result {
        Value::Promise(id) => {
            println!("✓ Created promise with ID: {}", id);
            assert!(id > 0);
        }
        _ => panic!("Expected promise, got {:?}", result),
    }
}

#[test]
fn test_promise_new_builtin() {
    println!("\n=== Testing promise_new builtin ===");
    
    // Create AST for: promise_new(() => 42)
    let mut graph = Graph::new();
    
    // Create lambda: () => 42
    let const_node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let lambda_node = graph.add_node(Node::Lambda {
        params: vec![],
        body: const_node,
    }).unwrap();
    
    // Call promise_new with the lambda
    let promise_new = graph.add_node(Node::Variable {
        name: "promise_new".to_string(),
    }).unwrap();
    let call_node = graph.add_node(Node::Application {
        function: promise_new,
        args: vec![lambda_node],
    }).unwrap();
    
    graph.root_id = Some(call_node);
    
    // Compile and run
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    println!("Result: {:?}", result);
    
    // Should get a promise
    match result {
        Value::Promise(id) => {
            println!("✓ promise_new created promise with ID: {}", id);
        }
        _ => panic!("Expected promise from promise_new, got {:?}", result),
    }
}

#[test] 
fn test_promise_all() {
    println!("\n=== Testing promise_all ===");
    
    // Create AST for: promise_all([p1, p2, p3])
    let mut graph = Graph::new();
    
    // Create three promises
    let mut promise_nodes = vec![];
    for i in 1..=3 {
        let const_node = graph.add_node(Node::Literal(Literal::Integer(i))).unwrap();
        let lambda_node = graph.add_node(Node::Lambda {
            params: vec![],
            body: const_node,
        }).unwrap();
        
        let promise_new = graph.add_node(Node::Variable {
            name: "promise_new".to_string(),
        }).unwrap();
        let promise_node = graph.add_node(Node::Application {
            function: promise_new,
            args: vec![lambda_node],
        }).unwrap();
        
        promise_nodes.push(promise_node);
    }
    
    // Create list of promises
    let list_node = graph.add_node(Node::List(promise_nodes)).unwrap();
    
    // Call promise_all
    let promise_all = graph.add_node(Node::Variable {
        name: "promise_all".to_string(),
    }).unwrap();
    let call_node = graph.add_node(Node::Application {
        function: promise_all,
        args: vec![list_node],
    }).unwrap();
    
    graph.root_id = Some(call_node);
    
    // Compile and run
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();
    
    println!("Result: {:?}", result);
    
    // Should get a promise that combines all three
    match result {
        Value::Promise(id) => {
            println!("✓ promise_all created combined promise with ID: {}", id);
        }
        _ => panic!("Expected promise from promise_all, got {:?}", result),
    }
}

#[test]
fn test_continuation_mechanics() {
    println!("\n=== Testing continuation mechanics ===");
    
    // Verify that our continuation support is available
    use fluentai_vm::continuation::ContinuationSupport;
    
    let bytecode = fluentai_vm::Bytecode::new();
    let mut vm = VM::new(bytecode);
    
    // Push a call frame
    vm.call_stack_mut().push(fluentai_vm::vm::CallFrame {
        chunk_id: 0,
        ip: 0,
        stack_base: 0,
        env: vec![],
        start_time: None,
    });
    
    // Test suspension
    match vm.suspend_execution(0) {
        Ok(continuation_id) => {
            println!("✓ Successfully created continuation: {:?}", continuation_id);
            
            // Test resumption
            match vm.resume_continuation(continuation_id, Value::Integer(42)) {
                Ok(()) => {
                    println!("✓ Successfully resumed continuation");
                }
                Err(e) => panic!("Failed to resume continuation: {}", e),
            }
        }
        Err(e) => panic!("Failed to suspend execution: {}", e),
    }
}