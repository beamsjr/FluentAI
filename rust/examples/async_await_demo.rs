//! Demonstrates async/await implementation in FluentAI
//!
//! This example shows:
//! 1. Creating async functions
//! 2. Awaiting promises
//! 3. Promise.all for concurrent operations
//! 4. The continuation/suspension mechanism

use fluentai_core::ast::{Graph, Literal, Node};
use fluentai_parser::parse_flc;
use fluentai_vm::{Compiler, VM};
use fluentai_core::value::Value;

fn main() {
    println!("=== FluentAI Async/Await Demo ===\n");
    
    // Example 1: Basic async function
    println!("1. Basic async function:");
    demo_basic_async();
    
    // Example 2: Parsing async from FLC syntax
    println!("\n2. Async from FLC syntax:");
    demo_flc_async();
    
    // Example 3: Promise operations
    println!("\n3. Promise operations:");
    demo_promise_operations();
    
    // Example 4: Nested async
    println!("\n4. Nested async:");
    demo_nested_async();
}

fn demo_basic_async() {
    // Create AST for: async { 42 }
    let mut graph = Graph::new();
    let const_node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let async_node = graph.add_node(Node::Async { body: const_node }).unwrap();
    graph.root_id = Some(async_node);
    
    // Compile and run
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(Value::Promise(id)) => {
            println!("  Created promise with ID: {}", id);
        }
        Ok(v) => println!("  Unexpected result: {:?}", v),
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_flc_async() {
    let source = r#"
        // Define an async computation
        let compute_async = () => promise_new(() => {
            // Simulate some computation
            40 + 2
        });
        
        // Create the promise
        let result = compute_async();
        result
    "#;
    
    match parse_flc(source) {
        Ok(graph) => {
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&graph).unwrap();
            let mut vm = VM::new(bytecode);
            
            match vm.run() {
                Ok(Value::Promise(id)) => {
                    println!("  Created promise from FLC: {}", id);
                }
                Ok(v) => println!("  Result: {:?}", v),
                Err(e) => println!("  Error: {}", e),
            }
        }
        Err(e) => {
            println!("  Parse error: {}", e);
            println!("  Note: FLC parser may not yet support async syntax");
        }
    }
}

fn demo_promise_operations() {
    let source = r#"
        // Create multiple promises
        let p1 = promise_new(() => 1);
        let p2 = promise_new(() => 2);
        let p3 = promise_new(() => 3);
        
        // Use Promise.all to wait for all
        let all_results = promise_all([p1, p2, p3]);
        all_results
    "#;
    
    match parse_flc(source) {
        Ok(graph) => {
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&graph).unwrap();
            let mut vm = VM::new(bytecode);
            
            match vm.run() {
                Ok(Value::Promise(id)) => {
                    println!("  Promise.all created promise: {}", id);
                    println!("  (Would resolve to [1, 2, 3] when executed async)");
                }
                Ok(v) => println!("  Result: {:?}", v),
                Err(e) => println!("  Error: {}", e),
            }
        }
        Err(e) => println!("  Parse error: {}", e),
    }
}

fn demo_nested_async() {
    // Create AST for: async { async { 42 } }
    let mut graph = Graph::new();
    
    // Inner async
    let const_node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let inner_async = graph.add_node(Node::Async { body: const_node }).unwrap();
    
    // Outer async
    let outer_async = graph.add_node(Node::Async { body: inner_async }).unwrap();
    graph.root_id = Some(outer_async);
    
    // Compile and run
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();
    let mut vm = VM::new(bytecode);
    
    match vm.run() {
        Ok(Value::Promise(id)) => {
            println!("  Created nested promise: {}", id);
            println!("  (Outer promise would resolve to inner promise)");
        }
        Ok(v) => println!("  Unexpected result: {:?}", v),
        Err(e) => println!("  Error: {}", e),
    }
}

// Note: To see actual async execution, you would need to use AsyncVM:
// 
// use fluentai_vm::AsyncVM;
// use tokio;
// 
// #[tokio::main]
// async fn main() {
//     let mut async_vm = AsyncVM::new(vm);
//     match async_vm.run().await {
//         Ok(value) => println!("Async result: {:?}", value),
//         Err(e) => println!("Async error: {}", e),
//     }
// }