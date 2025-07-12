//! Tests for JIT compilation integration with the VM

#[cfg(all(feature = "jit", target_arch = "x86_64"))]
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
#[cfg(all(feature = "jit", target_arch = "x86_64"))]
use fluentai_parser::parse;
#[cfg(all(feature = "jit", target_arch = "x86_64"))]
use fluentai_vm::{Bytecode, Compiler, VM, Value};
#[cfg(all(feature = "jit", target_arch = "x86_64"))]
use std::num::NonZeroU32;

#[test]
#[cfg(all(feature = "jit", target_arch = "x86_64"))]
fn test_jit_hot_path_compilation() {
    // Create a simple recursive factorial function
    let source = r#"
        (define (factorial n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1)))))
    "#;
    
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let mut vm = VM::new(bytecode);
    
    // Enable usage tracking
    vm.enable_usage_tracking();
    
    // Register chunk mappings for tracking
    let node_id = NodeId(NonZeroU32::new(1).unwrap());
    vm.register_chunk_mapping(1, node_id); // Assuming factorial is chunk 1
    
    // Call factorial multiple times to trigger JIT compilation
    for i in 1..100 {
        // Create AST for factorial call
        let mut graph = Graph::new();
        let n = graph.add_node(Node::Literal(Literal::Integer(5)));
        let fact_sym = graph.add_node(Node::Variable { name: "factorial".to_string() });
        let call = graph.add_node(Node::Application {
            function: fact_sym,
            args: vec![n],
        });
        graph.root = Some(call);
        
        // Compile and run
        let call_bytecode = compiler.compile(&graph).unwrap();
        let mut call_vm = VM::new(call_bytecode);
        
        // Copy globals from main VM
        if let Ok(globals) = vm.get_globals() {
            call_vm.set_globals(globals);
        }
        
        let result = call_vm.run().unwrap();
        assert_eq!(result, Value::Integer(120)); // 5! = 120
        
        // After 50 calls, check if JIT compilation happened
        if i == 51 {
            let stats = vm.jit_stats();
            assert!(stats.functions_compiled > 0, "Expected JIT compilation after 50 calls");
        }
    }
    
    // Verify JIT statistics
    let final_stats = vm.jit_stats();
    println!("JIT Stats: {:?}", final_stats);
    assert!(final_stats.functions_compiled > 0);
    assert_eq!(final_stats.compilation_failures, 0);
}

#[test]
#[cfg(all(feature = "jit", target_arch = "x86_64"))]
fn test_jit_simple_arithmetic() {
    // Create a simple function that does arithmetic
    let source = r#"
        (define (compute x y)
            (+ (* x x) (* y y)))
    "#;
    
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    let mut vm = VM::new(bytecode);
    vm.enable_usage_tracking();
    
    // Register chunk mapping
    let node_id = NodeId(NonZeroU32::new(1).unwrap());
    vm.register_chunk_mapping(1, node_id);
    
    // Call the function enough times to trigger JIT
    for _ in 0..60 {
        let mut graph = Graph::new();
        let x = graph.add_node(Node::Literal(Literal::Integer(3)));
        let y = graph.add_node(Node::Literal(Literal::Integer(4)));
        let compute = graph.add_node(Node::Variable { name: "compute".to_string() });
        let call = graph.add_node(Node::Application {
            function: compute,
            args: vec![x, y],
        });
        graph.root = Some(call);
        
        let call_bytecode = compiler.compile(&graph).unwrap();
        let mut call_vm = VM::new(call_bytecode);
        
        if let Ok(globals) = vm.get_globals() {
            call_vm.set_globals(globals);
        }
        
        let result = call_vm.run().unwrap();
        assert_eq!(result, Value::Integer(25)); // 3*3 + 4*4 = 9 + 16 = 25
    }
    
    let stats = vm.jit_stats();
    assert!(stats.functions_compiled > 0, "Expected JIT compilation");
}