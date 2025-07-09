//! Debug test for match optimization issue

use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::{
    compiler::{Compiler, CompilerOptions},
    Value,
    VM,
};
use fluentai_optimizer::OptimizationLevel;
use anyhow::Result;

fn compile_and_run_with_optimization(graph: &Graph, optimization_level: OptimizationLevel) -> Result<Value> {
    println!("\n=== Compiling with {:?} ===", optimization_level);
    
    let options = CompilerOptions {
        optimization_level,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    
    println!("Graph before compilation:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    match compiler.compile(graph) {
        Ok(bytecode) => {
            println!("Compilation successful");
            let mut vm = VM::new(bytecode);
            Ok(vm.run()?)
        }
        Err(e) => {
            println!("Compilation failed: {:?}", e);
            Err(e)
        }
    }
}

#[test]
fn test_debug_match_optimization() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create the exact same graph as the failing test
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let two = graph.add_node(Node::Literal(Literal::Integer(2))).expect("Failed to add node");
    let list_val = graph.add_node(Node::List(vec![one, two])).expect("Failed to add node");
    
    let result_val = graph.add_node(Node::Literal(Literal::Integer(99))).expect("Failed to add node");
    let zero = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    
    let match_node = graph.add_node(Node::Match {
        expr: list_val,
        branches: vec![
            (Pattern::Constructor { 
                name: "cons".to_string(), 
                patterns: vec![
                    Pattern::Variable("x".to_string()),
                    Pattern::Variable("xs".to_string()),
                ],
            }, result_val),
            (Pattern::Wildcard, zero),
        ],
    }).expect("Failed to add node");
    graph.root_id = Some(match_node);
    
    println!("Original graph:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Test without optimization first
    println!("\nTesting without optimization...");
    let result = compile_and_run_with_optimization(&graph, OptimizationLevel::None)?;
    assert_eq!(result, Value::Integer(99));
    println!("✓ Success without optimization");
    
    // Test with standard optimization
    println!("\nTesting with standard optimization...");
    let result = compile_and_run_with_optimization(&graph, OptimizationLevel::Standard)?;
    assert_eq!(result, Value::Integer(99));
    println!("✓ Success with standard optimization");
    
    Ok(())
}