//! Debug test to investigate the compilation issue

use fluentai_core::ast::{Graph, Node, Literal, Pattern};
use fluentai_vm::compiler::{Compiler, CompilerOptions};
use fluentai_optimizer::{OptimizationLevel, OptimizationConfig, OptimizationPipeline};
use anyhow::Result;

#[test]
fn test_debug_compilation_issue() -> Result<()> {
    let mut graph = Graph::new();
    
    // Create the exact same graph as the failing test
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let list_val = graph.add_node(Node::List(vec![one, two]));
    
    let result_val = graph.add_node(Node::Literal(Literal::Integer(99)));
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    
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
    });
    graph.root_id = Some(match_node);
    
    println!("Original graph:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Manually run optimization
    println!("\nRunning optimization...");
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph)?;
    
    println!("\nOptimized graph:");
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Now try to compile the optimized graph
    println!("\nCompiling optimized graph...");
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None, // Don't optimize again
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    
    match compiler.compile(&optimized) {
        Ok(_) => println!("✓ Compilation successful"),
        Err(e) => {
            println!("✗ Compilation failed: {}", e);
            return Err(e);
        }
    }
    
    Ok(())
}