use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use fluentai_vm::{compiler::{Compiler, CompilerOptions}};

#[test]
fn test_optimizer_preserves_closure_capture() {
    // Build AST for: (let ((x 10)) ((lambda () x)))
    let mut graph = Graph::new();
    
    // x = 10
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    
    // Variable reference to x inside lambda
    let x_ref = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    
    // Lambda that captures x
    let lambda = graph.add_node(Node::Lambda {
        params: vec![],
        body: x_ref,
    }).unwrap();
    
    // Let binding
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), ten)],
        body: lambda,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    
    // Apply optimizer
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    // The optimizer should NOT inline the lambda or replace x with 10
    // because the lambda needs to capture x as a closure
    
    // Check that the optimized graph still has a Lambda node
    let has_lambda = optimized.nodes.values().any(|node| matches!(node, Node::Lambda { .. }));
    assert!(has_lambda, "Optimizer removed the lambda node");
    
    // Compile both versions
    let mut options = CompilerOptions::default();
    options.optimization_level = OptimizationLevel::None;
    let compiler_no_opt = Compiler::with_options(options);
    let bytecode_no_opt = compiler_no_opt.compile(&graph).unwrap();
    
    let compiler_opt = Compiler::new();
    let bytecode_opt = compiler_opt.compile(&optimized).unwrap();
    
    println!("\n=== Original bytecode ===");
    for (i, chunk) in bytecode_no_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    println!("\n=== Optimized bytecode ===");
    for (i, chunk) in bytecode_opt.chunks.iter().enumerate() {
        println!("Chunk {}: {:?}", i, chunk.name);
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  {}: {:?}", j, instr);
        }
    }
    
    // Both should use MakeClosure
    assert!(
        bytecode_no_opt.chunks[0].instructions.iter().any(|i| i.opcode == fluentai_bytecode::Opcode::MakeClosure),
        "Original should use MakeClosure"
    );
    assert!(
        bytecode_opt.chunks[0].instructions.iter().any(|i| i.opcode == fluentai_bytecode::Opcode::MakeClosure),
        "Optimized should use MakeClosure"
    );
}

#[test]
fn test_optimizer_application_of_closure() {
    // Build AST for: (let ((x 10)) ((lambda () x))())
    // This applies the lambda immediately
    let mut graph = Graph::new();
    
    // x = 10
    let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
    
    // Variable reference to x inside lambda
    let x_ref = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
    
    // Lambda that captures x
    let lambda = graph.add_node(Node::Lambda {
        params: vec![],
        body: x_ref,
    }).unwrap();
    
    // Apply the lambda
    let app = graph.add_node(Node::Application {
        function: lambda,
        args: vec![],
    }).unwrap();
    
    // Let binding
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("x".to_string(), ten)],
        body: app,
    }).unwrap();
    
    graph.root_id = Some(let_node);
    
    // Apply optimizer
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    let optimized = pipeline.optimize(&graph).unwrap();
    
    println!("\n=== Original AST ===");
    println!("Nodes: {}", graph.nodes.len());
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    println!("\n=== Optimized AST ===");
    println!("Nodes: {}", optimized.nodes.len());
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // It's OK for the optimizer to inline this since the lambda is immediately applied
    // But it should still work correctly
}