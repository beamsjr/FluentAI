//! Debug test for assignment optimization issue

use fluentai_vm::{Compiler, CompilerOptions};
use fluentai_optimizer::OptimizationLevel;

#[test]
fn test_assignment_with_optimization() {
    // Simple assignment test
    let code = "let x = 10; x := 42";
    
    eprintln!("Parsing code: {}", code);
    let graph = parse(code).expect("Parse failed");
    
    eprintln!("\nParsed graph:");
    eprintln!("  Root: {:?}", graph.root_id);
    eprintln!("  Nodes:");
    for (id, node) in &graph.nodes {
        eprintln!("    {:?}: {:?}", id, node);
    }
    
    // Try with optimization enabled
    eprintln!("\nCompiling with optimization enabled...");
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::Standard,
        debug_info: true,
    };
    
    // Let's try to optimize the graph ourselves to see what happens
    eprintln!("\nOptimizing graph...");
    let config = fluentai_optimizer::OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = fluentai_optimizer::OptimizationPipeline::new(config);
    let optimized_graph = pipeline.optimize(&graph).expect("Optimization failed");
    
    eprintln!("\nOptimized graph:");
    eprintln!("  Root: {:?}", optimized_graph.root_id);
    eprintln!("  Nodes:");
    for (id, node) in &optimized_graph.nodes {
        eprintln!("    {:?}: {:?}", id, node);
    }
    
    match Compiler::with_options(options).compile(&graph) {
        Ok(bytecode) => {
            eprintln!("Compilation succeeded!");
            eprintln!("Bytecode chunks: {}", bytecode.chunks.len());
        },
        Err(e) => {
            eprintln!("Compilation failed: {}", e);
            panic!("Expected compilation to succeed, but it failed: {}", e);
        }
    }
}

#[test]
fn test_assignment_without_optimization() {
    // Simple assignment test
    let code = "let x = 10; x := 42";
    
    let graph = parse(code).expect("Parse failed");
    
    // Try without optimization
    println!("\nCompiling without optimization...");
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    
    match Compiler::with_options(options).compile(&graph) {
        Ok(_) => println!("Compilation succeeded!"),
        Err(e) => println!("Compilation failed: {}", e),
    }
}