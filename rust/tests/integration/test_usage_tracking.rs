//! Example demonstrating runtime usage tracking in the VM

use fluentai_vm::{Compiler, VM};
use fluentai_core::ast::NodeId;

fn main() -> anyhow::Result<()> {
    // Sample program with functions that will be tracked
    let source = r#"
        (letrec ((factorial (lambda (n)
                              (if (= n 0)
                                  1
                                  (* n (factorial (- n 1)))))))
          (begin
            (factorial 5)
            (factorial 10)
            (factorial 3)))
    "#;
    
    // Parse the source code
    let graph = parse(source)?;
    
    // Create a mapping of node IDs to human-readable names for demonstration
    let mut node_names = std::collections::HashMap::new();
    
    // Find the factorial function node
    for (id, node) in graph.nodes() {
        if let fluentai_core::ast::Node::Lambda { .. } = node {
            node_names.insert(*id, "factorial");
        }
    }
    
    // Compile to bytecode
    let mut compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Create VM with usage tracking enabled
    let mut vm = VM::new(bytecode);
    vm.enable_usage_tracking();
    
    // Register chunk to node mappings
    // In a real implementation, the compiler would provide these mappings
    if let Some(factorial_id) = node_names.iter().find(|(_, name)| **name == "factorial").map(|(id, _)| id) {
        vm.register_chunk_mapping(1, *factorial_id); // Assuming factorial is compiled to chunk 1
    }
    
    // Run the program
    let result = vm.run()?;
    println!("Result: {:?}", result);
    
    // Display usage statistics
    println!("\n=== Usage Statistics ===");
    if let Some(all_stats) = vm.get_all_usage_stats() {
        for (node_id, stats) in all_stats {
            if let Some(name) = node_names.get(node_id) {
                println!("\nFunction: {}", name);
                println!("  Execution count: {}", stats.execution_count);
                println!("  Average execution time: {}ns", stats.avg_execution_time_ns);
                println!("  Error count: {}", stats.error_count);
                println!("  Is hot path: {}", stats.is_hot_path);
                
                // Update the AST with the usage statistics
                if let Some(node) = graph.get_node(*node_id) {
                    if let Some(mut metadata) = node.metadata().cloned() {
                        metadata.update_usage_stats(stats.clone());
                        println!("  Updated AST node with usage statistics");
                    }
                }
            }
        }
    } else {
        println!("No usage statistics available");
    }
    
    Ok(())
}