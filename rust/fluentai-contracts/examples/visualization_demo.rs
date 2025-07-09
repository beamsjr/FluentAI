//! Example demonstrating symbolic execution path visualization

use fluentai_contracts::{
    visualization::{ExecutionTree, TreeBuilder},
    SymbolicExecutor,
};
use fluentai_parser::parse;
use std::fs;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Example: Function with multiple branches
    println!("=== Symbolic Execution Tree Visualization ===");
    {
        let program = r#"
            (define (classify-number x)
              (if (< x 0)
                  (if (< x -10)
                      'very-negative
                      'negative)
                  (if (= x 0)
                      'zero
                      (if (> x 10)
                          'very-positive
                          'positive))))
        "#;

        let graph = parse(program)?;

        // Execute symbolically
        let executor = SymbolicExecutor::new();
        let states = executor.execute_function_by_name(&graph, "classify-number")?;

        println!("Found {} execution paths\n", states.len());

        // Build execution tree
        let tree = TreeBuilder::build_from_states(states)?;

        // Generate ASCII visualization
        println!("ASCII Tree Visualization:");
        println!("{}", tree.to_ascii());

        // Generate DOT visualization
        let dot = tree.to_dot();
        fs::write("execution_tree.dot", &dot)?;
        println!("\nDOT visualization saved to execution_tree.dot");
        println!("To render: dot -Tpng execution_tree.dot -o execution_tree.png");

        // Generate Mermaid visualization
        let mermaid = tree.to_mermaid();
        fs::write("execution_tree.mmd", &mermaid)?;
        println!("\nMermaid visualization saved to execution_tree.mmd");

        // Display statistics
        let stats = tree.get_statistics();
        println!("\nExecution Tree Statistics:");
        println!("  Total nodes: {}", stats.total_nodes);
        println!("  Leaf nodes (paths): {}", stats.leaf_nodes);
        println!("  Satisfiable paths: {}", stats.satisfiable_paths);
        println!("  Unsatisfiable paths: {}", stats.unsatisfiable_paths);
        println!("  Maximum depth: {}", stats.max_depth);
        println!(
            "  Average constraints per path: {:.2}",
            stats.avg_constraints_per_path
        );
    }

    // Example: Recursive function with bounded depth
    println!("\n\n=== Recursive Function Visualization ===");
    {
        let program = r#"
            (define (countdown n)
              (if (<= n 0)
                  'done
                  (countdown (- n 1))))
        "#;

        let graph = parse(program)?;

        // Execute with limited depth
        let executor = SymbolicExecutor::with_limits(10, 20);
        let states = executor.execute_function_by_name(&graph, "countdown")?;

        println!("Found {} execution paths (depth limited)\n", states.len());

        // Build and visualize
        let tree = TreeBuilder::build_from_states(states)?;

        // Show compact ASCII visualization
        println!("Execution paths:");
        let ascii = tree.to_ascii();
        // Truncate if too long
        let lines: Vec<&str> = ascii.lines().take(20).collect();
        for line in lines {
            println!("{}", line);
        }
        if ascii.lines().count() > 20 {
            println!("... (truncated)");
        }

        let stats = tree.get_statistics();
        println!("\nStatistics:");
        println!("  Maximum recursion depth reached: {}", stats.max_depth);
    }

    Ok(())
}
