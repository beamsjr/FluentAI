//! Demonstrates the optimizer "Invalid node ID" bug
//! 
//! This test shows how the optimizer can fail when processing nodes in arbitrary order
//! during dead code elimination.

use fluentai_core::ast::{Graph, Node, NodeId, Pattern, Literal};
use fluentai_optimizer::graph_optimizer::GraphOptimizer;
use std::num::NonZeroU32;

fn main() {
    println!("=== Optimizer Bug Demonstration ===\n");
    
    // Step 1: Create a graph with specific structure
    println!("Step 1: Creating graph structure");
    let mut graph = Graph::new();
    
    // Create nodes manually with specific IDs to control the scenario
    // We'll insert them in a specific order to ensure predictable IDs
    
    // Node 1: Integer literal 42
    let lit_42 = graph.add_node(Node::Literal(Literal::Integer(42))).expect("Failed to add node");
    println!("  - Created literal 42 with ID: {}", lit_42.0.get());
    
    // Node 2: Variable "x" 
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).expect("Failed to add node");
    println!("  - Created variable 'x' with ID: {}", x_var.0.get());
    
    // Node 3: Integer literal 1
    let lit_1 = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    println!("  - Created literal 1 with ID: {}", lit_1.0.get());
    
    // Node 4: Match expression that references x_var in its branch
    let match_node = graph.add_node(Node::Match {
        expr: lit_42,  // Match on the literal 42
        branches: vec![
            (Pattern::Literal(Literal::Integer(42)), x_var),  // Branch references x_var
            (Pattern::Wildcard, lit_1),
        ],
    }).expect("Failed to add node");
    println!("  - Created match node with ID: {}", match_node.0.get());
    
    // Step 2: Set only the match node as root (x_var is not directly reachable)
    graph.root_id = Some(match_node);
    println!("\nStep 2: Set root to match node (ID {})", match_node.0.get());
    println!("  - Note: x_var is only reachable through the match branch");
    
    // Step 3: Show the graph structure
    println!("\nStep 3: Graph structure:");
    println!("  - Total nodes: {}", graph.nodes.len());
    println!("  - Root: {:?}", graph.root_id);
    println!("  - Nodes:");
    for (id, node) in &graph.nodes {
        println!("    {} -> {:?}", id.0.get(), match node {
            Node::Literal(Literal::Integer(n)) => format!("Integer({})", n),
            Node::Variable { name } => format!("Variable({})", name),
            Node::Match { .. } => "Match { ... }".to_string(),
            _ => format!("{:?}", node),
        });
    }
    
    // Step 4: Run the optimizer and observe the failure
    println!("\nStep 4: Running optimizer...");
    let mut optimizer = GraphOptimizer::new();
    
    match optimizer.optimize(&graph) {
        Ok(_) => {
            println!("ERROR: Optimizer succeeded when it should have failed!");
            println!("This might mean the bug has been fixed or the test needs adjustment.");
        }
        Err(e) => {
            println!("Optimizer failed as expected!");
            println!("Error: {}", e);
            
            // Step 5: Explain why it failed
            println!("\nStep 5: Why did it fail?");
            println!("---------------------------------------");
            println!("The dead_code_elimination_pass does the following:");
            println!("1. Marks all reachable nodes starting from root (match_node)");
            println!("   - It finds match_node is reachable");
            println!("   - It finds x_var is reachable (through match branch)");
            println!("   - It finds lit_42 and lit_1 are reachable");
            println!("\n2. Builds new graph with only reachable nodes");
            println!("   - It iterates through reachable nodes in HashMap order (arbitrary!)");
            println!("   - If it processes match_node before x_var:");
            println!("     * copy_with_mapping tries to map x_var reference in branch");
            println!("     * But x_var hasn't been added to mapping yet!");
            println!("     * Result: mapping.get(&x_var).unwrap_or(x_var) returns x_var");
            println!("     * The new graph doesn't have x_var yet -> Invalid node ID!");
            println!("\n3. The bug: Nodes are processed in arbitrary order, not dependency order");
            println!("\nThe fix would be to either:");
            println!("- Process nodes in dependency order (process x_var before match_node)");
            println!("- Use a two-pass approach: first create all nodes, then update references");
        }
    }
    
    // Additional demonstration: Show what happens with different processing orders
    println!("\n=== Demonstrating Order Dependency ===");
    demonstrate_order_dependency();
}

fn demonstrate_order_dependency() {
    println!("\nScenario A: Processing in dependency order (works):");
    println!("  1. Process lit_42 -> add to mapping");
    println!("  2. Process x_var -> add to mapping");
    println!("  3. Process lit_1 -> add to mapping");
    println!("  4. Process match_node -> all refs exist in mapping ✓");
    
    println!("\nScenario B: Processing in problematic order (fails):");
    println!("  1. Process match_node first");
    println!("     - copy_with_mapping needs to map x_var");
    println!("     - x_var not in mapping yet!");
    println!("     - Returns unmapped x_var");
    println!("     - New graph doesn't have x_var -> ERROR!");
    
    println!("\nRoot cause: HashMap iteration order is non-deterministic");
    println!("The optimizer relies on HashMap iteration which doesn't guarantee");
    println!("any particular order, leading to intermittent failures.");
}