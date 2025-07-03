//! Demonstrates incremental verification functionality

use claudelang_contracts::{
    Contract, ContractCondition, ContractKind,
    IncrementalVerifier, IncrementalStats,
};
use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use std::collections::HashMap;
use std::num::NonZeroU32;

#[cfg(feature = "static")]
use claudelang_contracts::{StaticVerifier, ResourceLimits};

fn main() {
    println!("=== Incremental Verification Demo ===\n");
    
    // Create a graph with some functions
    let mut graph = Graph::new();
    
    // Function: add(x, y) = x + y
    let add_params = vec!["x".to_string(), "y".to_string()];
    let x = graph.add_node(Node::Variable { name: "x".to_string() });
    let y = graph.add_node(Node::Variable { name: "y".to_string() });
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let add_body = graph.add_node(Node::Application {
        function: plus,
        args: vec![x, y],
    });
    
    // Function: double(n) = add(n, n)
    let double_params = vec!["n".to_string()];
    let n1 = graph.add_node(Node::Variable { name: "n".to_string() });
    let n2 = graph.add_node(Node::Variable { name: "n".to_string() });
    let add_ref = graph.add_node(Node::Variable { name: "add".to_string() });
    let double_body = graph.add_node(Node::Application {
        function: add_ref,
        args: vec![n1, n2],
    });
    
    // Create contracts
    let mut contracts = HashMap::new();
    
    // Contract for add: ensures result >= x and result >= y
    let mut add_contract = Contract::new("add".to_string(), add_body);
    add_contract.function_name = Some("add".to_string());
    
    let result = graph.add_node(Node::Variable { name: "result".to_string() });
    let ge = graph.add_node(Node::Variable { name: ">=".to_string() });
    let result_ge_x = graph.add_node(Node::Application {
        function: ge,
        args: vec![result, x],
    });
    let result_ge_y = graph.add_node(Node::Application {
        function: ge,
        args: vec![result, y],
    });
    let and_op = graph.add_node(Node::Variable { name: "and".to_string() });
    let postcond = graph.add_node(Node::Application {
        function: and_op,
        args: vec![result_ge_x, result_ge_y],
    });
    
    add_contract.add_postcondition(
        ContractCondition::new(postcond, ContractKind::Postcondition)
            .with_message("Result must be >= both inputs".to_string())
    );
    contracts.insert("add_contract".to_string(), add_contract);
    
    // Contract for double: ensures result = 2 * n
    let mut double_contract = Contract::new("double".to_string(), double_body);
    double_contract.function_name = Some("double".to_string());
    
    let two = graph.add_node(Node::Literal(Literal::Integer(2)));
    let times = graph.add_node(Node::Variable { name: "*".to_string() });
    let two_n = graph.add_node(Node::Application {
        function: times,
        args: vec![two, n1],
    });
    let eq = graph.add_node(Node::Variable { name: "=".to_string() });
    let double_postcond = graph.add_node(Node::Application {
        function: eq,
        args: vec![result, two_n],
    });
    
    double_contract.add_postcondition(
        ContractCondition::new(double_postcond, ContractKind::Postcondition)
            .with_message("Result must equal 2 * n".to_string())
    );
    contracts.insert("double_contract".to_string(), double_contract);
    
    // Create incremental verifier
    let mut inc_verifier = IncrementalVerifier::new(&graph);
    
    // Build dependencies
    println!("Building dependencies...");
    inc_verifier.build_dependencies(&contracts).unwrap();
    
    // Initial verification
    println!("\n1. Initial verification of all contracts:");
    let stats = inc_verifier.get_stats();
    print_stats(&stats);
    
    #[cfg(feature = "static")]
    {
        let mut static_verifier = StaticVerifier::new(&graph);
        static_verifier.set_resource_limits(ResourceLimits::default());
        
        let results = inc_verifier.verify_incremental(&contracts, &mut static_verifier)
            .unwrap();
        
        println!("Verified {} contracts", results.len());
        for (name, result) in &results {
            println!("  {}: {:?}", name, result);
        }
    }
    
    #[cfg(not(feature = "static"))]
    {
        println!("  [Static verification requires 'static' feature]");
    }
    
    // Simulate changing the 'add' function
    println!("\n2. Simulating change to 'add' function...");
    inc_verifier.mark_function_changed("add");
    inc_verifier.update_function_hash("add", add_body);
    
    let contracts_to_verify = inc_verifier.get_contracts_to_verify();
    println!("Contracts needing re-verification: {:?}", contracts_to_verify);
    
    let stats = inc_verifier.get_stats();
    print_stats(&stats);
    
    // Re-verify only affected contracts
    println!("\n3. Incremental re-verification:");
    #[cfg(feature = "static")]
    {
        let mut static_verifier = StaticVerifier::new(&graph);
        static_verifier.set_resource_limits(ResourceLimits::default());
        
        let results = inc_verifier.verify_incremental(&contracts, &mut static_verifier)
            .unwrap();
        
        println!("Re-verified {} contracts (skipped unchanged)", 
                 contracts_to_verify.len());
    }
    
    // Final stats
    println!("\n4. Final statistics:");
    let final_stats = inc_verifier.get_stats();
    print_stats(&final_stats);
    
    println!("\n✅ Incremental verification demo complete!");
}

fn print_stats(stats: &IncrementalStats) {
    println!("  Total functions: {}", stats.total_functions);
    println!("  Total contracts: {}", stats.total_contracts);
    println!("  Changed functions: {}", stats.changed_functions);
    println!("  Cached results: {}", stats.cached_results);
}