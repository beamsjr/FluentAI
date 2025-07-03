//! Demonstrates parallel contract verification

use claudelang_contracts::{
    Contract, ContractCondition, ContractKind,
    ParallelVerifier, ParallelVerificationConfig, ParallelCoordinator,
};
use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use std::collections::HashMap;
use std::num::NonZeroU32;
use std::time::Instant;

fn main() {
    println!("=== Parallel Contract Verification Demo ===\n");
    
    // Create a graph with multiple functions
    let mut graph = Graph::new();
    let contracts = create_sample_contracts(&mut graph);
    
    println!("Created {} contracts for verification\n", contracts.len());
    
    // Demo 1: Basic parallel verification
    println!("1. Basic Parallel Verification:");
    demo_basic_parallel(&graph, &contracts);
    
    // Demo 2: Custom configuration
    println!("\n2. Custom Configuration:");
    demo_custom_config(&graph, &contracts);
    
    // Demo 3: Progress reporting
    println!("\n3. Progress Reporting:");
    demo_progress_reporting(&graph, &contracts);
    
    // Demo 4: Performance comparison
    println!("\n4. Performance Comparison:");
    demo_performance_comparison(&graph, &contracts);
}

fn create_sample_contracts(graph: &mut Graph) -> HashMap<String, Contract> {
    let mut contracts = HashMap::new();
    
    // Create multiple contracts with varying complexity
    for i in 0..20 {
        let name = format!("function_{}", i);
        
        // Create a simple arithmetic contract
        let x = graph.add_node(Node::Variable { name: "x".to_string() });
        let y = graph.add_node(Node::Variable { name: "y".to_string() });
        let result = graph.add_node(Node::Variable { name: "result".to_string() });
        
        // Body: result = x + y + i
        let plus = graph.add_node(Node::Variable { name: "+".to_string() });
        let i_lit = graph.add_node(Node::Literal(Literal::Integer(i as i64)));
        let x_plus_y = graph.add_node(Node::Application {
            function: plus,
            args: vec![x, y],
        });
        let body = graph.add_node(Node::Application {
            function: plus,
            args: vec![x_plus_y, i_lit],
        });
        
        let mut contract = Contract::new(name.clone(), body);
        contract.function_name = Some(name.clone());
        
        // Add preconditions
        let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
        let ge = graph.add_node(Node::Variable { name: ">=".to_string() });
        
        // x >= 0
        let precond1 = graph.add_node(Node::Application {
            function: ge,
            args: vec![x, zero],
        });
        contract.add_precondition(
            ContractCondition::new(precond1, ContractKind::Precondition)
                .with_message("x must be non-negative".to_string())
        );
        
        // y >= 0
        let precond2 = graph.add_node(Node::Application {
            function: ge,
            args: vec![y, zero],
        });
        contract.add_precondition(
            ContractCondition::new(precond2, ContractKind::Precondition)
                .with_message("y must be non-negative".to_string())
        );
        
        // Add postconditions
        // result >= x
        let postcond1 = graph.add_node(Node::Application {
            function: ge,
            args: vec![result, x],
        });
        contract.add_postcondition(
            ContractCondition::new(postcond1, ContractKind::Postcondition)
                .with_message("result must be >= x".to_string())
        );
        
        // Add more conditions for some contracts to vary complexity
        if i % 3 == 0 {
            // result >= y
            let postcond2 = graph.add_node(Node::Application {
                function: ge,
                args: vec![result, y],
            });
            contract.add_postcondition(
                ContractCondition::new(postcond2, ContractKind::Postcondition)
                    .with_message("result must be >= y".to_string())
            );
        }
        
        contracts.insert(format!("contract_{}", i), contract);
    }
    
    contracts
}

fn demo_basic_parallel(graph: &Graph, contracts: &HashMap<String, Contract>) {
    let verifier = ParallelVerifier::new(graph);
    
    let start = Instant::now();
    match verifier.verify_contracts_parallel(contracts) {
        Ok(results) => {
            let elapsed = start.elapsed();
            println!("  Verified {} contracts in {:?}", results.len(), elapsed);
            
            let verified = results.values().filter(|r| {
                matches!(r, claudelang_contracts::VerificationResult::Verified)
            }).count();
            
            println!("  Successfully verified: {}/{}", verified, results.len());
        }
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_custom_config(graph: &Graph, contracts: &HashMap<String, Contract>) {
    let config = ParallelVerificationConfig {
        num_threads: 4,
        max_parallel_contracts: 10,
        enable_work_stealing: true,
        batch_size: 2,
        enable_priority_scheduling: true,
    };
    
    let verifier = ParallelVerifier::new(graph).with_config(config);
    
    println!("  Using custom configuration:");
    println!("    Threads: 4");
    println!("    Batch size: 2");
    println!("    Priority scheduling: enabled");
    
    let start = Instant::now();
    match verifier.verify_contracts_parallel(contracts) {
        Ok(_results) => {
            let elapsed = start.elapsed();
            println!("  Completed in {:?}", elapsed);
        }
        Err(e) => println!("  Error: {}", e),
    }
}

fn demo_progress_reporting(graph: &Graph, contracts: &HashMap<String, Contract>) {
    let coordinator = ParallelCoordinator::new(graph);
    
    println!("  Verifying with progress updates:");
    
    let progress_callback = |completed: usize, total: usize| {
        let percentage = (completed as f64 / total as f64) * 100.0;
        print!("\r  Progress: {}/{} ({:.1}%)", completed, total, percentage);
        use std::io::{self, Write};
        io::stdout().flush().unwrap();
    };
    
    match coordinator.verify_with_progress(contracts, progress_callback) {
        Ok(results) => {
            println!("\n  Completed verification of {} contracts", results.len());
            
            let stats = coordinator.get_stats();
            println!("  Statistics:");
            println!("    Total contracts: {}", stats.total_contracts);
            println!("    Threads used: {}", stats.threads_used);
        }
        Err(e) => println!("\n  Error: {}", e),
    }
}

fn demo_performance_comparison(graph: &Graph, contracts: &HashMap<String, Contract>) {
    println!("  Comparing sequential vs parallel verification...\n");
    
    // Sequential verification (simulated)
    println!("  Sequential verification:");
    let start_seq = Instant::now();
    let mut seq_results = HashMap::new();
    
    #[cfg(feature = "static")]
    {
        use claudelang_contracts::StaticVerifier;
        for (name, contract) in contracts {
            let mut verifier = StaticVerifier::new(graph);
            if let Ok(result) = verifier.verify_contract(contract) {
                seq_results.insert(name.clone(), result);
            }
        }
    }
    
    let elapsed_seq = start_seq.elapsed();
    println!("    Time: {:?}", elapsed_seq);
    println!("    Contracts verified: {}", seq_results.len());
    
    // Parallel verification
    println!("\n  Parallel verification:");
    let verifier = ParallelVerifier::new(graph);
    let start_par = Instant::now();
    
    if let Ok(par_results) = verifier.verify_contracts_parallel(contracts) {
        let elapsed_par = start_par.elapsed();
        println!("    Time: {:?}", elapsed_par);
        println!("    Contracts verified: {}", par_results.len());
        
        // Calculate speedup
        if elapsed_par.as_millis() > 0 {
            let speedup = elapsed_seq.as_millis() as f64 / elapsed_par.as_millis() as f64;
            println!("\n  Speedup: {:.2}x", speedup);
            println!("  Efficiency: {:.1}%", (speedup / num_cpus::get() as f64) * 100.0);
        }
    }
    
    #[cfg(not(feature = "static"))]
    {
        println!("    [Static verification requires 'static' feature]");
    }
}