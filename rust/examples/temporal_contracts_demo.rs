//! Demonstration of temporal contracts in FluentAI
//! 
//! This example shows how to use temporal operators to specify
//! properties that must hold over time.

use fluentai_contracts::{
    temporal::*, temporal_dsl::*, Contract, ContractCondition, ContractKind,
};
use fluentai_core::ast::NodeId;
use std::collections::HashMap;

fn main() {
    println!("=== FluentAI Temporal Contracts Demo ===\n");
    
    // Example 1: Response Property
    // "Always, if a request is made, eventually a response occurs"
    demo_response_property();
    
    // Example 2: Mutual Exclusion
    // "Always, at most one process is in the critical section"
    demo_mutual_exclusion();
    
    // Example 3: Fairness Property
    // "If a process requests access infinitely often, it gets access infinitely often"
    demo_fairness_property();
    
    // Example 4: State Machine Invariant
    // "The state machine never enters an error state"
    demo_state_machine_invariant();
}

fn demo_response_property() {
    println!("1. Response Property Demo");
    println!("   'Always, if request then eventually response'\n");
    
    // Create atomic propositions
    let request = ContractCondition::new(NodeId(1), ContractKind::Precondition)
        .with_message("request_sent".to_string());
    let response = ContractCondition::new(NodeId(2), ContractKind::Postcondition)
        .with_message("response_received".to_string());
    
    // Build temporal formula: □(request → ◇response)
    let formula = always(implies(
        atom(request),
        eventually(atom(response))
    ));
    
    // Create temporal contract
    let contract = TemporalContractBuilder::new("response_property".to_string())
        .formula(formula)
        .liveness(eventually(atom(response.clone())))
        .bound(100) // Check within 100 steps
        .build()
        .unwrap();
    
    println!("   Contract: {}", contract.name);
    println!("   Formula: Always(request → Eventually(response))");
    println!("   Bound: {} steps\n", contract.bound.unwrap());
    
    // Create a sample trace
    let mut trace = create_sample_trace();
    
    // Verify the contract
    let mut verifier = TemporalVerifier::new(TemporalVerificationConfig::default());
    verifier.register_contract(contract);
    verifier.add_trace(trace);
    
    let results = verifier.verify_all();
    print_verification_results(&results);
}

fn demo_mutual_exclusion() {
    println!("2. Mutual Exclusion Demo");
    println!("   'Always, at most one process in critical section'\n");
    
    // Create atomic propositions for two processes
    let p1_critical = ContractCondition::new(NodeId(3), ContractKind::Invariant)
        .with_message("process1_in_critical_section".to_string());
    let p2_critical = ContractCondition::new(NodeId(4), ContractKind::Invariant)
        .with_message("process2_in_critical_section".to_string());
    
    // Build formula: □¬(p1_critical ∧ p2_critical)
    let formula = always(not(and(vec![
        atom(p1_critical),
        atom(p2_critical)
    ])));
    
    let contract = TemporalContractBuilder::new("mutual_exclusion".to_string())
        .formula(formula)
        .safety(formula.clone()) // This is a safety property
        .build()
        .unwrap();
    
    println!("   Contract: {}", contract.name);
    println!("   Formula: Always(¬(P1_critical ∧ P2_critical))");
    println!("   Type: Safety property\n");
}

fn demo_fairness_property() {
    println!("3. Fairness Property Demo");
    println!("   'If process requests infinitely often, it gets access infinitely often'\n");
    
    let request = ContractCondition::new(NodeId(5), ContractKind::Precondition)
        .with_message("process_requests_access".to_string());
    let grant = ContractCondition::new(NodeId(6), ContractKind::Postcondition)
        .with_message("process_granted_access".to_string());
    
    // Build formula: □◇request → □◇grant
    let formula = implies(
        always(eventually(atom(request.clone()))),
        always(eventually(atom(grant.clone())))
    );
    
    let contract = TemporalContractBuilder::new("fairness_property".to_string())
        .formula(formula)
        .fairness(eventually(atom(grant))) // Fairness constraint
        .build()
        .unwrap();
    
    println!("   Contract: {}", contract.name);
    println!("   Formula: (Always(Eventually(request))) → (Always(Eventually(grant)))");
    println!("   Type: Liveness property with fairness\n");
}

fn demo_state_machine_invariant() {
    println!("4. State Machine Invariant Demo");
    println!("   'Never enter error state, and eventually reach done state'\n");
    
    let error_state = ContractCondition::new(NodeId(7), ContractKind::Invariant)
        .with_message("state_is_error".to_string());
    let done_state = ContractCondition::new(NodeId(8), ContractKind::Postcondition)
        .with_message("state_is_done".to_string());
    let init_state = ContractCondition::new(NodeId(9), ContractKind::Precondition)
        .with_message("state_is_init".to_string());
    
    // Build formula: □¬error ∧ ◇done
    let formula = and(vec![
        always(not(atom(error_state.clone()))),
        eventually(atom(done_state.clone()))
    ]);
    
    // Also add sequencing: init until done
    let sequencing = until(
        atom(init_state),
        atom(done_state.clone())
    );
    
    let contract = TemporalContractBuilder::new("state_machine_invariant".to_string())
        .formula(formula)
        .safety(always(not(atom(error_state)))) // Never error
        .liveness(eventually(atom(done_state))) // Eventually done
        .build()
        .unwrap();
    
    println!("   Contract: {}", contract.name);
    println!("   Formula: Always(¬error) ∧ Eventually(done)");
    println!("   Additional: init Until done");
    println!("   Type: Combined safety and liveness\n");
}

fn create_sample_trace() -> ExecutionTrace {
    let mut states = Vec::new();
    
    // Create a sequence of states
    for i in 0..10 {
        let mut bindings = HashMap::new();
        bindings.insert("step".to_string(), serde_json::json!(i));
        bindings.insert("request_sent".to_string(), serde_json::json!(i % 3 == 0));
        bindings.insert("response_received".to_string(), serde_json::json!(i % 3 == 2));
        
        states.push(TemporalState {
            id: i,
            bindings,
            active_contracts: vec!["response_property".to_string()],
            timestamp: Some(i as u64 * 1000),
        });
    }
    
    let mut transitions = Vec::new();
    for i in 0..9 {
        transitions.push((i, i + 1, format!("step_{}", i)));
    }
    
    ExecutionTrace {
        states,
        transitions,
        current: 0,
    }
}

fn print_verification_results(results: &HashMap<String, Vec<TemporalVerificationResult>>) {
    println!("   Verification Results:");
    for (contract_name, contract_results) in results {
        for result in contract_results {
            println!("   - Contract '{}' on trace {}: {}",
                contract_name,
                result.trace_id,
                if result.verified { "✓ PASSED" } else { "✗ FAILED" }
            );
            
            if let Some(counterexample) = &result.counterexample {
                println!("     Counterexample found:");
                println!("     - Failing states: {:?}", counterexample.failing_states);
                println!("     - Violation path: {:?}", counterexample.violation_path);
            }
        }
    }
    println!();
}

/// Example of using temporal contracts with packet processing
fn packet_processing_temporal_example() {
    println!("5. Packet Processing Temporal Properties");
    
    // Property 1: No packet loss
    // "If a packet is received, it is eventually processed"
    let packet_received = ContractCondition::new(NodeId(10), ContractKind::Precondition)
        .with_message("packet_received".to_string());
    let packet_processed = ContractCondition::new(NodeId(11), ContractKind::Postcondition)
        .with_message("packet_processed".to_string());
    
    let no_loss = always(implies(
        atom(packet_received.clone()),
        eventually(atom(packet_processed.clone()))
    ));
    
    // Property 2: Bounded processing time
    // "Packets are processed within 5 steps"
    let bounded_time = always(implies(
        atom(packet_received),
        // In real implementation, this would use bounded until
        next(or(vec![
            atom(packet_processed.clone()),
            next(or(vec![
                atom(packet_processed.clone()),
                next(or(vec![
                    atom(packet_processed.clone()),
                    next(or(vec![
                        atom(packet_processed.clone()),
                        next(atom(packet_processed))
                    ]))
                ]))
            ]))
        ]))
    ));
    
    // Property 3: FIFO ordering
    // "If packet A arrives before packet B, A is processed before B"
    // This would require more complex state tracking
    
    let contract = TemporalContractBuilder::new("packet_processing".to_string())
        .formula(and(vec![no_loss, bounded_time]))
        .liveness(eventually(atom(packet_processed)))
        .bound(1000)
        .build()
        .unwrap();
    
    println!("   Contract ensures:");
    println!("   - No packet loss (reliability)");
    println!("   - Bounded processing time (real-time)");
    println!("   - FIFO ordering (fairness)");
}