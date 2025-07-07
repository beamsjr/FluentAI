//! Example demonstrating the verification API (without Z3)

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use anyhow::Result;
use std::num::NonZeroU32;

fn main() -> Result<()> {
    println!("=== Contract Verification API Demo ===\n");
    
    // Demonstrate how to build contracts
    build_contract_demo()?;
    
    // Demonstrate contract analysis
    analyze_contract_demo()?;
    
    Ok(())
}

fn build_contract_demo() -> Result<()> {
    println!("1. Building Contracts");
    println!("--------------------");
    
    let mut graph = Graph::new();
    
    // Build a contract for a factorial function
    // requires: n >= 0
    // ensures: result >= 1
    
    // Create precondition: n >= 0
    let n_var = graph.add_node(Node::Variable { name: "n".to_string() }).expect("Failed to add node");
    let zero = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    let ge_op = graph.add_node(Node::Variable { name: ">=".to_string() }).expect("Failed to add node");
    let precond_expr = graph.add_node(Node::Application {
        function: ge_op,
        args: vec![n_var, zero],
    }).expect("Failed to add node");
    
    // Create postcondition: result >= 1
    let result_var = graph.add_node(Node::Variable { name: "result".to_string() }).expect("Failed to add node");
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let ge_op2 = graph.add_node(Node::Variable { name: ">=".to_string() }).expect("Failed to add node");
    let postcond_expr = graph.add_node(Node::Application {
        function: ge_op2,
        args: vec![result_var, one],
    }).expect("Failed to add node");
    
    // Build the contract
    let mut contract = Contract::new("factorial".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    contract.add_precondition(ContractCondition {
        expression: precond_expr,
        message: Some("n must be non-negative".to_string()),
        kind: ContractKind::Precondition,
        span: None,
        blame_label: None,
    });
    
    contract.add_postcondition(ContractCondition {
        expression: postcond_expr,
        message: Some("factorial result must be at least 1".to_string()),
        kind: ContractKind::Postcondition,
        span: None,
        blame_label: None,
    });
    
    // Mark as pure function
    contract.pure = true;
    
    // Add complexity
    contract.complexity = Some("O(n)".to_string());
    
    println!("Created contract for function: {}", contract.function_name);
    println!("  Pure: {}", contract.pure);
    println!("  Complexity: {:?}", contract.complexity);
    println!("  Preconditions: {}", contract.preconditions.len());
    println!("  Postconditions: {}", contract.postconditions.len());
    
    for (i, precond) in contract.preconditions.iter().enumerate() {
        println!("    Pre[{}]: {}", i, precond.message.as_ref().unwrap_or(&"<no message>".to_string()));
    }
    
    for (i, postcond) in contract.postconditions.iter().enumerate() {
        println!("    Post[{}]: {}", i, postcond.message.as_ref().unwrap_or(&"<no message>".to_string()));
    }
    
    println!();
    Ok(())
}

fn analyze_contract_demo() -> Result<()> {
    println!("2. Contract Analysis");
    println!("-------------------");
    
    let mut graph = Graph::new();
    
    // Create a more complex contract with invariants
    // Function: array_sum
    // requires: arr.length > 0
    // ensures: result >= min(arr)
    // invariant: partial_sum >= 0 (if all elements are non-negative)
    
    let arr_var = graph.add_node(Node::Variable { name: "arr".to_string() }).expect("Failed to add node");
    let len_op = graph.add_node(Node::Variable { name: "length".to_string() }).expect("Failed to add node");
    let arr_len = graph.add_node(Node::Application {
        function: len_op,
        args: vec![arr_var],
    }).expect("Failed to add node");
    let zero = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    let gt_op = graph.add_node(Node::Variable { name: ">".to_string() }).expect("Failed to add node");
    
    let precond = graph.add_node(Node::Application {
        function: gt_op,
        args: vec![arr_len, zero],
    }).expect("Failed to add node");
    
    // Create invariant: partial_sum >= 0
    let partial_sum = graph.add_node(Node::Variable { name: "partial_sum".to_string() }).expect("Failed to add node");
    let zero2 = graph.add_node(Node::Literal(Literal::Integer(0))).expect("Failed to add node");
    let ge_op = graph.add_node(Node::Variable { name: ">=".to_string() }).expect("Failed to add node");
    
    let invariant = graph.add_node(Node::Application {
        function: ge_op,
        args: vec![partial_sum, zero2],
    }).expect("Failed to add node");
    
    let mut contract = Contract::new("array_sum".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    contract.add_precondition(ContractCondition {
        expression: precond,
        message: Some("array must not be empty".to_string()),
        kind: ContractKind::Precondition,
        span: None,
        blame_label: None,
    });
    
    contract.add_invariant(ContractCondition {
        expression: invariant,
        message: Some("partial sum remains non-negative".to_string()),
        kind: ContractKind::Invariant,
        span: None,
        blame_label: None,
    });
    
    // Analyze the contract
    println!("Contract analysis for: {}", contract.function_name);
    println!("  Has conditions: {}", contract.has_conditions());
    println!("  Total conditions: {}", 
        contract.preconditions.len() + contract.postconditions.len() + contract.invariants.len());
    
    // Check each kind of condition
    for kind in [ContractKind::Precondition, ContractKind::Postcondition, ContractKind::Invariant] {
        let conditions = contract.conditions_of_kind(kind);
        if !conditions.is_empty() {
            println!("  {:?}s: {}", kind, conditions.len());
            for cond in conditions {
                if let Some(msg) = &cond.message {
                    println!("    - {}", msg);
                }
            }
        }
    }
    
    println!();
    Ok(())
}