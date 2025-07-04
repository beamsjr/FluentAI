//! Example demonstrating Z3-based static verification

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    StaticVerifier, VerificationResult,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use anyhow::Result;

fn main() -> Result<()> {
    println!("=== Z3 Static Verification Demo ===\n");
    
    // 1. Simple contract verification
    simple_contract_demo()?;
    
    // 2. Contract with contradictions
    contradiction_demo()?;
    
    // 3. Contract with counterexample
    counterexample_demo()?;
    
    Ok(())
}

fn simple_contract_demo() -> Result<()> {
    println!("1. Simple Contract Verification");
    println!("------------------------------");
    
    // Create a simple contract: requires x > 0, ensures result > 0
    let mut graph = Graph::new();
    
    // Create precondition: x > 0
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let gt_op = graph.add_node(Node::Variable { name: ">".to_string() });
    let precond_expr = graph.add_node(Node::Application {
        function: gt_op,
        args: vec![x_var, zero],
    });
    
    // Create postcondition: result > 0
    let result_var = graph.add_node(Node::Variable { name: "result".to_string() });
    let zero2 = graph.add_node(Node::Literal(Literal::Integer(0)));
    let gt_op2 = graph.add_node(Node::Variable { name: ">".to_string() });
    let postcond_expr = graph.add_node(Node::Application {
        function: gt_op2,
        args: vec![result_var, zero2],
    });
    
    // Create contract
    let mut contract = Contract::new("abs".to_string(), NodeId(0));
    contract.add_precondition(ContractCondition {
        expression: precond_expr,
        message: Some("x must be positive".to_string()),
        kind: ContractKind::Precondition,
    });
    contract.add_postcondition(ContractCondition {
        expression: postcond_expr,
        message: Some("result must be positive".to_string()),
        kind: ContractKind::Postcondition,
    });
    
    // Verify contract
    let verifier = StaticVerifier::new();
    match verifier.verify_contract(&contract, &graph)? {
        VerificationResult::Verified => {
            println!("✓ Contract verified successfully!");
        }
        VerificationResult::Violated(counter) => {
            println!("✗ Contract violated!");
            println!("  Condition: {}", counter.violated_condition);
            println!("  Context: {}", counter.context);
            println!("  Inputs: {:?}", counter.inputs);
        }
        VerificationResult::Unknown(reason) => {
            println!("? Verification inconclusive: {}", reason);
        }
        VerificationResult::Timeout => {
            println!("⏱ Verification timed out");
        }
    }
    
    println!();
    Ok(())
}

fn contradiction_demo() -> Result<()> {
    println!("2. Contract with Contradictory Preconditions");
    println!("------------------------------------------");
    
    let mut graph = Graph::new();
    
    // Create contradictory preconditions: x > 5 AND x < 3
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let five = graph.add_node(Node::Literal(Literal::Integer(5)));
    let three = graph.add_node(Node::Literal(Literal::Integer(3)));
    let gt_op = graph.add_node(Node::Variable { name: ">".to_string() });
    let lt_op = graph.add_node(Node::Variable { name: "<".to_string() });
    
    let x_gt_5 = graph.add_node(Node::Application {
        function: gt_op,
        args: vec![x_var, five],
    });
    
    let x_lt_3 = graph.add_node(Node::Application {
        function: lt_op,
        args: vec![x_var, three],
    });
    
    // Create contract with contradictory preconditions
    let mut contract = Contract::new("impossible".to_string(), NodeId(0));
    contract.add_precondition(ContractCondition {
        expression: x_gt_5,
        message: Some("x must be greater than 5".to_string()),
        kind: ContractKind::Precondition,
    });
    contract.add_precondition(ContractCondition {
        expression: x_lt_3,
        message: Some("x must be less than 3".to_string()),
        kind: ContractKind::Precondition,
    });
    
    // Verify contract
    let verifier = StaticVerifier::new();
    match verifier.verify_contract(&contract, &graph)? {
        VerificationResult::Verified => {
            println!("✓ Contract verified (unexpected!)");
        }
        VerificationResult::Violated(counter) => {
            println!("✗ Contract violated!");
            println!("  Condition: {}", counter.violated_condition);
            println!("  Context: {}", counter.context);
        }
        VerificationResult::Unknown(reason) => {
            println!("? Verification inconclusive: {}", reason);
        }
        VerificationResult::Timeout => {
            println!("⏱ Verification timed out");
        }
    }
    
    println!();
    Ok(())
}

fn counterexample_demo() -> Result<()> {
    println!("3. Contract with Counterexample");
    println!("------------------------------");
    
    let mut graph = Graph::new();
    
    // Create a contract that can be violated
    // Precondition: x > 0
    // Postcondition: x + y > 10 (this can be false!)
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let ten = graph.add_node(Node::Literal(Literal::Integer(10)));
    let gt_op = graph.add_node(Node::Variable { name: ">".to_string() });
    let plus_op = graph.add_node(Node::Variable { name: "+".to_string() });
    
    // Precondition: x > 0
    let precond = graph.add_node(Node::Application {
        function: gt_op,
        args: vec![x_var, zero],
    });
    
    // Postcondition: x + y > 10
    let x_plus_y = graph.add_node(Node::Application {
        function: plus_op,
        args: vec![x_var, y_var],
    });
    let gt_op2 = graph.add_node(Node::Variable { name: ">".to_string() });
    let postcond = graph.add_node(Node::Application {
        function: gt_op2,
        args: vec![x_plus_y, ten],
    });
    
    let mut contract = Contract::new("add_check".to_string(), NodeId(0));
    contract.add_precondition(ContractCondition {
        expression: precond,
        message: Some("x must be positive".to_string()),
        kind: ContractKind::Precondition,
    });
    contract.add_postcondition(ContractCondition {
        expression: postcond,
        message: Some("sum must be greater than 10".to_string()),
        kind: ContractKind::Postcondition,
    });
    
    // Verify contract
    let verifier = StaticVerifier::new();
    match verifier.verify_contract(&contract, &graph)? {
        VerificationResult::Verified => {
            println!("✓ Contract verified");
        }
        VerificationResult::Violated(counter) => {
            println!("✗ Contract violated!");
            println!("  Condition: {}", counter.violated_condition);
            println!("  Context: {}", counter.context);
            println!("  Counterexample inputs:");
            for (var, val) in &counter.inputs {
                println!("    {} = {}", var, val);
            }
        }
        VerificationResult::Unknown(reason) => {
            println!("? Verification inconclusive: {}", reason);
        }
        VerificationResult::Timeout => {
            println!("⏱ Verification timed out");
        }
    }
    
    println!();
    Ok(())
}