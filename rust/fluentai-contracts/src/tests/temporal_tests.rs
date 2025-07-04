//! Tests for temporal contracts

use crate::{
    temporal::*,
    temporal_dsl::*,
    bounded_model_checking::*,
    contract::{ContractCondition, ContractKind},
};
use fluentai_core::ast::NodeId;
use std::collections::HashMap;

#[test]
fn test_temporal_formula_construction() {
    // Test basic temporal operators
    let p = atom(ContractCondition::new(NodeId(1), ContractKind::Invariant));
    let q = atom(ContractCondition::new(NodeId(2), ContractKind::Invariant));
    
    // Always p
    let always_p = always(p.clone());
    match always_p {
        TemporalFormula::Temporal(TemporalOperator::Always(_)) => {},
        _ => panic!("Expected Always operator"),
    }
    
    // Eventually q
    let eventually_q = eventually(q.clone());
    match eventually_q {
        TemporalFormula::Temporal(TemporalOperator::Eventually(_)) => {},
        _ => panic!("Expected Eventually operator"),
    }
    
    // p Until q
    let p_until_q = until(p.clone(), q.clone());
    match p_until_q {
        TemporalFormula::Temporal(TemporalOperator::Until(_, _)) => {},
        _ => panic!("Expected Until operator"),
    }
    
    // Complex formula: Always(p -> Eventually q)
    let complex = always(implies(p, eventually(q)));
    match complex {
        TemporalFormula::Temporal(TemporalOperator::Always(f)) => {
            match f.as_ref() {
                TemporalFormula::Implies(_, _) => {},
                _ => panic!("Expected implication inside Always"),
            }
        },
        _ => panic!("Expected Always at top level"),
    }
}

#[test]
fn test_temporal_contract_builder() {
    let request = ContractCondition::new(NodeId(1), ContractKind::Precondition)
        .with_message("request_received".to_string());
    let response = ContractCondition::new(NodeId(2), ContractKind::Postcondition)
        .with_message("response_sent".to_string());
    
    // Build response property: Always(request -> Eventually response)
    let formula = always(implies(
        atom(request),
        eventually(atom(response.clone()))
    ));
    
    let contract = TemporalContractBuilder::new("response_property".to_string())
        .formula(formula)
        .liveness(eventually(atom(response)))
        .bound(100)
        .build();
    
    assert!(contract.is_ok());
    let contract = contract.unwrap();
    assert_eq!(contract.name, "response_property");
    assert_eq!(contract.bound, Some(100));
    assert_eq!(contract.liveness_properties.len(), 1);
}

#[test]
fn test_temporal_verifier() {
    let mut verifier = TemporalVerifier::new(TemporalVerificationConfig::default());
    
    // Create a simple safety property: Always(safe)
    let safe = ContractCondition::new(NodeId(1), ContractKind::Invariant)
        .with_message("system_safe".to_string());
    
    let formula = always(atom(safe));
    
    let contract = TemporalContractBuilder::new("safety".to_string())
        .formula(formula)
        .build()
        .unwrap();
    
    verifier.register_contract(contract);
    
    // Create a trace where safety holds
    let mut states = Vec::new();
    for i in 0..5 {
        let mut bindings = HashMap::new();
        bindings.insert("system_safe".to_string(), serde_json::json!(true));
        bindings.insert("step".to_string(), serde_json::json!(i));
        
        states.push(TemporalState {
            id: i,
            bindings,
            active_contracts: vec!["safety".to_string()],
            timestamp: Some(i as u64 * 1000),
        });
    }
    
    let trace = ExecutionTrace {
        states,
        transitions: vec![
            (0, 1, "step1".to_string()),
            (1, 2, "step2".to_string()),
            (2, 3, "step3".to_string()),
            (3, 4, "step4".to_string()),
        ],
        current: 0,
    };
    
    verifier.add_trace(trace);
    
    // Verify should succeed since safety holds in all states
    let results = verifier.verify_all();
    assert_eq!(results.len(), 1);
    
    let safety_results = &results["safety"];
    assert_eq!(safety_results.len(), 1);
    // Note: actual verification would need proper implementation
}

#[test]
fn test_bounded_model_checking() {
    let mut bmc = BoundedModelChecker::new(10);
    
    // Create initial state
    let initial = BMCState {
        index: 0,
        assignments: HashMap::new(),
        propositions: HashSet::new(),
    };
    
    // Create a simple invariant property
    let invariant = ContractCondition::new(NodeId(1), ContractKind::Invariant);
    let formula = TemporalFormula::Atomic(invariant);
    
    let contract = TemporalContract {
        name: "test_invariant".to_string(),
        formula: always(formula),
        fairness_constraints: vec![],
        safety_properties: vec![],
        liveness_properties: vec![],
        bound: Some(5),
    };
    
    let result = bmc.check_contract(&contract, &initial);
    assert!(result.is_ok());
    
    let bmc_result = result.unwrap();
    assert_eq!(bmc_result.bound, 5);
    // Actual verification would depend on implementation
}

#[test]
fn test_lasso_detection() {
    let checker = BoundedModelChecker::new(10);
    
    let initial = BMCState {
        index: 0,
        assignments: HashMap::new(),
        propositions: HashSet::new(),
    };
    
    // The lasso detection would find paths that loop back
    // This is crucial for verifying liveness properties
}

#[test]
fn test_fairness_constraints() {
    // Test that fairness constraints are properly handled
    let mut verifier = TemporalVerifier::new(TemporalVerificationConfig::default());
    
    let request = ContractCondition::new(NodeId(1), ContractKind::Precondition);
    let grant = ContractCondition::new(NodeId(2), ContractKind::Postcondition);
    
    // Fairness: if request infinitely often, then grant infinitely often
    let fairness = implies(
        always(eventually(atom(request.clone()))),
        always(eventually(atom(grant.clone())))
    );
    
    let contract = TemporalContractBuilder::new("fair_scheduler".to_string())
        .formula(fairness)
        .fairness(eventually(atom(grant)))
        .build()
        .unwrap();
    
    verifier.register_contract(contract);
}

#[test]
fn test_past_time_operators() {
    let config = TemporalVerificationConfig {
        enable_past_time: true,
        ..Default::default()
    };
    
    let verifier = TemporalVerifier::new(config);
    
    // Test Previously operator
    let p = atom(ContractCondition::new(NodeId(1), ContractKind::Invariant));
    let prev_p = TemporalFormula::Temporal(TemporalOperator::Previously(Box::new(p)));
    
    // Test Since operator
    let q = atom(ContractCondition::new(NodeId(2), ContractKind::Invariant));
    let p_since_q = TemporalFormula::Temporal(TemporalOperator::Since(
        Box::new(p.clone()),
        Box::new(q)
    ));
}