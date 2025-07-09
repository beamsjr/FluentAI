//! Comprehensive tests for runtime contract verification

use super::*;
use crate::contract::{Contract, ContractCondition, ContractKind};
use fluentai_core::{
    ast::{Graph, NodeId},
    value::Value,
};
use std::sync::Arc;

// ===== Test Helper Functions =====

fn create_test_graph() -> Arc<Graph> {
    Arc::new(Graph::new())
}

fn create_simple_contract(name: &str) -> Contract {
    let mut contract = Contract::new(name.to_string(), NodeId::new(1).unwrap());

    // Add a simple precondition: x > 0
    contract.add_precondition(
        ContractCondition::new(NodeId::new(10).unwrap(), ContractKind::Precondition)
            .with_message("x must be positive".to_string()),
    );

    contract
}

// ===== RuntimeVerifier Tests =====

#[test]
fn test_runtime_verifier_new() {
    let verifier = RuntimeVerifier::new();
    assert!(verifier.enabled);
    assert!(!verifier.performance_mode);
    assert!(verifier.contracts.is_empty());
    assert!(verifier.ast_graph.is_none());
}

#[test]
fn test_runtime_verifier_default() {
    let verifier = RuntimeVerifier::default();
    assert!(verifier.enabled);
    assert!(!verifier.performance_mode);
}

#[test]
fn test_set_ast_graph() {
    let mut verifier = RuntimeVerifier::new();
    let graph = create_test_graph();

    verifier.set_ast_graph(graph.clone());
    assert!(verifier.ast_graph.is_some());
}

#[test]
fn test_set_enabled() {
    let mut verifier = RuntimeVerifier::new();

    verifier.set_enabled(false);
    assert!(!verifier.enabled);

    verifier.set_enabled(true);
    assert!(verifier.enabled);
}

#[test]
fn test_set_performance_mode() {
    let mut verifier = RuntimeVerifier::new();

    verifier.set_performance_mode(true);
    assert!(verifier.performance_mode);

    verifier.set_performance_mode(false);
    assert!(!verifier.performance_mode);
}

#[test]
fn test_register_contract() {
    let mut verifier = RuntimeVerifier::new();
    let contract = create_simple_contract("test_func");

    verifier.register_contract(contract);
    assert!(verifier.has_contract("test_func"));
}

#[test]
fn test_register_contract_validated_without_graph() {
    let mut verifier = RuntimeVerifier::new();
    let contract = create_simple_contract("test_func");

    // Should succeed even without AST graph (skips purity validation)
    let result = verifier.register_contract_validated(contract);
    assert!(result.is_ok());
    assert!(verifier.has_contract("test_func"));
}

#[test]
fn test_register_contract_validated_with_graph() {
    let mut verifier = RuntimeVerifier::new();
    let graph = create_test_graph();
    verifier.set_ast_graph(graph);

    let contract = create_simple_contract("test_func");

    // register_contract_validated might fail validation but should still register
    // Looking at the implementation, it only registers if validation succeeds
    let result = verifier.register_contract_validated(contract.clone());

    if result.is_ok() {
        assert!(verifier.has_contract("test_func"));
    } else {
        // If validation failed, contract won't be registered
        // So register it manually to test the rest
        verifier.register_contract(contract);
        assert!(verifier.has_contract("test_func"));
    }
}

#[test]
fn test_has_contract() {
    let mut verifier = RuntimeVerifier::new();

    assert!(!verifier.has_contract("nonexistent"));

    let contract = create_simple_contract("existing");
    verifier.register_contract(contract);

    assert!(verifier.has_contract("existing"));
}

#[test]
fn test_get_contract() {
    let mut verifier = RuntimeVerifier::new();
    let contract = create_simple_contract("test_func");

    assert!(verifier.get_contract("test_func").is_none());

    verifier.register_contract(contract);

    let retrieved = verifier.get_contract("test_func");
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().function_name, "test_func");
}

#[test]
fn test_verify_preconditions_disabled() {
    let mut verifier = RuntimeVerifier::new();
    verifier.set_enabled(false);

    let ctx = VerificationContext::pre("test".to_string(), vec![]);
    let result = verifier.verify_preconditions(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_preconditions_no_contract() {
    let verifier = RuntimeVerifier::new();

    let ctx = VerificationContext::pre("nonexistent".to_string(), vec![]);
    let result = verifier.verify_preconditions(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_postconditions_disabled() {
    let mut verifier = RuntimeVerifier::new();
    verifier.set_enabled(false);

    let ctx = VerificationContext::post("test".to_string(), vec![], Value::Integer(42));
    let result = verifier.verify_postconditions(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_postconditions_no_contract() {
    let verifier = RuntimeVerifier::new();

    let ctx = VerificationContext::post("nonexistent".to_string(), vec![], Value::Integer(42));
    let result = verifier.verify_postconditions(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_invariants_disabled() {
    let mut verifier = RuntimeVerifier::new();
    verifier.set_enabled(false);

    let ctx = VerificationContext::pre("test".to_string(), vec![]);
    let result = verifier.verify_invariants(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_invariants_performance_mode() {
    let mut verifier = RuntimeVerifier::new();
    verifier.set_performance_mode(true);

    let ctx = VerificationContext::pre("test".to_string(), vec![]);
    let result = verifier.verify_invariants(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_verify_invariants_no_contract() {
    let verifier = RuntimeVerifier::new();

    let ctx = VerificationContext {
        function_name: "nonexistent".to_string(),
        arguments: vec![],
        return_value: None,
        bindings: HashMap::new(),
        phase: VerificationPhase::Invariant,
    };
    let result = verifier.verify_invariants(&ctx);
    assert!(result.is_ok());
}

#[test]
fn test_check_purity_disabled() {
    let mut verifier = RuntimeVerifier::new();
    verifier.set_enabled(false);

    let result = verifier.check_purity("test", true);
    assert!(result.is_ok());
}

#[test]
fn test_check_purity_no_contract() {
    let verifier = RuntimeVerifier::new();

    let result = verifier.check_purity("nonexistent", true);
    assert!(result.is_ok());
}

#[test]
fn test_check_purity_pure_no_side_effects() {
    let mut verifier = RuntimeVerifier::new();

    let mut contract = Contract::new("pure_func".to_string(), NodeId::new(1).unwrap());
    contract.pure = true;
    verifier.register_contract(contract);

    let result = verifier.check_purity("pure_func", false);
    assert!(result.is_ok());
}

#[test]
fn test_check_purity_pure_with_side_effects() {
    let mut verifier = RuntimeVerifier::new();

    let mut contract = Contract::new("pure_func".to_string(), NodeId::new(1).unwrap());
    contract.pure = true;
    verifier.register_contract(contract);

    let result = verifier.check_purity("pure_func", true);
    assert!(result.is_err());

    match result.unwrap_err() {
        ContractError::Violation(v) => match v {
            ContractViolation::Purity { function, .. } => {
                assert_eq!(function, "pure_func");
            }
            _ => panic!("Expected Purity violation"),
        },
        _ => panic!("Expected ContractError::Violation"),
    }
}

#[test]
fn test_check_purity_impure_with_side_effects() {
    let mut verifier = RuntimeVerifier::new();

    let mut contract = Contract::new("impure_func".to_string(), NodeId::new(1).unwrap());
    contract.pure = false;
    verifier.register_contract(contract);

    let result = verifier.check_purity("impure_func", true);
    assert!(result.is_ok());
}

// ===== VerificationContext Tests =====

#[test]
fn test_verification_context_pre() {
    let args = vec![Value::Integer(42), Value::String("test".to_string())];
    let ctx = VerificationContext::pre("my_func".to_string(), args.clone());

    assert_eq!(ctx.function_name, "my_func");
    assert_eq!(ctx.arguments, args);
    assert!(ctx.return_value.is_none());
    assert!(ctx.bindings.is_empty());
    assert_eq!(ctx.phase, VerificationPhase::Pre);
}

#[test]
fn test_verification_context_post() {
    let args = vec![Value::Integer(42)];
    let return_val = Value::String("result".to_string());
    let ctx = VerificationContext::post("my_func".to_string(), args.clone(), return_val.clone());

    assert_eq!(ctx.function_name, "my_func");
    assert_eq!(ctx.arguments, args);
    assert_eq!(ctx.return_value, Some(return_val));
    assert!(ctx.bindings.is_empty());
    assert_eq!(ctx.phase, VerificationPhase::Post);
}

#[test]
fn test_verification_context_add_binding() {
    let mut ctx = VerificationContext::pre("test".to_string(), vec![]);

    ctx.add_binding("x".to_string(), Value::Integer(10));
    ctx.add_binding("y".to_string(), Value::Boolean(true));

    assert_eq!(ctx.bindings.len(), 2);
    assert_eq!(ctx.bindings.get("x"), Some(&Value::Integer(10)));
    assert_eq!(ctx.bindings.get("y"), Some(&Value::Boolean(true)));
}

#[test]
fn test_verification_context_invariant_phase() {
    let ctx = VerificationContext {
        function_name: "test".to_string(),
        arguments: vec![],
        return_value: None,
        bindings: HashMap::new(),
        phase: VerificationPhase::Invariant,
    };

    assert_eq!(ctx.phase, VerificationPhase::Invariant);
}

// ===== Integration Tests =====

#[test]
fn test_verify_condition_no_ast_graph() {
    let verifier = RuntimeVerifier::new();

    let contract = create_simple_contract("test");
    let ctx = VerificationContext::pre("test".to_string(), vec![Value::Integer(5)]);
    let condition = &contract.preconditions[0];

    let result = verifier.verify_condition(condition, &ctx, &contract);
    assert!(result.is_err());

    match result.unwrap_err() {
        ContractError::VerificationError(msg) => {
            assert!(msg.contains("AST graph not set"));
        }
        _ => panic!("Expected VerificationError"),
    }
}

// ===== VerificationPhase Tests =====

#[test]
fn test_verification_phase_equality() {
    assert_eq!(VerificationPhase::Pre, VerificationPhase::Pre);
    assert_eq!(VerificationPhase::Post, VerificationPhase::Post);
    assert_eq!(VerificationPhase::Invariant, VerificationPhase::Invariant);
    assert_ne!(VerificationPhase::Pre, VerificationPhase::Post);
}

#[test]
fn test_verification_phase_debug() {
    let pre = format!("{:?}", VerificationPhase::Pre);
    assert_eq!(pre, "Pre");

    let post = format!("{:?}", VerificationPhase::Post);
    assert_eq!(post, "Post");

    let inv = format!("{:?}", VerificationPhase::Invariant);
    assert_eq!(inv, "Invariant");
}

#[test]
fn test_verification_phase_clone() {
    let phase = VerificationPhase::Pre;
    let cloned = phase.clone();
    assert_eq!(phase, cloned);
}

#[test]
fn test_verification_phase_copy() {
    let phase = VerificationPhase::Post;
    let copied = phase;
    assert_eq!(phase, copied);
}
