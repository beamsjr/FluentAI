//! Basic integration tests for fluentai-contracts

use fluentai_contracts::{
    contract::{Contract, ContractCondition, ContractKind},
    errors::ContractError,
};
use fluentai_core::ast::NodeId;
use std::num::NonZeroU32;

#[test]
fn test_contract_creation() {
    let contract = Contract::new("test_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    assert_eq!(contract.function_name, "test_func");
    assert!(contract.preconditions.is_empty());
    assert!(contract.postconditions.is_empty());
    assert!(contract.invariants.is_empty());
    assert_eq!(contract.complexity, None);
    assert!(!contract.pure);
}

#[test]
fn test_add_precondition() {
    let mut contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    let condition = ContractCondition::new(NodeId(NonZeroU32::new(2).unwrap()), ContractKind::Precondition);
    
    contract.add_precondition(condition);
    assert_eq!(contract.preconditions.len(), 1);
}

#[test]
fn test_add_postcondition() {
    let mut contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    let condition = ContractCondition::new(NodeId(NonZeroU32::new(2).unwrap()), ContractKind::Postcondition);
    
    contract.add_postcondition(condition);
    assert_eq!(contract.postconditions.len(), 1);
}

#[test]
fn test_has_conditions() {
    let mut contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    assert!(!contract.has_conditions());
    
    let condition = ContractCondition::new(NodeId(NonZeroU32::new(2).unwrap()), ContractKind::Precondition);
    contract.add_precondition(condition);
    assert!(contract.has_conditions());
}

#[test]
fn test_contract_condition_with_message() {
    let condition = ContractCondition::new(
        NodeId(NonZeroU32::new(1).unwrap()),
        ContractKind::Invariant
    )
    .with_message("Must be sorted".to_string());
    
    assert_eq!(condition.message, Some("Must be sorted".to_string()));
}

#[test]
fn test_contract_errors() {
    let error = ContractError::VerificationError("Test error".to_string());
    let display = format!("{}", error);
    assert!(display.contains("Verification error"));
}

#[test]
fn test_pure_contract() {
    let mut contract = Contract::new("pure_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.pure = true;
    assert!(contract.pure);
}

#[test]
fn test_contract_serialization() {
    let contract = Contract::new("test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    // Test that the contract can be serialized
    let json = serde_json::to_string(&contract).unwrap();
    assert!(json.contains("test"));
    
    // Test deserialization
    let deserialized: Contract = serde_json::from_str(&json).unwrap();
    assert_eq!(contract.function_name, deserialized.function_name);
}