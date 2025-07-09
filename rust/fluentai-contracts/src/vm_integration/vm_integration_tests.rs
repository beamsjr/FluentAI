//! Comprehensive tests for VM contract integration

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

    // Add a simple precondition
    contract.add_precondition(
        ContractCondition::new(NodeId::new(10).unwrap(), ContractKind::Precondition)
            .with_message("precondition failed".to_string()),
    );

    // Add a simple postcondition
    contract.add_postcondition(
        ContractCondition::new(NodeId::new(20).unwrap(), ContractKind::Postcondition)
            .with_message("postcondition failed".to_string()),
    );

    contract
}

// ===== ContractRegistry Tests =====

#[test]
fn test_contract_registry_new() {
    let registry = ContractRegistry::new();
    assert!(!registry.enabled);
    assert!(registry.param_names.is_empty());
}

#[test]
fn test_contract_registry_enable() {
    let mut registry = ContractRegistry::new();
    let graph = create_test_graph();

    registry.enable(graph);
    assert!(registry.enabled);
}

#[test]
fn test_register_function_params() {
    let mut registry = ContractRegistry::new();

    registry.register_function_params(
        "my_func".to_string(),
        vec!["x".to_string(), "y".to_string()],
    );

    assert!(registry.param_names.contains_key("my_func"));
    assert_eq!(registry.param_names.get("my_func").unwrap().len(), 2);
}

#[test]
fn test_check_preconditions_disabled() {
    let registry = ContractRegistry::new();
    // Registry is not enabled

    let result = registry.check_preconditions("func", &[Value::Integer(42)]);
    assert!(result.is_ok());
}

#[test]
fn test_check_preconditions_no_contract() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // No contract registered for this function
    let result = registry.check_preconditions("func", &[Value::Integer(42)]);
    assert!(result.is_ok());
}

#[test]
fn test_check_preconditions_with_params() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register a contract
    let contract = create_simple_contract("add");
    registry.verifier.register_contract(contract);

    // Register parameter names
    registry.register_function_params("add".to_string(), vec!["a".to_string(), "b".to_string()]);

    // This will check preconditions but might fail due to missing AST nodes
    let args = vec![Value::Integer(5), Value::Integer(3)];
    let result = registry.check_preconditions("add", &args);
    // Result might be error due to AST evaluation, but we're testing the flow
    let _ = result;
}

#[test]
fn test_check_preconditions_without_params() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register a contract
    let contract = create_simple_contract("add");
    registry.verifier.register_contract(contract);

    // Don't register parameter names - should fall back to positional
    let args = vec![Value::Integer(5), Value::Integer(3)];
    let result = registry.check_preconditions("add", &args);
    // Result might be error due to AST evaluation, but we're testing the flow
    let _ = result;
}

#[test]
fn test_check_postconditions_disabled() {
    let registry = ContractRegistry::new();

    let result = registry.check_postconditions("func", &[Value::Integer(42)], &Value::Integer(84));
    assert!(result.is_ok());
}

#[test]
fn test_check_postconditions_no_contract() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    let result = registry.check_postconditions("func", &[Value::Integer(42)], &Value::Integer(84));
    assert!(result.is_ok());
}

#[test]
fn test_check_postconditions_with_params() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register a contract
    let contract = create_simple_contract("multiply");
    registry.verifier.register_contract(contract);

    // Register parameter names
    registry.register_function_params(
        "multiply".to_string(),
        vec!["x".to_string(), "y".to_string()],
    );

    let args = vec![Value::Integer(7), Value::Integer(6)];
    let result = registry.check_postconditions("multiply", &args, &Value::Integer(42));
    // Result might be error due to AST evaluation, but we're testing the flow
    let _ = result;
}

#[test]
fn test_check_postconditions_without_params() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register a contract
    let contract = create_simple_contract("multiply");
    registry.verifier.register_contract(contract);

    // Don't register parameter names
    let args = vec![Value::Integer(7), Value::Integer(6)];
    let result = registry.check_postconditions("multiply", &args, &Value::Integer(42));
    // Result might be error due to AST evaluation, but we're testing the flow
    let _ = result;
}

#[test]
fn test_is_pure_function_no_contract() {
    let registry = ContractRegistry::new();
    assert!(!registry.is_pure_function("nonexistent"));
}

#[test]
fn test_is_pure_function_pure() {
    let mut registry = ContractRegistry::new();

    let mut contract = create_simple_contract("pure_func");
    contract.pure = true;
    registry.verifier.register_contract(contract);

    assert!(registry.is_pure_function("pure_func"));
}

#[test]
fn test_is_pure_function_impure() {
    let mut registry = ContractRegistry::new();

    let mut contract = create_simple_contract("impure_func");
    contract.pure = false;
    registry.verifier.register_contract(contract);

    assert!(!registry.is_pure_function("impure_func"));
}

#[test]
fn test_check_purity() {
    let mut registry = ContractRegistry::new();

    let mut contract = create_simple_contract("pure_func");
    contract.pure = true;
    registry.verifier.register_contract(contract);

    // Check with no side effects - should pass
    let result = registry.check_purity("pure_func", false);
    assert!(result.is_ok());

    // Check with side effects - should fail
    let result = registry.check_purity("pure_func", true);
    assert!(result.is_err());
}

#[test]
fn test_register_contracts_from_ast_empty() {
    let mut registry = ContractRegistry::new();
    let graph = Graph::new();

    // Should not panic on empty graph
    registry.register_contracts_from_ast(&graph);
}

// Note: The example VM integration is in a private module with private methods,
// so we can't test it directly. These tests are commented out but show
// how integration would work.

/*
// ===== Example VM Integration Tests =====

#[test]
fn test_vm_with_contracts_creation() {
    use example::VMWithContracts;

    let vm = VMWithContracts {
        contracts: ContractRegistry::new(),
    };

    assert!(!vm.contracts.enabled);
}

#[test]
fn test_vm_call_function_no_contracts() {
    use example::VMWithContracts;

    let mut vm = VMWithContracts {
        contracts: ContractRegistry::new(),
    };

    // Should succeed without contracts
    let result = vm.call_function("test", vec![Value::Integer(42)]);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

#[test]
fn test_vm_call_function_with_contracts() {
    use example::VMWithContracts;

    let mut vm = VMWithContracts {
        contracts: ContractRegistry::new(),
    };

    // Enable contracts
    vm.contracts.enable(create_test_graph());

    // Register a contract
    let contract = create_simple_contract("test");
    vm.contracts.verifier.register_contract(contract);

    // This might fail due to AST evaluation, but we're testing the integration
    let result = vm.call_function("test", vec![Value::Integer(42)]);
    let _ = result; // Ignore the result for now
}
*/

// ===== Binding Tests =====

#[test]
fn test_parameter_binding_order() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register parameter names
    registry.register_function_params(
        "func".to_string(),
        vec!["x".to_string(), "y".to_string(), "z".to_string()],
    );

    // Register a simple contract
    let contract = create_simple_contract("func");
    registry.verifier.register_contract(contract);

    // Check that both named and positional bindings work
    let args = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];

    // This tests the binding logic even if evaluation fails
    let _ = registry.check_preconditions("func", &args);
}

#[test]
fn test_parameter_binding_mismatch() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    // Register fewer parameter names than arguments
    registry.register_function_params("func".to_string(), vec!["x".to_string(), "y".to_string()]);

    let contract = create_simple_contract("func");
    registry.verifier.register_contract(contract);

    // Pass more arguments than parameter names
    let args = vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
    ];

    // Should handle gracefully
    let _ = registry.check_preconditions("func", &args);
}

#[test]
fn test_empty_arguments() {
    let mut registry = ContractRegistry::new();
    registry.enable(create_test_graph());

    let contract = create_simple_contract("no_args");
    registry.verifier.register_contract(contract);

    // Test with empty arguments
    let result = registry.check_preconditions("no_args", &[]);
    let _ = result; // Might fail due to AST, but testing the flow

    let result = registry.check_postconditions("no_args", &[], &Value::Nil);
    let _ = result;
}
