//! Component integration tests for fluentai-contracts
//! Tests the interaction between different components of the contract system

use fluentai_contracts::{
    contract::{Contract, ContractCondition, ContractKind},
    evaluator::ConditionEvaluator,
    frame_conditions::{FrameCondition, FrameConditionManager, FieldAccess},
    symbolic_execution::{SymbolicValue, SymbolicState, SymbolicType},
    errors::ContractError,
};
use fluentai_core::{
    ast::{Graph, Node, NodeId, Literal},
    value::Value,
};
use std::num::NonZeroU32;
use std::collections::HashMap;

#[test]
fn test_contract_with_evaluator() {
    let mut graph = Graph::new();
    
    // Create a simple condition: x > 5
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let five = graph.add_node(Node::Literal(Literal::Integer(5)));
    let gt = graph.add_node(Node::Variable { name: ">".to_string() });
    let condition = graph.add_node(Node::Application {
        function: gt,
        args: vec![x_var, five],
    });
    
    // Create a contract with this precondition
    let mut contract = Contract::new("test_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_precondition(ContractCondition::new(condition, ContractKind::Precondition));
    
    // Test evaluation with x = 10 (should pass)
    let mut bindings = HashMap::new();
    bindings.insert("x".to_string(), Value::Integer(10));
    let evaluator = ConditionEvaluator::new(&graph).with_bindings(bindings);
    
    let result = evaluator.evaluate_condition(condition);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
    
    // Test evaluation with x = 3 (should fail)
    let mut bindings2 = HashMap::new();
    bindings2.insert("x".to_string(), Value::Integer(3));
    let evaluator2 = ConditionEvaluator::new(&graph).with_bindings(bindings2);
    
    let result2 = evaluator2.evaluate_condition(condition);
    assert!(result2.is_ok());
    assert_eq!(result2.unwrap(), false);
}

#[test]
fn test_frame_condition_with_contract() {
    let _contract = Contract::new("increment".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    // Create frame condition that only modifies x
    let mut frame = FrameCondition::new();
    frame.add_modifies_var("x".to_string());
    
    // The contract has a frame condition (even though it's not stored in the Contract struct)
    assert!(frame.modifies.contains("x"));
    assert!(!frame.modifies.contains("y"));
    assert!(!frame.is_pure());
}

#[test]
fn test_symbolic_execution_with_conditions() {
    let mut state = SymbolicState::new();
    
    // Create symbolic values for contract verification
    let x = state.fresh_symbol_typed("x", SymbolicType::Integer);
    let y = state.fresh_symbol_typed("y", SymbolicType::Integer);
    
    // Create constraint: x > y
    let constraint = SymbolicValue::BinOp {
        op: ">".to_string(),
        left: Box::new(x.clone()),
        right: Box::new(y.clone()),
    };
    
    // Add to path constraints
    state.add_constraint(constraint.clone(), true);
    
    assert_eq!(state.path_constraints.len(), 1);
    assert!(state.path_constraints[0].expected);
    
    // Fork state for else branch
    let mut else_state = state.fork();
    else_state.add_constraint(constraint, false);
    
    assert_eq!(else_state.path_constraints.len(), 2);
    assert!(!else_state.path_constraints[1].expected);
}

#[test]
fn test_frame_condition_manager_integration() {
    let mut graph = Graph::new();
    
    // Create a simple function node
    let body_node = graph.add_node(Node::Variable { name: "x".to_string() });
    let func_node = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: body_node,
    });
    
    let mut manager = FrameConditionManager::new(&graph);
    let contract = Contract::new("identity".to_string(), func_node);
    
    // Extract frame conditions (should be pure for identity function)
    let frame_result = manager.extract_from_contract(&contract);
    assert!(frame_result.is_ok());
    let frame = frame_result.unwrap();
    assert!(frame.is_pure());
}

#[test]
fn test_contract_condition_evaluation_integration() {
    let mut graph = Graph::new();
    
    // Create a complex condition: (x > 0) && (x < 100)
    let x_var1 = graph.add_node(Node::Variable { name: "x".to_string() });
    let x_var2 = graph.add_node(Node::Variable { name: "x".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let hundred = graph.add_node(Node::Literal(Literal::Integer(100)));
    
    let gt = graph.add_node(Node::Variable { name: ">".to_string() });
    let lt = graph.add_node(Node::Variable { name: "<".to_string() });
    let and = graph.add_node(Node::Variable { name: "and".to_string() });
    
    let gt_zero = graph.add_node(Node::Application {
        function: gt,
        args: vec![x_var1, zero],
    });
    
    let lt_hundred = graph.add_node(Node::Application {
        function: lt,
        args: vec![x_var2, hundred],
    });
    
    let condition = graph.add_node(Node::Application {
        function: and,
        args: vec![gt_zero, lt_hundred],
    });
    
    // Create contract with range precondition
    let mut contract = Contract::new("bounded_func".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    contract.add_precondition(
        ContractCondition::new(condition, ContractKind::Precondition)
            .with_message("x must be between 0 and 100".to_string())
    );
    
    // Test various values
    let test_cases = vec![
        (50, true),   // in range
        (0, false),   // boundary (exclusive)
        (100, false), // boundary (exclusive)
        (-10, false), // out of range
        (150, false), // out of range
    ];
    
    for (value, expected) in test_cases {
        let mut bindings = HashMap::new();
        bindings.insert("x".to_string(), Value::Integer(value));
        let evaluator = ConditionEvaluator::new(&graph).with_bindings(bindings);
        
        let result = evaluator.evaluate_condition(condition);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), expected, "Failed for x = {}", value);
    }
}

#[test]
fn test_frame_condition_composition() {
    // Test composing frame conditions from multiple functions
    let mut frame1 = FrameCondition::new();
    frame1.add_modifies_var("x".to_string());
    frame1.add_modifies_field("obj".to_string(), "field1".to_string());
    
    let mut frame2 = FrameCondition::new();
    frame2.add_modifies_var("y".to_string());
    frame2.add_modifies_field("obj".to_string(), "field2".to_string());
    frame2.may_allocate = true;
    
    // Manually compose (since merge method doesn't exist in the actual API)
    let mut composed = FrameCondition::new();
    for var in &frame1.modifies {
        composed.modifies.insert(var.clone());
    }
    for var in &frame2.modifies {
        composed.modifies.insert(var.clone());
    }
    for field in &frame1.modifies_fields {
        composed.modifies_fields.insert(field.clone());
    }
    for field in &frame2.modifies_fields {
        composed.modifies_fields.insert(field.clone());
    }
    composed.may_allocate = frame1.may_allocate || frame2.may_allocate;
    
    // Verify composition
    assert!(composed.modifies.contains("x"));
    assert!(composed.modifies.contains("y"));
    assert_eq!(composed.modifies_fields.len(), 2);
    assert!(composed.may_allocate);
    assert!(!composed.is_pure());
}

#[test]
fn test_symbolic_value_evaluation() {
    // Test evaluating symbolic values to concrete values
    let concrete_5 = SymbolicValue::Concrete(Literal::Integer(5));
    let concrete_3 = SymbolicValue::Concrete(Literal::Integer(3));
    
    let sum = SymbolicValue::BinOp {
        op: "+".to_string(),
        left: Box::new(concrete_5),
        right: Box::new(concrete_3),
    };
    
    // In a real implementation, we would have a symbolic evaluator
    // For now, just verify the structure is correct
    match &sum {
        SymbolicValue::BinOp { op, left, right } => {
            assert_eq!(op, "+");
            assert!(matches!(**left, SymbolicValue::Concrete(Literal::Integer(5))));
            assert!(matches!(**right, SymbolicValue::Concrete(Literal::Integer(3))));
        }
        _ => panic!("Expected BinOp"),
    }
}

#[test]
fn test_contract_serialization_with_conditions() {
    let mut contract = Contract::new("serialize_test".to_string(), NodeId(NonZeroU32::new(1).unwrap()));
    
    // Add various conditions
    contract.add_precondition(
        ContractCondition::new(NodeId(NonZeroU32::new(2).unwrap()), ContractKind::Precondition)
            .with_message("Input must be positive".to_string())
    );
    
    contract.add_postcondition(
        ContractCondition::new(NodeId(NonZeroU32::new(3).unwrap()), ContractKind::Postcondition)
            .with_message("Result is non-negative".to_string())
    );
    
    contract.add_invariant(
        ContractCondition::new(NodeId(NonZeroU32::new(4).unwrap()), ContractKind::Invariant)
            .with_message("State consistency maintained".to_string())
    );
    
    contract.complexity = Some("O(n)".to_string());
    contract.pure = true;
    
    // Serialize and deserialize
    let json = serde_json::to_string_pretty(&contract).unwrap();
    let deserialized: Contract = serde_json::from_str(&json).unwrap();
    
    // Verify all fields preserved
    assert_eq!(deserialized.function_name, contract.function_name);
    assert_eq!(deserialized.preconditions.len(), 1);
    assert_eq!(deserialized.postconditions.len(), 1);
    assert_eq!(deserialized.invariants.len(), 1);
    assert_eq!(deserialized.complexity, Some("O(n)".to_string()));
    assert!(deserialized.pure);
}

#[test]
fn test_error_propagation() {
    let graph = Graph::new();
    let evaluator = ConditionEvaluator::new(&graph);
    
    // Try to evaluate non-existent node
    let result = evaluator.evaluate(NodeId(NonZeroU32::new(999).unwrap()));
    assert!(result.is_err());
    match result {
        Err(ContractError::VerificationError(msg)) => {
            assert!(msg.contains("Invalid node ID"));
        }
        _ => panic!("Expected VerificationError"),
    }
    
    // Try to evaluate undefined variable
    let mut graph2 = Graph::new();
    let var_node = graph2.add_node(Node::Variable { name: "undefined".to_string() });
    let evaluator2 = ConditionEvaluator::new(&graph2);
    
    let result2 = evaluator2.evaluate(var_node);
    assert!(result2.is_err());
    match result2 {
        Err(ContractError::VerificationError(msg)) => {
            assert!(msg.contains("Undefined variable"));
        }
        _ => panic!("Expected VerificationError"),
    }
}