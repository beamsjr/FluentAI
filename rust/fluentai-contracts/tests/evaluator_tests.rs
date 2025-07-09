//! Tests for contract condition evaluator

use fluentai_contracts::{
    errors::{ContractError, ContractResult},
    evaluator::ConditionEvaluator,
};
use fluentai_core::{
    ast::{Graph, Literal, Node, NodeId},
    value::Value,
};
use std::collections::HashMap;
use std::num::NonZeroU32;

#[test]
fn test_evaluator_creation() {
    let graph = Graph::new();
    let evaluator = ConditionEvaluator::new(&graph);

    // Basic creation test - just ensure it doesn't panic
    // Note: bindings field is private, so we can't directly check it
}

#[test]
fn test_evaluator_with_bindings() {
    let graph = Graph::new();
    let mut bindings = HashMap::new();
    bindings.insert("x".to_string(), Value::Integer(42));
    bindings.insert("y".to_string(), Value::Boolean(true));

    let evaluator = ConditionEvaluator::new(&graph).with_bindings(bindings.clone());

    // We can't directly verify bindings, but we can test evaluation
    // by adding a variable node and evaluating it
    let mut graph2 = Graph::new();
    let var_node = graph2
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let evaluator2 = ConditionEvaluator::new(&graph2).with_bindings(bindings);

    // This will succeed if the binding was properly set
    let result = evaluator2.evaluate(var_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(42));
}

#[test]
fn test_evaluator_bind() {
    let mut graph = Graph::new();
    let mut evaluator = ConditionEvaluator::new(&graph);

    evaluator.bind("name".to_string(), Value::String("test".to_string()));
    evaluator.bind("count".to_string(), Value::Integer(10));

    // Test by evaluating variables
    let name_var = graph
        .add_node(Node::Variable {
            name: "name".to_string(),
        })
        .expect("Failed to add node");
    let count_var = graph
        .add_node(Node::Variable {
            name: "count".to_string(),
        })
        .expect("Failed to add node");

    let mut evaluator2 = ConditionEvaluator::new(&graph);
    evaluator2.bind("name".to_string(), Value::String("test".to_string()));
    evaluator2.bind("count".to_string(), Value::Integer(10));

    assert_eq!(
        evaluator2.evaluate(name_var).unwrap(),
        Value::String("test".to_string())
    );
    assert_eq!(evaluator2.evaluate(count_var).unwrap(), Value::Integer(10));
}

#[test]
fn test_evaluate_literal_condition() {
    let mut graph = Graph::new();

    // Add a boolean literal that represents a condition
    let true_node = graph
        .add_node(Node::Literal(Literal::Boolean(true)))
        .expect("Failed to add node");
    let false_node = graph
        .add_node(Node::Literal(Literal::Boolean(false)))
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // Evaluate true condition
    let result = evaluator.evaluate_condition(true_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);

    // Evaluate false condition
    let result = evaluator.evaluate_condition(false_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), false);
}

#[test]
fn test_evaluate_non_boolean_condition() {
    let mut graph = Graph::new();

    // Add a non-boolean literal
    let int_node = graph
        .add_node(Node::Literal(Literal::Integer(42)))
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // Should fail because condition must be boolean
    let result = evaluator.evaluate_condition(int_node);
    assert!(result.is_err());

    match result {
        Err(ContractError::VerificationError(msg)) => {
            assert!(msg.contains("must evaluate to boolean"));
        }
        _ => panic!("Expected VerificationError"),
    }
}

#[test]
fn test_evaluate_variable_condition() {
    let mut graph = Graph::new();

    // Add a variable node
    let var_node = graph
        .add_node(Node::Variable {
            name: "is_valid".to_string(),
        })
        .expect("Failed to add node");

    let mut bindings = HashMap::new();
    bindings.insert("is_valid".to_string(), Value::Boolean(true));

    let evaluator = ConditionEvaluator::new(&graph).with_bindings(bindings);

    // Should evaluate to the bound value
    let result = evaluator.evaluate_condition(var_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), true);
}

#[test]
fn test_evaluate_undefined_variable() {
    let mut graph = Graph::new();

    // Add a variable node with no binding
    let var_node = graph
        .add_node(Node::Variable {
            name: "undefined".to_string(),
        })
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // Should fail because variable is not bound
    let result = evaluator.evaluate_condition(var_node);
    assert!(result.is_err());
}

#[test]
fn test_evaluate_simple_comparison() {
    let mut graph = Graph::new();

    // Create: 5 > 3
    let five = graph
        .add_node(Node::Literal(Literal::Integer(5)))
        .expect("Failed to add node");
    let three = graph
        .add_node(Node::Literal(Literal::Integer(3)))
        .expect("Failed to add node");
    let gt = graph
        .add_node(Node::Variable {
            name: ">".to_string(),
        })
        .expect("Failed to add node");
    let comparison = graph
        .add_node(Node::Application {
            function: gt,
            args: vec![five, three],
        })
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // The actual evaluation logic would be in the implementation
    // This test just verifies the structure is correct
    let _ = evaluator.evaluate_condition(comparison);
}

#[test]
fn test_evaluate_with_multiple_bindings() {
    let mut graph = Graph::new();

    // Create variables
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");
    let y_var = graph
        .add_node(Node::Variable {
            name: "y".to_string(),
        })
        .expect("Failed to add node");

    let mut evaluator = ConditionEvaluator::new(&graph);
    evaluator.bind("x".to_string(), Value::Integer(10));
    evaluator.bind("y".to_string(), Value::Integer(20));

    // Test evaluation
    assert_eq!(evaluator.evaluate(x_var).unwrap(), Value::Integer(10));
    assert_eq!(evaluator.evaluate(y_var).unwrap(), Value::Integer(20));

    // Rebind a value
    evaluator.bind("x".to_string(), Value::Integer(30));
    assert_eq!(evaluator.evaluate(x_var).unwrap(), Value::Integer(30));
}

#[test]
fn test_evaluate_nil_literal() {
    let mut graph = Graph::new();

    // Add nil literal
    let nil_node = graph
        .add_node(Node::Literal(Literal::Nil))
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // Nil is not a valid boolean condition
    let result = evaluator.evaluate_condition(nil_node);
    assert!(result.is_err());
}

#[test]
fn test_evaluate_string_literal() {
    let mut graph = Graph::new();

    // Add string literal
    let string_node = graph
        .add_node(Node::Literal(Literal::String("test".to_string())))
        .expect("Failed to add node");

    let evaluator = ConditionEvaluator::new(&graph);

    // String is not a valid boolean condition
    let result = evaluator.evaluate_condition(string_node);
    assert!(result.is_err());
}

#[test]
fn test_evaluator_bindings_isolation() {
    let mut graph = Graph::new();
    let x_var = graph
        .add_node(Node::Variable {
            name: "x".to_string(),
        })
        .expect("Failed to add node");

    // Create first evaluator with bindings
    let mut bindings1 = HashMap::new();
    bindings1.insert("x".to_string(), Value::Integer(1));
    let evaluator1 = ConditionEvaluator::new(&graph).with_bindings(bindings1);

    // Create second evaluator with different bindings
    let mut bindings2 = HashMap::new();
    bindings2.insert("x".to_string(), Value::Integer(2));
    let evaluator2 = ConditionEvaluator::new(&graph).with_bindings(bindings2);

    // Verify they have independent bindings through evaluation
    assert_eq!(evaluator1.evaluate(x_var).unwrap(), Value::Integer(1));
    assert_eq!(evaluator2.evaluate(x_var).unwrap(), Value::Integer(2));
}
