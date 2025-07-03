//! Integration tests for contract verification
//! Ported from Python test suite

use std::sync::Arc;

use claudelang_contracts::{
    ContractRegistry,
    ContractError,
    ContractViolation,
};
use claudelang_parser::parse;
use claudelang_core::{
    value::Value,
    ast::{Graph, Node},
};

/// Helper to set up a contract registry with parsed code
fn setup_contracts(code: &str) -> (Arc<Graph>, ContractRegistry) {
    let graph = Arc::new(parse(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    (graph, registry)
}

#[test]
fn test_parse_simple_contract() {
    let code = r#"
        (spec:contract add
          :requires [(>= x 0) (>= y 0)]
          :ensures [(>= result 0)]
          :complexity "O(1)")
    "#;
    
    let graph = parse(code).unwrap();
    
    // Find the contract node
    let contract_node = graph.nodes.values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");
    
    if let Node::Contract { 
        function_name,
        preconditions,
        postconditions,
        complexity,
        pure,
        ..
    } = contract_node {
        assert_eq!(function_name, "add");
        assert_eq!(preconditions.len(), 2);
        assert_eq!(postconditions.len(), 1);
        assert_eq!(complexity, &Some("O(1)".to_string()));
        assert_eq!(*pure, true);
    }
}

#[test]
fn test_parse_contract_with_invariants() {
    let code = r#"
        (spec:contract binary-search
          :requires [(sorted? arr)]
          :ensures [(or (= result -1) (= (nth arr result) target))]
          :invariant [(>= high low)]
          :complexity "O(log n)")
    "#;
    
    let graph = parse(code).unwrap();
    
    let contract_node = graph.nodes.values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");
    
    if let Node::Contract { 
        function_name,
        invariants,
        complexity,
        ..
    } = contract_node {
        assert_eq!(function_name, "binary-search");
        assert_eq!(invariants.len(), 1);
        assert_eq!(complexity, &Some("O(log n)".to_string()));
    }
}

#[test]
fn test_parse_impure_contract() {
    let code = r#"
        (spec:contract read-file
          :requires [(file-exists? path)]
          :ensures [(string? result)]
          :pure false)
    "#;
    
    let graph = parse(code).unwrap();
    
    let contract_node = graph.nodes.values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");
    
    if let Node::Contract { function_name, pure, .. } = contract_node {
        assert_eq!(function_name, "read-file");
        assert_eq!(*pure, false);
    }
}

#[test]
fn test_parse_contract_with_pre_post_keywords() {
    let code = r#"
        (spec:contract divide
          :pre [(not= y 0)]
          :post [(= result (/ x y))])
    "#;
    
    let graph = parse(code).unwrap();
    
    let contract_node = graph.nodes.values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");
    
    if let Node::Contract { 
        preconditions,
        postconditions,
        ..
    } = contract_node {
        assert_eq!(preconditions.len(), 1);
        assert_eq!(postconditions.len(), 1);
    }
}

#[test]
fn test_precondition_success() {
    let code = r#"
        (spec:contract divide
          :requires [(not= y 0)]
          :ensures [(= result (/ x y))])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("divide".to_string(), vec!["x".to_string(), "y".to_string()]);
    
    // Test valid precondition
    let result = registry.check_preconditions("divide", &[
        Value::Integer(10),
        Value::Integer(2),
    ]);
    assert!(result.is_ok());
}

#[test]
fn test_precondition_failure() {
    let code = r#"
        (spec:contract divide
          :requires [(not= y 0)]
          :ensures [(= result (/ x y))])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("divide".to_string(), vec!["x".to_string(), "y".to_string()]);
    
    // Test invalid precondition
    let result = registry.check_preconditions("divide", &[
        Value::Integer(10),
        Value::Integer(0),
    ]);
    
    assert!(result.is_err());
    if let Err(ContractError::Violation(violation)) = result {
        match violation {
            ContractViolation::Precondition { function, .. } => {
                assert_eq!(function, Some("divide".to_string()));
            }
            _ => panic!("Expected precondition violation"),
        }
    }
}

#[test]
fn test_postcondition_success() {
    let code = r#"
        (spec:contract abs
          :ensures [(>= result 0)])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("abs".to_string(), vec!["x".to_string()]);
    
    // Test valid postcondition
    let result = registry.check_postconditions("abs", 
        &[Value::Integer(-5)],
        &Value::Integer(5)
    );
    assert!(result.is_ok());
}

#[test]
fn test_postcondition_failure() {
    let code = r#"
        (spec:contract abs
          :ensures [(>= result 0)])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("abs".to_string(), vec!["x".to_string()]);
    
    // Test invalid postcondition (bug: returning negative)
    let result = registry.check_postconditions("abs", 
        &[Value::Integer(-5)],
        &Value::Integer(-5)  // Wrong!
    );
    
    assert!(result.is_err());
    if let Err(ContractError::Violation(violation)) = result {
        match violation {
            ContractViolation::Postcondition { function, .. } => {
                assert_eq!(function, Some("abs".to_string()));
            }
            _ => panic!("Expected postcondition violation"),
        }
    }
}

#[test]
fn test_type_predicates() {
    let code = r#"
        (spec:contract add-numbers
          :requires [(number? x) (number? y)]
          :ensures [(number? result)])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("add-numbers".to_string(), vec!["x".to_string(), "y".to_string()]);
    
    // Test with valid types
    let result = registry.check_preconditions("add-numbers", &[
        Value::Integer(3),
        Value::Float(4.5),
    ]);
    assert!(result.is_ok());
    
    // Test postcondition
    let result = registry.check_postconditions("add-numbers",
        &[Value::Integer(3), Value::Float(4.5)],
        &Value::Float(7.5)
    );
    assert!(result.is_ok());
    
    // Test with invalid types
    let result = registry.check_preconditions("add-numbers", &[
        Value::String("not a number".to_string()),
        Value::Integer(4),
    ]);
    assert!(result.is_err());
}

#[test]
fn test_complex_conditions() {
    let code = r#"
        (spec:contract safe-divide
          :requires [(and (number? x) (number? y) (not= y 0))]
          :ensures [(number? result)])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("safe-divide".to_string(), vec!["x".to_string(), "y".to_string()]);
    
    // Test valid case
    let result = registry.check_preconditions("safe-divide", &[
        Value::Integer(10),
        Value::Integer(2),
    ]);
    assert!(result.is_ok());
    
    // Test division by zero
    let result = registry.check_preconditions("safe-divide", &[
        Value::Integer(10),
        Value::Integer(0),
    ]);
    assert!(result.is_err());
    
    // Test non-number
    let result = registry.check_preconditions("safe-divide", &[
        Value::String("ten".to_string()),
        Value::Integer(2),
    ]);
    assert!(result.is_err());
}

#[test]
fn test_list_operations() {
    let code = r#"
        (spec:contract first-element
          :requires [(and (list? lst) (not (empty? lst)))]
          :ensures [(= result (nth lst 0))])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("first-element".to_string(), vec!["lst".to_string()]);
    
    // Test with non-empty list
    let list = Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)]);
    let result = registry.check_preconditions("first-element", &[list.clone()]);
    assert!(result.is_ok());
    
    // Test postcondition
    let result = registry.check_postconditions("first-element",
        &[list],
        &Value::Integer(1)
    );
    assert!(result.is_ok());
    
    // Test with empty list
    let empty_list = Value::List(vec![]);
    let result = registry.check_preconditions("first-element", &[empty_list]);
    assert!(result.is_err());
}

#[test]
fn test_purity_checking() {
    let code = r#"
        (spec:contract pure-add
          :requires [(number? x) (number? y)]
          :ensures [(= result (+ x y))]
          :pure true)
    "#;
    
    let (_, registry) = setup_contracts(code);
    
    // Check that function is marked as pure
    assert!(registry.is_pure_function("pure-add"));
    
    // Test purity violation
    let result = registry.check_purity("pure-add", true); // had_side_effects = true
    assert!(result.is_err());
    
    // Test no purity violation
    let result = registry.check_purity("pure-add", false);
    assert!(result.is_ok());
}

#[test]
fn test_contract_with_multiple_conditions() {
    let code = r#"
        (spec:contract range-check
          :requires [(number? x)]
          :ensures [(>= result 0) (<= result 100)])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("range-check".to_string(), vec!["x".to_string()]);
    
    // Test valid result
    let result = registry.check_postconditions("range-check",
        &[Value::Integer(150)],
        &Value::Integer(50)
    );
    assert!(result.is_ok());
    
    // Test result too low
    let result = registry.check_postconditions("range-check",
        &[Value::Integer(150)],
        &Value::Integer(-1)
    );
    assert!(result.is_err());
    
    // Test result too high
    let result = registry.check_postconditions("range-check",
        &[Value::Integer(150)],
        &Value::Integer(101)
    );
    assert!(result.is_err());
}

#[test]
fn test_sorted_predicate() {
    let code = r#"
        (spec:contract sort
          :requires [(list? input)]
          :ensures [(sorted? result) (= (length result) (length input))])
    "#;
    
    let (_, mut registry) = setup_contracts(code);
    registry.register_function_params("sort".to_string(), vec!["input".to_string()]);
    
    let input = Value::List(vec![
        Value::Integer(3),
        Value::Integer(1),
        Value::Integer(4),
        Value::Integer(1),
        Value::Integer(5),
    ]);
    
    let sorted_output = Value::List(vec![
        Value::Integer(1),
        Value::Integer(1),
        Value::Integer(3),
        Value::Integer(4),
        Value::Integer(5),
    ]);
    
    // Test valid postcondition
    let result = registry.check_postconditions("sort", &[input.clone()], &sorted_output);
    assert!(result.is_ok());
    
    // Test unsorted output
    let unsorted_output = Value::List(vec![
        Value::Integer(3),
        Value::Integer(1),
        Value::Integer(4),
        Value::Integer(5),
        Value::Integer(1),
    ]);
    
    let result = registry.check_postconditions("sort", &[input.clone()], &unsorted_output);
    assert!(result.is_err());
    
    // Test wrong length
    let wrong_length = Value::List(vec![
        Value::Integer(1),
        Value::Integer(3),
        Value::Integer(4),
    ]);
    
    let result = registry.check_postconditions("sort", &[input], &wrong_length);
    assert!(result.is_err());
}