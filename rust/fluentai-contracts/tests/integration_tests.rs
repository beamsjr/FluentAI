//! Integration tests for contract verification
//! Ported from Python test suite
//! 
//! NOTE: These tests use s-expression syntax for contract specifications.
//! They should be updated once FLC parser supports contract annotations.

#![cfg(test)]
#![allow(unused_imports)]

use std::sync::Arc;

use fluentai_contracts::{ContractError, ContractRegistry, ContractViolation};
use fluentai_core::{
    ast::{Graph, Node},
    value::Value,
};
use fluentai_parser::parse_flc;

/// Helper to set up a contract registry with parsed code
fn setup_contracts(code: &str) -> (Arc<Graph>, ContractRegistry) {
    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    (graph, registry)
}

#[test]
fn test_parse_simple_contract() {
    let code = r#"
        @contract(add)
        @requires(x >= 0)
        @requires(y >= 0)
        @ensures(result >= 0)
        @complexity("O(1)")
        @pure(true)
        private function add(x: int, y: int) -> int {
            x + y
        }
    "#;

    let graph = parse_flc(code).unwrap();

    // Find the contract node
    let contract_node = graph
        .nodes
        .values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");

    if let Node::Contract {
        function_name,
        preconditions,
        postconditions,
        complexity,
        pure,
        ..
    } = contract_node
    {
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
        @contract(binary_search)
        @requires(sorted(arr))
        @ensures(result == -1 || nth(arr, result) == target)
        @invariant(high >= low)
        @complexity("O(log n)")
        @pure(true)
        private function binary_search(arr, target, low, high) -> int {
            if (low > high) { -1 }
            else {
                let mid = (low + high) / 2;
                if (nth(arr, mid) == target) { mid }
                else { if (nth(arr, mid) < target) { binary_search(arr, target, mid + 1, high) }
                else { binary_search(arr, target, low, mid - 1) } }
            }
        }
    "#;

    let graph = parse_flc(code).unwrap();

    let contract_node = graph
        .nodes
        .values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");

    if let Node::Contract {
        function_name,
        invariants,
        complexity,
        ..
    } = contract_node
    {
        assert_eq!(function_name, "binary_search");
        assert_eq!(invariants.len(), 1);
        assert_eq!(complexity, &Some("O(log n)".to_string()));
    }
}

#[test]
fn test_parse_impure_contract() {
    let code = r#"
        @contract(read_file)
        @requires(file_exists(path))
        @ensures(is_string(result))
        @pure(false)
        private function read_file(path: string) -> string {
            perform IO.read_file(path)
        }
    "#;

    let graph = parse_flc(code).unwrap();

    let contract_node = graph
        .nodes
        .values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");

    if let Node::Contract {
        function_name,
        pure,
        ..
    } = contract_node
    {
        assert_eq!(function_name, "read_file");
        assert_eq!(*pure, false);
    }
}

#[test]
fn test_parse_contract_with_pre_post_keywords() {
    let code = r#"
        @contract(divide)
        @requires(y != 0)
        @ensures(result == x / y)
        private function divide(x: int, y: int) -> int {
            x / y
        }
    "#;

    let graph = parse_flc(code).unwrap();

    let contract_node = graph
        .nodes
        .values()
        .find(|n| matches!(n, Node::Contract { .. }))
        .expect("Contract node not found");

    if let Node::Contract {
        preconditions,
        postconditions,
        ..
    } = contract_node
    {
        assert_eq!(preconditions.len(), 1);
        assert_eq!(postconditions.len(), 1);
    }
}

#[test]
fn test_precondition_success() {
    let code = r#"
        @contract(divide)
        @requires(y != 0)
        @ensures(result == x / y)
        private function divide(x: int, y: int) -> int {
            x / y
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("divide".to_string(), vec!["x".to_string(), "y".to_string()]);

    // Test valid precondition
    let result = registry.check_preconditions("divide", &[Value::Integer(10), Value::Integer(2)]);
    assert!(result.is_ok());
}

#[test]
fn test_precondition_failure() {
    let code = r#"
        @contract(divide)
        @requires(y != 0)
        @ensures(result == x / y)
        private function divide(x: int, y: int) -> int {
            x / y
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("divide".to_string(), vec!["x".to_string(), "y".to_string()]);

    // Test invalid precondition
    let result = registry.check_preconditions("divide", &[Value::Integer(10), Value::Integer(0)]);

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
        @contract(abs)
        @ensures(result >= 0)
        private function abs(x: int) -> int {
            if (x < 0) { -x } else { x }
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("abs".to_string(), vec!["x".to_string()]);

    // Test valid postcondition
    let result = registry.check_postconditions("abs", &[Value::Integer(-5)], &Value::Integer(5));
    assert!(result.is_ok());
}

#[test]
fn test_postcondition_failure() {
    let code = r#"
        @contract(abs)
        @ensures(result >= 0)
        private function abs(x: int) -> int {
            if (x < 0) { -x } else { x }
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("abs".to_string(), vec!["x".to_string()]);

    // Test invalid postcondition (bug: returning negative)
    let result = registry.check_postconditions(
        "abs",
        &[Value::Integer(-5)],
        &Value::Integer(-5), // Wrong!
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
        @contract(add_numbers)
        @requires(is_number(x))
        @requires(is_number(y))
        @ensures(is_number(result))
        private function add_numbers(x: float, y: float) -> float {
            x + y
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params(
        "add_numbers".to_string(),
        vec!["x".to_string(), "y".to_string()],
    );

    // Test with valid types
    let result =
        registry.check_preconditions("add_numbers", &[Value::Integer(3), Value::Float(4.5)]);
    assert!(result.is_ok());

    // Test postcondition
    let result = registry.check_postconditions(
        "add_numbers",
        &[Value::Integer(3), Value::Float(4.5)],
        &Value::Float(7.5),
    );
    assert!(result.is_ok());

    // Test with invalid types
    let result = registry.check_preconditions(
        "add_numbers",
        &[Value::String("not a number".to_string()), Value::Integer(4)],
    );
    assert!(result.is_err());
}

#[test]
fn test_complex_conditions() {
    let code = r#"
        @contract(safe_divide)
        @requires(is_number(x) && is_number(y) && y != 0)
        @ensures(is_number(result))
        private function safe_divide(x: float, y: float) -> float {
            x / y
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params(
        "safe_divide".to_string(),
        vec!["x".to_string(), "y".to_string()],
    );

    // Test valid case
    let result =
        registry.check_preconditions("safe_divide", &[Value::Integer(10), Value::Integer(2)]);
    assert!(result.is_ok());

    // Test division by zero
    let result =
        registry.check_preconditions("safe_divide", &[Value::Integer(10), Value::Integer(0)]);
    assert!(result.is_err());

    // Test non-number
    let result = registry.check_preconditions(
        "safe_divide",
        &[Value::String("ten".to_string()), Value::Integer(2)],
    );
    assert!(result.is_err());
}

#[test]
fn test_list_operations() {
    let code = r#"
        @contract(first_element)
        @requires(is_list(lst) && !is_empty(lst))
        @ensures(result == nth(lst, 0))
        private function first_element(lst) -> int {
            nth(lst, 0)
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("first_element".to_string(), vec!["lst".to_string()]);

    // Test with non-empty list
    let list = Value::List(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
    ]);
    let result = registry.check_preconditions("first_element", &[list.clone()]);
    assert!(result.is_ok());

    // Test postcondition
    let result = registry.check_postconditions("first_element", &[list], &Value::Integer(1));
    assert!(result.is_ok());

    // Test with empty list
    let empty_list = Value::List(vec![]);
    let result = registry.check_preconditions("first_element", &[empty_list]);
    assert!(result.is_err());
}

#[test]
fn test_purity_checking() {
    let code = r#"
        @contract(pure_add)
        @requires(is_number(x))
        @requires(is_number(y))
        @ensures(result == x + y)
        @pure(true)
        private function pure_add(x: int, y: int) -> int {
            x + y
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let registry = ContractRegistry::new();
    let mut registry = registry;
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);

    // Check that function is marked as pure
    assert!(registry.is_pure_function("pure_add"));

    // Test purity violation
    let result = registry.check_purity("pure_add", true); // had_side_effects = true
    assert!(result.is_err());

    // Test no purity violation
    let result = registry.check_purity("pure_add", false);
    assert!(result.is_ok());
}

#[test]
fn test_contract_with_multiple_conditions() {
    let code = r#"
        @contract(range_check)
        @requires(is_number(x))
        @ensures(result >= 0)
        @ensures(result <= 100)
        private function range_check(x) -> int {
            if (x < 0) { 0 } else { if (x > 100) { 100 } else { x } }
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
    registry.register_function_params("range_check".to_string(), vec!["x".to_string()]);

    // Test valid result
    let result =
        registry.check_postconditions("range_check", &[Value::Integer(150)], &Value::Integer(50));
    assert!(result.is_ok());

    // Test result too low
    let result =
        registry.check_postconditions("range_check", &[Value::Integer(150)], &Value::Integer(-1));
    assert!(result.is_err());

    // Test result too high
    let result =
        registry.check_postconditions("range_check", &[Value::Integer(150)], &Value::Integer(101));
    assert!(result.is_err());
}

#[test]
fn test_sorted_predicate() {
    let code = r#"
        @contract(sort)
        @requires(is_list(input))
        @ensures(is_sorted(result))
        @ensures(length(result) == length(input))
        private function sort(input) {
            input
        }
    "#;

    let graph = Arc::new(parse_flc(code).expect("Failed to parse code"));
    let mut registry = ContractRegistry::new();
    registry.enable(graph.clone());
    registry.register_contracts_from_ast(&graph);
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
