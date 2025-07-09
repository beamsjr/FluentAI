//! Comprehensive tests for contract condition evaluator

use super::*;
use fluentai_core::{
    ast::{Graph, Literal, Node, NodeId},
    value::Value,
};
use std::collections::HashMap;

// ===== Test Helper Functions =====

struct TestGraphBuilder {
    graph: Graph,
}

impl TestGraphBuilder {
    fn new() -> Self {
        Self {
            graph: Graph::new(),
        }
    }

    fn add_literal(&mut self, lit: Literal) -> NodeId {
        self.graph
            .add_node(Node::Literal(lit))
            .expect("Failed to add literal node")
    }

    fn add_variable(&mut self, name: &str) -> NodeId {
        self.graph
            .add_node(Node::Variable {
                name: name.to_string(),
            })
            .expect("Failed to add variable node")
    }

    fn add_application(&mut self, func_name: &str, args: Vec<NodeId>) -> NodeId {
        let func_id = self.add_variable(func_name);
        self.graph
            .add_node(Node::Application {
                function: func_id,
                args,
            })
            .expect("Failed to add application node")
    }

    fn add_if(&mut self, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> NodeId {
        self.graph
            .add_node(Node::If {
                condition,
                then_branch,
                else_branch,
            })
            .expect("Failed to add if node")
    }

    fn add_list(&mut self, elements: Vec<NodeId>) -> NodeId {
        self.graph
            .add_node(Node::List(elements))
            .expect("Failed to add list node")
    }

    fn build(self) -> Graph {
        self.graph
    }
}

// ===== ConditionEvaluator Tests =====

#[test]
fn test_evaluator_creation() {
    let graph = Graph::new();
    let evaluator = ConditionEvaluator::new(&graph);
    // Test passes if no panic
}

#[test]
fn test_evaluator_with_bindings() {
    let graph = Graph::new();
    let mut bindings = HashMap::new();
    bindings.insert("x".to_string(), Value::Integer(42));

    let evaluator = ConditionEvaluator::new(&graph).with_bindings(bindings.clone());
    // Test the builder pattern works
}

#[test]
fn test_bind() {
    let graph = Graph::new();
    let mut evaluator = ConditionEvaluator::new(&graph);

    evaluator.bind("x".to_string(), Value::Integer(10));
    evaluator.bind("y".to_string(), Value::String("test".to_string()));
}

// ===== Literal Evaluation Tests =====

#[test]
fn test_evaluate_integer_literal() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_literal(Literal::Integer(42));
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(42));
}

#[test]
fn test_evaluate_float_literal() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_literal(Literal::Float(3.14));
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Float(3.14));
}

#[test]
fn test_evaluate_string_literal() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_literal(Literal::String("hello".to_string()));
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::String("hello".to_string()));
}

#[test]
fn test_evaluate_boolean_literal() {
    let mut builder = TestGraphBuilder::new();
    let true_node = builder.add_literal(Literal::Boolean(true));
    let false_node = builder.add_literal(Literal::Boolean(false));
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    let result = evaluator.evaluate(true_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));

    let result = evaluator.evaluate(false_node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(false));
}

#[test]
fn test_evaluate_nil_literal() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_literal(Literal::Nil);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Nil);
}

// ===== Variable Evaluation Tests =====

#[test]
fn test_evaluate_variable_defined() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_variable("x");
    let graph = builder.build();

    let mut evaluator = ConditionEvaluator::new(&graph);
    evaluator.bind("x".to_string(), Value::Integer(100));

    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(100));
}

#[test]
fn test_evaluate_variable_undefined() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_variable("undefined");
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Undefined variable"));
}

// ===== Comparison Operator Tests =====

#[test]
fn test_builtin_equal() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(5));
    let node = builder.add_application("=", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_equal_different() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(10));
    let node = builder.add_application("==", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(false));
}

#[test]
fn test_builtin_not_equal() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(10));
    let node = builder.add_application("!=", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_less() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(10));
    let node = builder.add_application("<", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_greater() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Float(10.5));
    let arg2 = builder.add_literal(Literal::Float(5.5));
    let node = builder.add_application(">", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_less_equal() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(5));
    let node = builder.add_application("<=", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_greater_equal() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(10));
    let arg2 = builder.add_literal(Literal::Integer(5));
    let node = builder.add_application(">=", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_comparison_mixed_types() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Float(5.5));
    let node = builder.add_application("<", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

// ===== Arithmetic Operator Tests =====

#[test]
fn test_builtin_add() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(10));
    let node = builder.add_application("+", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(15));
}

#[test]
fn test_builtin_add_multiple() {
    let mut builder = TestGraphBuilder::new();
    let args = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
        builder.add_literal(Literal::Integer(3)),
        builder.add_literal(Literal::Integer(4)),
    ];
    let node = builder.add_application("+", args);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(10));
}

#[test]
fn test_builtin_add_empty() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_application("+", vec![]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(0));
}

#[test]
fn test_builtin_subtract() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(10));
    let arg2 = builder.add_literal(Literal::Integer(3));
    let node = builder.add_application("-", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(7));
}

#[test]
fn test_builtin_multiply() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(5));
    let arg2 = builder.add_literal(Literal::Integer(6));
    let node = builder.add_application("*", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(30));
}

#[test]
fn test_builtin_multiply_empty() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_application("*", vec![]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(1));
}

#[test]
fn test_builtin_divide() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(20));
    let arg2 = builder.add_literal(Literal::Integer(4));
    let node = builder.add_application("/", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), Value::Integer(5));
}

#[test]
fn test_builtin_divide_by_zero() {
    let mut builder = TestGraphBuilder::new();
    let arg1 = builder.add_literal(Literal::Integer(10));
    let arg2 = builder.add_literal(Literal::Integer(0));
    let node = builder.add_application("/", vec![arg1, arg2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Division by zero"));
}

// ===== Type Predicate Tests =====

#[test]
fn test_builtin_is_integer() {
    let mut builder = TestGraphBuilder::new();
    let int_arg = builder.add_literal(Literal::Integer(42));
    let float_arg = builder.add_literal(Literal::Float(3.14));

    let int_node = builder.add_application("int?", vec![int_arg]);
    let float_node = builder.add_application("integer?", vec![float_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    let result = evaluator.evaluate(int_node);
    assert_eq!(result.unwrap(), Value::Boolean(true));

    let result = evaluator.evaluate(float_node);
    assert_eq!(result.unwrap(), Value::Boolean(false));
}

#[test]
fn test_builtin_is_float() {
    let mut builder = TestGraphBuilder::new();
    let float_arg = builder.add_literal(Literal::Float(3.14));
    let node = builder.add_application("float?", vec![float_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_is_number() {
    let mut builder = TestGraphBuilder::new();
    let int_arg = builder.add_literal(Literal::Integer(42));
    let float_arg = builder.add_literal(Literal::Float(3.14));
    let string_arg = builder.add_literal(Literal::String("not a number".to_string()));

    let int_node = builder.add_application("number?", vec![int_arg]);
    let float_node = builder.add_application("number?", vec![float_arg]);
    let string_node = builder.add_application("number?", vec![string_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(evaluator.evaluate(int_node).unwrap(), Value::Boolean(true));
    assert_eq!(
        evaluator.evaluate(float_node).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        evaluator.evaluate(string_node).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_builtin_is_string() {
    let mut builder = TestGraphBuilder::new();
    let string_arg = builder.add_literal(Literal::String("hello".to_string()));
    let node = builder.add_application("string?", vec![string_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_is_nil() {
    let mut builder = TestGraphBuilder::new();
    let nil_arg = builder.add_literal(Literal::Nil);
    let node = builder.add_application("nil?", vec![nil_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

// ===== List Operation Tests =====

#[test]
fn test_builtin_is_list() {
    let mut builder = TestGraphBuilder::new();
    let elem1 = builder.add_literal(Literal::Integer(1));
    let elem2 = builder.add_literal(Literal::Integer(2));
    let list = builder.add_list(vec![elem1, elem2]);
    let node = builder.add_application("list?", vec![list]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_length_list() {
    let mut builder = TestGraphBuilder::new();
    let elements = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
        builder.add_literal(Literal::Integer(3)),
    ];
    let list = builder.add_list(elements);
    let node = builder.add_application("length", vec![list]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Integer(3));
}

#[test]
fn test_builtin_length_string() {
    let mut builder = TestGraphBuilder::new();
    let string_arg = builder.add_literal(Literal::String("hello".to_string()));
    let node = builder.add_application("length", vec![string_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Integer(5));
}

#[test]
fn test_builtin_nth() {
    let mut builder = TestGraphBuilder::new();
    let elements = vec![
        builder.add_literal(Literal::Integer(10)),
        builder.add_literal(Literal::Integer(20)),
        builder.add_literal(Literal::Integer(30)),
    ];
    let list = builder.add_list(elements);
    let index = builder.add_literal(Literal::Integer(1));
    let node = builder.add_application("nth", vec![list, index]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Integer(20));
}

#[test]
fn test_builtin_nth_out_of_bounds() {
    let mut builder = TestGraphBuilder::new();
    let elements = vec![builder.add_literal(Literal::Integer(10))];
    let list = builder.add_list(elements);
    let index = builder.add_literal(Literal::Integer(5));
    let node = builder.add_application("nth", vec![list, index]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("out of bounds"));
}

#[test]
fn test_builtin_is_empty() {
    let mut builder = TestGraphBuilder::new();
    let empty_list = builder.add_list(vec![]);
    let elem = builder.add_literal(Literal::Integer(1));
    let non_empty_list = builder.add_list(vec![elem]);

    let empty_node = builder.add_application("empty?", vec![empty_list]);
    let non_empty_node = builder.add_application("empty?", vec![non_empty_list]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(
        evaluator.evaluate(empty_node).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        evaluator.evaluate(non_empty_node).unwrap(),
        Value::Boolean(false)
    );
}

// ===== Logical Operator Tests =====

#[test]
fn test_builtin_and() {
    let mut builder = TestGraphBuilder::new();
    let true1 = builder.add_literal(Literal::Boolean(true));
    let true2 = builder.add_literal(Literal::Boolean(true));
    let false1 = builder.add_literal(Literal::Boolean(false));

    let all_true = builder.add_application("and", vec![true1, true2]);
    let with_false = builder.add_application("and", vec![true1, false1]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(evaluator.evaluate(all_true).unwrap(), Value::Boolean(true));
    assert_eq!(
        evaluator.evaluate(with_false).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_builtin_and_empty() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_application("and", vec![]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_or() {
    let mut builder = TestGraphBuilder::new();
    let true1 = builder.add_literal(Literal::Boolean(true));
    let false1 = builder.add_literal(Literal::Boolean(false));
    let false2 = builder.add_literal(Literal::Boolean(false));

    let with_true = builder.add_application("or", vec![false1, true1]);
    let all_false = builder.add_application("or", vec![false1, false2]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(evaluator.evaluate(with_true).unwrap(), Value::Boolean(true));
    assert_eq!(
        evaluator.evaluate(all_false).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_builtin_or_empty() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_application("or", vec![]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(false));
}

#[test]
fn test_builtin_not() {
    let mut builder = TestGraphBuilder::new();
    let true_val = builder.add_literal(Literal::Boolean(true));
    let false_val = builder.add_literal(Literal::Boolean(false));

    let not_true = builder.add_application("not", vec![true_val]);
    let not_false = builder.add_application("not", vec![false_val]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(evaluator.evaluate(not_true).unwrap(), Value::Boolean(false));
    assert_eq!(evaluator.evaluate(not_false).unwrap(), Value::Boolean(true));
}

// ===== Custom Predicate Tests =====

#[test]
fn test_builtin_is_sorted() {
    let mut builder = TestGraphBuilder::new();
    let sorted_elements = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
        builder.add_literal(Literal::Integer(3)),
    ];
    let unsorted_elements = vec![
        builder.add_literal(Literal::Integer(3)),
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
    ];

    let sorted_list = builder.add_list(sorted_elements);
    let unsorted_list = builder.add_list(unsorted_elements);

    let sorted_node = builder.add_application("sorted?", vec![sorted_list]);
    let unsorted_node = builder.add_application("sorted?", vec![unsorted_list]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(
        evaluator.evaluate(sorted_node).unwrap(),
        Value::Boolean(true)
    );
    assert_eq!(
        evaluator.evaluate(unsorted_node).unwrap(),
        Value::Boolean(false)
    );
}

#[test]
fn test_builtin_is_sorted_empty() {
    let mut builder = TestGraphBuilder::new();
    let empty_list = builder.add_list(vec![]);
    let node = builder.add_application("sorted?", vec![empty_list]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

#[test]
fn test_builtin_file_exists() {
    let mut builder = TestGraphBuilder::new();
    let path = builder.add_literal(Literal::String("/tmp/test.txt".to_string()));
    let node = builder.add_application("file-exists?", vec![path]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    // Currently returns true as placeholder
    assert_eq!(result.unwrap(), Value::Boolean(true));
}

// ===== If Expression Tests =====

#[test]
fn test_evaluate_if_true() {
    let mut builder = TestGraphBuilder::new();
    let condition = builder.add_literal(Literal::Boolean(true));
    let then_branch = builder.add_literal(Literal::Integer(10));
    let else_branch = builder.add_literal(Literal::Integer(20));
    let if_node = builder.add_if(condition, then_branch, else_branch);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(if_node);
    assert_eq!(result.unwrap(), Value::Integer(10));
}

#[test]
fn test_evaluate_if_false() {
    let mut builder = TestGraphBuilder::new();
    let condition = builder.add_literal(Literal::Boolean(false));
    let then_branch = builder.add_literal(Literal::Integer(10));
    let else_branch = builder.add_literal(Literal::Integer(20));
    let if_node = builder.add_if(condition, then_branch, else_branch);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(if_node);
    assert_eq!(result.unwrap(), Value::Integer(20));
}

// ===== Error Cases =====

#[test]
fn test_evaluate_invalid_node_id() {
    let graph = Graph::new();
    let evaluator = ConditionEvaluator::new(&graph);

    let invalid_id = NodeId::new(9999).unwrap();
    let result = evaluator.evaluate(invalid_id);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Invalid node ID"));
}

#[test]
fn test_evaluate_condition_non_boolean() {
    let mut builder = TestGraphBuilder::new();
    let node = builder.add_literal(Literal::Integer(42));
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate_condition(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("must evaluate to boolean"));
}

#[test]
fn test_unknown_predicate() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::Integer(42));
    let node = builder.add_application("unknown-predicate", vec![arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Unknown predicate"));
}

#[test]
fn test_wrong_number_of_args() {
    let mut builder = TestGraphBuilder::new();
    let arg = builder.add_literal(Literal::Integer(42));
    // = requires exactly 2 arguments
    let node = builder.add_application("=", vec![arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("requires exactly 2 arguments"));
}

#[test]
fn test_type_mismatch_in_operation() {
    let mut builder = TestGraphBuilder::new();
    let string_arg = builder.add_literal(Literal::String("hello".to_string()));
    let int_arg = builder.add_literal(Literal::Integer(42));
    let node = builder.add_application("+", vec![string_arg, int_arg]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("requires numeric arguments"));
}

// ===== Complex Expression Tests =====

#[test]
fn test_nested_expressions() {
    let mut builder = TestGraphBuilder::new();

    // (+ 1 (* 2 3))
    let one = builder.add_literal(Literal::Integer(1));
    let two = builder.add_literal(Literal::Integer(2));
    let three = builder.add_literal(Literal::Integer(3));
    let multiply = builder.add_application("*", vec![two, three]);
    let add = builder.add_application("+", vec![one, multiply]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(add);
    assert_eq!(result.unwrap(), Value::Integer(7));
}

#[test]
fn test_complex_condition() {
    let mut builder = TestGraphBuilder::new();

    // (and (> x 0) (<= x 100))
    let x_var = builder.add_variable("x");
    let zero = builder.add_literal(Literal::Integer(0));
    let hundred = builder.add_literal(Literal::Integer(100));

    let greater_than_zero = builder.add_application(">", vec![x_var, zero]);
    let less_equal_hundred = builder.add_application("<=", vec![x_var, hundred]);
    let and_node = builder.add_application("and", vec![greater_than_zero, less_equal_hundred]);
    let graph = builder.build();

    let mut evaluator = ConditionEvaluator::new(&graph);
    evaluator.bind("x".to_string(), Value::Integer(50));

    let result = evaluator.evaluate(and_node);
    assert_eq!(result.unwrap(), Value::Boolean(true));

    // Test with x = 150
    evaluator.bind("x".to_string(), Value::Integer(150));
    let result = evaluator.evaluate(and_node);
    assert_eq!(result.unwrap(), Value::Boolean(false));
}

// ===== Helper Method Tests =====

#[test]
fn test_values_equal() {
    let mut builder = TestGraphBuilder::new();
    let list1_elements = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
    ];
    let list2_elements = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(2)),
    ];
    let list3_elements = vec![
        builder.add_literal(Literal::Integer(1)),
        builder.add_literal(Literal::Integer(3)),
    ];

    let list1 = builder.add_list(list1_elements);
    let list2 = builder.add_list(list2_elements);
    let list3 = builder.add_list(list3_elements);

    let eq1 = builder.add_application("=", vec![list1, list2]);
    let eq2 = builder.add_application("=", vec![list1, list3]);
    let graph = builder.build();

    let evaluator = ConditionEvaluator::new(&graph);

    assert_eq!(evaluator.evaluate(eq1).unwrap(), Value::Boolean(true));
    assert_eq!(evaluator.evaluate(eq2).unwrap(), Value::Boolean(false));
}

#[test]
fn test_unsupported_node_type() {
    let mut graph = Graph::new();
    // Lambda is not supported in contract conditions
    let node = graph
        .add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: NodeId::new(1).unwrap(),
        })
        .expect("Failed to add lambda node");

    let evaluator = ConditionEvaluator::new(&graph);
    let result = evaluator.evaluate(node);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Unsupported node type"));
}
