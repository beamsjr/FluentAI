//! Tests for purity checking

use fluentai_contracts::{PurityChecker, Contract, ContractCondition, ContractKind};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::num::NonZeroU32;

/// Helper to create a simple graph with test nodes
fn create_test_graph() -> Graph {
    let mut graph = Graph::new();
    
    // Add some test nodes
    let lit_1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let lit_2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    
    // Pure arithmetic: (+ 1 2)
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let pure_add = graph.add_node(Node::Application {
        function: plus,
        args: vec![lit_1, lit_2],
    });
    
    // Impure print: (print "hello")
    let print_fn = graph.add_node(Node::Variable { name: "print".to_string() });
    let hello = graph.add_node(Node::Literal(Literal::String("hello".to_string())));
    let impure_print = graph.add_node(Node::Application {
        function: print_fn,
        args: vec![hello],
    });
    
    // Pure lambda: (lambda (x) x)
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: x_var,
    });
    
    // Store node IDs for testing
    graph.root_id = Some(pure_add); // Arbitrary choice
    
    graph
}

#[test]
fn test_literal_is_pure() {
    let graph = create_test_graph();
    let mut checker = PurityChecker::new(&graph);
    
    let lit = graph.add_node(Node::Literal(Literal::Integer(42)));
    assert!(checker.is_pure(lit).unwrap());
}

#[test]
fn test_variable_is_pure() {
    let graph = create_test_graph();
    let mut checker = PurityChecker::new(&graph);
    
    let var = graph.add_node(Node::Variable { name: "x".to_string() });
    assert!(checker.is_pure(var).unwrap());
}

#[test]
fn test_pure_arithmetic_is_pure() {
    let mut graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    
    let lit_1 = graph.add_node(Node::Literal(Literal::Integer(1)));
    let lit_2 = graph.add_node(Node::Literal(Literal::Integer(2)));
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let add_expr = graph.add_node(Node::Application {
        function: plus,
        args: vec![lit_1, lit_2],
    });
    
    assert!(checker.is_pure(add_expr).unwrap());
}

#[test]
fn test_print_is_impure() {
    let mut graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();
    
    let print_fn = graph.add_node(Node::Variable { name: "print".to_string() });
    let msg = graph.add_node(Node::Literal(Literal::String("hello".to_string())));
    let print_expr = graph.add_node(Node::Application {
        function: print_fn,
        args: vec![msg],
    });
    
    assert!(!checker.is_pure(print_expr).unwrap());
}

#[test]
fn test_lambda_definition_is_pure() {
    let mut graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let lambda = graph.add_node(Node::Lambda {
        params: vec!["x".to_string()],
        body: x_var,
    });
    
    assert!(checker.is_pure(lambda).unwrap());
}

#[test]
fn test_if_expression_purity() {
    let mut graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    
    // Pure if: (if true 1 2)
    let cond = graph.add_node(Node::Literal(Literal::Boolean(true)));
    let then_branch = graph.add_node(Node::Literal(Literal::Integer(1)));
    let else_branch = graph.add_node(Node::Literal(Literal::Integer(2)));
    let pure_if = graph.add_node(Node::If {
        condition: cond,
        then_branch,
        else_branch,
    });
    
    assert!(checker.is_pure(pure_if).unwrap());
    
    // Impure if: (if true (print "yes") 2)
    checker.register_impure_builtins();
    let print_fn = graph.add_node(Node::Variable { name: "print".to_string() });
    let msg = graph.add_node(Node::Literal(Literal::String("yes".to_string())));
    let impure_then = graph.add_node(Node::Application {
        function: print_fn,
        args: vec![msg],
    });
    let impure_if = graph.add_node(Node::If {
        condition: cond,
        then_branch: impure_then,
        else_branch,
    });
    
    assert!(!checker.is_pure(impure_if).unwrap());
}

#[test]
fn test_contract_purity_validation() {
    let mut graph = Graph::new();
    let mut checker = PurityChecker::new(&graph);
    checker.register_impure_builtins();
    
    // Create a contract with pure precondition
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let gt = graph.add_node(Node::Variable { name: ">".to_string() });
    let pure_condition = graph.add_node(Node::Application {
        function: gt,
        args: vec![x_var, zero],
    });
    
    let mut contract = Contract::new("test_func".to_string(), NodeId::new(NonZeroU32::new(1).unwrap()));
    contract.add_precondition(ContractCondition::new(pure_condition, ContractKind::Precondition));
    
    // Should pass validation
    assert!(checker.validate_contract_purity(&contract).is_ok());
    
    // Create a contract with impure postcondition
    let print_fn = graph.add_node(Node::Variable { name: "print".to_string() });
    let msg = graph.add_node(Node::Literal(Literal::String("done".to_string())));
    let impure_condition = graph.add_node(Node::Application {
        function: print_fn,
        args: vec![msg],
    });
    
    contract.add_postcondition(ContractCondition::new(impure_condition, ContractKind::Postcondition));
    
    // Should fail validation
    assert!(checker.validate_contract_purity(&contract).is_err());
}