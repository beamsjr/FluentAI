//! Test for double negation parsing

use fluentai_parser::flc_parser::Parser;
use fluentai_core::ast::{Graph, Node, Literal};

#[test]
fn test_double_negation_parsing() {
    // Test simple negation
    let expr = "-5";
    let parser = Parser::new(expr);
    let graph = parser.parse().unwrap();
    
    // Get the root node
    let root_id = graph.root_id.expect("Expected root node");
    let root_node = graph.get_node(root_id).expect("Root node should exist");
    
    // Should be parsed as neg(5), not Integer(-5)
    match root_node {
        Node::Application { function, args } => {
            let func_node = graph.get_node(*function).expect("Function node should exist");
            match func_node {
                Node::Variable { name } => assert_eq!(name, "-"),
                _ => panic!("Expected - function, got {:?}", func_node),
            }
            assert_eq!(args.len(), 1);
            let arg_node = graph.get_node(args[0]).expect("Argument node should exist");
            match arg_node {
                Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 5),
                _ => panic!("Expected Integer(5), got {:?}", arg_node),
            }
        }
        _ => panic!("Expected Application node for negation, got {:?}", root_node),
    }
    
    // Test double negation
    let expr = "-(-5)";
    let parser = Parser::new(expr);
    let graph = parser.parse().unwrap();
    
    // Get the root node
    let root_id = graph.root_id.expect("Expected root node");
    let root_node = graph.get_node(root_id).expect("Root node should exist");
    
    // Should be parsed as neg(neg(5))
    match root_node {
        Node::Application { function, args } => {
            // Outer neg
            let func_node = graph.get_node(*function).expect("Function node should exist");
            match func_node {
                Node::Variable { name } => assert_eq!(name, "-"),
                _ => panic!("Expected outer neg function, got {:?}", func_node),
            }
            assert_eq!(args.len(), 1);
            
            // Inner neg(5)
            let inner_node = graph.get_node(args[0]).expect("Inner node should exist");
            match inner_node {
                Node::Application { function: inner_func, args: inner_args } => {
                    let inner_func_node = graph.get_node(*inner_func).expect("Inner function node should exist");
                    match inner_func_node {
                        Node::Variable { name } => assert_eq!(name, "-"),
                        _ => panic!("Expected inner neg function, got {:?}", inner_func_node),
                    }
                    assert_eq!(inner_args.len(), 1);
                    let inner_arg_node = graph.get_node(inner_args[0]).expect("Inner argument node should exist");
                    match inner_arg_node {
                        Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 5),
                        _ => panic!("Expected Integer(5), got {:?}", inner_arg_node),
                    }
                }
                _ => panic!("Expected inner Application for neg(5), got {:?}", inner_node),
            }
        }
        _ => panic!("Expected Application node for double negation, got {:?}", root_node),
    }
}

#[test]
fn test_negative_float_parsing() {
    // Test negative float
    let expr = "-3.14";
    let parser = Parser::new(expr);
    let graph = parser.parse().unwrap();
    
    // Get the root node
    let root_id = graph.root_id.expect("Expected root node");
    let root_node = graph.get_node(root_id).expect("Root node should exist");
    
    // Should be parsed as neg(3.14)
    match root_node {
        Node::Application { function, args } => {
            let func_node = graph.get_node(*function).expect("Function node should exist");
            match func_node {
                Node::Variable { name } => assert_eq!(name, "-"),
                _ => panic!("Expected - function, got {:?}", func_node),
            }
            assert_eq!(args.len(), 1);
            let arg_node = graph.get_node(args[0]).expect("Argument node should exist");
            match arg_node {
                Node::Literal(Literal::Float(f)) => assert!((f - 3.14).abs() < 0.001),
                _ => panic!("Expected Float(3.14), got {:?}", arg_node),
            }
        }
        _ => panic!("Expected Application node for float negation, got {:?}", root_node),
    }
}

#[test]
fn test_negative_numbers_in_expressions() {
    // Test that negative numbers work correctly in arithmetic expressions
    let expr = "5 + -3";
    let parser = Parser::new(expr);
    let graph = parser.parse().unwrap();
    
    // Get the root node
    let root_id = graph.root_id.expect("Expected root node");
    let root_node = graph.get_node(root_id).expect("Root node should exist");
    
    // Should be parsed as add(5, neg(3))
    match root_node {
        Node::Application { function, args } => {
            let func_node = graph.get_node(*function).expect("Function node should exist");
            match func_node {
                Node::Variable { name } => assert_eq!(name, "+"),
                _ => panic!("Expected add function, got {:?}", func_node),
            }
            assert_eq!(args.len(), 2);
            
            // First argument should be 5
            let first_arg = graph.get_node(args[0]).expect("First argument should exist");
            match first_arg {
                Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 5),
                _ => panic!("Expected Integer(5), got {:?}", first_arg),
            }
            
            // Second argument should be neg(3)
            let second_arg = graph.get_node(args[1]).expect("Second argument should exist");
            match second_arg {
                Node::Application { function: neg_func, args: neg_args } => {
                    let neg_func_node = graph.get_node(*neg_func).expect("Neg function node should exist");
                    match neg_func_node {
                        Node::Variable { name } => assert_eq!(name, "-"),
                        _ => panic!("Expected - function, got {:?}", neg_func_node),
                    }
                    assert_eq!(neg_args.len(), 1);
                    let neg_arg = graph.get_node(neg_args[0]).expect("Neg argument should exist");
                    match neg_arg {
                        Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 3),
                        _ => panic!("Expected Integer(3), got {:?}", neg_arg),
                    }
                }
                _ => panic!("Expected neg(3), got {:?}", second_arg),
            }
        }
        _ => panic!("Expected Application node for addition, got {:?}", root_node),
    }
}

#[test]
fn test_parenthesized_negative_numbers() {
    // Test that parentheses work correctly with negative numbers
    let expr = "(-5) * 2";
    let parser = Parser::new(expr);
    let graph = parser.parse().unwrap();
    
    // Get the root node
    let root_id = graph.root_id.expect("Expected root node");
    let root_node = graph.get_node(root_id).expect("Root node should exist");
    
    // Should be parsed as mul(neg(5), 2)
    match root_node {
        Node::Application { function, args } => {
            let func_node = graph.get_node(*function).expect("Function node should exist");
            match func_node {
                Node::Variable { name } => assert_eq!(name, "*"),
                _ => panic!("Expected mul function, got {:?}", func_node),
            }
            assert_eq!(args.len(), 2);
            
            // First argument should be neg(5)
            let first_arg = graph.get_node(args[0]).expect("First argument should exist");
            match first_arg {
                Node::Application { function: neg_func, args: neg_args } => {
                    let neg_func_node = graph.get_node(*neg_func).expect("Neg function node should exist");
                    match neg_func_node {
                        Node::Variable { name } => assert_eq!(name, "-"),
                        _ => panic!("Expected - function, got {:?}", neg_func_node),
                    }
                    assert_eq!(neg_args.len(), 1);
                    let neg_arg = graph.get_node(neg_args[0]).expect("Neg argument should exist");
                    match neg_arg {
                        Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 5),
                        _ => panic!("Expected Integer(5), got {:?}", neg_arg),
                    }
                }
                _ => panic!("Expected neg(5), got {:?}", first_arg),
            }
            
            // Second argument should be 2
            let second_arg = graph.get_node(args[1]).expect("Second argument should exist");
            match second_arg {
                Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 2),
                _ => panic!("Expected Integer(2), got {:?}", second_arg),
            }
        }
        _ => panic!("Expected Application node for multiplication, got {:?}", root_node),
    }
}