use fluentai_core::ast::{Literal, Node};
use fluentai_parser::flc_parser::Parser;

#[test]
fn test_assignment_in_expression_context() {
    // Test assignment in let binding
    let input = "let x = (y = 42); x";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // Verify the structure
    assert!(graph.root_id.is_some());
}

#[test]
fn test_chained_assignments() {
    // Test right-associativity of assignments
    let input = "a = b = c = 5";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // The AST should be: Assignment(a, Assignment(b, Assignment(c, 5)))
    if let Some(root_id) = graph.root_id {
        if let Some(Node::Assignment { target, value }) = graph.nodes.get(&root_id) {
            // Check that 'a' is the target
            if let Some(Node::Variable { name }) = graph.nodes.get(target) {
                assert_eq!(name, "a");
            } else {
                panic!("Expected variable 'a' as target");
            }

            // Check that value is another assignment
            if let Some(Node::Assignment {
                target: b_target,
                value: b_value,
            }) = graph.nodes.get(value)
            {
                if let Some(Node::Variable { name }) = graph.nodes.get(b_target) {
                    assert_eq!(name, "b");
                } else {
                    panic!("Expected variable 'b' as second target");
                }

                // Check the innermost assignment
                if let Some(Node::Assignment {
                    target: c_target,
                    value: c_value,
                }) = graph.nodes.get(b_value)
                {
                    if let Some(Node::Variable { name }) = graph.nodes.get(c_target) {
                        assert_eq!(name, "c");
                    } else {
                        panic!("Expected variable 'c' as third target");
                    }

                    if let Some(Node::Literal(Literal::Integer(5))) = graph.nodes.get(c_value) {
                        // Success!
                    } else {
                        panic!("Expected literal 5 as final value");
                    }
                } else {
                    panic!("Expected third assignment for 'c'");
                }
            } else {
                panic!("Expected second assignment for 'b'");
            }
        } else {
            panic!("Expected assignment at root");
        }
    }
}

#[test]
fn test_assignment_in_function_call() {
    let input = "foo(x = 10, y = 20)";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // Should parse successfully with assignments as arguments
    assert!(graph.root_id.is_some());
}

#[test]
fn test_assignment_in_array() {
    let input = "[a = 1, b = 2, c = 3]";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // Should parse successfully with assignments in array
    assert!(graph.root_id.is_some());
}

#[test]
fn test_assignment_in_conditional() {
    let input = "if (flag = true) { flag } else { false }";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // Should parse successfully with assignment in condition
    assert!(graph.root_id.is_some());
}

#[test]
fn test_assignment_with_arithmetic() {
    let input = "result = x + y * (z = 3)";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // Should parse successfully with assignment in arithmetic expression
    assert!(graph.root_id.is_some());
}

#[test]
fn test_assignment_precedence() {
    // Assignment should have lower precedence than pipe
    let input = "x = data |> transform";
    let parser = Parser::new(input);
    let graph = parser.parse().expect("Failed to parse");

    // The assignment should be at the root, with the pipe expression as its value
    if let Some(root_id) = graph.root_id {
        if let Some(Node::Assignment { target, value }) = graph.nodes.get(&root_id) {
            // Check that target is 'x'
            if let Some(Node::Variable { name }) = graph.nodes.get(target) {
                assert_eq!(name, "x");
            }
            // The value should be the pipe expression (Application node)
            assert!(matches!(
                graph.nodes.get(value),
                Some(Node::Application { .. })
            ));
        } else {
            panic!("Expected assignment at root");
        }
    }
}
