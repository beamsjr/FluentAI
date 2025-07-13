//! Parser tests for multiple top-level expressions

use fluentai_core::ast::{Graph, Node};

#[test]
fn test_parse_multiple_expressions() {
    let cases = vec![
        // Two expressions
        ("1 2", 2),
        // Three expressions
        ("1 2 3", 3),
        // Mixed types
        (r#"42 "hello" #t"#, 3),
        // With whitespace
        (
            r#"
            1
            
            2
            
            3
        "#,
            3,
        ),
        // Complex expressions
        ("(+ 1 2) (* 3 4) (- 10 5)", 3),
    ];

    for (code, expected_count) in cases {
        let graph = parse(code).unwrap();
        let root = graph.root_id.unwrap();
        let root_node = graph.get_node(root).unwrap();

        match root_node {
            Node::Begin { exprs } => {
                assert_eq!(
                    exprs.len(),
                    expected_count,
                    "Wrong expression count for: {}",
                    code
                );
            }
            _ => panic!("Expected Begin node for multiple expressions in: {}", code),
        }
    }
}

#[test]
fn test_parse_single_expression_not_wrapped() {
    let cases = vec![
        "42",
        r#""hello""#,
        "(+ 1 2 3)",
        "(lambda (x) (* x x))",
        "(let ((x 10)) x)",
    ];

    for code in cases {
        let graph = parse(code).unwrap();
        let root = graph.root_id.unwrap();
        let root_node = graph.get_node(root).unwrap();

        // Single expressions should NOT be wrapped in Begin
        assert!(
            !matches!(root_node, Node::Begin { .. }),
            "Single expression should not be wrapped in Begin: {}",
            code
        );
    }
}

#[test]
fn test_parse_empty_file() {
    let graph = parse("").unwrap();
    assert!(
        graph.root_id.is_none() || {
            if let Some(root) = graph.root_id {
                matches!(graph.get_node(root).unwrap(), Node::Begin { exprs } if exprs.is_empty())
            } else {
                true
            }
        }
    );
}

#[test]
fn test_parse_with_comments_between_expressions() {
    let code = r#"
        1  ; first expression
        ; comment line
        2  ; second expression
        ;; another comment
        3  ; third expression
    "#;

    let graph = parse(code).unwrap();
    let root = graph.root_id.unwrap();
    let root_node = graph.get_node(root).unwrap();

    match root_node {
        Node::Begin { exprs } => {
            assert_eq!(exprs.len(), 3);
        }
        _ => panic!("Expected Begin node"),
    }
}

#[test]
fn test_begin_node_children() {
    let code = "(+ 1 2) (* 3 4) (- 10 5)";
    let graph = parse(code).unwrap();
    let root = graph.root_id.unwrap();

    // Verify children() method works correctly
    let children = graph.children(root);
    assert_eq!(children.len(), 3);

    // Verify each child is an Application node
    for child_id in children {
        let child = graph.get_node(child_id).unwrap();
        assert!(matches!(child, Node::Application { .. }));
    }
}

#[test]
fn test_begin_with_nested_structures() {
    let code = r#"
        (let ((x 1)) x)
        (if #t 
            (let ((y 2)) y)
            (let ((z 3)) z))
        (list 1 2 3)
    "#;

    let graph = parse(code).unwrap();
    let root = graph.root_id.unwrap();
    let root_node = graph.get_node(root).unwrap();

    match root_node {
        Node::Begin { exprs } => {
            assert_eq!(exprs.len(), 3);

            // First should be Let
            assert!(matches!(
                graph.get_node(exprs[0]).unwrap(),
                Node::Let { .. }
            ));

            // Second should be If
            assert!(matches!(graph.get_node(exprs[1]).unwrap(), Node::If { .. }));

            // Third should be Application (list)
            assert!(matches!(
                graph.get_node(exprs[2]).unwrap(),
                Node::Application { .. }
            ));
        }
        _ => panic!("Expected Begin node"),
    }
}

#[test]
fn test_parser_state_after_multiple_expressions() {
    let code = "1 2 3";
    let graph = parse(code).unwrap();

    // Verify parser state is clean after parsing
    assert!(graph.root_id.is_some());
    let root = graph.root_id.unwrap();
    assert!(matches!(graph.get_node(root).unwrap(), Node::Begin { .. }));
}

#[test]
fn test_expression_ordering_preserved() {
    let code = r#"
        "first"
        "second"
        "third"
    "#;

    let graph = parse(code).unwrap();
    let root = graph.root_id.unwrap();

    match graph.get_node(root).unwrap() {
        Node::Begin { exprs } => {
            assert_eq!(exprs.len(), 3);

            // Check each expression in order
            for (i, expr_id) in exprs.iter().enumerate() {
                match graph.get_node(*expr_id).unwrap() {
                    Node::Literal(lit) => {
                        let expected = match i {
                            0 => "first",
                            1 => "second",
                            2 => "third",
                            _ => panic!("Unexpected index"),
                        };
                        assert_eq!(lit.to_string(), format!(r#""{}""#, expected));
                    }
                    _ => panic!("Expected literal"),
                }
            }
        }
        _ => panic!("Expected Begin node"),
    }
}
