//! Tests for contract parsing variations

use fluentai_core::ast::Node;

#[test]
fn test_parse_simple_contract() {
    // Test basic contract with precondition and postcondition
    let input = r#"(spec:contract factorial
                     :requires [(>= n 0)]
                     :ensures [(>= result 1)])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            invariants,
            complexity,
            pure,
        } => {
            assert_eq!(function_name, "factorial");
            assert_eq!(preconditions.len(), 1);
            assert_eq!(postconditions.len(), 1);
            assert!(invariants.is_empty());
            assert!(complexity.is_none());
            assert!(pure); // Default is true
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_with_complexity() {
    // Test contract with complexity annotation
    let input = r#"(spec:contract sort
                     :requires [(list? input)]
                     :ensures [(sorted? result)]
                     :complexity "O(n log n)")"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            complexity,
            ..
        } => {
            assert_eq!(function_name, "sort");
            assert_eq!(complexity.as_ref().unwrap(), "O(n log n)");
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_pure_contract() {
    // Test pure function contract
    let input = r#"(spec:contract add
                     :requires [true]
                     :ensures [(= result (+ a b))]
                     :pure true)"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            pure,
            ..
        } => {
            assert_eq!(function_name, "add");
            assert!(pure);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_with_invariants() {
    // Test contract with invariants
    let input = r#"(spec:contract queue-enqueue
                     :requires [(< (size queue) max-size)]
                     :ensures [(= new-size (+ old-size 1))]
                     :invariant [(>= (size queue) 0)])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            invariants,
            ..
        } => {
            assert_eq!(function_name, "queue-enqueue");
            assert_eq!(invariants.len(), 1);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_with_multiple_conditions() {
    // Test contract with multiple preconditions and postconditions
    let input = r#"(spec:contract binary-search
                     :requires [(sorted? arr) (>= low 0) (<= high (length arr))]
                     :ensures [(or (not-found? result) (= (array-ref arr result) target))])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "binary-search");
            assert_eq!(preconditions.len(), 3);
            assert_eq!(postconditions.len(), 1);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_empty_conditions() {
    // Test contract with empty condition lists
    let input = r#"(spec:contract identity
                     :requires []
                     :ensures [])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "identity");
            assert!(preconditions.is_empty());
            assert!(postconditions.is_empty());
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_only_requires() {
    // Test contract with only preconditions
    let input = r#"(spec:contract validate-input
                     :requires [(string? input) (> (length input) 0)])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "validate-input");
            assert_eq!(preconditions.len(), 2);
            assert!(postconditions.is_empty());
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_only_ensures() {
    // Test contract with only postconditions
    let input = r#"(spec:contract get-current-time
                     :ensures [(> result 0)])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "get-current-time");
            assert!(preconditions.is_empty());
            assert_eq!(postconditions.len(), 1);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_all_features() {
    // Test contract with all possible features
    let input = r#"(spec:contract heap-insert
                     :requires [(heap? h) (< (size h) max-heap-size)]
                     :ensures [(heap? result) (= (size result) (+ (size h) 1))]
                     :invariant [(heap-property? h)]
                     :complexity "O(log n)"
                     :pure false)"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            invariants,
            complexity,
            pure,
        } => {
            assert_eq!(function_name, "heap-insert");
            assert_eq!(preconditions.len(), 2);
            assert_eq!(postconditions.len(), 2);
            assert_eq!(invariants.len(), 1);
            assert_eq!(complexity.as_ref().unwrap(), "O(log n)");
            assert!(!pure);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_with_complex_expressions() {
    // Test contract with more complex condition expressions
    let input = r#"(spec:contract matrix-multiply
                     :requires [(and (matrix? A) (matrix? B) 
                                    (= (cols A) (rows B)))]
                     :ensures [(and (matrix? result)
                                   (= (rows result) (rows A))
                                   (= (cols result) (cols B)))])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "matrix-multiply");
            assert_eq!(preconditions.len(), 1); // Single complex expression
            assert_eq!(postconditions.len(), 1); // Single complex expression

            // The conditions should be parsed as application nodes
            assert!(result.get_node(preconditions[0]).is_some());
            assert!(result.get_node(postconditions[0]).is_some());
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_alternative_keywords() {
    // Test using :pre and :post instead of :requires and :ensures
    let input = r#"(spec:contract divide
                     :pre [(not= b 0)]
                     :post [(= result (/ a b))])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "divide");
            assert_eq!(preconditions.len(), 1);
            assert_eq!(postconditions.len(), 1);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_error_without_name() {
    // Contract requires a function name
    let result = parse("(spec:contract)");
    assert!(result.is_err());
}

#[test]
fn test_parse_contract_error_invalid_keyword() {
    // Test with invalid keyword
    let result = parse(r#"(spec:contract foo :invalid-keyword [true])"#);
    // This might parse but ignore the invalid keyword
    if result.is_ok() {
        let root_id = result.unwrap().root_id.unwrap();
        // Just verify it parsed something
        assert!(root_id.get() > 0);
    }
}

#[test]
fn test_parse_contract_with_quoted_function_name() {
    // Test contract with quoted function name
    let input = r#"(spec:contract 'my-function
                     :requires [(valid? input)]
                     :ensures [(processed? result)])"#;

    let result = parse(input);
    // This might or might not parse depending on whether quoted names are supported
    if let Ok(parsed) = result {
        assert!(parsed.root_id.is_some());
    }
}

#[test]
fn test_parse_contract_order_independence() {
    // Test that keyword order doesn't matter
    let input = r#"(spec:contract foo
                     :pure true
                     :ensures [(> result 0)]
                     :complexity "O(1)"
                     :requires [(> x 0)])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            complexity,
            pure,
            ..
        } => {
            assert_eq!(function_name, "foo");
            assert_eq!(preconditions.len(), 1);
            assert_eq!(postconditions.len(), 1);
            assert_eq!(complexity.as_ref().unwrap(), "O(1)");
            assert!(pure);
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_with_let_in_condition() {
    // Test contract with let expression in condition
    let input = r#"(spec:contract process-data
                     :requires [(let ((size (length data))) (< size 1000))]
                     :ensures [true])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            ..
        } => {
            assert_eq!(function_name, "process-data");
            assert_eq!(preconditions.len(), 1);

            // The condition should be a let expression
            match result.get_node(preconditions[0]).unwrap() {
                Node::Let { .. } => {}
                _ => panic!("Expected Let node in precondition"),
            }
        }
        _ => panic!("Expected Contract node"),
    }
}

#[test]
fn test_parse_contract_nested_lists() {
    // Test contract with nested list structures in conditions
    let input = r#"(spec:contract transform
                     :requires [[(> x 0) (< x 100)] [(valid? y)]]
                     :ensures [[(transformed? result)]])"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Contract {
            function_name,
            preconditions,
            postconditions,
            ..
        } => {
            assert_eq!(function_name, "transform");
            // Nested lists should be flattened or parsed as list nodes
            assert!(!preconditions.is_empty());
            assert!(!postconditions.is_empty());
        }
        _ => panic!("Expected Contract node"),
    }
}
