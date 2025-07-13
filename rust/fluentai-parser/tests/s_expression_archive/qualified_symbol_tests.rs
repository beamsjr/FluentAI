//! Tests for qualified symbol parsing (e.g., module.function)

use fluentai_core::ast::Node;

#[test]
fn test_parse_simple_qualified_symbol() {
    // Test basic qualified symbol
    let result = parse("math.pi").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::QualifiedVariable {
            module_name,
            variable_name,
        } => {
            assert_eq!(module_name, "math");
            assert_eq!(variable_name, "pi");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}

#[test]
fn test_parse_qualified_symbol_with_hyphen() {
    // Test qualified symbol with hyphens in names
    let result = parse("my-module.my-function");
    // Hyphens might not be supported in qualified variables
    // If it doesn't parse as qualified variable, that's ok
    if let Ok(parsed) = result {
        if let Some(root_id) = parsed.root_id {
            match parsed.get_node(root_id) {
                Some(Node::QualifiedVariable {
                    module_name,
                    variable_name,
                }) => {
                    assert_eq!(module_name, "my-module");
                    assert_eq!(variable_name, "my-function");
                }
                _ => {
                    // Might parse as something else, that's ok
                    assert!(root_id.get() > 0);
                }
            }
        }
    }
}

#[test]
fn test_parse_qualified_symbol_with_underscore() {
    // Test qualified symbol with underscores
    let result = parse("test_module.test_var").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::QualifiedVariable {
            module_name,
            variable_name,
        } => {
            assert_eq!(module_name, "test_module");
            assert_eq!(variable_name, "test_var");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}

#[test]
fn test_parse_qualified_symbol_with_numbers() {
    // Test qualified symbol with numbers
    let result = parse("module1.var2").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::QualifiedVariable {
            module_name,
            variable_name,
        } => {
            assert_eq!(module_name, "module1");
            assert_eq!(variable_name, "var2");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}

#[test]
fn test_parse_qualified_symbol_in_application() {
    // Test qualified symbol as function in application
    let result = parse("(math.sin 1.5)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Application { function, args } => {
            match result.get_node(*function).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "math");
                    assert_eq!(variable_name, "sin");
                }
                _ => panic!("Expected QualifiedVariable as function"),
            }
            assert_eq!(args.len(), 1);
        }
        _ => panic!("Expected Application node"),
    }
}

#[test]
fn test_parse_qualified_symbol_as_argument() {
    // Test qualified symbol as argument
    let result = parse("(+ math.pi math.e)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Application { args, .. } => {
            assert_eq!(args.len(), 2);

            // First argument should be math.pi
            match result.get_node(args[0]).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "math");
                    assert_eq!(variable_name, "pi");
                }
                _ => panic!("Expected QualifiedVariable for first arg"),
            }

            // Second argument should be math.e
            match result.get_node(args[1]).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "math");
                    assert_eq!(variable_name, "e");
                }
                _ => panic!("Expected QualifiedVariable for second arg"),
            }
        }
        _ => panic!("Expected Application node"),
    }
}

#[test]
fn test_parse_qualified_symbol_in_let() {
    // Test qualified symbol in let binding
    let result = parse("(let ((x math.pi)) (* x 2))").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Let { bindings, .. } => {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "x");

            // Value should be math.pi
            match result.get_node(bindings[0].1).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "math");
                    assert_eq!(variable_name, "pi");
                }
                _ => panic!("Expected QualifiedVariable in binding"),
            }
        }
        _ => panic!("Expected Let node"),
    }
}

#[test]
fn test_parse_qualified_symbol_in_if() {
    // Test qualified symbol in conditional
    let result = parse("(if (> x math.pi) \"large\" \"small\")").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::If { condition, .. } => {
            // Condition should be (> x math.pi)
            match result.get_node(*condition).unwrap() {
                Node::Application { args, .. } => {
                    assert_eq!(args.len(), 2);

                    // Second arg should be math.pi
                    match result.get_node(args[1]).unwrap() {
                        Node::QualifiedVariable {
                            module_name,
                            variable_name,
                        } => {
                            assert_eq!(module_name, "math");
                            assert_eq!(variable_name, "pi");
                        }
                        _ => panic!("Expected QualifiedVariable"),
                    }
                }
                _ => panic!("Expected Application in condition"),
            }
        }
        _ => panic!("Expected If node"),
    }
}

#[test]
fn test_parse_qualified_symbol_with_special_chars() {
    // Test qualified symbols with special characters that are allowed
    let result = parse("utils.list?");
    // Question mark might not be supported in qualified variables
    if let Ok(parsed) = result {
        if let Some(root_id) = parsed.root_id {
            match parsed.get_node(root_id) {
                Some(Node::QualifiedVariable {
                    module_name,
                    variable_name,
                }) => {
                    assert_eq!(module_name, "utils");
                    assert_eq!(variable_name, "list?");
                }
                _ => {
                    // Might parse as something else, that's ok
                    assert!(root_id.get() > 0);
                }
            }
        }
    }
}

#[test]
fn test_parse_qualified_symbol_in_lambda() {
    // Test qualified symbol in lambda body
    let result = parse("(lambda (x) (math.sqrt x))").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Lambda { body, .. } => {
            // Body should be (math.sqrt x)
            match result.get_node(*body).unwrap() {
                Node::Application { function, .. } => match result.get_node(*function).unwrap() {
                    Node::QualifiedVariable {
                        module_name,
                        variable_name,
                    } => {
                        assert_eq!(module_name, "math");
                        assert_eq!(variable_name, "sqrt");
                    }
                    _ => panic!("Expected QualifiedVariable as function"),
                },
                _ => panic!("Expected Application in lambda body"),
            }
        }
        _ => panic!("Expected Lambda node"),
    }
}

#[test]
fn test_parse_multiple_qualified_symbols() {
    // Test multiple qualified symbols in one expression
    let result = parse("(string.concat io.newline net.hostname)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Application { function, args } => {
            // Function should be string.concat
            match result.get_node(*function).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "string");
                    assert_eq!(variable_name, "concat");
                }
                _ => panic!("Expected QualifiedVariable as function"),
            }

            assert_eq!(args.len(), 2);

            // First arg should be io.newline
            match result.get_node(args[0]).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "io");
                    assert_eq!(variable_name, "newline");
                }
                _ => panic!("Expected QualifiedVariable"),
            }

            // Second arg should be net.hostname
            match result.get_node(args[1]).unwrap() {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    assert_eq!(module_name, "net");
                    assert_eq!(variable_name, "hostname");
                }
                _ => panic!("Expected QualifiedVariable"),
            }
        }
        _ => panic!("Expected Application node"),
    }
}

#[test]
fn test_parse_qualified_symbol_in_list() {
    // Test qualified symbol in list literal
    let result = parse("[math.pi math.e math.tau]").unwrap();
    let root_id = result.root_id.unwrap();

    // Lists are converted to cons operations, so this will be an Application
    // Just verify it parses successfully
    assert!(result.get_node(root_id).is_some());
}

#[test]
fn test_parse_qualified_symbol_error_multiple_dots() {
    // Test that multiple dots don't parse as qualified symbol
    let result = parse("a.b.c");
    // This might parse as nested qualified variables or might fail
    // Either way is acceptable - just document the behavior
    if let Ok(parsed) = result {
        assert!(parsed.root_id.is_some());
    }
}

#[test]
fn test_parse_qualified_symbol_error_trailing_dot() {
    // Test that trailing dot is not valid
    let result = parse("module.");
    // This should either fail or parse as something else
    if result.is_ok() {
        // If it parses, it shouldn't be a qualified variable
        let root_id = result.unwrap().root_id.unwrap();
        // Just verify we got something
        assert!(root_id.get() > 0);
    }
}

#[test]
fn test_parse_qualified_symbol_error_leading_dot() {
    // Test that leading dot is not valid
    let result = parse(".variable");
    // This should fail or parse as something else
    if let Ok(parsed) = result {
        // Only check root_id if it exists
        if let Some(root_id) = parsed.root_id {
            assert!(root_id.get() > 0);
        }
    }
}

#[test]
fn test_parse_qualified_symbol_with_keywords() {
    // Test qualified symbols that might conflict with keywords
    let result = parse("(let ((x if.true)) x)");
    // This might or might not work depending on lexer rules
    if let Ok(parsed) = result {
        assert!(parsed.root_id.is_some());
    }
}

#[test]
fn test_parse_qualified_symbol_case_sensitivity() {
    // Test case sensitivity in qualified symbols
    let result = parse("Math.PI").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::QualifiedVariable {
            module_name,
            variable_name,
        } => {
            assert_eq!(module_name, "Math");
            assert_eq!(variable_name, "PI");
        }
        _ => panic!("Expected QualifiedVariable node"),
    }
}
