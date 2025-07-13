#[cfg(test)]
mod test_fstring_complex {
    use fluentai_core::ast::Node;
    use fluentai_parser::flc_parser::Parser;

    #[test]
    fn test_fstring_with_expression() {
        let input = r#"f"Result: {x + y}""#;

        let mut parser = Parser::new(input);
        let graph = parser.parse().unwrap();

        // Find the interpolated expression (x + y)
        let mut found_addition = false;

        // Debug: print all nodes
        println!("All nodes in graph:");
        for (id, node) in &graph.nodes {
            println!("{:?}: {:?}", id, node);
        }

        for (_id, node) in &graph.nodes {
            if let Node::Application { function, args } = node {
                if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    println!("Found application of function: {}", name);
                    if name == "+" && args.len() == 2 {
                        // Check if args are x and y
                        if let (
                            Some(Node::Variable { name: n1 }),
                            Some(Node::Variable { name: n2 }),
                        ) = (graph.get_node(args[0]), graph.get_node(args[1]))
                        {
                            if n1 == "x" && n2 == "y" {
                                found_addition = true;
                                println!("Found interpolated expression: {} + {}", n1, n2);
                            }
                        }
                    }
                }
            }
        }

        assert!(found_addition, "Should find x + y expression in f-string");
    }

    #[test]
    fn test_fstring_with_multiple_interpolations() {
        let input = r#"f"User: {user.name} (ID: {user.id})""#;

        let mut parser = Parser::new(input);
        let graph = parser.parse().unwrap();

        // Count property accesses
        let mut property_accesses = 0;

        // Debug: print all nodes
        println!("\nAll nodes for property access test:");
        for (id, node) in &graph.nodes {
            println!("{:?}: {:?}", id, node);
        }

        for (_id, node) in &graph.nodes {
            match node {
                Node::QualifiedVariable {
                    module_name,
                    variable_name,
                } => {
                    if module_name == "user" && (variable_name == "name" || variable_name == "id") {
                        property_accesses += 1;
                        println!("Found property access: {}.{}", module_name, variable_name);
                    }
                }
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        println!("Found application: {} with {} args", name, args.len());
                        // Also check for old-style property access
                        if (name == "name" || name == "id") && args.len() == 1 {
                            property_accesses += 1;
                            if let Some(Node::Variable { name: obj }) = graph.get_node(args[0]) {
                                println!("Found property access: {}.{}", obj, name);
                            }
                        }
                    }
                }
                _ => {}
            }
        }

        // We expect at least 2 property accesses: user.name and user.id
        assert!(
            property_accesses >= 2,
            "Should find at least 2 property accesses, found {}",
            property_accesses
        );
    }

    #[test]
    fn test_fstring_with_escaped_braces() {
        let input = r#"f"Value: {{x}}""#;

        let mut parser = Parser::new(input);
        let graph = parser.parse().unwrap();

        // Should produce a simple string literal "Value: {x}"
        if let Some(root_id) = graph.root_id {
            if let Some(Node::Literal(fluentai_core::ast::Literal::String(s))) =
                graph.get_node(root_id)
            {
                assert_eq!(s, "Value: {x}");
                println!("Escaped braces correctly handled: {:?}", s);
            } else {
                panic!("Expected string literal for escaped braces");
            }
        }
    }
}
