#[cfg(test)]
mod test_fstring_ast {
    use fluentai_core::ast::{Literal, Node};
    use fluentai_parser::flc_parser::Parser;

    #[test]
    fn test_fstring_produces_correct_ast() {
        let input = r#"f"Hello, {name}!""#;

        let mut parser = Parser::new(input);
        let graph = parser.parse().unwrap();

        // Print the root node
        if let Some(root_id) = graph.root_id {
            if let Some(node) = graph.get_node(root_id) {
                match node {
                    Node::Application { function, args } => {
                        println!("Root is Application");
                        // Should be string-append
                        if let Some(Node::Variable { name }) = graph.get_node(*function) {
                            println!("Function: {}", name);
                            assert_eq!(name, "string-append");
                        }
                        // Check args
                        assert_eq!(args.len(), 2);

                        // First arg should be "Hello, "
                        if let Some(Node::Literal(Literal::String(s))) = graph.get_node(args[0]) {
                            println!("First part: {:?}", s);
                            assert_eq!(s, "Hello, ");
                        }

                        // Second arg should be to_string(name)
                        if let Some(Node::Application {
                            function: to_string_fn,
                            args: to_string_args,
                        }) = graph.get_node(args[1])
                        {
                            if let Some(Node::Variable { name }) = graph.get_node(*to_string_fn) {
                                assert_eq!(name, "to_string");
                            }
                            assert_eq!(to_string_args.len(), 1);
                            if let Some(Node::Variable { name }) = graph.get_node(to_string_args[0])
                            {
                                assert_eq!(name, "name");
                            }
                        }

                        // For "Hello, {name}!", we should have nested string-append
                        // string-append("Hello, ", string-append(to_string(name), "!"))
                        if let Some(Node::Application {
                            function: inner_append,
                            args: inner_args,
                        }) = graph.get_node(args[1])
                        {
                            if let Some(Node::Variable { name }) = graph.get_node(*inner_append) {
                                assert_eq!(name, "string-append");
                            }
                            assert_eq!(inner_args.len(), 2);

                            // First inner arg should be to_string(name)
                            if let Some(Node::Application {
                                function: to_string_fn,
                                args: to_string_args,
                            }) = graph.get_node(inner_args[0])
                            {
                                if let Some(Node::Variable { name }) = graph.get_node(*to_string_fn)
                                {
                                    assert_eq!(name, "to_string");
                                }
                                assert_eq!(to_string_args.len(), 1);
                                if let Some(Node::Variable { name }) =
                                    graph.get_node(to_string_args[0])
                                {
                                    assert_eq!(name, "name");
                                }
                            }

                            // Second inner arg should be "!"
                            if let Some(Node::Literal(Literal::String(s))) =
                                graph.get_node(inner_args[1])
                            {
                                println!("Last part: {:?}", s);
                                assert_eq!(s, "!");
                            }
                        }
                    }
                    _ => {
                        panic!("Expected Application node, got {:?}", node);
                    }
                }
            }
        }
    }
}
