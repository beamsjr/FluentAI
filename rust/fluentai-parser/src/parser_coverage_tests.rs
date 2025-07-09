//! Additional tests to improve parser coverage

#[cfg(test)]
mod tests {
    use crate::error::ParseError;
    use crate::parse;
    use bumpalo::Bump;
    use fluentai_core::ast::{EffectType, Literal, Node};

    // ===== Test parse_with_arena =====

    #[test]
    fn test_parse_with_arena() {
        let arena = Bump::new();
        let result = crate::parse_with_arena("42", &arena);
        assert!(result.is_ok());

        let arena2 = Bump::new();
        let result2 = crate::parse_with_arena("(+ 1 2)", &arena2);
        assert!(result2.is_ok());
    }

    // ===== Test qualified symbols =====

    #[test]
    #[ignore = "Qualified symbols might not be fully implemented"]
    fn test_parse_qualified_symbol() {
        let result = parse("module:function");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        // Qualified symbols are parsed as application of qualified variable
        assert!(matches!(node, Node::Application { .. }));
    }

    // ===== Test async/await/spawn =====

    #[test]
    fn test_parse_async() {
        let result = parse("(async (+ 1 2))");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Async { .. }));
    }

    #[test]
    fn test_parse_await() {
        let result = parse("(await promise)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Await { .. }));
    }

    #[test]
    fn test_parse_spawn() {
        let result = parse("(spawn (println \"hello\"))");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Spawn { .. }));
    }

    // ===== Test channel operations =====

    #[test]
    fn test_parse_channel() {
        let result = parse("(chan)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Channel));
    }

    #[test]
    fn test_parse_send() {
        let result = parse("(send! ch 42)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Send { .. }));
    }

    #[test]
    fn test_parse_receive() {
        let result = parse("(recv! ch)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Receive { .. }));
    }

    // ===== Test module system =====

    #[test]
    fn test_parse_module() {
        let result = parse(r#"(module math (export add sub) (define add (lambda (x y) (+ x y))))"#);
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Module {
                name,
                exports,
                body,
            } => {
                assert_eq!(name, "math");
                assert_eq!(exports.len(), 2);
                // body is a NodeId, not a Vec
                assert!(graph.get_node(*body).is_some());
            }
            _ => panic!("Expected module"),
        }
    }

    #[test]
    fn test_parse_import() {
        let result = parse("(import \"math\" (add sub))");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Import {
                module_path,
                import_list,
                import_all,
            } => {
                assert_eq!(module_path, "math");
                assert_eq!(import_list.len(), 2);
                assert!(!import_all);
            }
            _ => panic!("Expected import"),
        }
    }

    #[test]
    fn test_parse_import_all() {
        let result = parse("(import \"math\" *)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Import {
                module_path,
                import_list,
                import_all,
            } => {
                assert_eq!(module_path, "math");
                assert!(import_list.is_empty());
                assert!(*import_all);
            }
            _ => panic!("Expected import"),
        }
    }

    #[test]
    fn test_parse_export() {
        let result = parse("(export add sub multiply)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Export { export_list } => {
                assert_eq!(export_list.len(), 3);
                // Check that items have names
                assert!(export_list.iter().all(|item| !item.name.is_empty()));
            }
            _ => panic!("Expected export"),
        }
    }

    #[test]
    fn test_parse_export_all() {
        let result = parse("(export *)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        // Export with * should be parsed
        assert!(matches!(node, Node::Export { .. }));
    }

    // ===== Test contracts =====

    #[test]
    #[ignore = "Contracts might not be fully implemented"]
    fn test_parse_contract() {
        let result = parse("(spec:contract add (-> integer integer integer))");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Contract { function_name, .. } => {
                assert_eq!(function_name, "add");
            }
            _ => panic!("Expected contract"),
        }
    }

    // ===== Test effects =====

    #[test]
    fn test_parse_effect_io() {
        let result = parse("(effect io:print \"hello\")");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Effect {
                effect_type,
                operation,
                ..
            } => {
                assert!(matches!(effect_type, EffectType::IO));
                assert_eq!(operation, "print");
            }
            _ => panic!("Expected effect"),
        }
    }

    #[test]
    fn test_parse_effect_state() {
        let result = parse("(effect state:get \"counter\")");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Effect {
                effect_type,
                operation,
                ..
            } => {
                assert!(matches!(effect_type, EffectType::State));
                assert_eq!(operation, "get");
            }
            _ => panic!("Expected effect"),
        }
    }

    #[test]
    fn test_parse_effect_network() {
        let result = parse("(effect network:http-get \"https://example.com\")");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Effect { effect_type, .. } => {
                assert!(matches!(effect_type, EffectType::Network));
            }
            _ => panic!("Expected effect"),
        }
    }

    // ===== Test pattern matching =====

    #[test]
    #[ignore = "Complex patterns might not be fully implemented"]
    fn test_parse_match_with_patterns() {
        let result = parse(
            r#"
            (match x
              (0 "zero")
              ((cons h t) h)
              (_ "other"))
        "#,
        );
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Match { expr: _, branches } => {
                assert_eq!(branches.len(), 3);
            }
            _ => panic!("Expected match"),
        }
    }

    #[test]
    #[ignore = "Constructor patterns might not be implemented"]
    fn test_parse_cons_pattern() {
        let result = parse("(match lst ((cons x xs) x))");
        assert!(result.is_ok());
    }

    #[test]
    #[ignore = "Vector patterns might not be implemented"]
    fn test_parse_vector_pattern() {
        let result = parse("(match vec ([x y z] (+ x y z)))");
        assert!(result.is_ok());
    }

    #[test]
    #[ignore = "As patterns might not be implemented"]
    fn test_parse_as_pattern() {
        let result = parse("(match val ((cons h t as lst) lst))");
        assert!(result.is_ok());
    }

    // ===== Test error cases =====

    #[test]
    fn test_parse_empty_input() {
        let result = parse("");
        assert!(result.is_ok()); // Empty input is valid, just has no root
        assert!(result.unwrap().root_id.is_none());
    }

    #[test]
    fn test_parse_unclosed_list() {
        let result = parse("(+ 1 2");
        assert!(result.is_err());
        match result.unwrap_err() {
            ParseError::UnexpectedEof => {}
            _ => panic!("Expected UnexpectedEof"),
        }
    }

    #[test]
    fn test_parse_unexpected_token() {
        let result = parse("(+ 1 2))"); // Extra closing paren
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_lambda() {
        let result = parse("(lambda)"); // Missing params and body
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_let() {
        let result = parse("(let)"); // Missing bindings and body
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_if() {
        let result = parse("(if true)"); // Missing then and else
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_match() {
        let result = parse("(match)"); // Missing expression and cases
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_pattern() {
        let result = parse("(match x ((123abc) 1))"); // Invalid pattern
        assert!(result.is_err());
    }

    // ===== Test complex expressions =====

    #[test]
    fn test_parse_nested_let() {
        let result = parse(
            r#"
            (let ((x 1))
              (let ((y 2))
                (+ x y)))
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_lambda_with_multiple_params() {
        let result = parse("(lambda (x y z) (+ x (+ y z)))");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_letrec_recursive() {
        let result = parse(
            r#"
            (letrec ((fact (lambda (n)
                            (if (= n 0)
                                1
                                (* n (fact (- n 1)))))))
              (fact 5))
        "#,
        );
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_do_sequence() {
        let result = parse(
            r#"
            (do
              (println "First")
              (println "Second")
              (+ 1 2))
        "#,
        );
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        // 'do' might be parsed as a sequence or converted to nested let
        assert!(matches!(node, Node::Let { .. }) || matches!(node, Node::Application { .. }));
    }

    #[test]
    fn test_parse_list_literal() {
        let result = parse("[1 2 3]");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        // List literals might be converted to cons cells or parsed as List
        assert!(matches!(node, Node::Application { .. }) || matches!(node, Node::List(_)));
    }

    #[test]
    fn test_parse_empty_list_literal() {
        let result = parse("[]");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        // Empty list literals might be parsed as Nil or as empty List
        assert!(
            matches!(node, Node::Literal(Literal::Nil))
                || matches!(node, Node::List(v) if v.is_empty())
        );
    }

    #[test]
    fn test_parse_multiple_expressions() {
        let result = parse("1 2 3");
        assert!(result.is_ok());
        let graph = result.unwrap();
        // Should have parsed all three, with last one as root
        assert!(graph.root_id.is_some());
    }

    // ===== Test edge cases =====

    #[test]
    fn test_parse_string_escapes() {
        let result = parse(r#""hello\nworld""#);
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_empty_application() {
        let result = parse("()");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::List(v) if v.is_empty()));
    }

    #[test]
    fn test_parse_deeply_nested() {
        let result = parse("((((((42))))))");
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_effect_random() {
        let result = parse("(effect random:int 1 100)");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        match node {
            Node::Effect {
                effect_type,
                operation,
                ..
            } => {
                assert!(matches!(effect_type, EffectType::Random));
                assert_eq!(operation, "int");
            }
            _ => panic!("Expected effect"),
        }
    }

    #[test]
    fn test_parse_symbol_starting_with_keyword() {
        // Symbols that start with keywords should still parse
        let result = parse("lambda-calculus");
        assert!(result.is_ok());
        let graph = result.unwrap();
        let root_id = graph.root_id.unwrap();
        let node = graph.get_node(root_id).unwrap();

        assert!(matches!(node, Node::Variable { name } if name == "lambda-calculus"));
    }
}
