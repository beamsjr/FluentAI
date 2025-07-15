use bumpalo::Bump;
use fluentai_core::ast::Node;
use fluentai_parser::*;
use fluentai_parser::parse_flc as parse;

    #[test]
    fn test_parse_basic() {
        let code = "1 + 2";
        let graph = parse(code).unwrap();

        // Should have nodes
        assert!(!graph.nodes.is_empty());

        // Check the root node
        assert!(graph.root_id.is_some());
        let root_node = graph.get_node(graph.root_id.unwrap()).unwrap();
        assert!(matches!(root_node, Node::Application { .. }));
    }

    #[test]
    fn test_parse_with_arena() {
        let arena = Bump::new();
        let code = "let x = 10; x * 2";
        let graph = parse_with_arena(code, &arena).unwrap();

        // Should have nodes
        assert!(!graph.nodes.is_empty());

        // Check the root node
        assert!(graph.root_id.is_some());
        let root_node = graph.get_node(graph.root_id.unwrap()).unwrap();
        assert!(matches!(root_node, Node::Let { .. }));
    }

    #[test]
    fn test_parse_empty_string() {
        let result = parse("");
        assert!(result.is_ok());
        let graph = result.unwrap();
        // Empty string should parse to nil
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_whitespace_only() {
        let result = parse("   \n\t  \r\n  ");
        assert!(result.is_ok());
        let graph = result.unwrap();
        // Whitespace only should parse to nil
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_comments_only() {
        let code = r#"
        // This is a comment
        // Another comment
        // Yet another
        "#;
        let result = parse(code);
        assert!(result.is_ok());
        let graph = result.unwrap();
        // Comments only should parse to nil
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_multiple_expressions() {
        let code = "{ 1 + 2; 3 * 4; 5 - 6 }";
        let graph = parse(code).unwrap();

        // Should have multiple nodes
        assert!(graph.nodes.len() >= 3);
    }

    #[test]
    fn test_parse_with_all_literal_types() {
        let code = r#"{
            42;           // integer
            -17;          // negative integer
            3.14;         // float
            -2.718;       // negative float
            "hello";      // string
            true;         // true
            false;        // false
            nil           // nil
        }"#;

        let graph = parse(code).unwrap();
        // Should have parsed some literals
        assert!(!graph.nodes.is_empty());
    }

    #[test]
    fn test_parse_function_definition() {
        let code = r#"
            private function factorial(n) {
                if (n == 0) { 1 } else { n * factorial(n - 1) }
            }
        "#;
        let graph = parse(code).unwrap();

        // Should successfully parse recursive function
        assert!(!graph.nodes.is_empty());
        assert!(graph.root_id.is_some());
    }

    #[test]
    fn test_parse_lambda_expression() {
        let code = "list.map(x => x * 2)";
        let graph = parse(code).unwrap();

        // Should have nodes
        assert!(!graph.nodes.is_empty());
    }

    #[test]
    fn test_parse_async_await() {
        let code = r#"
            private async function fetch_and_process(url) {
                let result = fetch(url).await();
                process(result)
            }
        "#;
        let graph = parse(code).unwrap();

        // Should successfully parse async function
        assert!(!graph.nodes.is_empty());
    }

    #[test]
    fn test_parse_error_invalid_syntax() {
        let code = "let x ="; // Incomplete assignment
        let result = parse(code);
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_complex_expression() {
        let code = r#"
            users
                .filter(u => u.age >= 18)
                .map(u => u.name)
                .sort()
        "#;
        let graph = parse(code).unwrap();

        // Should parse method chain
        assert!(!graph.nodes.is_empty());
    }

    #[test]
    fn test_parse_pattern_matching() {
        let code = r#"
            value.match()
                .case(Some(x), x * 2)
                .case(None, 0)
                .get()
        "#;
        let graph = parse(code).unwrap();

        // Should parse pattern matching
        assert!(!graph.nodes.is_empty());
    }
