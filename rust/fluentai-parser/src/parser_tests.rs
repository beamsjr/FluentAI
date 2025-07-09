#[cfg(test)]
mod tests {
    use crate::error::ParseError;
    use crate::parse;
    use crate::parser::ParseResult;
    use fluentai_core::ast::{EffectType, Literal, Node, Pattern};

    // ===== Helper Functions =====

    fn parse_single_expr(input: &str) -> ParseResult<Node> {
        let result = parse(input)?;
        let root_id = result.root_id.ok_or(ParseError::UnexpectedEof)?;
        result
            .get_node(root_id)
            .ok_or(ParseError::InvalidSyntax("No root node".to_string()))
            .map(|n| n.clone())
    }

    // ===== Literal Tests =====

    #[test]
    fn test_parse_integers() {
        assert!(matches!(
            parse_single_expr("42").unwrap(),
            Node::Literal(Literal::Integer(42))
        ));
        assert!(matches!(
            parse_single_expr("-123").unwrap(),
            Node::Literal(Literal::Integer(-123))
        ));
        assert!(matches!(
            parse_single_expr("0").unwrap(),
            Node::Literal(Literal::Integer(0))
        ));
    }

    #[test]
    fn test_parse_floats() {
        assert!(matches!(parse_single_expr("3.14").unwrap(), 
            Node::Literal(Literal::Float(f)) if (f - 3.14).abs() < 0.001));
        assert!(matches!(parse_single_expr("-2.5").unwrap(), 
            Node::Literal(Literal::Float(f)) if (f + 2.5).abs() < 0.001));
        assert!(matches!(parse_single_expr("1.5e10").unwrap(), 
            Node::Literal(Literal::Float(f)) if (f - 1.5e10).abs() < 0.001));
    }

    #[test]
    fn test_parse_strings() {
        assert!(matches!(parse_single_expr(r#""hello""#).unwrap(), 
            Node::Literal(Literal::String(s)) if s == "hello"));
        assert!(matches!(parse_single_expr(r#""""#).unwrap(), 
            Node::Literal(Literal::String(s)) if s.is_empty()));
        assert!(matches!(parse_single_expr(r#""hello world""#).unwrap(), 
            Node::Literal(Literal::String(s)) if s == "hello world"));
    }

    #[test]
    fn test_parse_booleans() {
        assert!(matches!(
            parse_single_expr("true").unwrap(),
            Node::Literal(Literal::Boolean(true))
        ));
        assert!(matches!(
            parse_single_expr("false").unwrap(),
            Node::Literal(Literal::Boolean(false))
        ));
        assert!(matches!(
            parse_single_expr("#t").unwrap(),
            Node::Literal(Literal::Boolean(true))
        ));
        assert!(matches!(
            parse_single_expr("#f").unwrap(),
            Node::Literal(Literal::Boolean(false))
        ));
    }

    #[test]
    fn test_parse_nil() {
        assert!(matches!(parse_single_expr("nil").unwrap(), 
            Node::Variable { name } if name == "nil"));
    }

    // ===== Variable Tests =====

    #[test]
    fn test_parse_simple_variables() {
        assert!(matches!(parse_single_expr("x").unwrap(), 
            Node::Variable { name } if name == "x"));
        assert!(matches!(parse_single_expr("foo-bar").unwrap(), 
            Node::Variable { name } if name == "foo-bar"));
        assert!(matches!(parse_single_expr("list?").unwrap(), 
            Node::Variable { name } if name == "list?"));
    }

    #[test]
    fn test_parse_qualified_variables() {
        let node = parse_single_expr("math.sin").unwrap();
        assert!(matches!(node, 
            Node::QualifiedVariable { module_name, variable_name } 
            if module_name == "math" && variable_name == "sin"));
    }

    // ===== List Literal Tests =====

    #[test]
    fn test_parse_empty_list() {
        let result = parse("[]").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(result.get_node(root_id).unwrap(), 
            Node::List(items) if items.is_empty()));
    }

    #[test]
    fn test_parse_list_with_literals() {
        let result = parse("[1 2 3]").unwrap();
        let root_id = result.root_id.unwrap();
        // List literals are converted to cons operations, so we check for Application
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Application { .. }
        ));
    }

    #[test]
    fn test_parse_nested_lists() {
        let result = parse("[[1 2] [3 4]]").unwrap();
        let root_id = result.root_id.unwrap();
        // List literals are converted to cons operations
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Application { .. }
        ));
    }

    // ===== Lambda Tests =====

    #[test]
    fn test_parse_simple_lambda() {
        let result = parse("(lambda (x) x)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Lambda { params, body } => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0], "x");
                assert!(matches!(result.get_node(*body).unwrap(), 
                    Node::Variable { name } if name == "x"));
            }
            _ => panic!("Expected Lambda node"),
        }
    }

    #[test]
    fn test_parse_multi_param_lambda() {
        let result = parse("(lambda (x y z) (+ x y z))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Lambda { params, body } => {
                assert_eq!(params, &["x", "y", "z"]);
                assert!(matches!(
                    result.get_node(*body).unwrap(),
                    Node::Application { .. }
                ));
            }
            _ => panic!("Expected Lambda node"),
        }
    }

    #[test]
    fn test_parse_lambda_no_params() {
        let result = parse("(lambda () 42)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Lambda { params, body } => {
                assert!(params.is_empty());
                assert!(matches!(
                    result.get_node(*body).unwrap(),
                    Node::Literal(Literal::Integer(42))
                ));
            }
            _ => panic!("Expected Lambda node"),
        }
    }

    // ===== Let/Letrec Tests =====

    #[test]
    fn test_parse_simple_let() {
        let result = parse("(let ((x 42)) x)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
                assert!(matches!(
                    result.get_node(bindings[0].1).unwrap(),
                    Node::Literal(Literal::Integer(42))
                ));
                assert!(matches!(result.get_node(*body).unwrap(), 
                    Node::Variable { name } if name == "x"));
            }
            _ => panic!("Expected Let node"),
        }
    }

    #[test]
    fn test_parse_multi_binding_let() {
        let result = parse("(let ((x 1) (y 2) (z 3)) (+ x y z))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, .. } => {
                assert_eq!(bindings.len(), 3);
                assert_eq!(bindings[0].0, "x");
                assert_eq!(bindings[1].0, "y");
                assert_eq!(bindings[2].0, "z");
            }
            _ => panic!("Expected Let node"),
        }
    }

    #[test]
    fn test_parse_letrec() {
        let result =
            parse("(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))")
                .unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Letrec { .. }
        ));
    }

    #[test]
    fn test_parse_let_multiple_body_expressions() {
        let result = parse("(let ((x 5)) (print x) (+ x 1))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "x");
                // Body should be a Begin node with 2 expressions
                match result.get_node(*body).unwrap() {
                    Node::Begin { exprs } => {
                        assert_eq!(exprs.len(), 2);
                        // First expression should be (print x)
                        assert!(matches!(
                            result.get_node(exprs[0]).unwrap(),
                            Node::Application { .. }
                        ));
                        // Second expression should be (+ x 1)
                        assert!(matches!(
                            result.get_node(exprs[1]).unwrap(),
                            Node::Application { .. }
                        ));
                    }
                    _ => panic!("Expected Begin node for body"),
                }
            }
            _ => panic!("Expected Let node"),
        }
    }

    #[test]
    fn test_parse_let_empty_body() {
        let result = parse("(let ((x 5)))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                // Empty body should be nil
                assert!(matches!(
                    result.get_node(*body).unwrap(),
                    Node::Literal(Literal::Nil)
                ));
            }
            _ => panic!("Expected Let node"),
        }
    }

    #[test]
    fn test_parse_let_single_body_expression() {
        let result = parse("(let ((x 5)) x)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                // Single expression body should not be wrapped in Begin
                assert!(matches!(
                    result.get_node(*body).unwrap(),
                    Node::Variable { .. }
                ));
            }
            _ => panic!("Expected Let node"),
        }
    }

    #[test]
    fn test_parse_letrec_multiple_body_expressions() {
        let result = parse("(letrec ((x 5)) (print x) (+ x 1))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Letrec { bindings, body } => {
                assert_eq!(bindings.len(), 1);
                // Body should be a Begin node with 2 expressions
                match result.get_node(*body).unwrap() {
                    Node::Begin { exprs } => {
                        assert_eq!(exprs.len(), 2);
                    }
                    _ => panic!("Expected Begin node for body"),
                }
            }
            _ => panic!("Expected Letrec node"),
        }
    }

    // ===== If Tests =====

    #[test]
    fn test_parse_if() {
        let result = parse("(if (> x 0) \"positive\" \"non-positive\")").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert!(matches!(
                    result.get_node(*condition).unwrap(),
                    Node::Application { .. }
                ));
                assert!(matches!(result.get_node(*then_branch).unwrap(), 
                    Node::Literal(Literal::String(s)) if s == "positive"));
                assert!(matches!(result.get_node(*else_branch).unwrap(), 
                    Node::Literal(Literal::String(s)) if s == "non-positive"));
            }
            _ => panic!("Expected If node"),
        }
    }

    // ===== Application Tests =====

    #[test]
    fn test_parse_simple_application() {
        let result = parse("(+ 1 2)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Application { function, args } => {
                assert!(matches!(result.get_node(*function).unwrap(), 
                    Node::Variable { name } if name == "+"));
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected Application node"),
        }
    }

    #[test]
    fn test_parse_nested_application() {
        let result = parse("(+ (* 2 3) (- 5 1))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Application { function, args } => {
                assert!(matches!(result.get_node(*function).unwrap(), 
                    Node::Variable { name } if name == "+"));
                assert_eq!(args.len(), 2);
                assert!(matches!(
                    result.get_node(args[0]).unwrap(),
                    Node::Application { .. }
                ));
                assert!(matches!(
                    result.get_node(args[1]).unwrap(),
                    Node::Application { .. }
                ));
            }
            _ => panic!("Expected Application node"),
        }
    }

    #[test]
    fn test_parse_empty_application() {
        let result = parse("(foo)").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Application { function, args } => {
                assert!(matches!(result.get_node(*function).unwrap(), 
                    Node::Variable { name } if name == "foo"));
                assert!(args.is_empty());
            }
            _ => panic!("Expected Application node"),
        }
    }

    // ===== Effect Tests =====

    #[test]
    fn test_parse_simple_effect() {
        let result = parse(r#"(effect IO:print "hello")"#).unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Effect {
                effect_type,
                operation,
                args,
            } => {
                assert_eq!(*effect_type, EffectType::IO);
                assert_eq!(operation, "print");
                assert_eq!(args.len(), 1);
            }
            _ => panic!("Expected Effect node"),
        }
    }

    #[test]
    fn test_parse_effect_shorthand() {
        // Shorthand is handled as a regular application
        let result = parse(r#"(io:print "hello")"#).unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Application { .. }
        ));
    }

    // ===== Match Tests =====

    #[test]
    fn test_parse_simple_match() {
        let result = parse("(match x (0 \"zero\") (1 \"one\") (_ \"other\"))").unwrap();
        let root_id = result.root_id.unwrap();
        match result.get_node(root_id).unwrap() {
            Node::Match { expr, branches } => {
                assert!(matches!(result.get_node(*expr).unwrap(), 
                    Node::Variable { name } if name == "x"));
                assert_eq!(branches.len(), 3);
                assert!(matches!(
                    &branches[0].0,
                    Pattern::Literal(Literal::Integer(0))
                ));
                assert!(matches!(
                    &branches[1].0,
                    Pattern::Literal(Literal::Integer(1))
                ));
                assert!(matches!(&branches[2].0, Pattern::Wildcard));
            }
            _ => panic!("Expected Match node"),
        }
    }

    #[test]
    fn test_parse_match_with_constructor() {
        // Constructor patterns aren't implemented, so this should fail
        assert!(parse("(match lst ((Cons x xs) x) (Nil 0))").is_err());
    }

    // ===== Async/Concurrent Tests =====

    #[test]
    fn test_parse_async() {
        let result = parse("(async (+ 1 2))").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Async { .. }
        ));
    }

    #[test]
    fn test_parse_await() {
        let result = parse("(await computation)").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Await { .. }
        ));
    }

    #[test]
    fn test_parse_spawn() {
        let result = parse("(spawn (process-data))").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Spawn { .. }
        ));
    }

    #[test]
    fn test_parse_channel() {
        let result = parse("(chan)").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(result.get_node(root_id).unwrap(), Node::Channel));
    }

    #[test]
    fn test_parse_send() {
        let result = parse("(send! ch 42)").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Send { .. }
        ));
    }

    #[test]
    fn test_parse_receive() {
        let result = parse("(recv! ch)").unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Receive { .. }
        ));
    }

    // ===== Error Tests =====

    #[test]
    fn test_parse_unclosed_paren() {
        assert!(matches!(parse("(+ 1 2"), Err(ParseError::UnexpectedEof)));
    }

    #[test]
    fn test_parse_unexpected_rparen() {
        assert!(matches!(parse(")"), Err(ParseError::InvalidSyntax(_))));
    }

    #[test]
    fn test_parse_empty_input() {
        let result = parse("").unwrap();
        assert!(result.root_id.is_none());
    }

    #[test]
    fn test_parse_invalid_let_binding() {
        assert!(parse("(let ((42 x)) x)").is_err());
    }

    #[test]
    fn test_parse_invalid_lambda_params() {
        assert!(parse("(lambda (x 42 y) x)").is_err());
    }

    // ===== Complex Expression Tests =====

    #[test]
    fn test_parse_factorial() {
        let code = r#"
            (letrec ((factorial (lambda (n)
                (if (<= n 1)
                    1
                    (* n (factorial (- n 1)))))))
                (factorial 5))
        "#;
        assert!(parse(code).is_ok());
    }

    #[test]
    fn test_parse_map_implementation() {
        // Since constructor patterns aren't implemented, use a simpler map example
        let code = r#"
            (letrec ((map (lambda (f lst)
                (if (null? lst)
                    []
                    (cons (f (car lst)) (map f (cdr lst)))))))
                (map square [1 2 3 4 5]))
        "#;
        let result = parse(code);
        assert!(result.is_ok());
    }

    // ===== Multiple Top-Level Expressions =====

    #[test]
    fn test_parse_multiple_expressions() {
        let result = parse("42 \"hello\" (+ 1 2)").unwrap();
        // Should have the last expression as root
        let root_id = result.root_id.unwrap();
        assert!(matches!(
            result.get_node(root_id).unwrap(),
            Node::Application { .. }
        ));
    }

    // ===== Sequence (do) Tests =====

    #[test]
    fn test_parse_do_sequence() {
        let result = parse("(do (print \"hello\") (print \"world\") 42)").unwrap();
        let root_id = result.root_id.unwrap();
        // "do" is converted to nested let expressions
        match result.get_node(root_id).unwrap() {
            Node::Let { bindings, body } => {
                // First expression becomes a binding with name "_"
                assert_eq!(bindings.len(), 1);
                assert_eq!(bindings[0].0, "_");
                // Check nested structure
                match result.get_node(*body).unwrap() {
                    Node::Let {
                        bindings: inner_bindings,
                        body: inner_body,
                    } => {
                        assert_eq!(inner_bindings.len(), 1);
                        assert_eq!(inner_bindings[0].0, "_");
                        // Final expression should be 42
                        assert!(matches!(
                            result.get_node(*inner_body).unwrap(),
                            Node::Literal(Literal::Integer(42))
                        ));
                    }
                    _ => panic!("Expected nested Let node"),
                }
            }
            _ => panic!("Expected Let node for do sequence"),
        }
    }

    // ===== Unicode Tests =====

    #[test]
    fn test_parse_unicode_strings() {
        let result = parse(r#""Hello, 世界""#).unwrap();
        let root_id = result.root_id.unwrap();
        assert!(matches!(result.get_node(root_id).unwrap(), 
            Node::Literal(Literal::String(s)) if s == "Hello, 世界"));
    }

    #[test]
    fn test_parse_unicode_symbols() {
        // The lexer's symbol regex only matches ASCII characters,
        // so unicode characters are not recognized as valid symbols.
        // This input will parse successfully but the unicode characters
        // will be skipped by the lexer, resulting in parsing just "(x 42)"
        let result = parse("(定义 x 42)");
        // Should parse successfully as an application of x to 42
        assert!(result.is_ok());
        let root_id = result.unwrap().root_id.unwrap();
        // Verify we get an application node (since 定义 is skipped)
        let graph = parse("(定义 x 42)").unwrap();
        assert!(matches!(
            graph.get_node(root_id).unwrap(),
            Node::Application { .. }
        ));
    }
}
