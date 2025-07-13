#[cfg(test)]
mod tests {
    use crate::patterns::{MatchResult, NodePattern, Pattern, PatternMatcher};
    use fluentai_core::ast::{Graph, Literal, Node, NodeId};
    use fluentai_parser::parse_flc;
    
    

    // ===== Pattern Construction Tests =====

    #[test]
    fn test_pattern_any() {
        let pattern = Pattern::Any;
        match pattern {
            Pattern::Any => (),
            _ => panic!("Expected Pattern::Any"),
        }
    }

    #[test]
    fn test_pattern_literal() {
        let pattern = Pattern::Literal(Literal::Integer(42));
        match pattern {
            Pattern::Literal(Literal::Integer(n)) => assert_eq!(n, 42),
            _ => panic!("Expected Pattern::Literal"),
        }
    }

    #[test]
    fn test_pattern_bind() {
        let pattern = Pattern::Bind("x".to_string());
        match pattern {
            Pattern::Bind(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Pattern::Bind"),
        }
    }

    #[test]
    fn test_pattern_ref() {
        let pattern = Pattern::Ref("x".to_string());
        match pattern {
            Pattern::Ref(name) => assert_eq!(name, "x"),
            _ => panic!("Expected Pattern::Ref"),
        }
    }

    #[test]
    fn test_pattern_or() {
        let pattern = Pattern::Or(vec![
            Pattern::Literal(Literal::Integer(1)),
            Pattern::Literal(Literal::Integer(2)),
        ]);

        match pattern {
            Pattern::Or(patterns) => assert_eq!(patterns.len(), 2),
            _ => panic!("Expected Pattern::Or"),
        }
    }

    #[test]
    fn test_pattern_and() {
        let pattern = Pattern::And(vec![
            Pattern::Any,
            Pattern::Predicate("even?".to_string(), Box::new(Pattern::Any)),
        ]);

        match pattern {
            Pattern::And(patterns) => assert_eq!(patterns.len(), 2),
            _ => panic!("Expected Pattern::And"),
        }
    }

    #[test]
    fn test_pattern_not() {
        let pattern = Pattern::Not(Box::new(Pattern::Literal(Literal::Nil)));
        match pattern {
            Pattern::Not(_) => (),
            _ => panic!("Expected Pattern::Not"),
        }
    }

    // ===== NodePattern Tests =====

    #[test]
    fn test_node_pattern_variable() {
        let pattern = NodePattern::Variable {
            name: Some("x".to_string()),
        };
        match pattern {
            NodePattern::Variable { name } => assert_eq!(name, Some("x".to_string())),
            _ => panic!("Expected NodePattern::Variable"),
        }
    }

    #[test]
    fn test_node_pattern_lambda() {
        let pattern = NodePattern::Lambda {
            params: Some(vec!["x".to_string(), "y".to_string()]),
            body: Box::new(Pattern::Any),
        };

        match pattern {
            NodePattern::Lambda { params, .. } => {
                assert_eq!(params, Some(vec!["x".to_string(), "y".to_string()]));
            }
            _ => panic!("Expected NodePattern::Lambda"),
        }
    }

    #[test]
    fn test_node_pattern_application() {
        let pattern = NodePattern::Application {
            function: Box::new(Pattern::Bind("f".to_string())),
            args: vec![Pattern::Any, Pattern::Any],
        };

        match pattern {
            NodePattern::Application { args, .. } => assert_eq!(args.len(), 2),
            _ => panic!("Expected NodePattern::Application"),
        }
    }

    #[test]
    fn test_node_pattern_list() {
        let pattern = NodePattern::List(vec![
            Pattern::Literal(Literal::Integer(1)),
            Pattern::Bind("rest".to_string()),
        ]);

        match pattern {
            NodePattern::List(patterns) => assert_eq!(patterns.len(), 2),
            _ => panic!("Expected NodePattern::List"),
        }
    }

    // ===== MatchResult Tests =====

    #[test]
    fn test_match_result_new() {
        let result = MatchResult::new();
        assert!(result.bindings.is_empty());
        assert!(result.matched_nodes.is_empty());
    }

    #[test]
    fn test_match_result_merge() {
        let mut result1 = MatchResult::new();
        let node_id1 = NodeId(std::num::NonZeroU32::new(1).unwrap());
        let node_id2 = NodeId(std::num::NonZeroU32::new(2).unwrap());

        result1.bindings.insert("x".to_string(), node_id1);
        result1.matched_nodes.push(node_id1);

        let mut result2 = MatchResult::new();
        result2.bindings.insert("y".to_string(), node_id2);
        result2.matched_nodes.push(node_id2);

        result1.merge(result2);

        assert_eq!(result1.bindings.len(), 2);
        assert_eq!(result1.matched_nodes.len(), 2);
        assert!(result1.bindings.contains_key("x"));
        assert!(result1.bindings.contains_key("y"));
    }

    // ===== PatternMatcher Tests =====

    #[test]
    fn test_pattern_matcher_new() {
        let graph = Graph::new();
        let matcher = PatternMatcher::new(&graph);
        assert!(matcher.bindings.is_empty());
    }

    #[test]
    fn test_match_any_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let result = matcher.match_pattern(&Pattern::Any, node_id);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.matched_nodes.len(), 1);
        assert_eq!(result.matched_nodes[0], node_id);
    }

    #[test]
    fn test_match_literal_pattern_success() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::Literal(Literal::Integer(42));
        let result = matcher.match_pattern(&pattern, node_id);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.matched_nodes[0], node_id);
    }

    #[test]
    fn test_match_literal_pattern_failure() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::Literal(Literal::Integer(99));
        let result = matcher.match_pattern(&pattern, node_id);

        assert!(result.is_none());
    }

    #[test]
    fn test_match_bind_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::Bind("x".to_string());
        let result = matcher.match_pattern(&pattern, node_id);

        assert!(result.is_some());
        let result = result.unwrap();
        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.bindings["x"], node_id);
    }

    #[test]
    fn test_match_variable_node_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Variable {
                name: "foo".to_string(),
            })
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::NodeType(NodePattern::Variable {
            name: Some("foo".to_string()),
        });
        let result = matcher.match_pattern(&pattern, node_id);

        assert!(result.is_some());
    }

    #[test]
    fn test_match_or_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(2)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::Or(vec![
            Pattern::Literal(Literal::Integer(1)),
            Pattern::Literal(Literal::Integer(2)),
            Pattern::Literal(Literal::Integer(3)),
        ]);

        let result = matcher.match_pattern(&pattern, node_id);
        assert!(result.is_some());
    }

    #[test]
    fn test_match_and_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::And(vec![Pattern::Any, Pattern::Literal(Literal::Integer(42))]);

        let result = matcher.match_pattern(&pattern, node_id);
        assert!(result.is_some());
    }

    #[test]
    fn test_match_not_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::Not(Box::new(Pattern::Literal(Literal::Integer(99))));

        let result = matcher.match_pattern(&pattern, node_id);
        assert!(result.is_some());
    }

    #[test]
    fn test_match_lambda_pattern() {
        let code = "(x, y) => x + y";
        let graph = parse_flc(code).unwrap();
        let root_id = graph.root_id.unwrap();

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::NodeType(NodePattern::Lambda {
            params: Some(vec!["x".to_string(), "y".to_string()]),
            body: Box::new(Pattern::Any),
        });

        let result = matcher.match_pattern(&pattern, root_id);
        assert!(result.is_some());
    }

    #[test]
    fn test_match_application_pattern() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();
        let root_id = graph.root_id.unwrap();

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::NodeType(NodePattern::Application {
            function: Box::new(Pattern::NodeType(NodePattern::Variable {
                name: Some("+".to_string()),
            })),
            args: vec![Pattern::Any, Pattern::Any],
        });

        let result = matcher.match_pattern(&pattern, root_id);
        assert!(result.is_some());
    }

    #[test]
    fn test_match_ref_pattern() {
        let mut graph = Graph::new();
        let node_id = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);

        // First bind a value
        matcher.bindings.insert("x".to_string(), node_id);

        // Then match against the reference
        let pattern = Pattern::Ref("x".to_string());
        let result = matcher.match_pattern(&pattern, node_id);

        assert!(result.is_some());
    }

    #[test]
    fn test_match_ref_pattern_failure() {
        let mut graph = Graph::new();
        let node_id1 = graph
            .add_node(Node::Literal(Literal::Integer(42)))
            .expect("Failed to add node");
        let node_id2 = graph
            .add_node(Node::Literal(Literal::Integer(99)))
            .expect("Failed to add node");

        let mut matcher = PatternMatcher::new(&graph);

        // Bind to a different node
        matcher.bindings.insert("x".to_string(), node_id1);

        // Try to match against a different node
        let pattern = Pattern::Ref("x".to_string());
        let result = matcher.match_pattern(&pattern, node_id2);

        assert!(result.is_none());
    }

    #[test]
    #[ignore = "Pattern matching with backreferences not fully implemented"]
    fn test_complex_pattern() {
        // Match: (if <condition> <then> <else>) where then and else are the same
        let code = "if (x > 0) { 42 } else { 42 }";
        let graph = parse_flc(code).unwrap();
        let root_id = graph.root_id.unwrap();

        let mut matcher = PatternMatcher::new(&graph);
        let pattern = Pattern::NodeType(NodePattern::If {
            condition: Box::new(Pattern::Any),
            then_branch: Box::new(Pattern::Bind("branch".to_string())),
            else_branch: Box::new(Pattern::Ref("branch".to_string())),
        });

        let result = matcher.match_pattern(&pattern, root_id);
        // Should match because both branches are the same (42)
        assert!(result.is_some());
        let result = result.unwrap();
        assert!(result.bindings.contains_key("branch"));
    }

    #[test]
    fn test_match_multiple_nodes() {
        let code = "1 + (2 + (3 + 4))";
        let graph = parse_flc(code).unwrap();

        // Pattern to match all additions
        let pattern = Pattern::NodeType(NodePattern::Application {
            function: Box::new(Pattern::NodeType(NodePattern::Variable {
                name: Some("+".to_string()),
            })),
            args: vec![Pattern::Any, Pattern::Any],
        });

        // Count matches manually by traversing the graph
        let mut match_count = 0;
        for (node_id, _) in graph.nodes() {
            let mut matcher = PatternMatcher::new(&graph);
            if let Some(_) = matcher.match_pattern(&pattern, *node_id) {
                match_count += 1;
            }
        }

        // Should find 3 addition expressions
        assert_eq!(match_count, 3);
    }

    #[test]
    fn test_pattern_equality() {
        let p1 = Pattern::Literal(Literal::Integer(42));
        let p2 = Pattern::Literal(Literal::Integer(42));
        let p3 = Pattern::Literal(Literal::Integer(99));

        assert_eq!(p1, p2);
        assert_ne!(p1, p3);
    }

    #[test]
    fn test_node_pattern_equality() {
        let np1 = NodePattern::Variable {
            name: Some("x".to_string()),
        };
        let np2 = NodePattern::Variable {
            name: Some("x".to_string()),
        };
        let np3 = NodePattern::Variable {
            name: Some("y".to_string()),
        };

        assert_eq!(np1, np2);
        assert_ne!(np1, np3);
    }
}
