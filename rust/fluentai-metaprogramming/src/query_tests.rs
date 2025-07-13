#[cfg(test)]
mod tests {
    use crate::patterns::{MatchResult, NodePattern, Pattern};
    use crate::query::{Constraint, GraphQuery, OrderBy, QueryExecutor, QueryResult, SelectClause};
    use fluentai_core::ast::{Graph, Literal, NodeId};
    use fluentai_parser::parse_flc;
    use std::num::NonZeroU32;

    // ===== GraphQuery Tests =====

    #[test]
        fn test_graph_query_creation() {
        let query = GraphQuery {
            pattern: Pattern::Any,
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        assert!(matches!(query.pattern, Pattern::Any));
        assert!(query.constraints.is_empty());
        assert!(matches!(query.select, SelectClause::All));
        assert!(query.order_by.is_none());
        assert!(query.limit.is_none());
    }

    #[test]
        fn test_graph_query_with_limit() {
        let query = GraphQuery {
            pattern: Pattern::Any,
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: Some(10),
        };

        assert_eq!(query.limit, Some(10));
    }

    #[test]
        fn test_graph_query_with_ordering() {
        let query = GraphQuery {
            pattern: Pattern::Any,
            constraints: vec![],
            select: SelectClause::All,
            order_by: Some(OrderBy {
                field: "depth".to_string(),
                ascending: true,
            }),
            limit: None,
        };

        assert!(query.order_by.is_some());
        let order = query.order_by.unwrap();
        assert_eq!(order.field, "depth");
        assert!(order.ascending);
    }

    // ===== Constraint Tests =====

    #[test]
        fn test_path_constraint() {
        let constraint = Constraint::Path {
            from: "start".to_string(),
            to: "end".to_string(),
            max_length: Some(5),
        };

        match constraint {
            Constraint::Path {
                from,
                to,
                max_length,
            } => {
                assert_eq!(from, "start");
                assert_eq!(to, "end");
                assert_eq!(max_length, Some(5));
            }
            _ => panic!("Expected Path constraint"),
        }
    }

    #[test]
        fn test_ancestor_constraint() {
        let constraint = Constraint::Ancestor {
            ancestor: "parent".to_string(),
            descendant: "child".to_string(),
        };

        match constraint {
            Constraint::Ancestor {
                ancestor,
                descendant,
            } => {
                assert_eq!(ancestor, "parent");
                assert_eq!(descendant, "child");
            }
            _ => panic!("Expected Ancestor constraint"),
        }
    }

    #[test]
        fn test_sibling_constraint() {
        let constraint = Constraint::Sibling {
            node1: "a".to_string(),
            node2: "b".to_string(),
        };

        match constraint {
            Constraint::Sibling { node1, node2 } => {
                assert_eq!(node1, "a");
                assert_eq!(node2, "b");
            }
            _ => panic!("Expected Sibling constraint"),
        }
    }

    #[test]
        fn test_predicate_constraint() {
        let constraint = Constraint::Predicate {
            name: "is_pure".to_string(),
            args: vec!["func".to_string()],
        };

        match constraint {
            Constraint::Predicate { name, args } => {
                assert_eq!(name, "is_pure");
                assert_eq!(args, vec!["func"]);
            }
            _ => panic!("Expected Predicate constraint"),
        }
    }

    // ===== SelectClause Tests =====

    #[test]
        fn test_select_all() {
        let select = SelectClause::All;
        assert!(matches!(select, SelectClause::All));
    }

    #[test]
        fn test_select_bindings() {
        let select = SelectClause::Bindings(vec!["x".to_string(), "y".to_string()]);
        match select {
            SelectClause::Bindings(bindings) => {
                assert_eq!(bindings.len(), 2);
                assert_eq!(bindings[0], "x");
                assert_eq!(bindings[1], "y");
            }
            _ => panic!("Expected Bindings"),
        }
    }

    #[test]
        fn test_select_transform() {
        let select = SelectClause::Transform("node_type".to_string());
        match select {
            SelectClause::Transform(transform) => {
                assert_eq!(transform, "node_type");
            }
            _ => panic!("Expected Transform"),
        }
    }

    #[test]
        fn test_select_count() {
        let select = SelectClause::Count;
        assert!(matches!(select, SelectClause::Count));
    }

    // ===== QueryResult Tests =====

    #[test]
        fn test_query_result_empty() {
        let result = QueryResult {
            matches: vec![],
            total_count: 0,
        };

        assert!(result.matches.is_empty());
        assert_eq!(result.total_count, 0);
    }

    #[test]
        fn test_query_result_with_matches() {
        let mut match1 = MatchResult::new();
        match1
            .matched_nodes
            .push(NodeId(NonZeroU32::new(1).unwrap()));

        let mut match2 = MatchResult::new();
        match2
            .matched_nodes
            .push(NodeId(NonZeroU32::new(2).unwrap()));

        let result = QueryResult {
            matches: vec![match1, match2],
            total_count: 2,
        };

        assert_eq!(result.matches.len(), 2);
        assert_eq!(result.total_count, 2);
    }

    // ===== QueryExecutor Tests =====

    #[test]
        fn test_query_executor_new() {
        let graph = Graph::new();
        let executor = QueryExecutor::new(&graph);
        // Just verify it can be created
    }

    #[test]
        fn test_execute_simple_query() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::Literal(Literal::Integer(1)),
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.total_count, 1);
    }

    #[test]
        fn test_execute_query_with_bindings() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::NodeType(NodePattern::Application {
                function: Box::new(Pattern::Bind("op".to_string())),
                args: vec![
                    Pattern::Bind("a".to_string()),
                    Pattern::Bind("b".to_string()),
                ],
            }),
            constraints: vec![],
            select: SelectClause::Bindings(vec!["op".to_string(), "a".to_string()]),
            order_by: None,
            limit: None,
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());
    }

    #[test]
        fn test_execute_query_with_limit() {
        let code = "[1, 2, 3, 4, 5]";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::Literal(Literal::Integer(1)),
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: Some(2),
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());

        if let Ok(result) = result {
            assert!(result.matches.len() <= 2);
        }
    }

    #[test]
        fn test_execute_query_count() {
        let code = "1 + (2 + (3 + 4))";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::NodeType(NodePattern::Application {
                function: Box::new(Pattern::Any),
                args: vec![Pattern::Any, Pattern::Any],
            }),
            constraints: vec![],
            select: SelectClause::Count,
            order_by: None,
            limit: None,
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());

        if let Ok(result) = result {
            // Should count 3 application nodes
            assert_eq!(result.total_count, 3);
        }
    }

    #[test]
        fn test_execute_query_no_matches() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::Literal(Literal::String("not found".to_string())),
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());

        let result = result.unwrap();
        assert_eq!(result.total_count, 0);
        assert!(result.matches.is_empty());
    }

    #[test]
        fn test_query_with_ancestor_constraint() {
        let code = "let x = 1; x + 2";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);
        let query = GraphQuery {
            pattern: Pattern::And(vec![
                Pattern::Bind("parent".to_string()),
                Pattern::NodeType(NodePattern::Let {
                    bindings: vec![],
                    body: Box::new(Pattern::Bind("child".to_string())),
                }),
            ]),
            constraints: vec![Constraint::Ancestor {
                ancestor: "parent".to_string(),
                descendant: "child".to_string(),
            }],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        // Constraint checking might not be fully implemented
        let _ = executor.execute(&query);
        // TODO: Once constraints are implemented, verify the ancestor relationship is checked
    }

    #[test]
        fn test_complex_query() {
        let code = "private function factorial(n) { if (n <= 1) { 1 } else { n * factorial(n - 1) } }";
        let graph = parse_flc(code).unwrap();

        let executor = QueryExecutor::new(&graph);

        // Find all recursive calls
        let query = GraphQuery {
            pattern: Pattern::NodeType(NodePattern::Application {
                function: Box::new(Pattern::NodeType(NodePattern::Variable {
                    name: Some("factorial".to_string()),
                })),
                args: vec![Pattern::Any],
            }),
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        let result = executor.execute(&query);
        assert!(result.is_ok());

        if let Ok(result) = result {
            // Should find the recursive call
            assert!(result.total_count > 0);
        }
    }

    #[test]
        fn test_query_builder_pattern() {
        // Test that queries can be built incrementally
        let mut query = GraphQuery {
            pattern: Pattern::Any,
            constraints: vec![],
            select: SelectClause::All,
            order_by: None,
            limit: None,
        };

        // Add constraint
        query.constraints.push(Constraint::Predicate {
            name: "is_function".to_string(),
            args: vec!["node".to_string()],
        });

        // Set limit
        query.limit = Some(5);

        // Change selection
        query.select = SelectClause::Count;

        assert_eq!(query.constraints.len(), 1);
        assert_eq!(query.limit, Some(5));
        assert!(matches!(query.select, SelectClause::Count));
    }
}
