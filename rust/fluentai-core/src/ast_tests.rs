#[cfg(test)]
mod tests {
    use crate::ast::*;

    // ===== NodeId Tests =====

    #[test]
    fn test_node_id_creation() {
        // Valid NodeId
        let id = NodeId::new(1).unwrap();
        assert_eq!(id.get(), 1);

        // Invalid NodeId (0)
        assert!(NodeId::new(0).is_none());

        // Large NodeId
        let large_id = NodeId::new(u32::MAX).unwrap();
        assert_eq!(large_id.get(), u32::MAX);
    }

    #[test]
    fn test_node_id_display() {
        let id = NodeId::new(42).unwrap();
        assert_eq!(format!("{}", id), "n42");
    }

    #[test]
    fn test_node_id_equality() {
        let id1 = NodeId::new(1).unwrap();
        let id2 = NodeId::new(1).unwrap();
        let id3 = NodeId::new(2).unwrap();

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    // ===== Graph Tests =====

    #[test]
    fn test_graph_creation() {
        let graph = Graph::new();
        assert!(graph.nodes.is_empty());
        assert!(graph.root_id.is_none());
        assert!(graph.metadata.is_empty());
    }

    #[test]
    fn test_add_node() {
        let mut graph = Graph::new();

        // Add a literal node
        let node = Node::Literal(Literal::Integer(42));
        let id = graph.add_node(node.clone()).unwrap();

        assert_eq!(id.get(), 1);
        assert_eq!(graph.nodes.len(), 1);
        assert!(graph.get_node(id).is_some());

        // Add another node
        let node2 = Node::Literal(Literal::String("hello".to_string()));
        let id2 = graph.add_node(node2).unwrap();

        assert_eq!(id2.get(), 2);
        assert_eq!(graph.nodes.len(), 2);
    }

    #[test]
    fn test_add_node_overflow() {
        let mut graph = Graph::new();

        // Set next_id to u32::MAX to trigger overflow
        graph.next_id = u32::MAX;

        // Try to add a node - should fail
        let node = Node::Literal(Literal::Integer(42));
        let result = graph.add_node(node);

        assert!(result.is_err());
        match result.unwrap_err() {
            crate::error::Error::GraphNodeIdOverflow => (),
            _ => panic!("Expected GraphNodeIdOverflow error"),
        }
    }

    #[test]
    fn test_get_node() {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::Integer(42));
        let id = graph.add_node(node.clone()).unwrap();

        // Get existing node
        let retrieved = graph.get_node(id).unwrap();
        match retrieved {
            Node::Literal(Literal::Integer(n)) => assert_eq!(*n, 42),
            _ => panic!("Wrong node type"),
        }

        // Get non-existent node
        let fake_id = NodeId::new(999).unwrap();
        assert!(graph.get_node(fake_id).is_none());
    }

    #[test]
    fn test_get_node_mut() {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::Integer(42));
        let id = graph.add_node(node).unwrap();

        // Modify node
        if let Some(Node::Literal(lit)) = graph.get_node_mut(id) {
            *lit = Literal::Integer(100);
        }

        // Verify modification
        if let Some(Node::Literal(Literal::Integer(n))) = graph.get_node(id) {
            assert_eq!(*n, 100);
        } else {
            panic!("Node modification failed");
        }
    }

    #[test]
    fn test_metadata_operations() {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::Integer(42));
        let id = graph.add_node(node).unwrap();

        // Initially no metadata
        assert!(graph.get_metadata(id).is_none());

        // Set metadata
        let mut metadata = NodeMetadata::default();
        metadata.type_info = Some("Integer".to_string());
        metadata.is_pure = Some(true);
        graph.set_metadata(id, metadata);

        // Get metadata
        let retrieved = graph.get_metadata(id).unwrap();
        assert_eq!(retrieved.type_info, Some("Integer".to_string()));
        assert_eq!(retrieved.is_pure, Some(true));

        // Modify metadata
        graph.metadata_mut(id).annotations.push("test".to_string());
        assert_eq!(graph.get_metadata(id).unwrap().annotations.len(), 1);
    }

    #[test]
    fn test_documentation_operations() {
        let mut graph = Graph::new();
        let node = Node::Literal(Literal::Integer(42));
        let id = graph.add_node(node).unwrap();

        // Set documentation
        graph.set_documentation(id, "doc123".to_string());
        assert_eq!(graph.get_documentation(id).unwrap(), "doc123");

        // Non-existent node
        let fake_id = NodeId::new(999).unwrap();
        assert!(graph.get_documentation(fake_id).is_none());
    }

    #[test]
    fn test_node_ids_iterator() {
        let mut graph = Graph::new();

        // Add multiple nodes
        let id1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let id2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let id3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();

        let ids: Vec<NodeId> = graph.node_ids().collect();
        assert_eq!(ids.len(), 3);
        assert!(ids.contains(&id1));
        assert!(ids.contains(&id2));
        assert!(ids.contains(&id3));
    }

    #[test]
    fn test_nodes_iterator() {
        let mut graph = Graph::new();

        // Add various node types
        let int_id = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let str_id = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .unwrap();
        let var_id = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();

        let nodes: Vec<(&NodeId, &Node)> = graph.nodes().collect();
        assert_eq!(nodes.len(), 3);

        // Verify each node exists
        let node_map: std::collections::HashMap<_, _> = nodes.into_iter().collect();
        assert!(matches!(
            node_map.get(&int_id),
            Some(Node::Literal(Literal::Integer(42)))
        ));
        assert!(
            matches!(node_map.get(&str_id), Some(Node::Literal(Literal::String(s))) if s == "hello")
        );
        assert!(matches!(node_map.get(&var_id), Some(Node::Variable { name }) if name == "x"));
    }

    #[test]
    fn test_children() {
        let mut graph = Graph::new();

        // Create a simple if expression
        let cond = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();
        let then_branch = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let else_branch = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();

        let if_node = graph
            .add_node(Node::If {
                condition: cond,
                then_branch,
                else_branch,
            })
            .unwrap();

        let children = graph.children(if_node);
        assert_eq!(children.len(), 3);
        assert!(children.contains(&cond));
        assert!(children.contains(&then_branch));
        assert!(children.contains(&else_branch));

        // Test leaf node
        let leaf_children = graph.children(cond);
        assert!(leaf_children.is_empty());
    }

    #[test]
    fn test_dfs_traversal() {
        let mut graph = Graph::new();

        // Create a simple expression tree: (+ 1 2)
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .unwrap();
        let app = graph
            .add_node(Node::Application {
                function: plus,
                args: vec![one, two],
            })
            .unwrap();

        graph.root_id = Some(app);

        // Collect visited nodes
        let mut visited = Vec::new();
        graph.dfs_from(app, |id, _| {
            visited.push(id);
        });

        assert_eq!(visited.len(), 4);
        assert!(visited.contains(&app));
        assert!(visited.contains(&plus));
        assert!(visited.contains(&one));
        assert!(visited.contains(&two));
    }

    #[test]
    fn test_dfs_traversal_all_node_types() {
        let mut graph = Graph::new();

        // Test Lambda traversal
        let body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string()],
                body,
            })
            .unwrap();
        let mut visited = Vec::new();
        graph.dfs_from(lambda, |id, _| visited.push(id));
        assert_eq!(visited.len(), 2);

        // Test Let traversal
        let value = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .unwrap();
        let let_body = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let let_node = graph
            .add_node(Node::Let {
                bindings: vec![("x".to_string(), value)],
                body: let_body,
            })
            .unwrap();
        visited.clear();
        graph.dfs_from(let_node, |id, _| visited.push(id));
        assert_eq!(visited.len(), 3);

        // Test Letrec traversal
        let rec_value = graph.add_node(Node::Literal(Literal::Nil)).unwrap();
        let rec_body = graph
            .add_node(Node::Variable {
                name: "f".to_string(),
            })
            .unwrap();
        let letrec = graph
            .add_node(Node::Letrec {
                bindings: vec![("f".to_string(), rec_value)],
                body: rec_body,
            })
            .unwrap();
        visited.clear();
        graph.dfs_from(letrec, |id, _| visited.push(id));
        assert_eq!(visited.len(), 3);

        // Test If traversal
        let cond = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();
        let then_br = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let else_br = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let if_node = graph
            .add_node(Node::If {
                condition: cond,
                then_branch: then_br,
                else_branch: else_br,
            })
            .unwrap();
        visited.clear();
        graph.dfs_from(if_node, |id, _| visited.push(id));
        assert_eq!(visited.len(), 4);

        // Test Match traversal
        let match_expr = graph.add_node(Node::Literal(Literal::Integer(5))).unwrap();
        let branch1 = graph
            .add_node(Node::Literal(Literal::String("five".to_string())))
            .unwrap();
        let branch2 = graph
            .add_node(Node::Literal(Literal::String("other".to_string())))
            .unwrap();
        let match_node = graph
            .add_node(Node::Match {
                expr: match_expr,
                branches: vec![
                    (Pattern::Literal(Literal::Integer(5)), branch1),
                    (Pattern::Wildcard, branch2),
                ],
            })
            .unwrap();
        visited.clear();
        graph.dfs_from(match_node, |id, _| visited.push(id));
        assert_eq!(visited.len(), 4);

        // Test List traversal
        let item1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let item2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let list = graph.add_node(Node::List(vec![item1, item2])).unwrap();
        visited.clear();
        graph.dfs_from(list, |id, _| visited.push(id));
        assert_eq!(visited.len(), 3);

        // Test Effect traversal
        let arg1 = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .unwrap();
        let arg2 = graph
            .add_node(Node::Literal(Literal::String("world".to_string())))
            .unwrap();
        let effect = graph
            .add_node(Node::Effect {
                effect_type: EffectType::IO,
                operation: "print".to_string(),
                args: vec![arg1, arg2],
            })
            .unwrap();
        visited.clear();
        graph.dfs_from(effect, |id, _| visited.push(id));
        assert_eq!(visited.len(), 3);
    }

    #[test]
    fn test_dfs_with_cycles() {
        let mut graph = Graph::new();

        // Create nodes that would form a cycle if we didn't track visited
        let a = graph
            .add_node(Node::Variable {
                name: "a".to_string(),
            })
            .unwrap();
        let b = graph
            .add_node(Node::Variable {
                name: "b".to_string(),
            })
            .unwrap();
        let c = graph
            .add_node(Node::Variable {
                name: "c".to_string(),
            })
            .unwrap();

        // Create applications that reference each other
        let app1 = graph
            .add_node(Node::Application {
                function: a,
                args: vec![b],
            })
            .unwrap();
        let _app2 = graph
            .add_node(Node::Application {
                function: b,
                args: vec![c],
            })
            .unwrap();
        let app3 = graph
            .add_node(Node::Application {
                function: c,
                args: vec![app1], // This creates a cycle
            })
            .unwrap();

        // Start from app3 and verify we visit each node only once
        let mut visited_count = std::collections::HashMap::new();
        graph.dfs_from(app3, |id, _| {
            *visited_count.entry(id).or_insert(0) += 1;
        });

        // Each node should be visited exactly once
        for (_, count) in visited_count {
            assert_eq!(count, 1);
        }
    }

    // ===== Literal Tests =====

    #[test]
    fn test_literal_display() {
        assert_eq!(format!("{}", Literal::Integer(42)), "42");
        assert_eq!(format!("{}", Literal::Float(3.14)), "3.14");
        assert_eq!(
            format!("{}", Literal::String("hello".to_string())),
            "\"hello\""
        );
        assert_eq!(format!("{}", Literal::Boolean(true)), "true");
        assert_eq!(format!("{}", Literal::Boolean(false)), "false");
        assert_eq!(format!("{}", Literal::Nil), "nil");
    }

    #[test]
    fn test_literal_equality() {
        assert_eq!(Literal::Integer(42), Literal::Integer(42));
        assert_ne!(Literal::Integer(42), Literal::Integer(43));

        assert_eq!(
            Literal::String("hello".to_string()),
            Literal::String("hello".to_string())
        );
        assert_ne!(
            Literal::String("hello".to_string()),
            Literal::String("world".to_string())
        );

        assert_eq!(Literal::Nil, Literal::Nil);
    }

    // ===== EffectType Tests =====

    #[test]
    fn test_effect_type_display() {
        assert_eq!(format!("{}", EffectType::Pure), "Pure");
        assert_eq!(format!("{}", EffectType::IO), "IO");
        assert_eq!(format!("{}", EffectType::State), "State");
        assert_eq!(format!("{}", EffectType::Error), "Error");
        assert_eq!(format!("{}", EffectType::Time), "Time");
        assert_eq!(format!("{}", EffectType::Network), "Network");
        assert_eq!(format!("{}", EffectType::Random), "Random");
        assert_eq!(format!("{}", EffectType::Dom), "Dom");
        assert_eq!(format!("{}", EffectType::Async), "Async");
        assert_eq!(format!("{}", EffectType::Concurrent), "Concurrent");
    }

    #[test]
    fn test_effect_type_equality() {
        assert_eq!(EffectType::Pure, EffectType::Pure);
        assert_ne!(EffectType::Pure, EffectType::IO);
    }

    // ===== Node Type Tests =====

    #[test]
    fn test_lambda_node() {
        let mut graph = Graph::new();
        let body = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string()],
                body,
            })
            .unwrap();

        if let Some(Node::Lambda { params, body: b }) = graph.get_node(lambda) {
            assert_eq!(params.len(), 1);
            assert_eq!(params[0], "x");
            assert_eq!(*b, body);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_let_node() {
        let mut graph = Graph::new();
        let value = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let body = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();

        let let_node = graph
            .add_node(Node::Let {
                bindings: vec![("x".to_string(), value)],
                body,
            })
            .unwrap();

        if let Some(Node::Let { bindings, body: b }) = graph.get_node(let_node) {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "x");
            assert_eq!(bindings[0].1, value);
            assert_eq!(*b, body);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_list_node() {
        let mut graph = Graph::new();
        let item1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let item2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let item3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();

        let list = graph
            .add_node(Node::List(vec![item1, item2, item3]))
            .unwrap();

        if let Some(Node::List(items)) = graph.get_node(list) {
            assert_eq!(items.len(), 3);
            assert_eq!(items[0], item1);
            assert_eq!(items[1], item2);
            assert_eq!(items[2], item3);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_match_node() {
        let mut graph = Graph::new();
        let expr = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let branch1 = graph
            .add_node(Node::Literal(Literal::String("forty-two".to_string())))
            .unwrap();
        let branch2 = graph
            .add_node(Node::Literal(Literal::String("other".to_string())))
            .unwrap();

        let match_node = graph
            .add_node(Node::Match {
                expr,
                branches: vec![
                    (Pattern::Literal(Literal::Integer(42)), branch1),
                    (Pattern::Wildcard, branch2),
                ],
            })
            .unwrap();

        if let Some(Node::Match { expr: e, branches }) = graph.get_node(match_node) {
            assert_eq!(*e, expr);
            assert_eq!(branches.len(), 2);

            match &branches[0].0 {
                Pattern::Literal(Literal::Integer(n)) => assert_eq!(*n, 42),
                _ => panic!("Wrong pattern"),
            }

            assert!(matches!(branches[1].0, Pattern::Wildcard));
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_effect_node() {
        let mut graph = Graph::new();
        let arg = graph
            .add_node(Node::Literal(Literal::String("Hello, World!".to_string())))
            .unwrap();

        let effect = graph
            .add_node(Node::Effect {
                effect_type: EffectType::IO,
                operation: "print".to_string(),
                args: vec![arg],
            })
            .unwrap();

        if let Some(Node::Effect {
            effect_type,
            operation,
            args,
        }) = graph.get_node(effect)
        {
            assert_eq!(*effect_type, EffectType::IO);
            assert_eq!(operation, "print");
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], arg);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_module_node() {
        let mut graph = Graph::new();
        let body = graph.add_node(Node::Literal(Literal::Nil)).unwrap();

        let module = graph
            .add_node(Node::Module {
                name: "MyModule".to_string(),
                exports: vec!["foo".to_string(), "bar".to_string()],
                body,
            })
            .unwrap();

        if let Some(Node::Module {
            name,
            exports,
            body: b,
        }) = graph.get_node(module)
        {
            assert_eq!(name, "MyModule");
            assert_eq!(exports.len(), 2);
            assert_eq!(exports[0], "foo");
            assert_eq!(exports[1], "bar");
            assert_eq!(*b, body);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_iterative_dfs() {
        let mut graph = Graph::new();

        // Create a simple tree structure
        let leaf1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let leaf2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let leaf3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();

        let branch1 = graph.add_node(Node::List(vec![leaf1, leaf2])).unwrap();
        let root = graph.add_node(Node::List(vec![branch1, leaf3])).unwrap();

        // Collect nodes visited in DFS order
        let mut visited = Vec::new();
        graph.dfs_iterative(root, &mut |id, _node| {
            visited.push(id);
        });

        // Should visit root first, then branch1, then leaves in order
        assert_eq!(visited.len(), 5);
        assert_eq!(visited[0], root);
        assert_eq!(visited[1], branch1);
        assert_eq!(visited[2], leaf1);
        assert_eq!(visited[3], leaf2);
        assert_eq!(visited[4], leaf3);
    }

    #[test]
    fn test_deep_graph_iterative_dfs() {
        let mut graph = Graph::new();

        // Create a deep nested structure that would overflow with recursive DFS
        let mut current = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();

        // Create 10000 nested nodes
        for i in 1..10000 {
            current = graph
                .add_node(Node::Lambda {
                    params: vec![format!("x{}", i)],
                    body: current,
                })
                .unwrap();
        }

        // This should not stack overflow with iterative implementation
        let mut count = 0;
        graph.dfs_iterative(current, &mut |_id, _node| {
            count += 1;
        });

        assert_eq!(count, 10000);
    }

    #[test]
    fn test_async_constructs() {
        let mut graph = Graph::new();

        // Test Async node
        let body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let async_node = graph.add_node(Node::Async { body }).unwrap();

        if let Some(Node::Async { body: b }) = graph.get_node(async_node) {
            assert_eq!(*b, body);
        } else {
            panic!("Wrong node type");
        }

        // Test Channel node
        let channel = graph.add_node(Node::Channel { capacity: None }).unwrap();
        assert!(matches!(graph.get_node(channel), Some(Node::Channel { .. })));

        // Test Send node
        let value = graph
            .add_node(Node::Literal(Literal::String("message".to_string())))
            .unwrap();
        let send = graph.add_node(Node::Send { channel, value }).unwrap();

        if let Some(Node::Send {
            channel: c,
            value: v,
        }) = graph.get_node(send)
        {
            assert_eq!(*c, channel);
            assert_eq!(*v, value);
        } else {
            panic!("Wrong node type");
        }
    }

    // ===== Pattern Tests =====

    #[test]
    fn test_pattern_types() {
        // Variable pattern
        let var_pattern = Pattern::Variable("x".to_string());
        match var_pattern {
            Pattern::Variable(name) => assert_eq!(name, "x"),
            _ => panic!("Wrong pattern type"),
        }

        // Literal pattern
        let lit_pattern = Pattern::Literal(Literal::Integer(42));
        match lit_pattern {
            Pattern::Literal(Literal::Integer(n)) => assert_eq!(n, 42),
            _ => panic!("Wrong pattern type"),
        }

        // Constructor pattern
        let cons_pattern = Pattern::Constructor {
            name: "Cons".to_string(),
            patterns: vec![
                Pattern::Variable("head".to_string()),
                Pattern::Variable("tail".to_string()),
            ],
        };

        match cons_pattern {
            Pattern::Constructor { name, patterns } => {
                assert_eq!(name, "Cons");
                assert_eq!(patterns.len(), 2);
            }
            _ => panic!("Wrong pattern type"),
        }

        // Wildcard pattern
        assert!(matches!(Pattern::Wildcard, Pattern::Wildcard));
    }

    // ===== Complex Graph Tests =====

    #[test]
    fn test_complex_graph() {
        let mut graph = Graph::new();

        // Build: let x = 10 in let y = 20 in x + y
        let ten = graph.add_node(Node::Literal(Literal::Integer(10))).unwrap();
        let twenty = graph.add_node(Node::Literal(Literal::Integer(20))).unwrap();

        let x_var = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let y_var = graph
            .add_node(Node::Variable {
                name: "y".to_string(),
            })
            .unwrap();
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .unwrap();

        let add_expr = graph
            .add_node(Node::Application {
                function: plus,
                args: vec![x_var, y_var],
            })
            .unwrap();

        let inner_let = graph
            .add_node(Node::Let {
                bindings: vec![("y".to_string(), twenty)],
                body: add_expr,
            })
            .unwrap();

        let outer_let = graph
            .add_node(Node::Let {
                bindings: vec![("x".to_string(), ten)],
                body: inner_let,
            })
            .unwrap();

        graph.root_id = Some(outer_let);

        // Verify structure
        assert_eq!(graph.nodes.len(), 8);

        // Traverse and count nodes
        let mut count = 0;
        graph.dfs_from(outer_let, |_, _| count += 1);
        assert_eq!(count, 8);
    }

    #[test]
    fn test_context_memory() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Create context memory
        let context = ContextMemory {
            embedding_id: None,
            usage_stats: UsageStatistics {
                execution_count: 0,
                avg_execution_time_ns: 0,
                error_count: 0,
                is_hot_path: false,
            },
            rationale: Some("Test node".to_string()),
            performance_hints: vec![PerformanceHint {
                hint_type: PerformanceHintType::ShouldInline,
                confidence: 0.9,
                context: None,
            }],
            semantic_tags: vec!["test".to_string()],
            last_modified: None,
        };

        graph.set_context_memory(node, context);

        // Verify retrieval
        let retrieved = graph.get_context_memory(node).unwrap();
        assert_eq!(retrieved.rationale, Some("Test node".to_string()));
        assert_eq!(retrieved.semantic_tags.len(), 1);
        assert_eq!(retrieved.performance_hints.len(), 1);

        // Update usage stats
        graph.update_usage_stats(node, |stats| {
            stats.execution_count += 1;
        });

        assert_eq!(
            graph
                .get_context_memory(node)
                .unwrap()
                .usage_stats
                .execution_count,
            1
        );
    }

    // ===== Advanced Node Type Tests =====

    #[test]
    fn test_import_export_nodes() {
        let mut graph = Graph::new();

        // Test Import node
        let import = graph
            .add_node(Node::Import {
                module_path: "std/math".to_string(),
                import_list: vec![
                    ImportItem {
                        name: "sin".to_string(),
                        alias: None,
                    },
                    ImportItem {
                        name: "cos".to_string(),
                        alias: Some("cosine".to_string()),
                    },
                ],
                import_all: false,
            })
            .unwrap();

        if let Some(Node::Import {
            module_path,
            import_list,
            import_all,
        }) = graph.get_node(import)
        {
            assert_eq!(module_path, "std/math");
            assert_eq!(import_list.len(), 2);
            assert_eq!(import_list[0].name, "sin");
            assert!(import_list[0].alias.is_none());
            assert_eq!(import_list[1].name, "cos");
            assert_eq!(import_list[1].alias, Some("cosine".to_string()));
            assert!(!import_all);
        } else {
            panic!("Wrong node type");
        }

        // Test Export node
        let export = graph
            .add_node(Node::Export {
                export_list: vec![
                    ExportItem {
                        name: "foo".to_string(),
                        alias: None,
                    },
                    ExportItem {
                        name: "bar".to_string(),
                        alias: Some("baz".to_string()),
                    },
                ],
            })
            .unwrap();

        if let Some(Node::Export { export_list }) = graph.get_node(export) {
            assert_eq!(export_list.len(), 2);
            assert_eq!(export_list[0].name, "foo");
            assert!(export_list[0].alias.is_none());
            assert_eq!(export_list[1].name, "bar");
            assert_eq!(export_list[1].alias, Some("baz".to_string()));
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_qualified_variable() {
        let mut graph = Graph::new();

        let qual_var = graph
            .add_node(Node::QualifiedVariable {
                module_name: "math".to_string(),
                variable_name: "pi".to_string(),
            })
            .unwrap();

        if let Some(Node::QualifiedVariable {
            module_name,
            variable_name,
        }) = graph.get_node(qual_var)
        {
            assert_eq!(module_name, "math");
            assert_eq!(variable_name, "pi");
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_await_node() {
        let mut graph = Graph::new();

        let body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let async_expr = graph.add_node(Node::Async { body }).unwrap();

        let await_node = graph.add_node(Node::Await { expr: async_expr }).unwrap();

        if let Some(Node::Await { expr }) = graph.get_node(await_node) {
            assert_eq!(*expr, async_expr);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_spawn_node() {
        let mut graph = Graph::new();

        let computation = graph
            .add_node(Node::Literal(Literal::String("compute".to_string())))
            .unwrap();
        let spawn = graph.add_node(Node::Spawn { expr: computation }).unwrap();

        if let Some(Node::Spawn { expr }) = graph.get_node(spawn) {
            assert_eq!(*expr, computation);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_receive_node() {
        let mut graph = Graph::new();

        let channel = graph.add_node(Node::Channel { capacity: None }).unwrap();
        let receive = graph.add_node(Node::Receive { channel }).unwrap();

        if let Some(Node::Receive { channel: ch }) = graph.get_node(receive) {
            assert_eq!(*ch, channel);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_letrec_node() {
        let mut graph = Graph::new();

        // Create letrec for factorial function
        let _n_var = graph
            .add_node(Node::Variable {
                name: "n".to_string(),
            })
            .unwrap();
        let fact_var = graph
            .add_node(Node::Variable {
                name: "fact".to_string(),
            })
            .unwrap();

        let fact_body = graph.add_node(Node::Literal(Literal::Nil)).unwrap(); // Simplified
        let fact_lambda = graph
            .add_node(Node::Lambda {
                params: vec!["n".to_string()],
                body: fact_body,
            })
            .unwrap();

        let letrec = graph
            .add_node(Node::Letrec {
                bindings: vec![("fact".to_string(), fact_lambda)],
                body: fact_var,
            })
            .unwrap();

        if let Some(Node::Letrec { bindings, body }) = graph.get_node(letrec) {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "fact");
            assert_eq!(bindings[0].1, fact_lambda);
            assert_eq!(*body, fact_var);
        } else {
            panic!("Wrong node type");
        }
    }

    #[test]
    fn test_contract_node() {
        let mut graph = Graph::new();

        let precond = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();
        let postcond = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();
        let invariant = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();

        let contract = graph
            .add_node(Node::Contract {
                function_name: "factorial".to_string(),
                preconditions: vec![precond],
                postconditions: vec![postcond],
                invariants: vec![invariant],
                complexity: Some("O(n)".to_string()),
                pure: true,
            })
            .unwrap();

        if let Some(Node::Contract {
            function_name,
            preconditions,
            postconditions,
            invariants,
            complexity,
            pure,
        }) = graph.get_node(contract)
        {
            assert_eq!(function_name, "factorial");
            assert_eq!(preconditions.len(), 1);
            assert_eq!(postconditions.len(), 1);
            assert_eq!(invariants.len(), 1);
            assert_eq!(complexity, &Some("O(n)".to_string()));
            assert!(pure);
        } else {
            panic!("Wrong node type");
        }
    }

    // ===== Advanced Pattern Tests =====

    #[test]
    fn test_nested_patterns() {
        // Test nested constructor patterns: Cons(x, Cons(y, Nil))
        let inner_cons = Pattern::Constructor {
            name: "Cons".to_string(),
            patterns: vec![
                Pattern::Variable("y".to_string()),
                Pattern::Literal(Literal::Nil),
            ],
        };

        let outer_cons = Pattern::Constructor {
            name: "Cons".to_string(),
            patterns: vec![Pattern::Variable("x".to_string()), inner_cons.clone()],
        };

        match &outer_cons {
            Pattern::Constructor { name, patterns } => {
                assert_eq!(name, "Cons");
                assert_eq!(patterns.len(), 2);

                // Check first pattern is variable
                assert!(matches!(&patterns[0], Pattern::Variable(v) if v == "x"));

                // Check second pattern is nested constructor
                match &patterns[1] {
                    Pattern::Constructor {
                        name: inner_name,
                        patterns: inner_patterns,
                    } => {
                        assert_eq!(inner_name, "Cons");
                        assert_eq!(inner_patterns.len(), 2);
                        assert!(matches!(&inner_patterns[0], Pattern::Variable(v) if v == "y"));
                        assert!(matches!(&inner_patterns[1], Pattern::Literal(Literal::Nil)));
                    }
                    _ => panic!("Expected nested constructor pattern"),
                }
            }
            _ => panic!("Expected constructor pattern"),
        }
    }

    #[test]
    fn test_pattern_equality() {
        let pat1 = Pattern::Variable("x".to_string());
        let pat2 = Pattern::Variable("x".to_string());
        let pat3 = Pattern::Variable("y".to_string());

        // Same variable patterns should be equal
        assert_eq!(pat1, pat2);
        assert_ne!(pat1, pat3);

        let lit1 = Pattern::Literal(Literal::Integer(42));
        let lit2 = Pattern::Literal(Literal::Integer(42));
        let lit3 = Pattern::Literal(Literal::Integer(43));

        assert_eq!(lit1, lit2);
        assert_ne!(lit1, lit3);

        // Different pattern types are not equal
        assert_ne!(pat1, lit1);
        assert_ne!(Pattern::Wildcard, pat1);
    }

    // ===== Graph Manipulation Tests =====

    #[test]
    fn test_graph_node_removal() {
        let mut graph = Graph::new();

        let node1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let node2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let node3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();

        assert_eq!(graph.nodes.len(), 3);

        // Remove node2
        graph.nodes.remove(&node2);

        assert_eq!(graph.nodes.len(), 2);
        assert!(graph.get_node(node1).is_some());
        assert!(graph.get_node(node2).is_none());
        assert!(graph.get_node(node3).is_some());
    }

    #[test]
    fn test_graph_metadata_persistence() {
        let mut graph = Graph::new();

        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Set multiple metadata fields
        let meta = graph.metadata_mut(node);
        meta.span = Some((10, 20));
        meta.type_info = Some("Int".to_string());
        meta.is_pure = Some(true);
        meta.annotations.push("const".to_string());
        meta.annotations.push("inline".to_string());

        // Verify all fields persist
        let retrieved = graph.get_metadata(node).unwrap();
        assert_eq!(retrieved.span, Some((10, 20)));
        assert_eq!(retrieved.type_info, Some("Int".to_string()));
        assert_eq!(retrieved.is_pure, Some(true));
        assert_eq!(retrieved.annotations.len(), 2);
        assert_eq!(retrieved.annotations[0], "const");
        assert_eq!(retrieved.annotations[1], "inline");
    }

    #[test]
    fn test_children_of_complex_nodes() {
        let mut graph = Graph::new();

        // Test children of Match node
        let expr = graph.add_node(Node::Literal(Literal::Integer(5))).unwrap();
        let branch1 = graph
            .add_node(Node::Literal(Literal::String("five".to_string())))
            .unwrap();
        let branch2 = graph
            .add_node(Node::Literal(Literal::String("other".to_string())))
            .unwrap();

        let match_node = graph
            .add_node(Node::Match {
                expr,
                branches: vec![
                    (Pattern::Literal(Literal::Integer(5)), branch1),
                    (Pattern::Wildcard, branch2),
                ],
            })
            .unwrap();

        let children = graph.children(match_node);
        assert_eq!(children.len(), 3);
        assert!(children.contains(&expr));
        assert!(children.contains(&branch1));
        assert!(children.contains(&branch2));

        // Test children of Effect node
        let arg1 = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .unwrap();
        let arg2 = graph
            .add_node(Node::Literal(Literal::String("world".to_string())))
            .unwrap();

        let effect = graph
            .add_node(Node::Effect {
                effect_type: EffectType::IO,
                operation: "print2".to_string(),
                args: vec![arg1, arg2],
            })
            .unwrap();

        let effect_children = graph.children(effect);
        assert_eq!(effect_children.len(), 2);
        assert!(effect_children.contains(&arg1));
        assert!(effect_children.contains(&arg2));
    }

    #[test]
    fn test_performance_hints() {
        let hint1 = PerformanceHint {
            hint_type: PerformanceHintType::ShouldInline,
            confidence: 0.9,
            context: Some("Hot path".to_string()),
        };

        let _hint2 = PerformanceHint {
            hint_type: PerformanceHintType::CanVectorize { simd_width: None },
            confidence: 0.7,
            context: None,
        };

        let hint3 = PerformanceHint {
            hint_type: PerformanceHintType::Custom("GPU candidate".to_string()),
            confidence: 0.5,
            context: Some("Matrix operations".to_string()),
        };

        // Verify hint types
        match &hint1.hint_type {
            PerformanceHintType::ShouldInline => (),
            _ => panic!("Wrong hint type"),
        }

        match &hint3.hint_type {
            PerformanceHintType::Custom(msg) => assert_eq!(msg, "GPU candidate"),
            _ => panic!("Wrong hint type"),
        }
    }

    // ===== Node Documentation Tests =====

    #[test]
    fn test_node_documentation() {
        use crate::documentation::{DocumentationCategory, DocumentationVisibility};

        // Test literal documentation
        let int_node = Node::Literal(Literal::Integer(42));
        let int_docs = int_node.get_node_docs();
        assert_eq!(int_docs.name, "Integer");
        assert!(int_docs.description.contains("whole numbers"));
        assert_eq!(int_docs.category, DocumentationCategory::Literal);
        assert_eq!(int_docs.visibility, DocumentationVisibility::Public);

        let float_node = Node::Literal(Literal::Float(3.14));
        let float_docs = float_node.get_node_docs();
        assert_eq!(float_docs.name, "Float");
        assert!(float_docs.description.contains("decimal"));

        let string_node = Node::Literal(Literal::String("hello".to_string()));
        let string_docs = string_node.get_node_docs();
        assert_eq!(string_docs.name, "String");
        assert!(string_docs.syntax.contains("\""));

        let bool_node = Node::Literal(Literal::Boolean(true));
        let bool_docs = bool_node.get_node_docs();
        assert_eq!(bool_docs.name, "Boolean");
        assert!(bool_docs.syntax.contains("true | false"));

        let nil_node = Node::Literal(Literal::Nil);
        let nil_docs = nil_node.get_node_docs();
        assert_eq!(nil_docs.name, "Nil");
        assert_eq!(nil_docs.syntax, "nil");
    }

    #[test]
    fn test_variable_node_docs() {
        let var_node = Node::Variable {
            name: "x".to_string(),
        };
        let docs = var_node.get_node_docs();
        assert_eq!(docs.name, "Variable");
        assert!(docs.description.contains("reference"));
        assert_eq!(docs.category, DocumentationCategory::Variable);
        assert!(!docs.see_also.is_empty());
    }

    #[test]
    fn test_lambda_node_docs() {
        let lambda_node = Node::Lambda {
            params: vec!["x".to_string()],
            body: NodeId::new(1).unwrap(),
        };
        let docs = lambda_node.get_node_docs();
        assert_eq!(docs.name, "Lambda");
        assert!(docs.syntax.contains("lambda"));
        assert_eq!(docs.category, DocumentationCategory::Function);
        assert!(docs.see_also.contains(&"Application".to_string()));
    }

    #[test]
    fn test_let_and_letrec_docs() {
        let let_node = Node::Let {
            bindings: vec![],
            body: NodeId::new(1).unwrap(),
        };
        let let_docs = let_node.get_node_docs();
        assert_eq!(let_docs.name, "Let");
        assert!(let_docs.description.contains("local variable"));
        assert!(let_docs.see_also.contains(&"Letrec".to_string()));

        let letrec_node = Node::Letrec {
            bindings: vec![],
            body: NodeId::new(1).unwrap(),
        };
        let letrec_docs = letrec_node.get_node_docs();
        assert_eq!(letrec_docs.name, "Letrec");
        assert!(letrec_docs.description.contains("recursive"));
        assert!(letrec_docs.see_also.contains(&"Let".to_string()));
    }

    #[test]
    fn test_control_flow_docs() {
        let if_node = Node::If {
            condition: NodeId::new(1).unwrap(),
            then_branch: NodeId::new(2).unwrap(),
            else_branch: NodeId::new(3).unwrap(),
        };
        let if_docs = if_node.get_node_docs();
        assert_eq!(if_docs.name, "If");
        assert_eq!(if_docs.category, DocumentationCategory::ControlFlow);
        assert!(if_docs.syntax.contains("if"));

        let match_node = Node::Match {
            expr: NodeId::new(1).unwrap(),
            branches: vec![],
        };
        let match_docs = match_node.get_node_docs();
        assert_eq!(match_docs.name, "Match");
        assert_eq!(match_docs.category, DocumentationCategory::PatternMatching);
        assert!(match_docs.syntax.contains("match"));
    }

    #[test]
    fn test_application_and_effect_docs() {
        let app_node = Node::Application {
            function: NodeId::new(1).unwrap(),
            args: vec![],
        };
        let app_docs = app_node.get_node_docs();
        assert_eq!(app_docs.name, "Application");
        assert!(app_docs.description.contains("function"));
        assert_eq!(app_docs.category, DocumentationCategory::Function);

        let effect_node = Node::Effect {
            effect_type: EffectType::IO,
            operation: "print".to_string(),
            args: vec![],
        };
        let effect_docs = effect_node.get_node_docs();
        assert_eq!(effect_docs.name, "Effect");
        assert_eq!(effect_docs.category, DocumentationCategory::Effect);
        assert!(effect_docs.description.contains("effectful"));
    }

    #[test]
    fn test_data_structure_docs() {
        let list_node = Node::List(vec![]);
        let list_docs = list_node.get_node_docs();
        assert_eq!(list_docs.name, "List");
        assert_eq!(list_docs.category, DocumentationCategory::DataStructure);
        assert!(list_docs.syntax.contains("["));
        assert!(list_docs.see_also.contains(&"cons".to_string()));
    }

    // ===== Graph Validation Tests =====

    #[test]
    fn test_validate_empty_graph() {
        let graph = Graph::new();
        assert!(graph.validate().is_ok());
    }

    #[test]
    fn test_validate_simple_graph() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        graph.root_id = Some(node);

        assert!(graph.validate().is_ok());
    }

    #[test]
    fn test_validate_invalid_root() {
        let mut graph = Graph::new();
        graph.root_id = Some(NodeId::new(999).unwrap());

        let result = graph.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Root node"));
    }

    #[test]
    fn test_validate_dangling_reference() {
        let mut graph = Graph::new();
        let func = graph
            .add_node(Node::Variable {
                name: "f".to_string(),
            })
            .unwrap();

        // Create application with invalid arg reference
        let app = graph
            .add_node(Node::Application {
                function: func,
                args: vec![NodeId::new(999).unwrap()], // Non-existent node
            })
            .unwrap();

        graph.root_id = Some(app);

        let result = graph.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("non-existent"));
    }

    #[test]
    fn test_validate_metadata_for_nonexistent_node() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        graph.root_id = Some(node);

        // Add metadata for non-existent node
        let fake_id = NodeId::new(999).unwrap();
        graph.metadata.insert(fake_id, NodeMetadata::default());

        let result = graph.validate();
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Metadata exists for non-existent"));
    }

    #[test]
    fn test_validate_cycle_detection() {
        let mut graph = Graph::new();

        // Create a cycle: A -> B -> A
        let node_a = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let node_b_placeholder = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();

        // Update node_a to reference node_b
        graph
            .nodes
            .insert(node_a, Node::List(vec![node_b_placeholder]));

        // Update node_b to reference node_a (creating cycle)
        graph
            .nodes
            .insert(node_b_placeholder, Node::List(vec![node_a]));

        graph.root_id = Some(node_a);

        let result = graph.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cycle"));
    }

    #[test]
    fn test_validate_complex_valid_graph() {
        let mut graph = Graph::new();

        // Create a complex but valid graph
        let x = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let y = graph
            .add_node(Node::Variable {
                name: "y".to_string(),
            })
            .unwrap();
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .unwrap();

        let app = graph
            .add_node(Node::Application {
                function: plus,
                args: vec![x, y],
            })
            .unwrap();

        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string(), "y".to_string()],
                body: app,
            })
            .unwrap();

        graph.root_id = Some(lambda);

        // Add some metadata
        graph.metadata_mut(lambda).type_info = Some("(Int, Int) -> Int".to_string());

        assert!(graph.validate().is_ok());
    }

    // ===== Performance Hint Tests =====

    #[test]
    fn test_structured_performance_hints() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Create context memory with various performance hints
        let context = ContextMemory {
            embedding_id: None,
            usage_stats: UsageStatistics::default(),
            rationale: Some("Performance critical loop".to_string()),
            performance_hints: vec![
                PerformanceHint {
                    hint_type: PerformanceHintType::ShouldUnroll { factor: Some(4) },
                    confidence: 0.9,
                    context: Some("Small fixed iteration count".to_string()),
                },
                PerformanceHint {
                    hint_type: PerformanceHintType::CanVectorize {
                        simd_width: Some(256),
                    },
                    confidence: 0.8,
                    context: Some("AVX2 available".to_string()),
                },
                PerformanceHint {
                    hint_type: PerformanceHintType::MemoryAccessPattern(MemoryPattern::Sequential),
                    confidence: 0.95,
                    context: None,
                },
                PerformanceHint {
                    hint_type: PerformanceHintType::CanParallelize {
                        strategy: ParallelismStrategy::DataParallel,
                    },
                    confidence: 0.7,
                    context: Some("Independent iterations".to_string()),
                },
            ],
            semantic_tags: vec!["hot-path".to_string(), "performance-critical".to_string()],
            last_modified: Some(1234567890),
        };

        graph.set_context_memory(node, context);

        // Verify the hints were stored correctly
        let retrieved = graph.get_context_memory(node).unwrap();
        assert_eq!(retrieved.performance_hints.len(), 4);

        // Check specific hint details
        match &retrieved.performance_hints[0].hint_type {
            PerformanceHintType::ShouldUnroll { factor } => {
                assert_eq!(*factor, Some(4));
            }
            _ => panic!("Expected ShouldUnroll hint"),
        }

        match &retrieved.performance_hints[1].hint_type {
            PerformanceHintType::CanVectorize { simd_width } => {
                assert_eq!(*simd_width, Some(256));
            }
            _ => panic!("Expected CanVectorize hint"),
        }

        match &retrieved.performance_hints[2].hint_type {
            PerformanceHintType::MemoryAccessPattern(pattern) => match pattern {
                MemoryPattern::Sequential => (),
                _ => panic!("Expected Sequential memory pattern"),
            },
            _ => panic!("Expected MemoryAccessPattern hint"),
        }
    }

    #[test]
    fn test_module_system_docs() {
        let module_node = Node::Module {
            name: "test".to_string(),
            exports: vec![],
            body: NodeId::new(1).unwrap(),
        };
        let module_docs = module_node.get_node_docs();
        assert_eq!(module_docs.name, "Module");
        assert_eq!(module_docs.category, DocumentationCategory::Module);
        assert!(module_docs.description.contains("namespace"));

        let import_node = Node::Import {
            module_path: "test".to_string(),
            import_list: vec![],
            import_all: false,
        };
        let import_docs = import_node.get_node_docs();
        assert_eq!(import_docs.name, "Import");
        assert!(import_docs.syntax.contains("import"));

        let export_node = Node::Export {
            export_list: vec![],
        };
        let export_docs = export_node.get_node_docs();
        assert_eq!(export_docs.name, "Export");
        assert!(export_docs.description.contains("Exports"));

        let qual_var = Node::QualifiedVariable {
            module_name: "std".to_string(),
            variable_name: "print".to_string(),
        };
        let qual_docs = qual_var.get_node_docs();
        assert_eq!(qual_docs.name, "QualifiedVariable");
        assert!(qual_docs.syntax.contains("."));
    }

    #[test]
    fn test_async_docs() {
        let async_node = Node::Async {
            body: NodeId::new(1).unwrap(),
        };
        let async_docs = async_node.get_node_docs();
        assert_eq!(async_docs.name, "Async");
        assert_eq!(async_docs.category, DocumentationCategory::Async);
        assert!(async_docs.description.contains("asynchronous"));

        let await_node = Node::Await {
            expr: NodeId::new(1).unwrap(),
        };
        let await_docs = await_node.get_node_docs();
        assert_eq!(await_docs.name, "Await");
        assert!(await_docs.syntax.contains("await"));

        let spawn_node = Node::Spawn {
            expr: NodeId::new(1).unwrap(),
        };
        let spawn_docs = spawn_node.get_node_docs();
        assert_eq!(spawn_docs.name, "Spawn");
        assert!(spawn_docs.description.contains("concurrent"));

        let channel_node = Node::Channel { capacity: None };
        let channel_docs = channel_node.get_node_docs();
        assert_eq!(channel_docs.name, "Channel");
        assert!(channel_docs.syntax.contains("chan"));

        let send_node = Node::Send {
            channel: NodeId::new(1).unwrap(),
            value: NodeId::new(2).unwrap(),
        };
        let send_docs = send_node.get_node_docs();
        assert_eq!(send_docs.name, "Send");
        assert!(send_docs.syntax.contains("send!"));

        let recv_node = Node::Receive {
            channel: NodeId::new(1).unwrap(),
        };
        let recv_docs = recv_node.get_node_docs();
        assert_eq!(recv_docs.name, "Receive");
        assert!(recv_docs.syntax.contains("recv!"));
    }

    #[test]
    fn test_contract_docs() {
        let contract_node = Node::Contract {
            function_name: "test".to_string(),
            preconditions: vec![],
            postconditions: vec![],
            invariants: vec![],
            complexity: None,
            pure: true,
        };
        let contract_docs = contract_node.get_node_docs();
        assert_eq!(contract_docs.name, "Contract");
        assert_eq!(contract_docs.category, DocumentationCategory::Verification);
        assert!(contract_docs.description.contains("formal contracts"));
        assert!(contract_docs.syntax.contains("spec:contract"));
    }

    // ===== Context Memory Update Tests =====

    #[test]
    fn test_update_usage_stats_creates_context() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Initially no context memory
        assert!(graph.get_context_memory(node).is_none());

        // Update usage stats - should create context memory
        graph.update_usage_stats(node, |stats| {
            stats.execution_count = 10;
            stats.is_hot_path = true;
        });

        // Verify context was created with the updates
        let context = graph.get_context_memory(node).unwrap();
        assert_eq!(context.usage_stats.execution_count, 10);
        assert!(context.usage_stats.is_hot_path);
        assert_eq!(context.usage_stats.error_count, 0); // Other fields are default
    }

    #[test]
    fn test_update_usage_stats_existing_context() {
        let mut graph = Graph::new();
        let node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Create initial context
        let initial_context = ContextMemory {
            embedding_id: Some(EmbeddingId(123)),
            usage_stats: UsageStatistics {
                execution_count: 5,
                avg_execution_time_ns: 1000,
                error_count: 1,
                is_hot_path: false,
            },
            rationale: Some("Initial".to_string()),
            performance_hints: vec![],
            semantic_tags: vec!["tag1".to_string()],
            last_modified: Some(12345),
        };
        graph.set_context_memory(node, initial_context);

        // Update just the usage stats
        graph.update_usage_stats(node, |stats| {
            stats.execution_count += 5;
            stats.error_count += 1;
        });

        // Verify only usage stats were updated, other fields preserved
        let context = graph.get_context_memory(node).unwrap();
        assert_eq!(context.usage_stats.execution_count, 10);
        assert_eq!(context.usage_stats.error_count, 2);
        assert_eq!(context.usage_stats.avg_execution_time_ns, 1000); // Unchanged
        assert_eq!(context.embedding_id, Some(EmbeddingId(123))); // Preserved
        assert_eq!(context.rationale, Some("Initial".to_string())); // Preserved
        assert_eq!(context.semantic_tags, vec!["tag1".to_string()]); // Preserved
    }

    // ===== Additional Coverage Tests =====

    #[test]
    fn test_all_performance_hint_types() {
        let hints = vec![
            PerformanceHintType::ShouldInline,
            PerformanceHintType::ShouldUnroll { factor: None },
            PerformanceHintType::CanVectorize { simd_width: None },
            PerformanceHintType::CanParallelize {
                strategy: ParallelismStrategy::DataParallel,
            },
            PerformanceHintType::ShouldMemoize {
                max_cache_size: None,
            },
            PerformanceHintType::MemoryAccessPattern(MemoryPattern::Sequential),
            PerformanceHintType::Custom("Custom hint".to_string()),
        ];

        // Just verify we can create all types
        for (i, hint_type) in hints.into_iter().enumerate() {
            let hint = PerformanceHint {
                hint_type,
                confidence: 0.5 + (i as f32) * 0.1,
                context: None,
            };
            assert!(hint.confidence >= 0.5);
        }
    }

    #[test]
    fn test_embedding_id() {
        let id1 = EmbeddingId(100);
        let id2 = EmbeddingId(100);
        let id3 = EmbeddingId(200);

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);

        // Test Debug and Clone
        let id_clone = id1.clone();
        assert_eq!(id1, id_clone);

        let debug_str = format!("{:?}", id1);
        assert!(debug_str.contains("100"));
    }

    #[test]
    fn test_usage_statistics_default() {
        let stats = UsageStatistics::default();
        assert_eq!(stats.execution_count, 0);
        assert_eq!(stats.avg_execution_time_ns, 0);
        assert_eq!(stats.error_count, 0);
        assert!(!stats.is_hot_path);
    }

    // ===== DocumentedNode Trait Tests =====

    #[test]
    fn test_literal_documented_node() {
        use crate::documentation::{DocumentationCategory, DocumentedNode};

        // Test Literal's DocumentedNode implementation
        assert_eq!(Literal::name(), "Literal");
        assert_eq!(Literal::syntax(), "<literal>");
        assert_eq!(Literal::description(), "Literal values in FluentAi");
        assert_eq!(Literal::category(), DocumentationCategory::Literal);

        let examples = Literal::examples();
        assert_eq!(examples.len(), 5);
        assert!(examples.contains(&"42"));
        assert!(examples.contains(&"3.14"));
        assert!(examples.contains(&"\"hello\""));
        assert!(examples.contains(&"true"));
        assert!(examples.contains(&"nil"));
    }

    #[test]
    fn test_node_documented_node() {
        use crate::documentation::{DocumentationCategory, DocumentedNode};

        // Test Node's DocumentedNode implementation
        assert_eq!(Node::name(), "AST Node");
        assert_eq!(Node::syntax(), "Various - see specific node types");
        assert!(Node::description().contains("Abstract Syntax Tree"));
        assert_eq!(Node::category(), DocumentationCategory::DataStructure);
        assert_eq!(Node::visibility(), DocumentationVisibility::Internal);

        // Test get_docs method
        let docs = Node::get_docs();
        assert_eq!(docs.name, "AST Node");
        assert_eq!(docs.category, DocumentationCategory::DataStructure);
        assert_eq!(docs.visibility, DocumentationVisibility::Internal);
        assert!(docs.see_also.is_empty());
    }

    #[test]
    fn test_default_graph() {
        // Test Default implementation for Graph
        let graph = Graph::default();
        assert!(graph.nodes.is_empty());
        assert!(graph.root_id.is_none());
        assert!(graph.metadata.is_empty());
        // Default next_id should be 1
        // We can't directly test next_id, but we can add a node and check its ID
        let mut graph = Graph::default();
        let id = graph.add_node(Node::Literal(Literal::Nil)).unwrap();
        assert_eq!(id.get(), 1);
    }

    // ===== Additional Graph Iterator Tests =====

    #[test]
    fn test_node_ids_iterator_empty_and_sorted() {
        let mut graph = Graph::new();

        // Empty graph
        let ids: Vec<NodeId> = graph.node_ids().collect();
        assert!(ids.is_empty());

        // Add some nodes
        let id1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let id2 = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let id3 = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();

        // Collect all IDs
        let mut ids: Vec<NodeId> = graph.node_ids().collect();
        ids.sort_by_key(|id| id.get());

        assert_eq!(ids.len(), 3);
        assert_eq!(ids[0], id1);
        assert_eq!(ids[1], id2);
        assert_eq!(ids[2], id3);
    }

    #[test]
    fn test_nodes_iterator_with_types() {
        let mut graph = Graph::new();

        // Empty graph
        let nodes: Vec<_> = graph.nodes().collect();
        assert!(nodes.is_empty());

        // Add some nodes
        let id1 = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let id2 = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let id3 = graph
            .add_node(Node::Literal(Literal::String("hello".to_string())))
            .unwrap();

        // Iterate over nodes
        let mut found = std::collections::HashMap::new();
        for (id, node) in graph.nodes() {
            found.insert(*id, node.clone());
        }

        assert_eq!(found.len(), 3);
        assert!(matches!(
            found.get(&id1),
            Some(Node::Literal(Literal::Integer(1)))
        ));
        assert!(matches!(found.get(&id2), Some(Node::Variable { name }) if name == "x"));
        assert!(matches!(found.get(&id3), Some(Node::Literal(Literal::String(s))) if s == "hello"));
    }

    // ===== DFS Traversal Tests =====

    #[test]
    fn test_dfs_lambda_traversal() {
        let mut graph = Graph::new();

        // Build: (lambda (x) (+ x 1))
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let x_var = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let plus = graph
            .add_node(Node::Variable {
                name: "+".to_string(),
            })
            .unwrap();

        let app = graph
            .add_node(Node::Application {
                function: plus,
                args: vec![x_var, one],
            })
            .unwrap();

        let lambda = graph
            .add_node(Node::Lambda {
                params: vec!["x".to_string()],
                body: app,
            })
            .unwrap();

        // Traverse and collect visited nodes
        let mut visited = Vec::new();
        graph.dfs_from(lambda, |id, _| visited.push(id));

        assert_eq!(visited.len(), 5);
        assert!(visited.contains(&lambda));
        assert!(visited.contains(&app));
        assert!(visited.contains(&plus));
        assert!(visited.contains(&x_var));
        assert!(visited.contains(&one));
    }

    #[test]
    fn test_dfs_if_traversal() {
        let mut graph = Graph::new();

        // Build: (if (> x 0) "positive" "non-positive")
        let x = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let gt = graph
            .add_node(Node::Variable {
                name: ">".to_string(),
            })
            .unwrap();

        let condition = graph
            .add_node(Node::Application {
                function: gt,
                args: vec![x, zero],
            })
            .unwrap();

        let then_branch = graph
            .add_node(Node::Literal(Literal::String("positive".to_string())))
            .unwrap();
        let else_branch = graph
            .add_node(Node::Literal(Literal::String("non-positive".to_string())))
            .unwrap();

        let if_node = graph
            .add_node(Node::If {
                condition,
                then_branch,
                else_branch,
            })
            .unwrap();

        // Traverse and verify all nodes are visited
        let mut count = 0;
        graph.dfs_from(if_node, |_, _| count += 1);
        assert_eq!(count, 7);
    }

    #[test]
    fn test_dfs_match_traversal() {
        let mut graph = Graph::new();

        // Build: (match x (0 "zero") (1 "one") (_ "other"))
        let x = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let zero_branch = graph
            .add_node(Node::Literal(Literal::String("zero".to_string())))
            .unwrap();
        let one_branch = graph
            .add_node(Node::Literal(Literal::String("one".to_string())))
            .unwrap();
        let other_branch = graph
            .add_node(Node::Literal(Literal::String("other".to_string())))
            .unwrap();

        let match_node = graph
            .add_node(Node::Match {
                expr: x,
                branches: vec![
                    (Pattern::Literal(Literal::Integer(0)), zero_branch),
                    (Pattern::Literal(Literal::Integer(1)), one_branch),
                    (Pattern::Wildcard, other_branch),
                ],
            })
            .unwrap();

        // Traverse and collect nodes
        let mut visited = std::collections::HashSet::new();
        graph.dfs_from(match_node, |id, _| {
            visited.insert(id);
        });

        assert_eq!(visited.len(), 5);
        assert!(visited.contains(&match_node));
        assert!(visited.contains(&x));
        assert!(visited.contains(&zero_branch));
        assert!(visited.contains(&one_branch));
        assert!(visited.contains(&other_branch));
    }

    #[test]
    fn test_dfs_list_traversal() {
        let mut graph = Graph::new();

        // Build: [1, 2, [3, 4]]
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let three = graph.add_node(Node::Literal(Literal::Integer(3))).unwrap();
        let four = graph.add_node(Node::Literal(Literal::Integer(4))).unwrap();

        let inner_list = graph.add_node(Node::List(vec![three, four])).unwrap();
        let outer_list = graph
            .add_node(Node::List(vec![one, two, inner_list]))
            .unwrap();

        // Traverse and count nodes
        let mut count = 0;
        graph.dfs_from(outer_list, |_, _| count += 1);
        assert_eq!(count, 6);
    }

    #[test]
    fn test_dfs_effect_traversal() {
        let mut graph = Graph::new();

        // Build: (effect IO print "Hello" "World")
        let hello = graph
            .add_node(Node::Literal(Literal::String("Hello".to_string())))
            .unwrap();
        let world = graph
            .add_node(Node::Literal(Literal::String("World".to_string())))
            .unwrap();

        let effect = graph
            .add_node(Node::Effect {
                effect_type: EffectType::IO,
                operation: "print".to_string(),
                args: vec![hello, world],
            })
            .unwrap();

        // Traverse and verify
        let mut visited = Vec::new();
        graph.dfs_from(effect, |id, node| {
            visited.push(id);
            // Also verify the node type
            match node {
                Node::Effect { .. } => assert_eq!(id, effect),
                Node::Literal(Literal::String(s)) => {
                    assert!(s == "Hello" || s == "World");
                }
                _ => panic!("Unexpected node type"),
            }
        });

        assert_eq!(visited.len(), 3);
    }

    #[test]
    fn test_dfs_no_cycles() {
        let mut graph = Graph::new();

        // Build a diamond-shaped graph: A -> B, A -> C, B -> D, C -> D
        let d = graph.add_node(Node::Literal(Literal::Integer(4))).unwrap();
        let b = graph.add_node(Node::List(vec![d])).unwrap();
        let c = graph.add_node(Node::List(vec![d])).unwrap();
        let a = graph.add_node(Node::List(vec![b, c])).unwrap();

        // Traverse from A - D should only be visited once
        let mut visit_count = std::collections::HashMap::new();
        graph.dfs_from(a, |id, _| {
            *visit_count.entry(id).or_insert(0) += 1;
        });

        // Each node visited exactly once
        assert_eq!(visit_count.len(), 4);
        for (_, count) in visit_count {
            assert_eq!(count, 1);
        }
    }

    #[test]
    fn test_dfs_empty_collections() {
        let mut graph = Graph::new();

        // Empty let
        let nil_body = graph.add_node(Node::Literal(Literal::Nil)).unwrap();
        let empty_let = graph
            .add_node(Node::Let {
                bindings: vec![],
                body: nil_body,
            })
            .unwrap();

        let mut count = 0;
        graph.dfs_from(empty_let, |_, _| count += 1);
        assert_eq!(count, 2); // Let node + Nil

        // Empty list
        let empty_list = graph.add_node(Node::List(vec![])).unwrap();
        count = 0;
        graph.dfs_from(empty_list, |_, _| count += 1);
        assert_eq!(count, 1); // Just the list node

        // Effect with no args
        let no_arg_effect = graph
            .add_node(Node::Effect {
                effect_type: EffectType::Pure,
                operation: "noop".to_string(),
                args: vec![],
            })
            .unwrap();
        count = 0;
        graph.dfs_from(no_arg_effect, |_, _| count += 1);
        assert_eq!(count, 1); // Just the effect node
    }

    // ===== Documentation Coverage Tests =====

    #[test]
    fn test_all_nodes_have_documentation() {
        // This test ensures that all Node variants have documentation.
        // The get_node_docs() method must handle all variants exhaustively.
        // If a new Node variant is added without updating get_node_docs(),
        // this will cause a compile-time error due to non-exhaustive patterns.

        let mut graph = Graph::new();

        // Test literals
        let _int_node = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
        let _float_node = graph.add_node(Node::Literal(Literal::Float(3.14))).unwrap();
        let _string_node = graph
            .add_node(Node::Literal(Literal::String("test".to_string())))
            .unwrap();
        let _bool_node = graph
            .add_node(Node::Literal(Literal::Boolean(true)))
            .unwrap();
        let _nil_node = graph.add_node(Node::Literal(Literal::Nil)).unwrap();

        // Test all node types
        let var_node = graph
            .add_node(Node::Variable {
                name: "x".to_string(),
            })
            .unwrap();
        let _lambda_node = graph
            .add_node(Node::Lambda {
                params: vec![],
                body: var_node,
            })
            .unwrap();
        let _let_node = graph
            .add_node(Node::Let {
                bindings: vec![],
                body: var_node,
            })
            .unwrap();
        let _letrec_node = graph
            .add_node(Node::Letrec {
                bindings: vec![],
                body: var_node,
            })
            .unwrap();
        let _if_node = graph
            .add_node(Node::If {
                condition: var_node,
                then_branch: var_node,
                else_branch: var_node,
            })
            .unwrap();
        let _app_node = graph
            .add_node(Node::Application {
                function: var_node,
                args: vec![],
            })
            .unwrap();
        let _effect_node = graph
            .add_node(Node::Effect {
                effect_type: EffectType::Pure,
                operation: "test".to_string(),
                args: vec![],
            })
            .unwrap();
        let _list_node = graph.add_node(Node::List(vec![])).unwrap();
        let _match_node = graph
            .add_node(Node::Match {
                expr: var_node,
                branches: vec![],
            })
            .unwrap();
        let _module_node = graph
            .add_node(Node::Module {
                name: "test".to_string(),
                exports: vec![],
                body: var_node,
            })
            .unwrap();
        let _import_node = graph
            .add_node(Node::Import {
                module_path: "test".to_string(),
                import_list: vec![],
                import_all: false,
            })
            .unwrap();
        let _export_node = graph
            .add_node(Node::Export {
                export_list: vec![],
            })
            .unwrap();
        let _qualified_var_node = graph
            .add_node(Node::QualifiedVariable {
                module_name: "test".to_string(),
                variable_name: "x".to_string(),
            })
            .unwrap();
        let _async_node = graph.add_node(Node::Async { body: var_node }).unwrap();
        let _await_node = graph.add_node(Node::Await { expr: var_node }).unwrap();
        let _spawn_node = graph.add_node(Node::Spawn { expr: var_node }).unwrap();
        let _channel_node = graph.add_node(Node::Channel { capacity: None }).unwrap();
        let _send_node = graph
            .add_node(Node::Send {
                channel: var_node,
                value: var_node,
            })
            .unwrap();
        let _receive_node = graph.add_node(Node::Receive { channel: var_node }).unwrap();
        let _contract_node = graph
            .add_node(Node::Contract {
                function_name: "test".to_string(),
                preconditions: vec![],
                postconditions: vec![],
                invariants: vec![],
                complexity: None,
                pure: true,
            })
            .unwrap();

        // Get documentation for each node - this will fail at compile time if any variant is missing
        for node_id in graph.nodes.keys() {
            let node = &graph.nodes[node_id];
            let docs = node.get_node_docs();
            assert!(!docs.name.is_empty());
            assert!(!docs.syntax.is_empty());
            assert!(!docs.description.is_empty());
        }
    }

    #[test]
    fn test_exhaustive_documentation_match() {
        // This is a compile-time test that ensures the match in get_node_docs is exhaustive.
        // The #[deny(non_exhaustive_patterns)] attribute on get_node_docs enforces this.
        // If you see a compile error here, it means a new Node variant was added
        // without updating the get_node_docs() method.
        fn _compile_time_exhaustiveness_check(node: &Node) {
            // This function exists only to trigger compile-time checking
            let _ = node.get_node_docs();
        }
    }
}
