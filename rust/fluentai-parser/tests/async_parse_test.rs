use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

    #[test]
    fn test_async_block_parsing() {
        // Test 1: Simple async block
        let result = parse_flc("async { 42 }").unwrap();
        println!("Test 1: async {{ 42 }}");
        
        if let Some(root_id) = result.root_id {
            if let Some(node) = result.nodes.get(&root_id) {
                assert!(matches!(node, Node::Async { .. }), "Expected Async node, got {:?}", node);
                
                if let Node::Async { body } = node {
                    if let Some(body_node) = result.nodes.get(body) {
                        println!("Body node: {:?}", body_node);
                        assert!(matches!(body_node, Node::Literal(_)), "Expected literal in async body");
                    }
                }
            }
        }
    }

    #[test]
    fn test_async_with_await() {
        // Test 2: Async block with await
        let result = parse_flc("async { fetch().await() }").unwrap();
        
        if let Some(root_id) = result.root_id {
            if let Some(node) = result.nodes.get(&root_id) {
                assert!(matches!(node, Node::Async { .. }), "Expected Async node");
                
                if let Node::Async { body } = node {
                    if let Some(body_node) = result.nodes.get(body) {
                        assert!(matches!(body_node, Node::Await { .. }), "Expected Await in async body");
                    }
                }
            }
        }
    }

    #[test]
    fn test_await_async_block() {
        // Test 3: Await on async block
        let result = parse_flc("async { 42 }.await()").unwrap();
        
        if let Some(root_id) = result.root_id {
            if let Some(node) = result.nodes.get(&root_id) {
                assert!(matches!(node, Node::Await { .. }), "Expected Await node");
                
                if let Node::Await { expr } = node {
                    if let Some(async_node) = result.nodes.get(expr) {
                        assert!(matches!(async_node, Node::Async { .. }), "Expected Async node in await");
                    }
                }
            }
        }
    }