//! Test for async function parsing

use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_async_function_parsing() {
    let source = r#"
        private async function fetch_data(url: string) {
            http.get(url).await()
        }
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Find the Define node - should be the root or near it
    // Look for the Define node that has an Async value
    let define_node = graph.nodes.iter()
        .find(|(_id, node)| {
            if let Node::Define { name, value } = node {
                if name == "fetch_data" {
                    // Check if this Define points to an Async node
                    if let Some(Node::Async { .. }) = graph.nodes.get(value) {
                        return true;
                    }
                }
            }
            false
        })
        .or_else(|| {
            // Fallback to any Define with the right name
            graph.nodes.iter()
                .find(|(_, node)| matches!(node, Node::Define { name, .. } if name == "fetch_data"))
        })
        .expect("Should find fetch_data definition");
    
    // Check if the function is properly marked as async
    if let Node::Define { value, .. } = &define_node.1 {
        // The value should be an Async node wrapping a Lambda
        match graph.nodes.get(value).unwrap() {
            Node::Async { body } => {
                // Good! The function is wrapped in Async
                if let Some(Node::Lambda { .. }) = graph.nodes.get(body) {
                    // Success - async function is properly structured
                    assert!(true, "async function properly wrapped in Async node");
                } else {
                    panic!("Async node should contain a Lambda");
                }
            }
            Node::Lambda { .. } => {
                panic!("async function was parsed as regular Lambda - async metadata lost!");
            }
            _ => panic!("Unexpected node type for function definition")
        }
    }
}

#[test] 
fn test_async_block_parsing() {
    let source = r#"
        async {
            fetch_data().await()
        }
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Find the Async node
    let async_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Async { .. }))
        .expect("Should find Async node");
    
    // Verify it contains a body
    if let Node::Async { body } = async_node.1 {
        assert!(graph.nodes.contains_key(body), "Async node should have valid body");
    }
}

#[test]
fn test_async_function_with_await() {
    let source = r#"
        private async function process_data() {
            let data = fetch().await();
            transform(data)
        }
    "#;
    
    let graph = parse_flc(source).unwrap();
    
    // Find the Define node that has an Async value
    let define_node = graph.nodes.iter()
        .find(|(_, node)| {
            if let Node::Define { name, value } = node {
                if name == "process_data" {
                    // Check if this Define points to an Async node
                    if let Some(Node::Async { .. }) = graph.nodes.get(value) {
                        return true;
                    }
                }
            }
            false
        })
        .expect("Should find async process_data definition");
    
    // Verify async structure
    if let Node::Define { value, .. } = &define_node.1 {
        match graph.nodes.get(value).unwrap() {
            Node::Async { body } => {
                // Find await inside the function body
                let mut has_await = false;
                let mut stack = vec![*body];
                let mut visited = std::collections::HashSet::new();
                
                while let Some(node_id) = stack.pop() {
                    if !visited.insert(node_id) {
                        continue; // Skip already visited nodes
                    }
                    
                    if let Some(node) = graph.nodes.get(&node_id) {
                        match node {
                            Node::Await { .. } => {
                                has_await = true;
                                break;
                            }
                            Node::Let { bindings, body } => {
                                stack.push(*body);
                                // Also check binding values
                                for (_, value_id) in bindings {
                                    stack.push(*value_id);
                                }
                            }
                            Node::Lambda { body, .. } => stack.push(*body),
                            Node::Begin { exprs } => stack.extend(exprs),
                            Node::List(items) => stack.extend(items),
                            Node::Application { function, args } => {
                                stack.push(*function);
                                stack.extend(args);
                            }
                            _ => {}
                        }
                    }
                }
                
                assert!(has_await, "async function should contain await expression");
            }
            _ => panic!("async function should be wrapped in Async node")
        }
    }
}