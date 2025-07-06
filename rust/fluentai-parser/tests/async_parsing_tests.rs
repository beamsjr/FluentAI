//! Tests for async/await/spawn parsing functionality

use fluentai_parser::parse;
use fluentai_core::ast::Node;

// Helper function (unused but kept for potential future use)
#[allow(dead_code)]
fn parse_single_expr(input: &str) -> Result<Node, Box<dyn std::error::Error>> {
    let result = parse(input)?;
    let root_id = result.root_id.ok_or("No root node")?;
    result.get_node(root_id)
        .ok_or("Root node not found".into())
        .map(|n| n.clone())
}

#[test]
fn test_parse_async_basic() {
    // Test basic async expression
    let result = parse("(async (+ 1 2))").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Async { body, .. } => {
            // Body should be the addition expression
            match result.get_node(*body) {
                Some(Node::Application { function, args }) => {
                    // Check it's a + application
                    if let Some(Node::Variable { name }) = result.get_node(*function) {
                        assert_eq!(name, "+");
                    }
                    assert_eq!(args.len(), 2);
                }
                _ => panic!("Expected application node in async body"),
            }
        }
        _ => panic!("Expected async node, got {:?}", node),
    }
}

#[test]
fn test_parse_async_with_complex_body() {
    // Test async with more complex body
    let input = r#"(async 
        (let ((x 10)
              (y 20))
          (+ x y)))"#;
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    assert!(matches!(node, Node::Async { .. }));
}

#[test]
fn test_parse_await_basic() {
    // Test basic await expression
    let result = parse("(await promise)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Await { expr, .. } => {
            // Expression should be a variable
            match result.get_node(*expr) {
                Some(Node::Variable { name }) => {
                    assert_eq!(name, "promise");
                }
                _ => panic!("Expected variable node for await expression"),
            }
        }
        _ => panic!("Expected await node, got {:?}", node),
    }
}

#[test]
fn test_parse_await_with_expression() {
    // Test await with complex expression
    let input = "(await (fetch-data url))";
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Await { expr, .. } => {
            // Expression should be an application
            match result.get_node(*expr) {
                Some(Node::Application { function, args }) => {
                    if let Some(Node::Variable { name }) = result.get_node(*function) {
                        assert_eq!(name, "fetch-data");
                    }
                    assert_eq!(args.len(), 1);
                }
                _ => panic!("Expected application node for await expression"),
            }
        }
        _ => panic!("Expected await node"),
    }
}

#[test]
fn test_parse_spawn_basic() {
    // Test basic spawn expression
    let result = parse("(spawn (worker-task))").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Spawn { expr, .. } => {
            // Expression should be an application
            match result.get_node(*expr) {
                Some(Node::Application { function, args }) => {
                    if let Some(Node::Variable { name }) = result.get_node(*function) {
                        assert_eq!(name, "worker-task");
                    }
                    assert_eq!(args.len(), 0);
                }
                _ => panic!("Expected application node for spawn expression"),
            }
        }
        _ => panic!("Expected spawn node, got {:?}", node),
    }
}

#[test]
fn test_parse_spawn_with_lambda() {
    // Test spawn with lambda expression
    let input = r#"(spawn (lambda () 
                     (do
                       (println "Worker started")
                       (process-data))))"#;
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Spawn { expr, .. } => {
            // Expression should be a lambda
            match result.get_node(*expr) {
                Some(Node::Lambda { params, body: _, .. }) => {
                    assert_eq!(params.len(), 0); // No parameters
                    // Body is a NodeId, not a vector
                }
                _ => panic!("Expected lambda node for spawn expression"),
            }
        }
        _ => panic!("Expected spawn node"),
    }
}

#[test]
fn test_parse_nested_async_await() {
    // Test nested async/await
    let input = r#"(async 
                     (let ((result (await (fetch-api))))
                       (process result)))"#;
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    // Should be async at top level
    assert!(matches!(node, Node::Async { .. }));
}

#[test]
fn test_parse_multiple_awaits() {
    // Test multiple await expressions
    let input = r#"(async
                     (let ((a (await promise1))
                           (b (await promise2)))
                       (+ a b)))"#;
    
    let result = parse(input).unwrap();
    assert!(result.root_id.is_some());
    
    // Just verify it parses without error
    // The structure is complex but valid
}

#[test]
fn test_parse_spawn_in_async() {
    // Test spawn inside async
    let input = r#"(async
                     (let ((worker (spawn (process-batch))))
                       (await worker)))"#;
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    assert!(matches!(node, Node::Async { .. }));
}

#[test]
fn test_parse_async_with_multiple_statements() {
    // Test async with multiple statements
    let input = r#"(async
                     (let ((data (await (load-data))))
                       (do
                         (println "Processing")
                         (process data)
                         (println "Done"))))"#;
    
    let result = parse(input).unwrap();
    assert!(result.root_id.is_some());
}

#[test]
fn test_parse_spawn_with_arguments() {
    // Test spawn with a function that takes arguments
    let input = "(spawn (worker-fn arg1 arg2))";
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Spawn { expr, .. } => {
            match result.get_node(*expr) {
                Some(Node::Application { args, .. }) => {
                    assert_eq!(args.len(), 2); // Two arguments
                }
                _ => panic!("Expected application"),
            }
        }
        _ => panic!("Expected spawn node"),
    }
}

#[test]
fn test_parse_await_in_condition() {
    // Test await in conditional expression
    let input = r#"(if (await check-condition)
                     (do-something)
                     (do-something-else))"#;
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::If { condition, .. } => {
            // Condition should be await
            match result.get_node(*condition) {
                Some(Node::Await { .. }) => {
                    // Good, await in condition
                }
                _ => panic!("Expected await in condition"),
            }
        }
        _ => panic!("Expected if node"),
    }
}

#[test]
fn test_parse_async_lambda() {
    // Test async lambda
    let input = "(async (lambda (x) (await (process x))))";
    
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();
    
    match node {
        Node::Async { body, .. } => {
            // Body should be lambda
            match result.get_node(*body) {
                Some(Node::Lambda { .. }) => {
                    // Good
                }
                _ => panic!("Expected lambda in async body"),
            }
        }
        _ => panic!("Expected async node"),
    }
}

#[test]
fn test_parse_error_unclosed_async() {
    // Test error handling for unclosed async
    let result = parse("(async (+ 1 2");
    assert!(result.is_err());
    
    if let Err(e) = result {
        // Should be unexpected EOF error  
        assert!(e.to_string().contains("Unexpected end of input") || 
                e.to_string().contains("EOF") || 
                e.to_string().contains("Unclosed") || 
                e.to_string().contains("delimiter"));
    }
}

#[test]
fn test_parse_error_await_without_arg() {
    // Test error handling for await without argument
    let result = parse("(await)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_spawn_without_arg() {
    // Test error handling for spawn without argument
    let result = parse("(spawn)");
    assert!(result.is_err());
}

#[test]
fn test_parse_async_empty_body() {
    // Test async with empty body - should error
    let result = parse("(async)");
    assert!(result.is_err());
}

#[test]
fn test_parse_complex_async_pattern() {
    // Test a realistic async pattern
    let input = r#"
        (async
          (let ((urls (list "http://api1.com" "http://api2.com" "http://api3.com")))
            (let ((promises (map (lambda (url) (spawn (fetch url))) urls)))
              (let ((results (map await promises)))
                (process-results results)))))"#;
    
    let result = parse(input);
    assert!(result.is_ok(), "Failed to parse complex async pattern: {:?}", result);
}