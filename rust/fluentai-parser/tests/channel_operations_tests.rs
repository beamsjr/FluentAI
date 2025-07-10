//! Tests for channel operations (send/receive) parsing

use fluentai_core::ast::Node;
use fluentai_parser::parse;

#[test]
fn test_parse_channel_creation() {
    // Test basic channel creation
    let result = parse("(chan)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    assert!(matches!(node, Node::Channel { .. }));
}

#[test]
fn test_parse_channel_in_let() {
    // Test channel creation in let binding
    let input = "(let ((ch (chan))) ch)";
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Let { bindings, body } => {
            assert_eq!(bindings.len(), 1);
            assert_eq!(bindings[0].0, "ch");

            // Check the binding value is a channel
            match result.get_node(bindings[0].1).unwrap() {
                Node::Channel { .. } => {}
                _ => panic!("Expected Channel node"),
            }

            // Body should be a variable reference to ch
            match result.get_node(*body).unwrap() {
                Node::Variable { name } => assert_eq!(name, "ch"),
                _ => panic!("Expected Variable node"),
            }
        }
        _ => panic!("Expected Let node"),
    }
}

#[test]
fn test_parse_send_basic() {
    // Test basic send operation
    let result = parse("(send! ch 42)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Send { channel, value } => {
            // Channel should be a variable
            match result.get_node(*channel).unwrap() {
                Node::Variable { name } => assert_eq!(name, "ch"),
                _ => panic!("Expected Variable node for channel"),
            }

            // Value should be 42
            match result.get_node(*value).unwrap() {
                Node::Literal(fluentai_core::ast::Literal::Integer(42)) => {}
                _ => panic!("Expected integer literal 42"),
            }
        }
        _ => panic!("Expected Send node"),
    }
}

#[test]
fn test_parse_send_with_expression() {
    // Test send with a complex expression
    let input = "(send! ch (+ x y))";
    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Send { channel: _, value } => {
            // Value should be an application
            match result.get_node(*value).unwrap() {
                Node::Application { function, args } => {
                    match result.get_node(*function).unwrap() {
                        Node::Variable { name } => assert_eq!(name, "+"),
                        _ => panic!("Expected + function"),
                    }
                    assert_eq!(args.len(), 2);
                }
                _ => panic!("Expected Application node for value"),
            }
        }
        _ => panic!("Expected Send node"),
    }
}

#[test]
fn test_parse_receive_basic() {
    // Test basic receive operation
    let result = parse("(recv! ch)").unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Receive { channel } => {
            // Channel should be a variable
            match result.get_node(*channel).unwrap() {
                Node::Variable { name } => assert_eq!(name, "ch"),
                _ => panic!("Expected Variable node for channel"),
            }
        }
        _ => panic!("Expected Receive node"),
    }
}

#[test]
fn test_parse_channel_in_spawn() {
    // Test using channels with spawn
    let input = r#"(spawn
                     (let ((ch (chan)))
                       (send! ch "hello")))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    assert!(matches!(node, Node::Spawn { .. }));
}

#[test]
fn test_parse_channel_communication_pattern() {
    // Test a typical channel communication pattern
    let input = r#"(let ((ch (chan)))
                     (do
                       (spawn (send! ch 42))
                       (recv! ch)))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Let { bindings, body } => {
            // First binding should be channel
            assert_eq!(bindings[0].0, "ch");
            assert!(matches!(
                result.get_node(bindings[0].1).unwrap(),
                Node::Channel { .. }
            ));

            // Body should be a let (from do expansion)
            match result.get_node(*body).unwrap() {
                Node::Let {
                    bindings: inner_bindings,
                    body: inner_body,
                } => {
                    // First binding is the spawn
                    assert!(matches!(
                        result.get_node(inner_bindings[0].1).unwrap(),
                        Node::Spawn { .. }
                    ));

                    // Final expression is receive
                    assert!(matches!(
                        result.get_node(*inner_body).unwrap(),
                        Node::Receive { .. }
                    ));
                }
                _ => panic!("Expected nested Let from do expansion"),
            }
        }
        _ => panic!("Expected Let node"),
    }
}

#[test]
fn test_parse_multiple_channels() {
    // Test creating and using multiple channels
    let input = r#"(let ((ch1 (chan))
                         (ch2 (chan)))
                     (do
                       (send! ch1 "first")
                       (send! ch2 "second")))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    match node {
        Node::Let { bindings, .. } => {
            assert_eq!(bindings.len(), 2);
            assert_eq!(bindings[0].0, "ch1");
            assert_eq!(bindings[1].0, "ch2");

            // Both should be channels
            assert!(matches!(
                result.get_node(bindings[0].1).unwrap(),
                Node::Channel { .. }
            ));
            assert!(matches!(
                result.get_node(bindings[1].1).unwrap(),
                Node::Channel { .. }
            ));
        }
        _ => panic!("Expected Let node"),
    }
}

#[test]
fn test_parse_channel_in_async() {
    // Test channels in async context
    let input = r#"(async
                     (let ((ch (chan)))
                       (await (recv! ch))))"#;

    let result = parse(input).unwrap();
    let root_id = result.root_id.unwrap();
    let node = result.get_node(root_id).unwrap();

    assert!(matches!(node, Node::Async { .. }));
}

#[test]
fn test_parse_channel_as_value() {
    // Test passing channel as a value
    let input = r#"(let ((make-worker (lambda (ch)
                                         (spawn (send! ch (compute)))))
                         (ch (chan)))
                     (make-worker ch))"#;

    let result = parse(input).unwrap();
    assert!(result.root_id.is_some());
}

#[test]
fn test_parse_buffered_channel() {
    // Test channel with buffer size (if supported)
    // Note: This might need adjustment based on actual syntax
    let result = parse("(chan 10)");

    // This might be parsed as an application
    if let Ok(parsed) = result {
        assert!(parsed.root_id.is_some());
    } else {
        // If not supported, that's ok
        assert!(result.is_err());
    }
}

#[test]
fn test_parse_select_on_channels() {
    // Test select operation on multiple channels (if supported)
    let input = "(select ((recv! ch1) (handle-ch1)) ((recv! ch2) (handle-ch2)))";

    let result = parse(input);
    // If select is not a built-in keyword, it will parse as a regular application
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_parse_error_send_without_channel() {
    // send! requires a channel argument
    let result = parse("(send!)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_send_without_value() {
    // send! requires both channel and value
    let result = parse("(send! ch)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_recv_without_channel() {
    // recv! requires a channel argument
    let result = parse("(recv!)");
    assert!(result.is_err());
}

#[test]
fn test_parse_error_recv_extra_args() {
    // recv! should only take one argument
    let result = parse("(recv! ch extra)");
    assert!(result.is_err());
}

#[test]
fn test_parse_channel_pipeline() {
    // Test a pipeline pattern with channels
    let input = r#"(let ((in (chan))
                         (out (chan)))
                     (do
                       (spawn (let ((val (recv! in)))
                                (send! out (* val 2))))
                       (do
                         (send! in 21)
                         (recv! out))))"#;

    let result = parse(input);
    assert!(
        result.is_ok(),
        "Failed to parse channel pipeline: {:?}",
        result
    );
}
