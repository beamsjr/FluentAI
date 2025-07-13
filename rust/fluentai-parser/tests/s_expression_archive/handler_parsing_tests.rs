//! Tests for parsing handler syntax

use fluentai_core::ast::{EffectType, Node};

#[test]
fn test_parse_simple_handler() {
    let code = r#"
        (handler
            ((error (lambda (err) "default-value")))
            (risky-operation))
    "#;

    let graph = parse(code).expect("Should parse successfully");
    let root = graph.root_id.expect("Should have root");

    match &graph.nodes[&root] {
        Node::Handler { handlers, body } => {
            assert_eq!(handlers.len(), 1);
            let (effect_type, operation, _handler_fn) = &handlers[0];
            assert_eq!(*effect_type, EffectType::Error);
            assert_eq!(*operation, None);

            // Check body is a function call
            match &graph.nodes[body] {
                Node::Application { function, args } => {
                    match &graph.nodes[function] {
                        Node::Variable { name } => assert_eq!(name, "risky-operation"),
                        _ => panic!("Expected variable in application"),
                    }
                    assert_eq!(args.len(), 0);
                }
                _ => panic!("Expected application as body"),
            }
        }
        _ => panic!("Expected Handler node"),
    }
}

#[test]
fn test_parse_handler_with_operation() {
    let code = r#"
        (handler
            ((io:print (lambda (msg) (debug-print msg))))
            (do-something))
    "#;

    let graph = parse(code).expect("Should parse successfully");
    let root = graph.root_id.expect("Should have root");

    match &graph.nodes[&root] {
        Node::Handler { handlers, body: _ } => {
            assert_eq!(handlers.len(), 1);
            let (effect_type, operation, _handler_fn) = &handlers[0];
            assert_eq!(*effect_type, EffectType::IO);
            assert_eq!(*operation, Some("print".to_string()));
        }
        _ => panic!("Expected Handler node"),
    }
}

#[test]
fn test_parse_handler_multiple_handlers() {
    let code = r#"
        (handler
            ((error (lambda (err) (log err)))
             (state:get (lambda (key) (lookup key)))
             (io:write (lambda (data) (safe-write data))))
            (complex-operation))
    "#;

    let graph = parse(code).expect("Should parse successfully");
    let root = graph.root_id.expect("Should have root");

    match &graph.nodes[&root] {
        Node::Handler { handlers, body: _ } => {
            assert_eq!(handlers.len(), 3);

            // Check first handler
            let (effect_type, operation, _) = &handlers[0];
            assert_eq!(*effect_type, EffectType::Error);
            assert_eq!(*operation, None);

            // Check second handler
            let (effect_type, operation, _) = &handlers[1];
            assert_eq!(*effect_type, EffectType::State);
            assert_eq!(*operation, Some("get".to_string()));

            // Check third handler
            let (effect_type, operation, _) = &handlers[2];
            assert_eq!(*effect_type, EffectType::IO);
            assert_eq!(*operation, Some("write".to_string()));
        }
        _ => panic!("Expected Handler node"),
    }
}

#[test]
fn test_parse_nested_handlers() {
    let code = r#"
        (handler
            ((error (lambda (e) "outer-default")))
            (handler
                ((error (lambda (e) "inner-default")))
                (dangerous-op)))
    "#;

    let graph = parse(code).expect("Should parse successfully");
    let root = graph.root_id.expect("Should have root");

    match &graph.nodes[&root] {
        Node::Handler { handlers, body } => {
            assert_eq!(handlers.len(), 1);

            // Check that body is also a handler
            match &graph.nodes[body] {
                Node::Handler {
                    handlers: inner_handlers,
                    body: _,
                } => {
                    assert_eq!(inner_handlers.len(), 1);
                }
                _ => panic!("Expected nested Handler node"),
            }
        }
        _ => panic!("Expected Handler node"),
    }
}

#[test]
fn test_parse_handler_with_complex_body() {
    let code = r#"
        (handler
            ((network:timeout (lambda (_) (retry-with-backoff))))
            (let ((url "https://api.example.com"))
                (fetch url)))
    "#;

    let graph = parse(code).expect("Should parse successfully");
    let root = graph.root_id.expect("Should have root");

    match &graph.nodes[&root] {
        Node::Handler { handlers, body } => {
            assert_eq!(handlers.len(), 1);

            let (effect_type, operation, _) = &handlers[0];
            assert_eq!(*effect_type, EffectType::Network);
            assert_eq!(*operation, Some("timeout".to_string()));

            // Check that body is a let expression
            match &graph.nodes[body] {
                Node::Let { bindings, body: _ } => {
                    assert_eq!(bindings.len(), 1);
                    assert_eq!(bindings[0].0, "url");
                }
                _ => panic!("Expected Let node as body"),
            }
        }
        _ => panic!("Expected Handler node"),
    }
}

#[test]
fn test_parse_handler_error_no_handlers() {
    let code = r#"
        (handler
            ()
            (do-something))
    "#;

    let result = parse(code);
    assert!(
        result.is_ok(),
        "Empty handler list should parse successfully"
    );
}

#[test]
fn test_parse_handler_error_invalid_syntax() {
    let code = r#"
        (handler
            (error (lambda (e) "default"))
            (do-something))
    "#;

    let result = parse(code);
    assert!(
        result.is_err(),
        "Handler without proper list syntax should fail"
    );
}
