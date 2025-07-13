//! Tests for verifying the AST structure generated for actors

use fluentai_parser::parse_flc;
use fluentai_core::ast::{Node, Literal};
use std::collections::HashSet;

#[test]
fn test_actor_node_structure() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle increment() {
                count + 1
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find the Actor node
    let actor_node = graph.nodes.values().find(|node| {
        matches!(node, Node::Actor { .. })
    });
    
    assert!(actor_node.is_some(), "Should have an Actor node in the AST");
    
    if let Some(Node::Actor { initial_state, handler }) = actor_node {
        // Verify initial_state is not Nil (since we have state fields)
        let state_node = graph.nodes.get(initial_state);
        assert!(state_node.is_some(), "Actor should have initial state");
        assert!(!matches!(state_node.unwrap(), Node::Literal(Literal::Nil)), 
                "Initial state should not be Nil when state fields exist");
        
        // Verify handler is a Lambda
        let handler_node = graph.nodes.get(handler);
        assert!(handler_node.is_some(), "Actor should have a handler");
        assert!(matches!(handler_node.unwrap(), Node::Lambda { .. }), 
                "Handler should be a Lambda node");
        
        // Verify the lambda has 2 parameters (state, message)
        if let Some(Node::Lambda { params, .. }) = handler_node {
            assert_eq!(params.len(), 2, "Handler lambda should have 2 parameters (state, message)");
        }
    }
}

#[test]
fn test_actor_getter_functions() {
    let source = r#"
        private actor DataStore {
            data: string = "initial";
            count: int = 42;
            
            private handle get_data() {
                data
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Collect all Define nodes
    let getter_functions: Vec<_> = graph.nodes.values()
        .filter_map(|node| {
            if let Node::Define { name, .. } = node {
                Some(name.clone())
            } else {
                None
            }
        })
        .collect();
    
    // Should have getter functions for each state field
    assert!(getter_functions.contains(&"get_data".to_string()), 
            "Should have get_data getter function");
    assert!(getter_functions.contains(&"get_count".to_string()), 
            "Should have get_count getter function");
}

#[test]
fn test_field_access_transformation() {
    let source = r#"
        private actor Calculator {
            result: float = 0.0;
            
            private handle add(x: float) {
                result + x
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that field access 'result' is transformed to 'get_result(state)'
    let has_getter_call = graph.nodes.values().any(|node| {
        if let Node::Application { function, args } = node {
            if let Some(Node::Variable { name }) = graph.nodes.get(function) {
                // The getter takes state as a parameter
                name == "get_result" && args.len() == 1
            } else {
                false
            }
        } else {
            false
        }
    });
    
    assert!(has_getter_call, "Field access 'result' should be transformed to 'get_result(state)' call");
}

#[test]
fn test_become_node_structure() {
    let source = r#"
        private actor StateMachine {
            state: string = "idle";
            
            private handle transition(new_state: string) {
                become new_state
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with become: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find Become node
    let become_node = graph.nodes.values().find(|node| {
        matches!(node, Node::Become { .. })
    });
    
    assert!(become_node.is_some(), "Should have a Become node in the AST");
    
    if let Some(Node::Become { new_state }) = become_node {
        // Verify the state expression exists
        let state_expr = graph.nodes.get(new_state);
        assert!(state_expr.is_some(), "Become should have a state expression");
    }
}

#[test]
fn test_receive_node_structure() {
    let source = r#"
        private actor Receiver {
            private handle process() {
                receive {
                    Msg(msg) => msg
                } after(1000) {
                    "timeout"
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with receive: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find ActorReceive node (receive inside actors uses ActorReceive, not Receive)
    let receive_node = graph.nodes.values().find(|node| {
        matches!(node, Node::ActorReceive { .. })
    });
    
    assert!(receive_node.is_some(), "Should have an ActorReceive node in the AST");
    
    if let Some(Node::ActorReceive { patterns, timeout }) = receive_node {
        assert!(!patterns.is_empty(), "ActorReceive should have at least one pattern");
        assert!(timeout.is_some(), "ActorReceive should have a timeout clause");
        
        // Verify timeout structure
        if let Some((timeout_expr, timeout_handler)) = timeout {
            assert!(graph.nodes.get(timeout_expr).is_some(), "Timeout should have duration expression");
            assert!(graph.nodes.get(timeout_handler).is_some(), "Timeout should have handler expression");
        }
    }
}

#[test]
fn test_multiple_handlers_structure() {
    let source = r#"
        private actor MultiHandler {
            value: int = 0;
            
            private handle inc() {
                value + 1
            }
            
            private handle dec() {
                value - 1
            }
            
            private handle reset() {
                0
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with multiple handlers: {:?}", result);
    
    let graph = result.unwrap();
    
    // The message handler lambda should contain a match expression
    let actor_node = graph.nodes.values().find(|node| {
        matches!(node, Node::Actor { .. })
    }).unwrap();
    
    if let Node::Actor { handler, .. } = actor_node {
        let handler_node = graph.nodes.get(handler).unwrap();
        if let Node::Lambda { body, .. } = handler_node {
            // The body should lead to a match expression
            let has_match = find_match_in_subtree(&graph, body);
            assert!(has_match, "Handler lambda body should contain a match expression for multiple handlers");
        }
    }
}

fn find_match_in_subtree(graph: &fluentai_core::ast::Graph, node_id: &fluentai_core::ast::NodeId) -> bool {
    if let Some(node) = graph.nodes.get(node_id) {
        if matches!(node, Node::Match { .. }) {
            return true;
        }
        
        // Check child nodes
        match node {
            Node::Let { bindings, body } => {
                bindings.iter().any(|(_, value)| find_match_in_subtree(graph, value)) 
                    || find_match_in_subtree(graph, body)
            }
            Node::Begin { exprs } => {
                exprs.iter().any(|expr| find_match_in_subtree(graph, expr))
            }
            _ => false
        }
    } else {
        false
    }
}

#[test]
fn test_stateless_actor_structure() {
    let source = r#"
        private actor Echo {
            private handle echo(msg: string) -> string {
                msg
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse stateless actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find the Actor node
    let actor_node = graph.nodes.values().find(|node| {
        matches!(node, Node::Actor { .. })
    }).unwrap();
    
    if let Node::Actor { initial_state, .. } = actor_node {
        // For stateless actors, initial state should be Nil
        let state_node = graph.nodes.get(initial_state).unwrap();
        assert!(matches!(state_node, Node::Literal(Literal::Nil)), 
                "Stateless actor should have Nil initial state");
    }
}

#[test]
fn test_typed_handler_parameter_structure() {
    let source = r#"
        private actor TypedProcessor {
            private handle process(data: List<int>, options: Map<string, bool>) {
                data.len()
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with typed parameters: {:?}", result);
    
    let graph = result.unwrap();
    
    // The AST should be successfully built even with complex typed parameters
    // This test mainly verifies that the parser doesn't fail on typed parameters
    let actor_exists = graph.nodes.values().any(|node| {
        matches!(node, Node::Actor { .. })
    });
    
    assert!(actor_exists, "Should successfully parse actor with typed handler parameters");
}

#[test]
fn test_self_field_access_not_shadowed() {
    let source = r#"
        private actor SelfTest {
            id: string = "actor1";
            data: List<int> = [];
            
            private handle process() {
                // These should be transformed to getters
                let id_len = id.len();
                let data_count = data.len();
                id_len + data_count
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Count getter calls - getters take state as parameter
    let getter_calls: HashSet<_> = graph.nodes.values()
        .filter_map(|node| {
            if let Node::Application { function, args } = node {
                if args.len() == 1 {  // Getters take state as parameter
                    if let Some(Node::Variable { name }) = graph.nodes.get(function) {
                        if name.starts_with("get_") {
                            Some(name.clone())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        })
        .collect();
    
    assert!(getter_calls.contains("get_id"), "Should have get_id(state) call");
    assert!(getter_calls.contains("get_data"), "Should have get_data(state) call");
}

#[test]
fn test_actor_definition_creates_correct_top_level() {
    let source = r#"
        private actor MyActor {
            value: int = 10;
            
            private handle get() {
                value
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // The root should be a Define node for the actor
    if let Some(root_id) = graph.root_id {
        let root_node = graph.nodes.get(&root_id).unwrap();
        assert!(matches!(root_node, Node::Define { name, .. } if name == "MyActor"), 
                "Root should be a Define node for the actor");
        
        if let Node::Define { value, .. } = root_node {
            // The value might be wrapped in a Begin node
            let value_node = graph.nodes.get(value).unwrap();
            
            // Check if it's a Begin node containing the Actor
            if let Node::Begin { exprs } = value_node {
                // Look for the Actor node in the Begin expressions
                let has_actor = exprs.iter().any(|expr_id| {
                    matches!(graph.nodes.get(expr_id), Some(Node::Actor { .. }))
                });
                assert!(has_actor, "Begin should contain an Actor node");
            } else {
                assert!(matches!(value_node, Node::Actor { .. }), 
                        "Define value should be an Actor node");
            }
        }
    } else {
        panic!("Graph should have a root node");
    }
}