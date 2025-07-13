//! Tests for actor state initialization consistency

use fluentai_parser::parse_flc;
use fluentai_core::ast::{Node, Literal};

#[test]
fn test_actor_state_default_initialization() {
    let source = r#"
        private actor Counter {
            count: int;
            name: string;
            active: bool;
            value: float;
            
            private handle get() {
                count
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with uninitialized fields: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that the Actor node was created
    let has_actor_node = graph.nodes.values().any(|node| {
        matches!(node, Node::Actor { .. })
    });
    assert!(has_actor_node, "Should create an Actor node");
    
    // Check that initial state is represented (as Let bindings)
    let has_initial_state = graph.nodes.values().any(|node| {
        matches!(node, Node::Let { .. })
    });
    assert!(has_initial_state, "Initial state should be represented with Let bindings");
}

#[test]
fn test_actor_state_explicit_initialization() {
    let source = r#"
        private actor BankAccount {
            balance: float = 1000.0;
            owner: string = "Alice";
            locked: bool = false;
            
            private handle deposit(amount: float) {
                balance + amount
            }
            
            private handle withdraw(amount: float) {
                if (balance >= amount) {
                    balance - amount
                } else {
                    balance
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with initialized fields: {:?}", result);
    
    let graph = result.unwrap();
    
    // Verify initial values are preserved
    let has_initial_balance = graph.nodes.values().any(|node| {
        if let Node::Literal(Literal::Float(f)) = node {
            *f == 1000.0
        } else {
            false
        }
    });
    assert!(has_initial_balance, "Should preserve initial balance value");
    
    let has_owner_name = graph.nodes.values().any(|node| {
        if let Node::Literal(Literal::String(s)) = node {
            s == "Alice"
        } else {
            false
        }
    });
    assert!(has_owner_name, "Should preserve initial owner value");
}

#[test]
fn test_actor_state_mixed_initialization() {
    let source = r#"
        private actor Server {
            port: int = 8080;
            host: string;  // Should default to ""
            running: bool = true;
            connections: int;  // Should default to 0
            
            private handle start() {
                running
            }
            
            private handle stop() {
                false
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with mixed initialization: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check specific initialized values
    let has_port_8080 = graph.nodes.values().any(|node| {
        if let Node::Literal(Literal::Integer(i)) = node {
            *i == 8080
        } else {
            false
        }
    });
    assert!(has_port_8080, "Should have port initialized to 8080");
    
    // Check that uninitialized fields get defaults
    let has_zero_literal = graph.nodes.values().any(|node| {
        if let Node::Literal(Literal::Integer(i)) = node {
            *i == 0
        } else {
            false
        }
    });
    assert!(has_zero_literal, "Should have default value 0 for uninitialized int");
    
    let has_empty_string = graph.nodes.values().any(|node| {
        if let Node::Literal(Literal::String(s)) = node {
            s.is_empty()
        } else {
            false
        }
    });
    assert!(has_empty_string, "Should have default empty string for uninitialized string");
}

#[test]
fn test_actor_state_getters_created() {
    let source = r#"
        private actor DataStore {
            data: string = "initial";
            version: int = 1;
            
            private handle get_data() {
                data
            }
            
            private handle get_version() {
                version
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that getter functions are created
    let has_get_data_getter = graph.nodes.values().any(|node| {
        if let Node::Define { name, .. } = node {
            name == "get_data"
        } else {
            false
        }
    });
    assert!(has_get_data_getter, "Should create get_data getter function");
    
    let has_get_version_getter = graph.nodes.values().any(|node| {
        if let Node::Define { name, .. } = node {
            name == "get_version"
        } else {
            false
        }
    });
    assert!(has_get_version_getter, "Should create get_version getter function");
}

#[test]
fn test_actor_message_handler_structure() {
    let source = r#"
        private actor Calculator {
            result: float = 0.0;
            
            private handle add(x: float) {
                result + x
            }
            
            private handle multiply(x: float) {
                result * x
            }
            
            private handle reset() {
                0.0
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that a message handler lambda is created
    let has_message_handler = graph.nodes.values().any(|node| {
        if let Node::Actor { handler, .. } = node {
            // Check if the handler is a lambda
            if let Some(handler_node) = graph.nodes.get(handler) {
                matches!(handler_node, Node::Lambda { params, .. } if params.len() == 2)
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_message_handler, "Actor should have a message handler lambda with 2 params (state, message)");
}

#[test] 
fn test_actor_no_state_fields() {
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
    
    // Should still create an Actor node with empty initial state
    let has_actor = graph.nodes.values().any(|node| {
        matches!(node, Node::Actor { .. })
    });
    assert!(has_actor, "Should create Actor node even without state fields");
    
    // Initial state should be Nil for stateless actors
    let has_nil_state = graph.nodes.values().any(|node| {
        if let Node::Actor { initial_state, .. } = node {
            if let Some(state_node) = graph.nodes.get(initial_state) {
                matches!(state_node, Node::Literal(Literal::Nil))
            } else {
                false
            }
        } else {
            false
        }
    });
    assert!(has_nil_state, "Should have Nil initial state for stateless actors");
}