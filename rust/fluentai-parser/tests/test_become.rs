use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_become_simple() {
    let source = r#"
        let new_state = 42;
        become new_state
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse simple become: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find the Become node
    let has_become = graph.nodes.values().any(|node| {
        matches!(node, Node::Become { .. })
    });
    
    assert!(has_become, "Should have a Become node");
}

#[test]
fn test_become_with_parentheses() {
    let source = r#"
        let counter = 0;
        become(counter + 1)
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse become with parentheses: {:?}", result);
}

#[test]
fn test_become_in_actor_handler() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle increment(n: int) {
                become count + n
            }
            
            private handle reset() {
                become 0
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse become in actor handler: {:?}", result);
}

#[test]
fn test_become_with_complex_state() {
    let source = r#"
        private actor StateMachine {
            state: string = "idle";
            counter: int = 0;
            
            private handle transition(event: string) {
                match event {
                    "start" => become {
                        "state": "running",
                        "counter": counter + 1
                    },
                    "stop" => become {
                        "state": "idle",
                        "counter": 0
                    },
                    _ => state
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse become with complex state: {:?}", result);
}

#[test]
fn test_become_in_receive() {
    let source = r#"
        private actor Worker {
            status: string = "idle";
            
            private handle work() {
                receive {
                    "start" => {
                        become "working";
                        "Started working"
                    },
                    "pause" => {
                        become "paused";
                        "Work paused"
                    },
                    "stop" => {
                        become "idle";
                        "Stopped"
                    }
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse become in receive: {:?}", result);
}

#[test]
fn test_become_with_handler_change_future() {
    // This test documents the desired behavior for changing handlers
    // which might not be supported by the current AST
    let source = r#"
        private actor BehaviorActor {
            mode: string = "normal";
            
            private handle switch_mode() {
                if (mode == "normal") {
                    // In the future, we might support changing the handler too
                    become "special"
                } else {
                    become "normal"
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    // This should parse successfully even if full handler change isn't supported
    assert!(result.is_ok(), "Failed to parse actor with mode switch: {:?}", result);
}