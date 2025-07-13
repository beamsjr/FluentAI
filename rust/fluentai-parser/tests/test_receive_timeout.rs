use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_receive_with_timeout_simple() {
    let source = r#"
        let result = receive {
            "ping" => "pong",
            "status" => "ok"
        } after(1000) {
            "timeout"
        };
        result
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse receive with timeout: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find the ActorReceive node and verify it has a timeout
    let has_timeout = graph.nodes.values().any(|node| {
        matches!(node, Node::ActorReceive { timeout: Some(_), .. })
    });
    
    assert!(has_timeout, "ActorReceive node should have a timeout");
}

#[test]
fn test_receive_with_timeout_in_actor() {
    let source = r#"
        private actor TimeoutActor {
            state: string = "idle";
            
            private handle process(msg) {
                receive {
                    "work" => "working",
                    "cancel" => "cancelled"
                } after(5000) {
                    "timed out"
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with receive timeout: {:?}", result);
}

#[test]
fn test_receive_timeout_with_expression() {
    let source = r#"
        let timeout_ms = 3000;
        let result = receive {
            msg => msg
        } after(timeout_ms * 2) {
            f"No message received in {timeout_ms * 2}ms"
        };
        result
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse receive with expression timeout: {:?}", result);
}

#[test]
fn test_receive_without_timeout() {
    let source = r#"
        let result = receive {
            "hello" => "world",
            x => x
        };
        result
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse receive without timeout: {:?}", result);
    
    let graph = result.unwrap();
    
    // Find the ActorReceive node and verify it has no timeout
    let has_no_timeout = graph.nodes.values().any(|node| {
        matches!(node, Node::ActorReceive { timeout: None, .. })
    });
    
    assert!(has_no_timeout, "ActorReceive node should not have a timeout");
}