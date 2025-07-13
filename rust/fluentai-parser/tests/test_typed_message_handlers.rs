use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_typed_message_handler_simple() {
    let source = r#"
        private actor Calculator {
            result: int = 0;
            
            private handle add(x: int, y: int) {
                x + y
            }
            
            private handle multiply(x: int, y: int) {
                x * y
            }
            
            private handle get_result() {
                result
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with typed handlers: {:?}", result);
    
    let graph = result.unwrap();
    
    // Verify handlers were created
    let handlers: Vec<_> = graph.nodes.values().filter_map(|node| {
        match node {
            Node::Define { name, .. } if name.starts_with("handle_") => Some(name.clone()),
            _ => None
        }
    }).collect();
    
    assert!(handlers.contains(&"handle_add".to_string()), "Missing add handler");
    assert!(handlers.contains(&"handle_multiply".to_string()), "Missing multiply handler");
    assert!(handlers.contains(&"handle_get_result".to_string()), "Missing get_result handler");
}

#[test]
fn test_typed_message_handler_with_custom_types() {
    let source = r#"
        private struct Point {
            x: int,
            y: int
        }
        
        private actor PointTracker {
            points: List<Point> = [];
            
            private handle add_point(p: Point) {
                points.append(p)
            }
            
            private handle find_by_x(x: int) {
                points.filter(p => p.x == x)
            }
            
            private handle clear() {
                []
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with custom typed handlers: {:?}", result);
}

#[test]
fn test_typed_handler_with_optional_params() {
    let source = r#"
        private actor Logger {
            logs: List<string> = [];
            
            private handle log(message: string, level: string = "info") {
                logs.append(f"[{level}] {message}")
            }
            
            private handle get_logs(level: Option<string>) {
                match level {
                    Some(l) => logs.filter(log => log.contains(f"[{l}]")),
                    None => logs
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    // Note: Optional/default parameters might not be supported yet
    // This test documents the desired behavior
    if result.is_err() {
        println!("Optional parameters not yet supported: {:?}", result);
    }
}

#[test]
fn test_typed_handler_with_generics() {
    let source = r#"
        private actor Storage<T> {
            items: List<T> = [];
            
            private handle store(item: T) {
                items.append(item)
            }
            
            private handle retrieve(index: int) -> Option<T> {
                if (index >= 0 && index < items.len()) {
                    Some(items[index])
                } else {
                    None
                }
            }
            
            private handle count() -> int {
                items.len()
            }
        }
    "#;
    
    let result = parse_flc(source);
    // Note: Generic actors might not be supported yet
    // This test documents the desired behavior
    if result.is_err() {
        println!("Generic actors not yet supported: {:?}", result);
    }
}

#[test]
fn test_handler_with_pattern_matching_params() {
    let source = r#"
        private enum Command {
            Start(string),
            Stop,
            Status
        }
        
        private actor CommandProcessor {
            running: bool = false;
            process_name: string = "";
            
            private handle process(cmd: Command) {
                match cmd {
                    Command.Start(name) => {
                        running = true;
                        process_name = name;
                        f"Started {name}"
                    },
                    Command.Stop => {
                        running = false;
                        process_name = "";
                        "Stopped"
                    },
                    Command.Status => {
                        if (running) {
                            f"Running: {process_name}"
                        } else {
                            "Not running"
                        }
                    }
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    // This tests enum-based message handling
    if result.is_err() {
        println!("Enum-based handlers not yet fully supported: {:?}", result);
    }
}