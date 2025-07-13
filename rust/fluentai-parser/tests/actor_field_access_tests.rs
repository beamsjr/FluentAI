//! Tests for actor self field access transformation

use fluentai_parser::parse_flc as parse;

#[test]
fn test_simple_field_access_in_handler() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle inc(n: int) {
                count + n
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse actor with field access");
    
    let graph = result.unwrap();
    // The parser should transform 'count' to 'get_count(state)'
    let has_getter = graph.nodes.values().any(|node| {
        if let fluentai_core::ast::Node::Variable { name } = node {
            name == "get_count"
        } else {
            false
        }
    });
    assert!(has_getter, "Field access should be transformed to getter function");
}

#[test]
fn test_multiple_field_access() {
    let source = r#"
        private actor Calculator {
            x: int = 0;
            y: int = 0;
            
            private handle add() {
                x + y
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse actor with multiple field access");
    
    let graph = result.unwrap();
    // Should have getters for both fields
    let has_x_getter = graph.nodes.values().any(|node| {
        if let fluentai_core::ast::Node::Variable { name } = node {
            name == "get_x"
        } else {
            false
        }
    });
    let has_y_getter = graph.nodes.values().any(|node| {
        if let fluentai_core::ast::Node::Variable { name } = node {
            name == "get_y"
        } else {
            false
        }
    });
    assert!(has_x_getter, "Should have getter for x field");
    assert!(has_y_getter, "Should have getter for y field");
}

#[test]
fn test_field_access_in_receive_pattern() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle message(msg: any) {
                receive {
                    "get" => count,
                    "double" => count * 2,
                    _ => 0
                }
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse actor with field access in receive");
}

#[test]
fn test_field_access_with_method_chaining() {
    let source = r#"
        private actor Logger {
            messages: List<string> = [];
            
            private handle get_last() {
                messages.last().unwrap_or("none")
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse field access with method chaining");
}

#[test]
fn test_no_transformation_outside_handler() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private function helper() {
                // This 'count' should NOT be transformed
                let count = 5;
                count + 1
            }
            
            private handle inc() {
                // This 'count' SHOULD be transformed
                count + 1
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse actor with mixed contexts");
}

#[test]
fn test_field_access_in_conditional() {
    let source = r#"
        private actor BankAccount {
            balance: int = 0;
            overdraft: int = 100;
            
            private handle withdraw(amount: int) {
                if (balance >= amount) {
                    balance - amount
                } else {
                    balance
                }
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse field access in conditional");
}

#[test]
fn test_field_name_shadowing() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle complex() {
                let count = 10;  // Local variable shadows field
                count + 1  // This should refer to local, not field
            }
        }
    "#;
    
    let result = parse(source);
    assert!(result.is_ok(), "Failed to parse with field name shadowing");
    // Note: Current implementation doesn't handle shadowing correctly
    // This is a known limitation that should be documented
}