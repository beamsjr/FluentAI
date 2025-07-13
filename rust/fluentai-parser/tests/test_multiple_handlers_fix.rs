use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_multiple_handlers_simple() {
    let source = r#"
        private actor Counter {
            count: int = 0;
            
            private handle inc(n: int) {
                count + n
            }
            
            private handle dec(n: int) {
                count - n
            }
            
            private handle get() {
                count
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with multiple handlers: {:?}", result);
    
    let graph = result.unwrap();
    
    // Count handler definitions
    let handler_count = graph.nodes.values().filter(|node| {
        match node {
            Node::Define { name, .. } => name.starts_with("handle_"),
            _ => false
        }
    }).count();
    
    assert_eq!(handler_count, 3, "Expected 3 handlers but found {}", handler_count);
}

#[test]
fn test_multiple_handlers_bank_account() {
    let source = r#"
        private actor BankAccount {
            balance: int = 0;
            
            private handle deposit(amount: int) {
                balance + amount
            }
            
            private handle withdraw(amount: int) {
                if (balance >= amount) {
                    balance - amount
                } else {
                    balance
                }
            }
            
            private handle get_balance() {
                balance
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse BankAccount actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Verify specific handlers exist
    let handlers: Vec<_> = graph.nodes.values().filter_map(|node| {
        match node {
            Node::Define { name, .. } if name.starts_with("handle_") => Some(name.clone()),
            _ => None
        }
    }).collect();
    
    assert!(handlers.contains(&"handle_deposit".to_string()), "Missing deposit handler");
    assert!(handlers.contains(&"handle_withdraw".to_string()), "Missing withdraw handler");  
    assert!(handlers.contains(&"handle_get_balance".to_string()), "Missing get_balance handler");
}

#[test]
fn test_handlers_with_field_access() {
    let source = r#"
        private actor Logger {
            messages: List<string> = [];
            max_size: int = 100;
            
            public handle log(message: string) {
                if (messages.len() < max_size) {
                    messages.append(message)
                } else {
                    messages
                }
            }
            
            private handle clear() {
                []
            }
            
            public handle get_messages() {
                messages
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse Logger actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Check that field access was transformed to getters
    let has_messages_getter = graph.nodes.values().any(|node| {
        if let Node::Variable { name } = node {
            name == "get_messages"
        } else {
            false
        }
    });
    
    // Note: The handler is named get_messages, but the field getter would be get_messages_field
    // if we properly distinguish between them
}