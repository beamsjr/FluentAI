use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_actor_with_all_features() {
    let source = r#"
        private actor BankAccount {
            balance: float = 0.0;
            overdraft_limit: float = 100.0;
            status: string = "active";
            
            // Multiple typed handlers
            private handle deposit(amount: float) {
                if (amount > 0.0) {
                    f"Deposited ${amount}. New balance: ${balance + amount}"
                } else {
                    "Invalid deposit amount"
                }
            }
            
            private handle withdraw(amount: float) {
                let new_balance = balance - amount;
                if (new_balance >= -overdraft_limit) {
                    f"Withdrew ${amount}. New balance: ${new_balance}"
                } else {
                    "Insufficient funds"
                }
            }
            
            private handle get_balance() {
                f"Current balance: ${balance}"
            }
            
            private handle freeze() {
                "Account frozen"
            }
            
            // Handler with receive and timeout
            private handle process_transaction(tx) {
                receive {
                    "deposit" => "Processing deposit",
                    "withdraw" => "Processing withdraw",
                    "query" => get_balance()
                } after(30000) {
                    "Transaction timeout"
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse comprehensive actor: {:?}", result);
    
    let graph = result.unwrap();
    
    // Verify multiple handlers were created
    let handlers: Vec<_> = graph.nodes.values().filter_map(|node| {
        match node {
            Node::Define { name, .. } if name.starts_with("handle_") => Some(name.clone()),
            _ => None
        }
    }).collect();
    
    assert!(handlers.len() >= 5, "Expected at least 5 handlers, found {}", handlers.len());
    assert!(handlers.contains(&"handle_deposit".to_string()));
    assert!(handlers.contains(&"handle_withdraw".to_string()));
    assert!(handlers.contains(&"handle_get_balance".to_string()));
    assert!(handlers.contains(&"handle_freeze".to_string()));
    assert!(handlers.contains(&"handle_process_transaction".to_string()));
    
    // Verify field access transformation occurred
    let has_balance_getter = graph.nodes.values().any(|node| {
        if let Node::Variable { name } = node {
            name == "get_balance" || name == "get_overdraft_limit" || name == "get_status"
        } else {
            false
        }
    });
    assert!(has_balance_getter, "Field access should be transformed to getters");
    
    
    // Verify receive with timeout exists
    let has_receive_timeout = graph.nodes.values().any(|node| {
        matches!(node, Node::ActorReceive { timeout: Some(_), .. })
    });
    assert!(has_receive_timeout, "Should have receive with timeout");
}


#[test] 
fn test_actor_state_machine() {
    let source = r#"
        private actor TrafficLight {
            color: string = "red";
            timer: int = 0;
            
            private handle tick() {
                match color {
                    "red" => {
                        if (timer >= 30) {
                            "Changed to green"
                        } else {
                            f"Red for {timer + 1} seconds"
                        }
                    },
                    "green" => {
                        if (timer >= 25) {
                            "Changed to yellow"
                        } else {
                            f"Green for {timer + 1} seconds"
                        }
                    },
                    "yellow" => {
                        if (timer >= 5) {
                            "Changed to red"
                        } else {
                            f"Yellow for {timer + 1} seconds"
                        }
                    }
                }
            }
            
            private handle get_status() {
                f"Light is {color}, timer at {timer}"
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse state machine actor: {:?}", result);
}

#[test]
fn test_actor_with_private_helpers() {
    let source = r#"
        private actor OrderProcessor {
            orders: List<Map<string, any>> = [];
            total_revenue: float = 0.0;
            
            // Private helper function
            private function validate_order(order: Map<string, any>) {
                order.get("amount").is_some() && 
                order.get("item").is_some() &&
                order.get("amount").unwrap() > 0
            }
            
            // Handler that uses the helper
            private handle process_order(order: Map<string, any>) {
                if (validate_order(order)) {
                    let amount = order.get("amount").unwrap();
                    f"Order processed. Total revenue: ${total_revenue + amount}"
                } else {
                    "Invalid order"
                }
            }
            
            private handle get_stats() {
                {
                    "order_count": orders.len(),
                    "total_revenue": total_revenue,
                    "average_order": if (orders.len() > 0) {
                        total_revenue / orders.len()
                    } else {
                        0.0
                    }
                }
            }
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse actor with private helpers: {:?}", result);
}

