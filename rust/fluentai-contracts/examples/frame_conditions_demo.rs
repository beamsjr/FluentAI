//! Demonstrates frame conditions in contracts

use fluentai_contracts::{
    Contract,
    FrameConditionManager, FrameConditionBuilder, FrameCondition,
    FieldAccess, IndexExpr, HeapRegion,
};
use fluentai_core::ast::{Graph, Node, Literal};

fn main() {
    println!("=== Frame Conditions Demo ===\n");
    
    // Example 1: Pure function with empty frame
    println!("Example 1: Pure function (modifies nothing)");
    demo_pure_function();
    
    // Example 2: Function that modifies specific variables
    println!("\nExample 2: Function modifying specific variables");
    demo_variable_modification();
    
    // Example 3: Function that modifies object fields
    println!("\nExample 3: Function modifying object fields");
    demo_field_modification();
    
    // Example 4: Function that modifies array elements
    println!("\nExample 4: Function modifying array elements");
    demo_array_modification();
    
    // Example 5: Complex frame condition
    println!("\nExample 5: Complex frame with multiple modifications");
    demo_complex_frame();
}

fn demo_pure_function() {
    let mut graph = Graph::new();
    
    // Pure function: add(x, y) = x + y
    let body = graph.add_node(Node::Variable { name: "add-body".to_string() }).expect("Failed to add node");
    let mut contract = Contract::new("add".to_string(), body);
    contract.pure = true;
    
    // Pure functions have empty frame conditions
    let frame = FrameCondition::pure();
    contract.frame_condition = Some(frame.clone());
    
    println!("  Function: add(x, y)");
    println!("  Frame: modifies {{}}  // Pure - modifies nothing");
    println!("  Is pure: {}", frame.is_pure());
}

fn demo_variable_modification() {
    let mut graph = Graph::new();
    
    // Function that increments a counter
    // counter := counter + 1
    let counter = graph.add_node(Node::Variable { name: "counter".to_string() }).expect("Failed to add node");
    let one = graph.add_node(Node::Literal(Literal::Integer(1))).expect("Failed to add node");
    let plus = graph.add_node(Node::Variable { name: "+".to_string() }).expect("Failed to add node");
    let new_value = graph.add_node(Node::Application {
        function: plus,
        args: vec![counter, one],
    }).expect("Failed to add node");
    
    let set = graph.add_node(Node::Variable { name: "set!".to_string() }).expect("Failed to add node");
    let body = graph.add_node(Node::Application {
        function: set,
        args: vec![counter, new_value],
    }).expect("Failed to add node");
    
    let mut contract = Contract::new("increment".to_string(), body);
    
    // Frame condition: only modifies counter
    let frame = FrameConditionBuilder::new()
        .modifies_vars(vec!["counter".to_string()])
        .build();
    contract.frame_condition = Some(frame);
    
    println!("  Function: increment()");
    println!("  Body: counter := counter + 1");
    println!("  Frame: modifies {{counter}}");
    
    // Extract frame condition from contract
    let mut manager = FrameConditionManager::new(&graph);
    if let Ok(extracted) = manager.extract_from_contract(&contract) {
        println!("  Extracted modifies: {:?}", extracted.modifies);
    }
}

fn demo_field_modification() {
    let mut graph = Graph::new();
    
    // Function that updates account balance
    // account.balance := account.balance + amount
    let _account = graph.add_node(Node::Variable { name: "account".to_string() }).expect("Failed to add node");
    let _amount = graph.add_node(Node::Variable { name: "amount".to_string() }).expect("Failed to add node");
    
    // Simplified representation of field access/update
    let body = graph.add_node(Node::Variable { name: "update-balance-body".to_string() }).expect("Failed to add node");
    
    let mut contract = Contract::new("deposit".to_string(), body);
    
    // Frame condition: only modifies account.balance field
    let frame = FrameConditionBuilder::new()
        .modifies_fields(vec![("account".to_string(), "balance".to_string())])
        .build();
    contract.frame_condition = Some(frame.clone());
    
    println!("  Function: deposit(account, amount)");
    println!("  Frame: modifies {{account.balance}}");
    println!("  Other fields unchanged: account.id, account.name, etc.");
    
    // Check if a specific modification is allowed
    let _manager = FrameConditionManager::new(&graph);
    let allowed_field = FieldAccess {
        object: "account".to_string(),
        field: "balance".to_string(),
    };
    let disallowed_field = FieldAccess {
        object: "account".to_string(),
        field: "id".to_string(),
    };
    
    println!("  Can modify account.balance? {}", frame.modifies_fields.contains(&allowed_field));
    println!("  Can modify account.id? {}", frame.modifies_fields.contains(&disallowed_field));
}

fn demo_array_modification() {
    let mut graph = Graph::new();
    
    // Function that updates array element
    // arr[i] := value
    let body = graph.add_node(Node::Variable { name: "array-set-body".to_string() }).expect("Failed to add node");
    
    let mut contract = Contract::new("array_set".to_string(), body);
    
    // Frame condition: modifies arr[i] where i is a parameter
    let mut frame = FrameCondition::new();
    frame.add_modifies_index("arr".to_string(), IndexExpr::Variable("i".to_string()));
    contract.frame_condition = Some(frame);
    
    println!("  Function: array_set(arr, i, value)");
    println!("  Frame: modifies {{arr[i]}}");
    println!("  Other indices unchanged: arr[j] for j != i");
    
    // Example with range modification
    let mut range_frame = FrameCondition::new();
    range_frame.add_modifies_index(
        "arr".to_string(), 
        IndexExpr::Range(
            Box::new(IndexExpr::Constant(0)),
            Box::new(IndexExpr::Variable("n".to_string()))
        )
    );
    
    println!("\n  Function: clear_array(arr, n)");
    println!("  Frame: modifies {{arr[0..n]}}");
}

fn demo_complex_frame() {
    let mut graph = Graph::new();
    
    // Complex function with multiple modifications
    let body = graph.add_node(Node::Variable { name: "complex-body".to_string() }).expect("Failed to add node");
    let mut contract = Contract::new("process_data".to_string(), body);
    
    // Build complex frame condition
    let frame = FrameConditionBuilder::new()
        .modifies_vars(vec!["total".to_string(), "count".to_string()])
        .modifies_fields(vec![
            ("stats".to_string(), "sum".to_string()),
            ("stats".to_string(), "avg".to_string()),
        ])
        .allows_allocation()
        .build();
    
    contract.frame_condition = Some(frame.clone());
    
    println!("  Function: process_data(items, stats)");
    println!("  Frame:");
    println!("    - modifies {{total, count}}");
    println!("    - modifies {{stats.sum, stats.avg}}");
    println!("    - may allocate new objects");
    println!("    - preserves: stats.min, stats.max, all items");
    
    // Add heap region modification
    let mut region_frame = frame.clone();
    region_frame.add_modifies_region(HeapRegion::ReachableFrom("temp".to_string()));
    
    println!("\n  Extended frame:");
    println!("    - also modifies: heap reachable from 'temp'");
}

// Example: Bank transfer with precise frame conditions
fn demo_bank_transfer() {
    println!("\n\nExample 6: Bank Transfer with Precise Frame Conditions\n");
    
    let mut graph = Graph::new();
    let body = graph.add_node(Node::Variable { name: "transfer-body".to_string() }).expect("Failed to add node");
    let mut contract = Contract::new("transfer".to_string(), body);
    
    // Frame condition for transfer
    let frame = FrameConditionBuilder::new()
        .modifies_fields(vec![
            ("from_account".to_string(), "balance".to_string()),
            ("to_account".to_string(), "balance".to_string()),
            ("transaction_log".to_string(), "entries".to_string()),
        ])
        .modifies_vars(vec!["last_transaction_id".to_string()])
        .build();
    
    contract.frame_condition = Some(frame);
    
    // Add postconditions that rely on frame conditions
    // old(from_account.balance) - amount = from_account.balance
    // old(to_account.balance) + amount = to_account.balance
    // All other fields remain unchanged (guaranteed by frame)
    
    println!("  Function: transfer(from_account, to_account, amount)");
    println!("  Frame:");
    println!("    modifies {{");
    println!("      from_account.balance,");
    println!("      to_account.balance,");
    println!("      transaction_log.entries,");
    println!("      last_transaction_id");
    println!("    }}");
    println!("  Guarantees:");
    println!("    - Account IDs unchanged");
    println!("    - Account names unchanged");
    println!("    - No other accounts affected");
    println!("    - Total money conserved (sum of balances)");
}

fn main_extended() {
    main();
    demo_bank_transfer();
}