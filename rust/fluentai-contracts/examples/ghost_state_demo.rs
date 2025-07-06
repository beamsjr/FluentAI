//! Demonstrates ghost state support in contracts

use fluentai_contracts::{
    Contract, ContractCondition, ContractKind,
    GhostStateManager, GhostStateBuilder, GhostVariable, HistoryVariable,
};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::num::NonZeroU32;

fn main() {
    println!("=== Ghost State Demo ===\n");
    
    // Example 1: Using old() to refer to pre-state values
    println!("Example 1: old() expressions");
    demo_old_values();
    
    // Example 2: Ghost variables for specification
    println!("\nExample 2: Ghost variables");
    demo_ghost_variables();
    
    // Example 3: History variables
    println!("\nExample 3: History tracking");
    demo_history_variables();
    
    // Example 4: Model fields
    println!("\nExample 4: Model fields");
    demo_model_fields();
}

fn demo_old_values() {
    let mut graph = Graph::new();
    
    // Variables
    let balance = graph.add_node(Node::Variable { name: "balance".to_string() });
    let amount = graph.add_node(Node::Variable { name: "amount".to_string() });
    let result = graph.add_node(Node::Variable { name: "result".to_string() });
    let minus = graph.add_node(Node::Variable { name: "-".to_string() });
    
    let eq = graph.add_node(Node::Variable { name: "=".to_string() });
    let body = graph.add_node(Node::Variable { name: "withdraw-body".to_string() });
    
    // Contract for withdraw function
    // Postcondition: result = old(balance) - amount
    let mut builder = GhostStateBuilder::new(&mut graph);
    let old_balance = builder.old(balance);
    
    // Create expected value: old(balance) - amount
    let expected = graph.add_node(Node::Application {
        function: minus,
        args: vec![old_balance, amount],
    });
    
    // Create postcondition: result = expected
    let postcond = graph.add_node(Node::Application {
        function: eq,
        args: vec![result, expected],
    });
    
    let mut contract = Contract::new("withdraw".to_string(), body);
    contract.add_postcondition(
        ContractCondition {
            expression: postcond,
            message: Some("result = old(balance) - amount".to_string()),
            kind: ContractKind::Postcondition,
            span: None,
            blame_label: Some("postcondition".to_string()),
        }
    );
    
    // Process ghost state
    let mut ghost_manager = GhostStateManager::new(&graph);
    ghost_manager.process_contract(&contract).unwrap();
    
    println!("  Contract: withdraw");
    println!("  Postcondition: result = old(balance) - amount");
    println!("  Old values captured: {}", ghost_manager.old_values().len());
}

fn demo_ghost_variables() {
    let mut graph = Graph::new();
    
    // Create nodes first
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let one = graph.add_node(Node::Literal(Literal::Integer(1)));
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let eq = graph.add_node(Node::Variable { name: "=".to_string() });
    
    // Use builder for ghost operations
    let mut builder = GhostStateBuilder::new(&mut graph);
    let ghost_count = builder.ghost_var("operation_count", Some(zero));
    let old_count = builder.old(ghost_count);
    
    // Drop builder to release borrow
    drop(builder);
    
    // Now we can use graph again
    let new_count = graph.add_node(Node::Application {
        function: plus,
        args: vec![old_count, one],
    });
    
    let postcond = graph.add_node(Node::Application {
        function: eq,
        args: vec![ghost_count, new_count],
    });
    
    let body = graph.add_node(Node::Variable { name: "operation-body".to_string() });
    let mut contract = Contract::new("operation".to_string(), body);
    contract.add_postcondition(
        ContractCondition {
            expression: postcond,
            message: Some("ghost operation_count = old(operation_count) + 1".to_string()),
            kind: ContractKind::Postcondition,
            span: None,
            blame_label: Some("postcondition".to_string()),
        }
    );
    
    // Add ghost variable to manager
    let mut ghost_manager = GhostStateManager::new(&graph);
    let ghost_var = GhostVariable {
        name: "operation_count".to_string(),
        var_type: Some("Int".to_string()),
        initial_value: Some(zero),
        is_model_field: false,
    };
    ghost_manager.add_ghost_variable(ghost_var);
    
    println!("  Ghost variable: operation_count");
    println!("  Initial value: 0");
    println!("  Tracks: number of operations performed");
    println!("  Used in postcondition to ensure counter increases");
}

fn demo_history_variables() {
    let mut graph = Graph::new();
    
    // Add nodes first
    let temp = graph.add_node(Node::Variable { name: "temperature".to_string() });
    let body = graph.add_node(Node::Variable { name: "update-temp-body".to_string() });
    
    // Now use builder for ghost state
    let mut builder = GhostStateBuilder::new(&mut graph);
    let history_expr = builder.history(temp, "temp_history");
    drop(builder); // Release the borrow
    
    // Contract: temperature should not change too rapidly
    // forall i in indices(temp_history), 
    //   abs(temp_history[i] - temp_history[i-1]) < 10
    
    let mut contract = Contract::new("update_temperature".to_string(), body);
    
    // Add history variable
    let mut ghost_manager = GhostStateManager::new(&graph);
    let history_var = HistoryVariable {
        name: "temp_history".to_string(),
        tracked_expr: temp,
        max_length: Some(100), // Keep last 100 values
    };
    ghost_manager.add_history_variable(history_var);
    
    println!("  History variable: temp_history");
    println!("  Tracks: temperature readings");
    println!("  Max length: 100 values");
    println!("  Use case: Ensure temperature changes gradually");
}

fn demo_model_fields() {
    let mut graph = Graph::new();
    
    // Add nodes first
    let list = graph.add_node(Node::Variable { name: "list".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    
    // Now use builder for model fields
    let mut builder = GhostStateBuilder::new(&mut graph);
    
    // Model field: list.size (abstract size, not computed)
    let size_field = builder.model_field(list, "size");
    
    // Model field: list.capacity (abstract capacity)
    let capacity_field = builder.model_field(list, "capacity");
    
    drop(builder); // Release the borrow
    
    // Invariant: 0 <= size <= capacity
    let le = graph.add_node(Node::Variable { name: "<=".to_string() });
    
    let size_ge_zero = graph.add_node(Node::Application {
        function: le,
        args: vec![zero, size_field],
    });
    
    let size_le_capacity = graph.add_node(Node::Application {
        function: le,
        args: vec![size_field, capacity_field],
    });
    
    let and_op = graph.add_node(Node::Variable { name: "and".to_string() });
    let invariant = graph.add_node(Node::Application {
        function: and_op,
        args: vec![size_ge_zero, size_le_capacity],
    });
    
    let body = graph.add_node(Node::Variable { name: "list-op-body".to_string() });
    let mut contract = Contract::new("list_operation".to_string(), body);
    contract.add_invariant(
        ContractCondition {
            expression: invariant,
            message: Some("0 <= list.size <= list.capacity".to_string()),
            kind: ContractKind::Invariant,
            span: None,
            blame_label: Some("invariant".to_string()),
        }
    );
    
    println!("  Model fields for list:");
    println!("    - size: abstract size property");
    println!("    - capacity: abstract capacity property");
    println!("  Invariant: 0 <= size <= capacity");
    println!("  Benefits: Specify properties without implementation details");
}

// Example: Bank account with ghost state
fn demo_bank_account() {
    println!("\n\nExample 5: Bank Account with Complete Ghost State\n");
    
    let mut graph = Graph::new();
    
    // First, add all the basic nodes we need
    let balance = graph.add_node(Node::Variable { name: "balance".to_string() });
    let amount = graph.add_node(Node::Variable { name: "amount".to_string() });
    let zero = graph.add_node(Node::Literal(Literal::Integer(0)));
    let account = graph.add_node(Node::Variable { name: "account".to_string() });
    let deposit_body = graph.add_node(Node::Variable { name: "deposit-body".to_string() });
    let plus = graph.add_node(Node::Variable { name: "+".to_string() });
    let eq = graph.add_node(Node::Variable { name: "=".to_string() });
    
    // Now create the builder and use it for ghost state
    let mut builder = GhostStateBuilder::new(&mut graph);
    
    // Ghost variables
    let total_deposits = builder.ghost_var("total_deposits", Some(zero));
    let total_withdrawals = builder.ghost_var("total_withdrawals", Some(zero));
    let transaction_count = builder.ghost_var("transaction_count", Some(zero));
    
    // History tracking
    let balance_history = builder.history(balance, "balance_history");
    
    // Model fields
    let min_balance = builder.model_field(account, "min_balance");
    let max_balance = builder.model_field(account, "max_balance");
    
    // Get old values
    let old_balance = builder.old(balance);
    let old_deposits = builder.old(total_deposits);
    
    // Drop the builder to release the mutable borrow
    drop(builder);
    
    // Now we can use graph again
    let mut deposit_contract = Contract::new("deposit".to_string(), deposit_body);
    
    // Postconditions using ghost state
    // 1. balance = old(balance) + amount
    let new_balance = graph.add_node(Node::Application {
        function: plus,
        args: vec![old_balance, amount],
    });
    let balance_postcond = graph.add_node(Node::Application {
        function: eq,
        args: vec![balance, new_balance],
    });
    
    deposit_contract.add_postcondition(
        ContractCondition::new(balance_postcond, ContractKind::Postcondition)
            .with_blame("balance = old(balance) + amount".to_string())
    );
    
    // 2. total_deposits = old(total_deposits) + amount
    let new_deposits = graph.add_node(Node::Application {
        function: plus,
        args: vec![old_deposits, amount],
    });
    let deposits_postcond = graph.add_node(Node::Application {
        function: eq,
        args: vec![total_deposits, new_deposits],
    });
    
    deposit_contract.add_postcondition(
        ContractCondition {
            expression: deposits_postcond,
            message: Some("Ghost: total_deposits increases by amount".to_string()),
            kind: ContractKind::Postcondition,
            span: None,
            blame_label: Some("postcondition".to_string()),
        }
    );
    
    // Invariant: balance = total_deposits - total_withdrawals
    let minus = graph.add_node(Node::Variable { name: "-".to_string() });
    let net_flow = graph.add_node(Node::Application {
        function: minus,
        args: vec![total_deposits, total_withdrawals],
    });
    let invariant = graph.add_node(Node::Application {
        function: eq,
        args: vec![balance, net_flow],
    });
    
    deposit_contract.add_invariant(
        ContractCondition {
            expression: invariant,
            message: Some("balance = total_deposits - total_withdrawals".to_string()),
            kind: ContractKind::Invariant,
            span: None,
            blame_label: Some("invariant".to_string()),
        }
    );
    
    println!("  Bank Account Contract with Ghost State:");
    println!("  Ghost variables:");
    println!("    - total_deposits: sum of all deposits");
    println!("    - total_withdrawals: sum of all withdrawals");
    println!("    - transaction_count: number of transactions");
    println!("  History: balance_history tracks all balance changes");
    println!("  Model fields: min_balance, max_balance");
    println!("  Invariant: balance = total_deposits - total_withdrawals");
}

fn main_extended() {
    main();
    demo_bank_account();
}