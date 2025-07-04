//! Example demonstrating advanced transaction management

use std::sync::Arc;
use fluentai_db::{
    DbConfig, DbConnection, Transaction, TransactionManager,
    TransactionOptions, IsolationLevel, with_transaction, with_transaction_retry,
    DbResult,
};
use fluentai_vm::bytecode::Value;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("=== FluentAi Database Transaction Example ===\n");
    
    // Setup database connection
    let config = DbConfig {
        url: "sqlite::memory:".to_string(),
        ..Default::default()
    };
    
    let connection = Arc::new(DbConnection::new(&config).await?);
    
    // Create test table
    connection.execute_raw_unsafe(
        "CREATE TABLE accounts (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            balance DECIMAL(10,2) NOT NULL
        )"
    ).await?;
    
    // Insert test data
    connection.execute(
        "INSERT INTO accounts (name, balance) VALUES (?, ?), (?, ?)",
        vec![
            Value::String("Alice".to_string()),
            Value::Float(1000.0),
            Value::String("Bob".to_string()),
            Value::Float(500.0),
        ]
    ).await?;
    
    // Example 1: Basic transaction
    println!("Example 1: Basic Transaction");
    basic_transaction_example(connection.clone()).await?;
    
    // Example 2: Transaction with savepoints
    println!("\nExample 2: Transaction with Savepoints");
    savepoint_example(connection.clone()).await?;
    
    // Example 3: Transaction manager
    println!("\nExample 3: Transaction Manager");
    transaction_manager_example(connection.clone()).await?;
    
    // Example 4: Automatic transaction management
    println!("\nExample 4: Automatic Transaction Management");
    automatic_transaction_example(connection.clone()).await?;
    
    // Example 5: Transaction with retry
    println!("\nExample 5: Transaction with Retry");
    retry_transaction_example(connection.clone()).await?;
    
    // Example 6: Transaction options
    println!("\nExample 6: Transaction with Options");
    transaction_options_example(connection.clone()).await?;
    
    Ok(())
}

async fn basic_transaction_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Transferring $200 from Alice to Bob...");
    
    let tx = Transaction::begin(conn).await?;
    
    // Deduct from Alice
    tx.execute(
        "UPDATE accounts SET balance = balance - ? WHERE name = ?",
        vec![Value::Float(200.0), Value::String("Alice".to_string())]
    ).await?;
    
    // Add to Bob
    tx.execute(
        "UPDATE accounts SET balance = balance + ? WHERE name = ?",
        vec![Value::Float(200.0), Value::String("Bob".to_string())]
    ).await?;
    
    // Check balances
    let rows = tx.fetch_all("SELECT name, balance FROM accounts ORDER BY name", vec![]).await?;
    
    tx.commit().await?;
    
    println!("  Transfer completed!");
    print_accounts(&rows);
    
    Ok(())
}

async fn savepoint_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Demonstrating savepoints with partial rollback...");
    
    let tx = Transaction::begin(conn).await?;
    
    // First update
    tx.execute(
        "UPDATE accounts SET balance = balance + ? WHERE name = ?",
        vec![Value::Float(100.0), Value::String("Alice".to_string())]
    ).await?;
    println!("  Added $100 to Alice");
    
    // Create savepoint
    tx.savepoint("before_bob_update").await?;
    println!("  Created savepoint 'before_bob_update'");
    
    // Second update
    tx.execute(
        "UPDATE accounts SET balance = balance - ? WHERE name = ?",
        vec![Value::Float(50.0), Value::String("Bob".to_string())]
    ).await?;
    println!("  Deducted $50 from Bob");
    
    // Rollback to savepoint
    tx.rollback_to_savepoint("before_bob_update").await?;
    println!("  Rolled back to savepoint - Bob's change undone");
    
    // Check final state
    let rows = tx.fetch_all("SELECT name, balance FROM accounts ORDER BY name", vec![]).await?;
    
    tx.commit().await?;
    
    println!("  Transaction committed with partial rollback");
    print_accounts(&rows);
    
    Ok(())
}

async fn transaction_manager_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Using transaction manager for coordinated transactions...");
    
    let manager = TransactionManager::new(conn);
    
    // Start multiple managed transactions
    let tx1 = manager.begin_transaction().await?;
    let tx2 = manager.begin_transaction().await?;
    
    println!("  Active transactions: {}", manager.active_count().await);
    
    // Update in first transaction
    tx1.execute(
        "UPDATE accounts SET balance = balance + ? WHERE name = ?",
        vec![Value::Float(25.0), Value::String("Alice".to_string())]
    ).await?;
    
    // Update in second transaction
    tx2.execute(
        "UPDATE accounts SET balance = balance + ? WHERE name = ?",
        vec![Value::Float(25.0), Value::String("Bob".to_string())]
    ).await?;
    
    // Commit both
    if let Ok(tx1) = Arc::try_unwrap(tx1) {
        tx1.commit().await?;
        println!("  Transaction 1 committed");
    }
    
    if let Ok(tx2) = Arc::try_unwrap(tx2) {
        tx2.commit().await?;
        println!("  Transaction 2 committed");
    }
    
    println!("  Active transactions after commit: {}", manager.active_count().await);
    
    Ok(())
}

async fn automatic_transaction_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Using automatic transaction management...");
    
    let result = with_transaction(conn, |tx| {
        Box::pin(async move {
            // Perform operations
            tx.execute(
                "UPDATE accounts SET balance = balance * ? WHERE balance > ?",
                vec![Value::Float(1.01), Value::Float(500.0)]
            ).await?;
            
            // Return result
            let count = tx.fetch_all(
                "SELECT COUNT(*) as count FROM accounts WHERE balance > ?",
                vec![Value::Float(500.0)]
            ).await?;
            
            Ok(count)
        })
    }).await?;
    
    println!("  Transaction completed automatically");
    println!("  Accounts with balance > $500: {:?}", result.len());
    
    Ok(())
}

async fn retry_transaction_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Using transaction with retry logic...");
    
    let mut attempt = 0;
    let _result = with_transaction_retry(conn, 3, |tx| {
        Box::pin(async move {
            attempt += 1;
            println!("  Attempt #{}", attempt);
            
            // Simulate potential conflict
            tx.execute(
                "UPDATE accounts SET balance = balance + ? WHERE name = ?",
                vec![Value::Float(10.0), Value::String("Alice".to_string())]
            ).await?;
            
            Ok(())
        })
    }).await?;
    
    println!("  Transaction succeeded after {} attempt(s)", attempt);
    
    Ok(())
}

async fn transaction_options_example(conn: Arc<DbConnection>) -> DbResult<()> {
    println!("  Using transaction with custom options...");
    
    let options = TransactionOptions {
        isolation_level: Some(IsolationLevel::ReadCommitted),
        read_only: false,
        deferrable: false,
        timeout_ms: Some(5000),
    };
    
    let tx = Transaction::begin_with_options(conn, options).await?;
    
    // Read data
    let rows = tx.fetch_all(
        "SELECT name, balance FROM accounts WHERE balance > ?",
        vec![Value::Float(0.0)]
    ).await?;
    
    println!("  Read {} accounts with READ COMMITTED isolation", rows.len());
    
    tx.commit().await?;
    
    Ok(())
}

fn print_accounts(rows: &[sqlx::any::AnyRow]) {
    use sqlx::Row;
    
    for row in rows {
        let name: String = row.try_get("name").unwrap_or_default();
        let balance: f64 = row.try_get("balance").unwrap_or(0.0);
        println!("    {}: ${:.2}", name, balance);
    }
}