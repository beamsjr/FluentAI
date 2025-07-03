//! Integration tests for claudelang-db
//! 
//! These tests demonstrate how to use the database system in practice.
//! Note: These tests require an actual database connection to run.

use claudelang_db::*;
use claudelang_di::prelude::*;
use std::sync::Arc;

#[tokio::test]
#[ignore] // Ignore by default since it requires a real database
async fn test_full_database_workflow() {
    // Set up DI container with database services
    let mut builder = ContainerBuilder::new();
    
    let config = DbConfig {
        url: "sqlite::memory:".to_string(),
        max_connections: 5,
        ..Default::default()
    };
    
    builder.register_database(config).unwrap();
    let container = Arc::new(builder.build());
    
    // Get database provider
    let db_provider = ContainerDatabaseProvider::new(container.clone());
    
    // Get connection
    let conn = db_provider.get_connection().unwrap();
    
    // Create a table
    conn.execute_raw_unsafe(
        "CREATE TABLE test_users (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            email TEXT UNIQUE,
            age INTEGER CHECK(age >= 0)
        )"
    ).await.unwrap();
    
    // Insert data using parameterized query
    let rows_affected = conn.execute(
        "INSERT INTO test_users (name, email, age) VALUES (?, ?, ?)",
        vec![
            claudelang_vm::bytecode::Value::String("Alice".to_string()),
            claudelang_vm::bytecode::Value::String("alice@example.com".to_string()),
            claudelang_vm::bytecode::Value::Int(25),
        ]
    ).await.unwrap();
    
    assert_eq!(rows_affected, 1);
    
    // Query data
    let rows = conn.fetch_all(
        "SELECT * FROM test_users WHERE age > ?",
        vec![claudelang_vm::bytecode::Value::Int(20)]
    ).await.unwrap();
    
    assert_eq!(rows.len(), 1);
    
    // Use transaction
    let result = with_transaction(conn.clone(), |tx| {
        Box::pin(async move {
            // Insert another user
            tx.execute(
                "INSERT INTO test_users (name, email, age) VALUES (?, ?, ?)",
                vec![
                    claudelang_vm::bytecode::Value::String("Bob".to_string()),
                    claudelang_vm::bytecode::Value::String("bob@example.com".to_string()),
                    claudelang_vm::bytecode::Value::Int(30),
                ]
            ).await?;
            
            // Update age
            tx.execute(
                "UPDATE test_users SET age = age + 1 WHERE name = ?",
                vec![claudelang_vm::bytecode::Value::String("Alice".to_string())]
            ).await?;
            
            Ok(())
        })
    }).await.unwrap();
    
    // Verify transaction results
    let all_users = conn.fetch_all("SELECT * FROM test_users", vec![]).await.unwrap();
    assert_eq!(all_users.len(), 2);
}
