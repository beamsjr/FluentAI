//! Simple example demonstrating database DI integration without actual database connections

use std::sync::Arc;
use fluentai_db::{
    DbConfig, DatabaseContainerBuilderExt, DatabaseServiceProvider,
    DatabaseServicesBuilder,
};
use fluentai_di::prelude::*;
use anyhow::Result;

fn main() -> Result<()> {
    println!("=== FluentAi Database DI Integration Example ===\n");
    
    // Method 1: Using extension trait
    println!("Method 1: Using DatabaseContainerBuilderExt");
    method1_extension_trait()?;
    
    // Method 2: Using DatabaseServicesBuilder
    println!("\n\nMethod 2: Using DatabaseServicesBuilder");
    method2_services_builder()?;
    
    Ok(())
}

fn method1_extension_trait() -> Result<()> {
    // Create DI container with database services
    let mut builder = ContainerBuilder::new();
    
    // Register database with custom config
    let config = DbConfig {
        url: "sqlite::memory:".to_string(),
        max_connections: 5,
        ..Default::default()
    };
    
    builder.register_database(config)?;
    
    let container = Arc::new(builder.build());
    
    // Check that configuration was registered
    let registered_config = container.resolve::<DbConfig>()?;
    println!("✓ Got database config from DI container: {}", registered_config.url);
    println!("✓ Max connections: {}", registered_config.max_connections);
    
    // Check that pool was registered
    let pool_result = container.resolve::<Arc<fluentai_db::ConnectionPool>>();
    match pool_result {
        Ok(_) => println!("✓ ConnectionPool is registered in the container"),
        Err(e) => println!("✗ ConnectionPool not found: {}", e),
    }
    
    Ok(())
}

fn method2_services_builder() -> Result<()> {
    // Create container with custom service configuration
    let mut builder = ContainerBuilder::new();
    
    DatabaseServicesBuilder::new()
        .with_url("postgres://localhost/test")
        .with_max_connections(10)
        .enable_migrations(true)
        .enable_transactions(true)
        .enable_effects(false) // Disable effects for this example
        .build(&mut builder)?;
    
    let container = builder.build();
    
    // Check configuration
    let config = container.resolve::<DbConfig>()?;
    println!("✓ Database URL: {}", config.url);
    println!("✓ Max connections: {}", config.max_connections);
    
    // Check that pool was registered
    let pool_result = container.resolve::<Arc<fluentai_db::ConnectionPool>>();
    match pool_result {
        Ok(_) => println!("✓ ConnectionPool is registered"),
        Err(e) => println!("✗ ConnectionPool not found: {}", e),
    }
    
    // Call mock example
    mock_example();
    
    Ok(())
}

/// Example showing how to create a mock database provider for testing
struct MockDatabaseProvider;

impl DatabaseServiceProvider for MockDatabaseProvider {
    fn get_connection(&self) -> fluentai_db::error::DbResult<Arc<fluentai_db::DbConnection>> {
        Err(fluentai_db::error::DbError::Connection("Mock provider - no real connection".into()))
    }
    
    fn get_transaction_manager(&self) -> fluentai_db::error::DbResult<Arc<fluentai_db::TransactionManager>> {
        Err(fluentai_db::error::DbError::Connection("Mock provider - no real transaction manager".into()))
    }
    
    fn get_migration_runner(&self) -> fluentai_db::error::DbResult<Arc<fluentai_db::MigrationRunner>> {
        Err(fluentai_db::error::DbError::Connection("Mock provider - no real migration runner".into()))
    }
}

// Example showing the mock provider
fn mock_example() {
    println!("\n\nMock Provider Example:");
    let provider = MockDatabaseProvider;
    
    match provider.get_connection() {
        Ok(_) => println!("✗ Unexpected success"),
        Err(e) => println!("✓ Mock provider correctly returns error: {}", e),
    }
}