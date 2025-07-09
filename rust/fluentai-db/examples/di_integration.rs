//! Example demonstrating database DI integration

use std::sync::Arc;
use fluentai_db::{
    DbConfig, DatabaseContainerBuilderExt, DatabaseServiceProvider,
    ContainerDatabaseProvider, DatabaseServicesBuilder,
    with_transaction,
};
use fluentai_di::prelude::*;
use fluentai_effects::{EffectHandlerProvider, EffectHandlerBuilder};
use anyhow::Result;

#[tokio::main]
async fn main() -> Result<()> {
    println!("=== FluentAi Database DI Integration Example ===\n");
    
    // Method 1: Using extension trait
    println!("Method 1: Using DatabaseContainerBuilderExt");
    method1_extension_trait().await?;
    
    // Method 2: Using DatabaseServicesBuilder
    println!("\n\nMethod 2: Using DatabaseServicesBuilder");
    method2_services_builder().await?;
    
    // Method 3: Complete integration with effects
    println!("\n\nMethod 3: Complete Integration with Effects");
    method3_complete_integration().await?;
    
    Ok(())
}

async fn method1_extension_trait() -> Result<()> {
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
    
    // Create database provider
    let db_provider = ContainerDatabaseProvider::new(container.clone());
    
    // Get services
    let conn = db_provider.get_connection()?;
    println!("✓ Got database connection from DI container");
    
    // Create a table using the connection
    conn.execute_raw_unsafe(
        "CREATE TABLE users (id INTEGER PRIMARY KEY, name TEXT NOT NULL)"
    ).await?;
    println!("✓ Created users table");
    
    // Get transaction manager
    let tx_manager = db_provider.get_transaction_manager()?;
    println!("✓ Got transaction manager, active transactions: {}", 
        tx_manager.active_count().await);
    
    // Get migration runner
    let migration_runner = db_provider.get_migration_runner()?;
    migration_runner.init().await?;
    println!("✓ Initialized migration system");
    
    Ok(())
}

async fn method2_services_builder() -> Result<()> {
    // Create container with custom service configuration
    let mut builder = ContainerBuilder::new();
    
    DatabaseServicesBuilder::new()
        .with_url("sqlite::memory:")
        .with_max_connections(10)
        .enable_migrations(true)
        .enable_transactions(true)
        .enable_effects(false) // Disable effects for this example
        .build(&mut builder)?;
    
    let container = builder.build();
    
    // Resolve connection
    let conn = container.resolve::<Arc<fluentai_db::DbConnection>>()?;
    println!("✓ Resolved connection directly from container");
    
    // Use connection with transaction
    let _result = with_transaction(conn.clone(), |tx| {
        Box::pin(async move {
            // Create table
            tx.execute(
                "CREATE TABLE products (id INTEGER PRIMARY KEY, name TEXT, price REAL)",
                vec![]
            ).await?;
            
            // Insert data
            tx.execute(
                "INSERT INTO products (name, price) VALUES (?, ?), (?, ?)",
                vec![
                    fluentai_vm::Value::String("Widget".to_string()),
                    fluentai_vm::Value::Float(9.99),
                    fluentai_vm::Value::String("Gadget".to_string()),
                    fluentai_vm::Value::Float(19.99),
                ]
            ).await?;
            
            Ok(())
        })
    }).await?;
    
    println!("✓ Transaction completed successfully");
    
    // Verify data
    let rows = conn.fetch_all("SELECT COUNT(*) as count FROM products", vec![]).await?;
    println!("✓ Verified data: {} products", rows.len());
    
    Ok(())
}

async fn method3_complete_integration() -> Result<()> {
    // Create comprehensive DI setup with database and effects
    let mut builder = ContainerBuilder::new();
    
    // Register database services
    let db_config = DbConfig {
        url: "sqlite::memory:".to_string(),
        ..Default::default()
    };
    builder.register_database(db_config)?;
    
    // Register effect handlers
    builder.register_singleton(|| {
        Arc::new(
            EffectHandlerBuilder::new()
                .with_defaults()
                .build()
        )
    });
    
    // Build container
    let container = Arc::new(builder.build());
    
    // Create application service that uses both database and effects
    let app_service = ApplicationService::new(container.clone());
    
    // Run application logic
    app_service.run().await?;
    
    Ok(())
}

/// Example application service that uses DI
struct ApplicationService {
    container: Arc<Container>,
}

impl ApplicationService {
    fn new(container: Arc<Container>) -> Self {
        Self { container }
    }
    
    async fn run(&self) -> Result<()> {
        println!("Running application service with DI...");
        
        // Get database provider
        let db_provider = ContainerDatabaseProvider::new(self.container.clone());
        
        // Get services
        let conn = db_provider.get_connection()?;
        let migration_runner = db_provider.get_migration_runner()?;
        
        // Initialize migrations
        migration_runner.init().await?;
        println!("  ✓ Migration system initialized");
        
        // Run migrations
        let result = migration_runner.migrate(None).await?;
        println!("  ✓ Ran {} migrations", result.migrations_run.len());
        
        // Use the database
        self.use_database(conn).await?;
        
        // Get effect handler
        if let Ok(effect_provider) = self.container.resolve::<Arc<EffectHandlerProvider>>() {
            let context = effect_provider.create_context()?;
            println!("  ✓ Created effect context from DI");
            
            // Use database through effects
            use fluentai_effects::EffectType;
            
            let result = context.perform_sync(
                EffectType::IO,
                "db:is-connected",
                &[]
            )?;
            println!("  ✓ Database connected: {:?}", result);
        }
        
        Ok(())
    }
    
    async fn use_database(&self, conn: Arc<fluentai_db::DbConnection>) -> Result<()> {
        // Create table first
        conn.execute_raw_unsafe(
            "CREATE TABLE IF NOT EXISTS user_preferences (
                user_id INTEGER PRIMARY KEY,
                theme TEXT NOT NULL,
                language TEXT NOT NULL,
                notifications_enabled INTEGER NOT NULL,
                updated_at TEXT NOT NULL
            )"
        ).await?;
        println!("  ✓ Created user_preferences table");
        
        // Insert test data
        conn.execute(
            "INSERT INTO user_preferences (user_id, theme, language, notifications_enabled, updated_at) VALUES (?, ?, ?, ?, ?)",
            vec![
                fluentai_vm::Value::Integer(1),
                fluentai_vm::Value::String("dark".to_string()),
                fluentai_vm::Value::String("en".to_string()),
                fluentai_vm::Value::Boolean(true),
                fluentai_vm::Value::String("2024-01-01".to_string()),
            ]
        ).await?;
        
        println!("  ✓ Inserted test data");
        
        // Query data
        let rows = conn.fetch_all(
            "SELECT * FROM user_preferences WHERE user_id = ?",
            vec![fluentai_vm::Value::Integer(1)]
        ).await?;
        
        println!("  ✓ Retrieved {} user preferences", rows.len());
        
        Ok(())
    }
}