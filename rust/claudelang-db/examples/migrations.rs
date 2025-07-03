//! Example demonstrating database migrations

use std::sync::Arc;
use claudelang_db::{
    DbConfig, DbConnection, MigrationBuilder, MigrationRunner,
    SqlMigration, Direction, SchemaBuilder, FieldType,
};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    println!("=== ClaudeLang Database Migration Example ===\n");
    
    // Setup database connection
    let config = DbConfig {
        url: "sqlite::memory:".to_string(),
        ..Default::default()
    };
    
    let connection = Arc::new(DbConnection::new(&config).await?);
    
    // Create migration runner
    let mut runner = MigrationRunner::new(connection.clone());
    
    // Initialize migration system
    runner.init().await?;
    println!("✓ Migration system initialized");
    
    // Define migrations
    create_migrations(&mut runner);
    
    // Show migration status
    println!("\n=== Migration Status ===");
    show_status(&runner).await?;
    
    // Run all migrations
    println!("\n=== Running Migrations ===");
    let result = runner.migrate(None).await?;
    println!("✓ Ran {} migrations", result.migrations_run.len());
    for info in &result.migrations_run {
        println!("  - {} ({}): {} ms", 
            info.version, 
            info.name,
            info.execution_time_ms.unwrap_or(0)
        );
    }
    
    // Show updated status
    println!("\n=== Updated Status ===");
    show_status(&runner).await?;
    
    // Verify tables exist
    println!("\n=== Verifying Database Schema ===");
    verify_schema(connection.clone()).await?;
    
    // Demonstrate rollback
    println!("\n=== Rolling Back Last Migration ===");
    let rollback_result = runner.rollback(Some("002")).await?;
    println!("✓ Rolled back {} migrations", rollback_result.migrations_run.len());
    
    // Show final status
    println!("\n=== Final Status ===");
    show_status(&runner).await?;
    
    Ok(())
}

fn create_migrations(runner: &mut MigrationRunner) {
    // Migration 001: Create users table
    let users_schema = SchemaBuilder::new("users")
        .add_field_with("id", FieldType::Int, |f| f.primary_key())
        .add_field_with("username", FieldType::Text, |f| f.not_null())
        .add_field_with("email", FieldType::Text, |f| f.not_null())
        .add_field_with("created_at", FieldType::Text, |f| f.not_null())
        .build();
    
    let migration_001 = MigrationBuilder::new("001", "create_users_table")
        .description("Create the users table")
        .create_table("users", users_schema)
        .build();
    
    runner.add_migration(migration_001);
    
    // Migration 002: Create posts table
    let posts_schema = SchemaBuilder::new("posts")
        .add_field_with("id", FieldType::Int, |f| f.primary_key())
        .add_field_with("user_id", FieldType::Int, |f| f.not_null())
        .add_field_with("title", FieldType::Text, |f| f.not_null())
        .add_field_with("content", FieldType::Text, |f| f)
        .add_field_with("published", FieldType::Bool, |f| f.not_null())
        .add_field_with("created_at", FieldType::Text, |f| f.not_null())
        .build();
    
    let migration_002 = MigrationBuilder::new("002", "create_posts_table")
        .description("Create the posts table")
        .create_table("posts", posts_schema)
        .create_index("idx_posts_user_id", "posts", vec!["user_id".to_string()])
        .build();
    
    runner.add_migration(migration_002);
    
    // Migration 003: Add user profile fields
    let migration_003 = MigrationBuilder::new("003", "add_user_profile")
        .description("Add profile fields to users table")
        .add_column("users", "bio", FieldType::Text)
        .add_column_not_null("users", "active", FieldType::Bool, "1")
        .build();
    
    runner.add_migration(migration_003);
    
    // Migration 004: Raw SQL migration
    let migration_004 = Arc::new(SqlMigration::new(
        "004",
        "seed_initial_data",
        r#"
        INSERT INTO users (username, email, created_at, bio, active) 
        VALUES 
            ('admin', 'admin@example.com', datetime('now'), 'System administrator', 1),
            ('demo', 'demo@example.com', datetime('now'), 'Demo user', 1);
        
        INSERT INTO posts (user_id, title, content, published, created_at)
        VALUES
            (1, 'Welcome to ClaudeLang', 'This is the first post!', 1, datetime('now')),
            (1, 'Migration System', 'Database migrations are now supported', 1, datetime('now'));
        "#,
        r#"
        DELETE FROM posts WHERE user_id IN (1, 2);
        DELETE FROM users WHERE username IN ('admin', 'demo');
        "#
    ).with_description("Seed initial data"));
    
    runner.add_migration(migration_004);
}

async fn show_status(runner: &MigrationRunner) -> anyhow::Result<()> {
    let status = runner.status().await?;
    
    for s in status {
        match s {
            claudelang_db::migration::runner::MigrationStatus::Pending { migration } => {
                println!("  [ ] {} - {} (pending)", 
                    migration.version(), 
                    migration.name()
                );
            }
            claudelang_db::migration::runner::MigrationStatus::Applied { metadata, checksum_valid } => {
                let checksum_status = if checksum_valid { "✓" } else { "✗" };
                println!("  [✓] {} - {} ({} checksum)", 
                    metadata.version, 
                    metadata.name,
                    checksum_status
                );
            }
            claudelang_db::migration::runner::MigrationStatus::Missing { metadata } => {
                println!("  [?] {} - {} (missing)", 
                    metadata.version, 
                    metadata.name
                );
            }
        }
    }
    
    Ok(())
}

async fn verify_schema(connection: Arc<DbConnection>) -> anyhow::Result<()> {
    // Check users table
    let users_count = connection.fetch_all(
        "SELECT COUNT(*) as count FROM users",
        vec![]
    ).await?;
    println!("  ✓ Users table exists with {} records", users_count.len());
    
    // Check posts table
    let posts_count = connection.fetch_all(
        "SELECT COUNT(*) as count FROM posts",
        vec![]
    ).await?;
    println!("  ✓ Posts table exists with {} records", posts_count.len());
    
    // Show sample data
    let users = connection.fetch_all(
        "SELECT username, email, active FROM users",
        vec![]
    ).await?;
    
    if !users.is_empty() {
        println!("\n  Sample users:");
        for row in users {
            use sqlx::Row;
            let username: String = row.try_get("username").unwrap_or_default();
            let email: String = row.try_get("email").unwrap_or_default();
            let active: bool = row.try_get("active").unwrap_or(false);
            println!("    - {} ({}) [active: {}]", username, email, active);
        }
    }
    
    Ok(())
}