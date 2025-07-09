//! Complete demonstration of FluentAi database features
//! 
//! This example shows:
//! - Query builder usage
//! - Schema creation
//! - Migration system
//! - Transaction management
//! - DI integration
//! - Effect handlers

use fluentai_db::*;
use fluentai_db::query::*;
use fluentai_db::schema::*;
use fluentai_di::prelude::*;
use std::sync::Arc;

fn main() {
    println!("=== FluentAi Database System Demo ===\n");
    
    // 1. Query Builder Demo
    query_builder_demo();
    
    // 2. Schema Builder Demo
    schema_builder_demo();
    
    // 3. Migration Builder Demo
    migration_builder_demo();
    
    // 4. DI Integration Demo
    di_integration_demo();
}

fn query_builder_demo() {
    println!("1. Query Builder Demo");
    println!("--------------------");
    
    // Simple SELECT query
    let mut simple_query = QueryBuilder::new()
        .from("users")
        .select(vec!["id", "name", "email"])
        .limit(10)
        .build();
    
    let (sql, _) = simple_query.to_parameterized_sql().unwrap();
    println!("Simple query: {}", sql);
    
    // Complex query with parameters
    let mut builder = QueryBuilder::new();
    let age_param = builder.next_param(fluentai_vm::Value::Integer(18));
    let status_param = builder.next_param(fluentai_vm::Value::String("active".to_string()));
    
    let mut complex_query = builder
        .from("users")
        .select(vec!["id", "name", "email"])
        .where_clause(and(
            gt(col("age"), age_param),
            eq(col("status"), status_param)
        ))
        .order_by("created_at", OrderDir::Desc)
        .limit(50)
        .build();
    
    let (sql, params) = complex_query.to_parameterized_sql().unwrap();
    println!("\nComplex query: {}", sql);
    println!("Parameters: {} values", params.len());
    
    // Aggregate query
    let count_expr = QueryExpr::Aggregate {
        func: AggregateFunc::Count,
        expr: Box::new(col("id")),
    };
    
    let sum_expr = QueryExpr::Aggregate {
        func: AggregateFunc::Sum,
        expr: Box::new(col("amount")),
    };
    
    let mut agg_query = QueryBuilder::new()
        .from("orders")
        .select_expr(col("customer_id"), None)
        .select_expr(count_expr, Some("order_count".to_string()))
        .select_expr(sum_expr, Some("total_amount".to_string()))
        .group_by(vec!["customer_id"])
        .build();
    
    let (sql, _) = agg_query.to_parameterized_sql().unwrap();
    println!("\nAggregate query: {}", sql);
    
    println!();
}

fn schema_builder_demo() {
    println!("2. Schema Builder Demo");
    println!("---------------------");
    
    // Build a user table schema
    let users_schema = SchemaBuilder::new("users")
        .add_field(Field::new("id", FieldType::Uuid).primary_key())
        .add_field_with("username", FieldType::String(Some(50)), |f| 
            f.unique().not_null()
        )
        .add_field_with("email", FieldType::String(Some(255)), |f| 
            f.unique().not_null()
        )
        .add_field_with("password_hash", FieldType::String(Some(255)), |f| 
            f.not_null()
        )
        .add_field(Field::new("profile", FieldType::Json))
        .add_field_with("is_active", FieldType::Bool, |f| 
            f.not_null().default("true")
        )
        .add_field_with("created_at", FieldType::Timestamp, |f| 
            f.not_null().default("CURRENT_TIMESTAMP")
        )
        .add_field_with("updated_at", FieldType::Timestamp, |f| 
            f.not_null().default("CURRENT_TIMESTAMP")
        )
        .add_index("idx_email", vec!["email".to_string()], false)
        .add_index("idx_username", vec!["username".to_string()], false)
        .comment("User accounts table")
        .build();
    
    // Generate SQL for different dialects
    println!("PostgreSQL:");
    println!("{}\n", users_schema.to_create_sql(SqlDialect::Postgres).unwrap());
    
    println!("MySQL:");
    println!("{}\n", users_schema.to_create_sql(SqlDialect::MySQL).unwrap());
    
    println!("SQLite:");
    println!("{}\n", users_schema.to_create_sql(SqlDialect::SQLite).unwrap());
    
    // Build a related table with foreign key
    let posts_schema = SchemaBuilder::new("posts")
        .add_field(Field::new("id", FieldType::Int).primary_key())
        .add_field_with("user_id", FieldType::Uuid, |f| 
            f.not_null()
             .foreign_key("users", "id", ForeignKeyAction::Cascade, ForeignKeyAction::Cascade)
        )
        .add_field(Field::new("title", FieldType::String(Some(200))).not_null())
        .add_field(Field::new("content", FieldType::Text))
        .add_field_with("published", FieldType::Bool, |f| f.default("false"))
        .add_field_with("created_at", FieldType::Timestamp, |f| 
            f.not_null().default("CURRENT_TIMESTAMP")
        )
        .add_index("idx_user_posts", vec!["user_id".to_string(), "created_at".to_string()], false)
        .build();
    
    println!("Posts table (with foreign key):");
    println!("{}\n", posts_schema.to_create_sql(SqlDialect::Postgres).unwrap());
}

fn migration_builder_demo() {
    println!("3. Migration Builder Demo");
    println!("------------------------");
    
    // Create initial migration
    let users_schema = SchemaBuilder::new("users")
        .add_field(Field::new("id", FieldType::Int).primary_key())
        .add_field(Field::new("username", FieldType::String(Some(50))).unique().not_null())
        .add_field(Field::new("email", FieldType::String(Some(255))).unique().not_null())
        .add_field(Field::new("created_at", FieldType::Timestamp).not_null())
        .build();
    
    let initial_migration = MigrationBuilder::new("001", "create_users")
        .description("Create initial users table")
        .create_table("users", users_schema)
        .build();
    
    println!("Migration 001 - Create Users:");
    println!("Version: {}", initial_migration.version());
    println!("Name: {}", initial_migration.name());
    println!("Description: {:?}\n", initial_migration.description());
    
    // Create evolution migration
    let evolution_migration = MigrationBuilder::new("002", "add_user_features")
        .description("Add profile and preferences to users")
        .add_column("users", "profile_json", FieldType::Json)
        .add_column_not_null("users", "is_active", FieldType::Bool, "true")
        .add_column("users", "last_login", FieldType::Timestamp)
        .create_index("idx_active_users", "users", vec!["is_active".to_string(), "last_login".to_string()])
        .build();
    
    println!("Migration 002 - Add Features:");
    println!("Version: {}", evolution_migration.version());
    println!("Name: {}", evolution_migration.name());
    println!("Description: {:?}\n", evolution_migration.description());
    
    // Create complex migration with multiple operations
    let complex_migration = MigrationBuilder::new("003", "user_preferences")
        .description("Create user preferences system")
        .create_table("user_preferences", 
            SchemaBuilder::new("user_preferences")
                .add_field(Field::new("user_id", FieldType::Int)
                    .foreign_key("users", "id", ForeignKeyAction::Cascade, ForeignKeyAction::Cascade)
                    .primary_key())
                .add_field(Field::new("theme", FieldType::String(Some(20))).default("'light'"))
                .add_field(Field::new("language", FieldType::String(Some(10))).default("'en'"))
                .add_field(Field::new("notifications", FieldType::Bool).default("true"))
                .build()
        )
        .rename_column("users", "profile_json", "profile")
        .up_sql("UPDATE users SET is_active = true WHERE is_active IS NULL")
        .down_sql("UPDATE users SET is_active = NULL WHERE is_active = true")
        .build();
    
    println!("Migration 003 - User Preferences:");
    println!("Version: {}", complex_migration.version());
    println!("Name: {}", complex_migration.name());
    println!("Description: {:?}\n", complex_migration.description());
}

fn di_integration_demo() {
    println!("4. DI Integration Demo");
    println!("---------------------");
    
    // Create DI container with database services
    let mut builder = ContainerBuilder::new();
    
    // Method 1: Using extension trait
    let config = DbConfig {
        url: "sqlite::memory:".to_string(),
        max_connections: 10,
        min_connections: 2,
        connect_timeout: std::time::Duration::from_secs(5),
        idle_timeout: Some(std::time::Duration::from_secs(300)),
    };
    
    builder.register_database(config).unwrap();
    
    // Method 2: Using DatabaseServicesBuilder for custom configuration
    let mut builder2 = ContainerBuilder::new();
    DatabaseServicesBuilder::new()
        .with_url("postgres://localhost/myapp")
        .with_max_connections(20)
        .enable_migrations(true)
        .enable_transactions(true)
        .enable_effects(true)
        .build(&mut builder2)
        .unwrap();
    
    let container = builder.build();
    
    // Resolve services
    match container.resolve::<DbConfig>() {
        Ok(config) => {
            println!("✓ Resolved DbConfig:");
            println!("  URL: {}", config.url);
            println!("  Max connections: {}", config.max_connections);
            println!("  Min connections: {}", config.min_connections);
        }
        Err(e) => println!("✗ Failed to resolve DbConfig: {}", e),
    }
    
    match container.resolve::<Arc<ConnectionPool>>() {
        Ok(_) => println!("✓ Resolved ConnectionPool"),
        Err(e) => println!("✗ Failed to resolve ConnectionPool: {}", e),
    }
    
    // Create database provider
    let provider = ContainerDatabaseProvider::new(Arc::new(container));
    println!("\n✓ Created ContainerDatabaseProvider");
    
    // In a real application, you would use these services:
    // - provider.get_connection() for database connections
    // - provider.get_transaction_manager() for transaction management
    // - provider.get_migration_runner() for running migrations
    
    println!("\nDatabase system is ready for use!");
}