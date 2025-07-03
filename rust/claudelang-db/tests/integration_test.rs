//! Integration tests for claudelang-db

use claudelang_db::*;
use claudelang_db::query::*;
use claudelang_db::schema::*;
use claudelang_vm::bytecode::Value;

#[test]
fn test_query_builder() {
    let query = QueryBuilder::new()
        .from("users")
        .select(vec!["id", "name", "email"])
        .where_clause(gt(col("age"), val(Value::Int(18))))
        .order_by("name", OrderDir::Asc)
        .limit(10)
        .build();
    
    let sql = query.to_sql().unwrap();
    assert!(sql.contains("SELECT id, name, email"));
    assert!(sql.contains("FROM users"));
    assert!(sql.contains("WHERE (age > 18)"));
    assert!(sql.contains("ORDER BY name ASC"));
    assert!(sql.contains("LIMIT 10"));
}

#[test]
fn test_complex_query() {
    let subquery = QueryBuilder::new()
        .from("orders")
        .select(vec!["user_id"])
        .where_clause(gt(col("total"), val(Value::Float(100.0))))
        .build();
    
    let query = QueryBuilder::new()
        .from("users")
        .select(vec!["name", "email"])
        .where_clause(QueryExpr::BinOp {
            op: BinOp::In,
            left: Box::new(col("id")),
            right: Box::new(QueryExpr::Subquery(Box::new(subquery))),
        })
        .build();
    
    let sql = query.to_sql().unwrap();
    assert!(sql.contains("SELECT name, email FROM users"));
    assert!(sql.contains("WHERE (id IN (SELECT user_id FROM orders WHERE (total > 100)))"));
}

#[test]
fn test_schema_builder() {
    let schema = SchemaBuilder::new("users")
        .add_field(
            Field::new("id", FieldType::Int)
                .primary_key()
                .not_null()
        )
        .add_field(
            Field::new("email", FieldType::String(Some(255)))
                .unique()
                .not_null()
        )
        .add_field(
            Field::new("name", FieldType::String(Some(100)))
                .not_null()
        )
        .add_field(
            Field::new("age", FieldType::Int)
                .check("age >= 0")
        )
        .add_field(
            Field::new("created_at", FieldType::Timestamp)
                .not_null()
                .default("CURRENT_TIMESTAMP")
        )
        .add_index("idx_email", vec!["email".to_string()], true)
        .build();
    
    let sql = schema.to_create_sql(SqlDialect::Postgres).unwrap();
    assert!(sql.contains("CREATE TABLE users"));
    assert!(sql.contains("id INTEGER PRIMARY KEY NOT NULL"));
    assert!(sql.contains("email VARCHAR(255) UNIQUE NOT NULL"));
    assert!(sql.contains("age INTEGER CHECK (age >= 0)"));
    assert!(sql.contains("CREATE UNIQUE INDEX idx_email ON users (email)"));
}

#[test]
fn test_foreign_key_schema() {
    let orders_schema = SchemaBuilder::new("orders")
        .add_field(
            Field::new("id", FieldType::Int)
                .primary_key()
                .not_null()
        )
        .add_field(
            Field::new("user_id", FieldType::Int)
                .not_null()
                .foreign_key("users", "id", ForeignKeyAction::Cascade, ForeignKeyAction::Cascade)
        )
        .add_field(
            Field::new("total", FieldType::Decimal(10, 2))
                .not_null()
        )
        .build();
    
    let sql = orders_schema.to_create_sql(SqlDialect::Postgres).unwrap();
    assert!(sql.contains("FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE ON UPDATE CASCADE"));
}

#[test]
fn test_aggregate_query() {
    let query = QueryBuilder::new()
        .from("orders")
        .select_expr(
            QueryExpr::Aggregate {
                func: AggregateFunc::Count,
                expr: Box::new(col("*")),
            },
            Some("order_count".to_string())
        )
        .select_expr(
            QueryExpr::Aggregate {
                func: AggregateFunc::Sum,
                expr: Box::new(col("total")),
            },
            Some("total_revenue".to_string())
        )
        .group_by(vec!["user_id"])
        .build();
    
    let sql = query.to_sql().unwrap();
    assert!(sql.contains("SELECT COUNT(*) AS order_count, SUM(total) AS total_revenue"));
    assert!(sql.contains("GROUP BY user_id"));
}

#[tokio::test]
async fn test_connection_pool() {
    // This test requires a test database to be available
    // Skip if no test database URL is provided
    if std::env::var("TEST_DATABASE_URL").is_err() {
        println!("Skipping connection pool test - no TEST_DATABASE_URL provided");
        return;
    }
    
    let config = DbConfig {
        url: std::env::var("TEST_DATABASE_URL").unwrap(),
        ..Default::default()
    };
    
    let pool = ConnectionPool::new(config);
    let conn = pool.get_connection().await.unwrap();
    assert!(conn.is_connected().await);
    
    pool.close().await;
}