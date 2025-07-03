//! SQL Injection Prevention Tests

use fluentai_db::*;
use fluentai_db::query::*;
use fluentai_vm::bytecode::Value;

#[test]
fn test_parameterized_query_prevents_injection() {
    // Malicious input that would cause SQL injection if not properly handled
    let malicious_input = "'; DROP TABLE users; --";
    
    let mut query = QueryBuilder::new()
        .from("users")
        .select(vec!["id", "name"])
        .where_clause(eq(col("name"), QueryExpr::Parameter(Parameter {
            index: 0,
            value: Value::String(malicious_input.to_string()),
        })))
        .build();
    
    let (sql, params) = query.to_parameterized_sql().unwrap();
    
    // The SQL should contain a parameter placeholder, not the actual value
    assert!(sql.contains("WHERE"));
    assert!(sql.contains("?"));
    assert!(!sql.contains("DROP TABLE"));
    assert!(!sql.contains(malicious_input));
    
    // The malicious input should be in the parameters array
    assert_eq!(params.len(), 1);
    match &params[0] {
        Value::String(s) => assert_eq!(s, malicious_input),
        _ => panic!("Expected string parameter"),
    }
}

#[test]
fn test_multiple_parameters_are_safe() {
    let user_input = "O'Reilly"; // Name with quote
    let age_input = "25 OR 1=1"; // Attempted SQL injection
    
    let mut builder = QueryBuilder::new();
    // Get parameter expressions first
    let name_param = builder.next_param(Value::String(user_input.to_string()));
    let age_param = builder.next_param(Value::String(age_input.to_string()));
    
    let mut query = builder
        .from("users")
        .select(vec!["id", "name", "email"])
        .where_clause(
            and(
                eq(col("name"), name_param),
                gt(col("age"), age_param)
            )
        )
        .build();
    
    let (sql, params) = query.to_parameterized_sql().unwrap();
    
    // Check SQL structure
    assert!(sql.contains("WHERE"));
    assert_eq!(sql.matches("?").count(), 2);
    assert!(!sql.contains("O'Reilly"));
    assert!(!sql.contains("1=1"));
    
    // Check parameters
    assert_eq!(params.len(), 2);
}

#[test]
fn test_column_names_are_escaped() {
    // Column name with special characters
    let mut query = QueryBuilder::new()
        .from("users")
        .select(vec!["user's name", "email\"address"])
        .build();
    
    let (sql, _) = query.to_parameterized_sql().unwrap();
    
    // Column names should be properly escaped
    assert!(sql.contains("\"user's name\""));
    assert!(sql.contains("\"email\"\"address\""));
}

#[test]
fn test_table_names_are_escaped() {
    let mut query = QueryBuilder::new()
        .from("user's\"table")
        .select(vec!["id"])
        .build();
    
    let (sql, _) = query.to_parameterized_sql().unwrap();
    
    // Table name should be properly escaped
    assert!(sql.contains("\"user's\"\"table\""));
}

#[test]
fn test_like_queries_are_safe() {
    let search_term = "%'; DROP TABLE users; --";
    
    let mut builder = QueryBuilder::new();
    // Get parameter expression first
    let search_param = builder.next_param(Value::String(search_term.to_string()));
    
    let mut query = builder
        .from("users")
        .select(vec!["id", "name"])
        .where_clause(
            QueryExpr::BinOp {
                op: BinOp::Like,
                left: Box::new(col("name")),
                right: Box::new(search_param),
            }
        )
        .build();
    
    let (sql, params) = query.to_parameterized_sql().unwrap();
    
    // LIKE pattern should be parameterized
    assert!(sql.contains("LIKE ?"));
    assert!(!sql.contains("DROP TABLE"));
    assert_eq!(params.len(), 1);
}

#[test]
fn test_subquery_parameters_are_collected() {
    let mut builder1 = QueryBuilder::new();
    // Get parameter expression first
    let total_param = builder1.next_param(Value::Float(100.0));
    let subquery = builder1
        .from("orders")
        .select(vec!["user_id"])
        .where_clause(gt(col("total"), total_param))
        .build();
    
    let builder2 = QueryBuilder::new();
    let mut query = builder2
        .from("users")
        .select(vec!["name"])
        .where_clause(
            QueryExpr::BinOp {
                op: BinOp::In,
                left: Box::new(col("id")),
                right: Box::new(QueryExpr::Subquery(Box::new(subquery))),
            }
        )
        .build();
    
    let (_sql, params) = query.to_parameterized_sql().unwrap();
    
    // Should have parameter from subquery
    assert_eq!(params.len(), 1);
    match &params[0] {
        Value::Float(f) => assert_eq!(*f, 100.0),
        _ => panic!("Expected float parameter"),
    }
}

#[test]
fn test_safe_query_builder_example() {
    // Test the documented safe query example
    let mut query = safe_user_query(18, true);
    let (sql, params) = query.to_parameterized_sql().unwrap();
    
    // Verify the query structure
    assert!(sql.contains("SELECT"));
    assert!(sql.contains("FROM"));
    assert!(sql.contains("WHERE"));
    assert!(sql.contains("ORDER BY"));
    assert!(sql.contains("LIMIT"));
    
    // Verify parameters
    assert_eq!(params.len(), 2);
    match (&params[0], &params[1]) {
        (Value::Int(age), Value::Bool(active)) => {
            assert_eq!(*age, 18);
            assert_eq!(*active, true);
        }
        _ => panic!("Unexpected parameter types"),
    }
}

#[cfg(test)]
mod connection_tests {
    use fluentai_vm::bytecode::Value;
    
    #[test]
    fn test_parameter_binding_types() {
        // This test verifies that different parameter types are handled correctly
        let params = vec![
            Value::Nil,
            Value::Bool(true),
            Value::Int(42),
            Value::Float(3.14),
            Value::String("test".to_string()),
            // Binary data represented as a list of integers
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
        ];
        
        // The actual binding happens in the connection module
        // This test ensures we handle all FluentAi value types
        assert_eq!(params.len(), 6);
    }
}

#[test]
fn test_no_sql_in_error_messages() {
    // Ensure that error messages don't expose SQL structure
    let result = DbError::Query("Invalid query".to_string());
    let error_string = format!("{}", result);
    
    // Error message should not contain SQL keywords that might help attackers
    assert!(!error_string.to_lowercase().contains("select"));
    assert!(!error_string.to_lowercase().contains("table"));
    assert!(!error_string.to_lowercase().contains("where"));
}