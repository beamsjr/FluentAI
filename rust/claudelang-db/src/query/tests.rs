//! Tests for query builder

#[cfg(test)]
mod tests {
    use super::super::*;
    use claudelang_vm::bytecode::Value;
    
    #[test]
    fn test_query_builder_basic() {
        let mut query = QueryBuilder::new()
            .from("users")
            .select(vec!["id", "name", "email"])
            .limit(10)
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("SELECT"));
        assert!(sql.contains("FROM"));
        assert!(sql.contains("LIMIT 10"));
        assert_eq!(params.len(), 0); // No parameters in this query
    }
    
    #[test]
    fn test_query_with_parameters() {
        let mut builder = QueryBuilder::new();
        
        // Create parameter expressions
        let age_param = builder.next_param(Value::Int(18));
        let active_param = builder.next_param(Value::Bool(true));
        
        let mut query = builder
            .from("users")
            .select(vec!["id", "name"])
            .where_clause(and(
                gt(col("age"), age_param),
                eq(col("active"), active_param)
            ))
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("WHERE"));
        assert!(sql.contains("age"));
        assert!(sql.contains("active"));
        assert_eq!(params.len(), 2);
        assert_eq!(params[0], Value::Int(18));
        assert_eq!(params[1], Value::Bool(true));
    }
    
    #[test]
    fn test_query_expressions() {
        // Test column expression
        let col_expr = col("username");
        match col_expr {
            QueryExpr::Column(name) => assert_eq!(name, "username"),
            _ => panic!("Expected Column expression"),
        }
        
        // Test equality expression
        let eq_expr = eq(col("status"), col("expected_status"));
        match eq_expr {
            QueryExpr::BinOp { op, left, right } => {
                assert_eq!(op, BinOp::Eq);
                match (left.as_ref(), right.as_ref()) {
                    (QueryExpr::Column(l), QueryExpr::Column(r)) => {
                        assert_eq!(l, "status");
                        assert_eq!(r, "expected_status");
                    }
                    _ => panic!("Expected Column expressions"),
                }
            }
            _ => panic!("Expected BinOp expression"),
        }
    }
    
    #[test]
    fn test_complex_where_clause() {
        let mut builder = QueryBuilder::new();
        
        let age_param = builder.next_param(Value::Int(21));
        let city_param = builder.next_param(Value::String("NYC".to_string()));
        let vip_param = builder.next_param(Value::Bool(true));
        
        // (age >= 21 AND city = 'NYC') OR vip = true
        let where_expr = QueryExpr::BinOp {
            op: BinOp::Or,
            left: Box::new(and(
                QueryExpr::BinOp {
                    op: BinOp::Ge,
                    left: Box::new(col("age")),
                    right: Box::new(age_param),
                },
                eq(col("city"), city_param)
            )),
            right: Box::new(eq(col("vip"), vip_param)),
        };
        
        let mut query = builder
            .from("users")
            .select(vec!["*"])
            .where_clause(where_expr)
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("WHERE"));
        assert!(sql.contains("AND"));
        assert!(sql.contains("OR"));
        assert_eq!(params.len(), 3);
    }
    
    #[test]
    fn test_order_by() {
        let mut query = QueryBuilder::new()
            .from("products")
            .select(vec!["name", "price"])
            .order_by("price", OrderDir::Desc)
            .order_by("name", OrderDir::Asc)
            .build();
        
        let (sql, _) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("ORDER BY"));
        assert!(sql.contains("DESC"));
        assert!(sql.contains("ASC"));
    }
    
    #[test]
    fn test_aggregate_functions() {
        let count_expr = QueryExpr::Aggregate {
            func: AggregateFunc::Count,
            expr: Box::new(col("id")),
        };
        
        let sum_expr = QueryExpr::Aggregate {
            func: AggregateFunc::Sum,
            expr: Box::new(col("amount")),
        };
        
        let mut query = QueryBuilder::new()
            .from("orders")
            .select_expr(count_expr, Some("total_orders".to_string()))
            .select_expr(sum_expr, Some("total_amount".to_string()))
            .group_by(vec!["customer_id"])
            .build();
        
        let (sql, _) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("COUNT"));
        assert!(sql.contains("SUM"));
        assert!(sql.contains("GROUP BY"));
        assert!(sql.contains("AS total_orders"));
        assert!(sql.contains("AS total_amount"));
    }
    
    #[test]
    fn test_join_clause() {
        let join_condition = eq(
            col("users.id"),
            col("orders.user_id")
        );
        
        let mut query = Query::new();
        query.from = Some(QueryExpr::Table("users".to_string()));
        query.select = vec![
            (col("users.name"), None),
            (col("orders.total"), None),
        ];
        query.joins = vec![
            (JoinType::Inner, QueryExpr::Table("orders".to_string()), join_condition)
        ];
        
        let (sql, _) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("INNER JOIN"));
        assert!(sql.contains("ON"));
    }
    
    #[test]
    fn test_case_expression() {
        let case_expr = QueryExpr::Case {
            conditions: vec![
                (
                    gt(col("age"), QueryExpr::Parameter(Parameter { index: 0, value: Value::Int(65) })),
                    QueryExpr::Parameter(Parameter { index: 1, value: Value::String("Senior".to_string()) })
                ),
                (
                    gt(col("age"), QueryExpr::Parameter(Parameter { index: 2, value: Value::Int(18) })),
                    QueryExpr::Parameter(Parameter { index: 3, value: Value::String("Adult".to_string()) })
                ),
            ],
            else_expr: Some(Box::new(QueryExpr::Parameter(Parameter { 
                index: 4, 
                value: Value::String("Minor".to_string()) 
            }))),
        };
        
        let mut query = QueryBuilder::new()
            .from("users")
            .select_expr(col("name"), None)
            .select_expr(case_expr, Some("category".to_string()))
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("CASE"));
        assert!(sql.contains("WHEN"));
        assert!(sql.contains("THEN"));
        assert!(sql.contains("ELSE"));
        assert!(sql.contains("END"));
        assert_eq!(params.len(), 5);
    }
    
    #[test]
    fn test_subquery() {
        let subquery = QueryBuilder::new()
            .from("orders")
            .select(vec!["user_id"])
            .where_clause(gt(col("total"), QueryExpr::Parameter(Parameter {
                index: 0,
                value: Value::Float(100.0)
            })))
            .build();
        
        let in_expr = QueryExpr::BinOp {
            op: BinOp::In,
            left: Box::new(col("id")),
            right: Box::new(QueryExpr::Subquery(Box::new(subquery))),
        };
        
        let mut query = QueryBuilder::new()
            .from("users")
            .select(vec!["name", "email"])
            .where_clause(in_expr)
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("IN"));
        assert!(sql.contains("SELECT")); // Should have two SELECTs
        assert_eq!(params.len(), 1);
        assert_eq!(params[0], Value::Float(100.0));
    }
    
    #[test]
    fn test_function_call() {
        let upper_name = QueryExpr::Function {
            name: "UPPER".to_string(),
            args: vec![col("name")],
        };
        
        let concat_expr = QueryExpr::Function {
            name: "CONCAT".to_string(),
            args: vec![
                col("first_name"),
                QueryExpr::Parameter(Parameter { index: 0, value: Value::String(" ".to_string()) }),
                col("last_name"),
            ],
        };
        
        let mut query = QueryBuilder::new()
            .from("users")
            .select_expr(upper_name, Some("name_upper".to_string()))
            .select_expr(concat_expr, Some("full_name".to_string()))
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("UPPER"));
        assert!(sql.contains("CONCAT"));
        assert_eq!(params.len(), 1);
    }
    
    #[test]
    fn test_unary_operators() {
        let not_expr = QueryExpr::UnaryOp {
            op: UnaryOp::Not,
            expr: Box::new(col("active")),
        };
        
        let is_null_expr = QueryExpr::UnaryOp {
            op: UnaryOp::IsNull,
            expr: Box::new(col("deleted_at")),
        };
        
        let is_not_null_expr = QueryExpr::UnaryOp {
            op: UnaryOp::IsNotNull,
            expr: Box::new(col("email")),
        };
        
        let where_clause = and(
            not_expr,
            and(is_null_expr, is_not_null_expr)
        );
        
        let mut query = QueryBuilder::new()
            .from("users")
            .select(vec!["id"])
            .where_clause(where_clause)
            .build();
        
        let (sql, _) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("NOT"));
        assert!(sql.contains("IS NULL"));
        assert!(sql.contains("IS NOT NULL"));
    }
    
    #[test]
    fn test_like_operator() {
        let mut builder = QueryBuilder::new();
        let pattern = builder.next_param(Value::String("%john%".to_string()));
        
        let like_expr = QueryExpr::BinOp {
            op: BinOp::Like,
            left: Box::new(col("name")),
            right: Box::new(pattern),
        };
        
        let mut query = builder
            .from("users")
            .select(vec!["id", "name"])
            .where_clause(like_expr)
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("LIKE"));
        assert_eq!(params[0], Value::String("%john%".to_string()));
    }
    
    #[test]
    fn test_escape_identifier() {
        // Test that identifiers are properly escaped
        let mut query = QueryBuilder::new()
            .from("user\"table")
            .select(vec!["id\"field"])
            .build();
        
        let (sql, _) = query.to_parameterized_sql().unwrap();
        // Should properly escape quotes in identifiers
        assert!(sql.contains("\"\""));
    }
    
    #[test]
    fn test_safe_user_query_example() {
        let query = safe_user_query(21, true);
        
        assert!(query.from.is_some());
        assert_eq!(query.select.len(), 3);
        assert!(query.where_clause.is_some());
        assert_eq!(query.order_by.len(), 1);
        assert_eq!(query.limit, Some(100));
    }
    
    #[test]
    fn test_arithmetic_operators() {
        let mut builder = QueryBuilder::new();
        let multiplier = builder.next_param(Value::Float(1.1));
        
        let price_expr = QueryExpr::BinOp {
            op: BinOp::Mul,
            left: Box::new(col("price")),
            right: Box::new(multiplier),
        };
        
        let mut query = builder
            .from("products")
            .select_expr(col("name"), None)
            .select_expr(price_expr, Some("adjusted_price".to_string()))
            .build();
        
        let (sql, params) = query.to_parameterized_sql().unwrap();
        assert!(sql.contains("*"));
        assert!(sql.contains("AS adjusted_price"));
        assert_eq!(params[0], Value::Float(1.1));
    }
}