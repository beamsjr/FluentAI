//! Tests for database effects

#[cfg(test)]
mod tests {
    use super::super::*;
    use fluentai_core::value::Value as CoreValue;
    use fluentai_core::ast::EffectType;
    
    #[test]
    fn test_db_effect_type_creation() {
        // Test predefined effect types
        assert_eq!(DbEffectType::Connect.as_str(), "db:connect");
        assert_eq!(DbEffectType::Query.as_str(), "db:query");
        assert_eq!(DbEffectType::Execute.as_str(), "db:execute");
        assert_eq!(DbEffectType::BeginTransaction.as_str(), "db:begin-transaction");
        assert_eq!(DbEffectType::IsConnected.as_str(), "db:is-connected");
        
        // Test custom effect type
        let custom = DbEffectType::Custom("db:custom-operation".to_string());
        assert_eq!(custom.as_str(), "db:custom-operation");
    }
    
    #[test]
    fn test_db_handler_creation() {
        let handler = DbHandler::new();
        
        // Handler should use IO effect type for database operations
        assert_eq!(handler.effect_type(), EffectType::IO);
    }
    
    #[test]
    fn test_db_handler_with_pool() {
        use crate::{ConnectionPool, DbConfig};
        
        let config = DbConfig {
            url: "sqlite::memory:".to_string(),
            ..Default::default()
        };
        let pool = ConnectionPool::new(config);
        let handler = DbHandler::with_pool(pool);
        
        // Should create handler with pool
        assert_eq!(handler.effect_type(), EffectType::IO);
    }
    
    #[tokio::test]
    async fn test_db_handler_is_connected_sync() {
        let handler = DbHandler::new();
        
        // Test synchronous is_connected operation
        let result = handler.handle_sync("db:is-connected", &[]);
        
        assert!(result.is_ok());
        match result.unwrap() {
            CoreValue::Boolean(connected) => assert!(!connected), // Not connected without pool
            _ => panic!("Expected Boolean result"),
        }
    }
    
    #[tokio::test]
    async fn test_db_handler_query_without_connection() {
        let handler = DbHandler::new();
        
        // Should fail without a connection
        let result = handler.handle_async("db:query", &[
            CoreValue::String("SELECT * FROM users".to_string()),
            CoreValue::List(vec![]),
        ]).await;
        
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_db_handler_execute_without_connection() {
        let handler = DbHandler::new();
        
        // Should fail without a connection
        let result = handler.handle_async("db:execute", &[
            CoreValue::String("INSERT INTO users (name) VALUES (?)".to_string()),
            CoreValue::List(vec![CoreValue::String("John".to_string())]),
        ]).await;
        
        assert!(result.is_err());
    }
    
    #[tokio::test]
    async fn test_db_handler_unsupported_operation() {
        let handler = DbHandler::new();
        
        // Should fail for unknown operation
        let result = handler.handle_sync("unknown:operation", &[]);
        
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Unknown database operation"));
    }
    
    #[tokio::test]
    async fn test_db_handler_invalid_args() {
        let handler = DbHandler::new();
        
        // Query with wrong number of args
        let result = handler.handle_async("db:query", &[
            CoreValue::String("SELECT * FROM users".to_string())
            // Missing params argument
        ]).await;
        assert!(result.is_err());
        
        // Execute with non-string query
        let result2 = handler.handle_async("db:execute", &[
            CoreValue::Integer(123), // Should be string
            CoreValue::List(vec![]),
        ]).await;
        assert!(result2.is_err());
    }
    
    #[test]
    fn test_db_effect_type_equality() {
        let connect1 = DbEffectType::Connect;
        let connect2 = DbEffectType::Connect;
        let query = DbEffectType::Query;
        
        assert_eq!(connect1, connect2);
        assert_ne!(connect1, query);
        
        let custom1 = DbEffectType::Custom("db:custom".to_string());
        let custom2 = DbEffectType::Custom("db:custom".to_string());
        let custom3 = DbEffectType::Custom("db:other".to_string());
        
        assert_eq!(custom1, custom2);
        assert_ne!(custom1, custom3);
    }
    
    // Tests for internal helper functions would go here if they were exposed
    // For now, we test them indirectly through the handler operations
}