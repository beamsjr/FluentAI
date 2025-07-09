//! Tests for database effects

#[cfg(test)]
mod tests {
    use super::super::*;
    use fluentai_core::ast::EffectType;
    use fluentai_core::value::Value as CoreValue;

    #[test]
    fn test_db_effect_type_creation() {
        // Test predefined effect types
        assert_eq!(DbEffectType::Connect.as_str(), "db:connect");
        assert_eq!(DbEffectType::Query.as_str(), "db:query");
        assert_eq!(DbEffectType::Execute.as_str(), "db:execute");
        assert_eq!(
            DbEffectType::BeginTransaction.as_str(),
            "db:begin-transaction"
        );
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

    #[test]
    fn test_db_handler_is_connected_sync() {
        let handler = DbHandler::new();

        // Test synchronous is_connected operation
        let result = handler.handle_sync("db:is-connected", &[]);

        assert!(result.is_ok());
        match result.unwrap() {
            CoreValue::Boolean(connected) => assert!(!connected), // Not connected without pool
            _ => panic!("Expected Boolean result"),
        }
    }

    #[test]
    fn test_db_handler_stats_sync() {
        let handler = DbHandler::new();

        // Test synchronous stats operation
        let result = handler.handle_sync("db:stats", &[]);

        assert!(result.is_ok());
        match result.unwrap() {
            CoreValue::Map(stats) => {
                // Should have expected keys
                assert!(stats.contains_key("connected"));
                assert!(stats.contains_key("transaction_depth"));
                assert!(stats.contains_key("prepared_statements"));

                // Check values
                if let Some(CoreValue::Boolean(connected)) = stats.get("connected") {
                    assert!(!connected);
                }
                if let Some(CoreValue::Integer(depth)) = stats.get("transaction_depth") {
                    assert_eq!(*depth, 0);
                }
                if let Some(CoreValue::Integer(count)) = stats.get("prepared_statements") {
                    assert_eq!(*count, 0);
                }
            }
            _ => panic!("Expected Map result"),
        }
    }

    #[tokio::test]
    async fn test_db_handler_query_without_connection() {
        let handler = DbHandler::new();

        // Should fail without a connection
        let result = handler
            .handle_async(
                "db:query",
                &[
                    CoreValue::String("SELECT * FROM users".to_string()),
                    CoreValue::List(vec![]),
                ],
            )
            .await;

        // When not connected, it returns an error message as a string
        assert!(result.is_ok());
        match result.unwrap() {
            CoreValue::String(msg) => assert!(msg.contains("Not connected")),
            _ => panic!("Expected String result"),
        }
    }

    #[tokio::test]
    async fn test_db_handler_execute_without_connection() {
        let handler = DbHandler::new();

        // Should fail without a connection
        let result = handler
            .handle_async(
                "db:execute",
                &[
                    CoreValue::String("INSERT INTO users (name) VALUES (?)".to_string()),
                    CoreValue::List(vec![CoreValue::String("John".to_string())]),
                ],
            )
            .await;

        // When not connected, it returns an error message as a string
        assert!(result.is_ok());
        match result.unwrap() {
            CoreValue::String(msg) => assert!(msg.contains("Not connected")),
            _ => panic!("Expected String result"),
        }
    }

    #[tokio::test]
    async fn test_db_handler_unsupported_operation() {
        let handler = DbHandler::new();

        // Should fail for unknown operation
        let result = handler.handle_sync("unknown:operation", &[]);

        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("requires async handler"));
    }

    #[test]
    fn test_db_handler_sync_requires_async() {
        let handler = DbHandler::new();

        // Query operation requires async handler
        let result = handler.handle_sync("db:query", &[CoreValue::String("SELECT 1".to_string())]);
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("requires async handler"));

        // Execute operation requires async handler
        let result = handler.handle_sync(
            "db:execute",
            &[CoreValue::String("INSERT INTO test VALUES (1)".to_string())],
        );
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("requires async handler"));
    }

    #[tokio::test]
    async fn test_db_handler_invalid_args() {
        let handler = DbHandler::new();

        // Query with missing params is ok - defaults to empty params
        let result = handler
            .handle_async(
                "db:query",
                &[
                    CoreValue::String("SELECT * FROM users".to_string()), // Missing params argument - this is ok, defaults to empty
                ],
            )
            .await;
        assert!(result.is_ok());

        // Execute with non-string query
        let result2 = handler
            .handle_async(
                "db:execute",
                &[
                    CoreValue::Integer(123), // Should be string
                    CoreValue::List(vec![]),
                ],
            )
            .await;
        assert!(result2.is_ok());
        match result2.unwrap() {
            CoreValue::String(msg) => assert!(msg.contains("Error")),
            _ => panic!("Expected error string"),
        }
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
