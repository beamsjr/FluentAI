//! Tests for connection module

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::DbConfig;
    use std::sync::Arc;

    #[test]
    fn test_db_config_default() {
        let config = DbConfig::default();
        assert_eq!(config.url, "");
        assert_eq!(config.max_connections, 10);
        assert_eq!(config.min_connections, 1);
        assert_eq!(config.connect_timeout, std::time::Duration::from_secs(30));
        assert_eq!(
            config.idle_timeout,
            Some(std::time::Duration::from_secs(600))
        );
    }

    #[test]
    fn test_db_config_custom() {
        let config = DbConfig {
            url: "postgres://localhost/test".to_string(),
            max_connections: 20,
            min_connections: 5,
            connect_timeout: std::time::Duration::from_secs(10),
            idle_timeout: None,
        };

        assert_eq!(config.url, "postgres://localhost/test");
        assert_eq!(config.max_connections, 20);
        assert_eq!(config.min_connections, 5);
        assert_eq!(config.connect_timeout.as_secs(), 10);
        assert!(config.idle_timeout.is_none());
    }

    #[test]
    fn test_connection_pool_creation() {
        let config = DbConfig {
            url: "sqlite::memory:".to_string(),
            ..Default::default()
        };

        let pool = ConnectionPool::new(config.clone());

        // Pool should be created but no connection yet
        let inner = pool.inner.try_read();
        assert!(inner.is_ok());
    }

    #[tokio::test]
    async fn test_connection_pool_config_update() {
        let initial_config = DbConfig {
            url: "sqlite::memory:".to_string(),
            max_connections: 5,
            ..Default::default()
        };

        let pool = ConnectionPool::new(initial_config);

        let new_config = DbConfig {
            url: "sqlite::memory:".to_string(),
            max_connections: 10,
            ..Default::default()
        };

        pool.update_config(new_config.clone()).await;

        let inner = pool.inner.read().await;
        assert_eq!(inner.config.max_connections, 10);
    }

    #[test]
    fn test_db_connection_service_creation() {
        let config = DbConfig {
            url: "sqlite::memory:".to_string(),
            ..Default::default()
        };

        let service = DbConnectionService::new(config.clone());

        // Should be able to get the pool
        let retrieved_pool = service.pool();

        // Create another pool with same config to compare
        let pool2 = ConnectionPool::new(config);

        // Both should be different instances (not the same Arc)
        assert!(!Arc::ptr_eq(&retrieved_pool.inner, &pool2.inner));
    }

    // Mock tests for parameter binding
    #[test]
    fn test_value_binding_nil() {
        use fluentai_vm::Value;

        let val = Value::Nil;
        // In actual implementation, this would bind None::<i32>
        match val {
            Value::Nil => assert!(true),
            _ => panic!("Expected Nil"),
        }
    }

    #[test]
    fn test_value_binding_primitives() {
        use fluentai_vm::Value;

        let bool_val = Value::Boolean(true);
        let int_val = Value::Integer(42);
        let float_val = Value::Float(3.14);
        let string_val = Value::String("hello".to_string());

        match bool_val {
            Value::Boolean(b) => assert_eq!(b, true),
            _ => panic!("Expected Bool"),
        }

        match int_val {
            Value::Integer(i) => assert_eq!(i, 42),
            _ => panic!("Expected Int"),
        }

        match float_val {
            Value::Float(f) => assert!((f - 3.14).abs() < f64::EPSILON),
            _ => panic!("Expected Float"),
        }

        match string_val {
            Value::String(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected String"),
        }
    }

    #[test]
    fn test_value_binding_binary() {
        use fluentai_vm::Value;

        let binary_data = vec![
            Value::Integer(0x48), // 'H'
            Value::Integer(0x65), // 'e'
            Value::Integer(0x6c), // 'l'
            Value::Integer(0x6c), // 'l'
            Value::Integer(0x6f), // 'o'
        ];

        let binary_val = Value::List(binary_data);

        match binary_val {
            Value::List(bytes) => {
                let byte_vec: Result<Vec<u8>, _> = bytes
                    .iter()
                    .map(|v| match v {
                        Value::Integer(i) => Ok(*i as u8),
                        _ => Err("Binary data must be a list of integers"),
                    })
                    .collect();

                assert!(byte_vec.is_ok());
                let bytes = byte_vec.unwrap();
                assert_eq!(bytes, vec![0x48, 0x65, 0x6c, 0x6c, 0x6f]);
                assert_eq!(String::from_utf8(bytes).unwrap(), "Hello");
            }
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_value_binding_invalid_binary() {
        use crate::error::DbError;
        use fluentai_vm::Value;

        let invalid_binary = vec![
            Value::Integer(0x48),
            Value::String("not a byte".to_string()), // Invalid
            Value::Integer(0x6c),
        ];

        let binary_val = Value::List(invalid_binary);

        match binary_val {
            Value::List(bytes) => {
                let byte_vec: Result<Vec<u8>, DbError> = bytes
                    .iter()
                    .map(|v| match v {
                        Value::Integer(i) => Ok(*i as u8),
                        _ => Err(DbError::Query(
                            "Binary data must be a list of integers".into(),
                        )),
                    })
                    .collect();

                assert!(byte_vec.is_err());
            }
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_connection_pool_clone() {
        let config = DbConfig::default();
        let pool1 = ConnectionPool::new(config);
        let pool2 = pool1.clone();

        // Both should point to the same inner Arc
        assert!(Arc::ptr_eq(&pool1.inner, &pool2.inner));
    }

    #[test]
    fn test_db_connection_clone() {
        // This is a mock test since we can't create actual connections in unit tests
        // In real tests with a database, we would test actual cloning behavior

        // Just verify the struct has Clone derive
        fn assert_clone<T: Clone>() {}
        assert_clone::<DbConnection>();
    }
}
