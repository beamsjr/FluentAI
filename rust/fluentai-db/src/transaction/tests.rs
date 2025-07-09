//! Tests for transaction management

use super::*;

#[cfg(test)]
mod transaction_tests {
    use super::*;

    #[test]
    fn test_transaction_options() {
        let opts = TransactionOptions {
            isolation_level: Some(IsolationLevel::Serializable),
            read_only: true,
            deferrable: false,
            timeout_ms: Some(1000),
        };

        assert_eq!(opts.isolation_level.unwrap().to_sql(), "SERIALIZABLE");
        assert!(opts.read_only);
        assert!(!opts.deferrable);
        assert_eq!(opts.timeout_ms, Some(1000));
    }

    #[test]
    fn test_transaction_id() {
        // Transaction IDs should be unique
        let id1 = Uuid::new_v4();
        let id2 = Uuid::new_v4();
        assert_ne!(id1, id2);
    }

    #[test]
    fn test_retryable_errors() {
        assert!(is_retryable_error(&DbError::Query(
            "deadlock detected".into()
        )));
        assert!(is_retryable_error(&DbError::Transaction(
            "serialization failure".into()
        )));
        assert!(is_retryable_error(&DbError::Query(
            "could not serialize access".into()
        )));
        assert!(!is_retryable_error(&DbError::Connection(
            "connection refused".into()
        )));
    }
}

#[cfg(test)]
mod savepoint_tests {
    use super::*;

    #[tokio::test]
    async fn test_savepoint_manager_operations() {
        let manager = SavepointManager::new();

        // Test adding savepoints
        manager.add_savepoint("sp1").await;
        manager.add_savepoint("sp2").await;
        manager.add_savepoint("sp3").await;

        assert_eq!(manager.count().await, 3);
        assert_eq!(manager.latest_savepoint().await, Some("sp3".to_string()));

        // Test getting all savepoints
        let all = manager.all_savepoints().await;
        assert_eq!(all, vec!["sp1", "sp2", "sp3"]);

        // Test rollback
        manager.rollback_to("sp2").await;
        assert_eq!(manager.count().await, 2);
        assert_eq!(manager.all_savepoints().await, vec!["sp1", "sp2"]);

        // Test remove specific
        manager.remove_savepoint("sp1").await;
        assert_eq!(manager.all_savepoints().await, vec!["sp2"]);

        // Test clear
        manager.clear().await;
        assert_eq!(manager.count().await, 0);
        assert!(manager.latest_savepoint().await.is_none());
    }

    #[test]
    fn test_savepoint_name_generation() {
        let manager = SavepointManager::new();

        // Generate multiple names
        let names: Vec<_> = (0..5).map(|_| manager.generate_name()).collect();

        // All should be unique
        for i in 0..names.len() {
            for j in (i + 1)..names.len() {
                assert_ne!(names[i], names[j]);
            }
        }

        // All should have correct prefix
        for name in &names {
            assert!(name.starts_with("sp_"));
        }
    }
}

// Note: Manager tests require actual database connection
// which isn't available in the test environment without proper setup
#[cfg(test)]
mod manager_tests {
    #[test]
    fn test_transaction_manager_basic() {
        // Just test that the module compiles correctly
        // Actual tests would require database setup
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;

    #[test]
    fn test_isolation_level_sql() {
        assert_eq!(IsolationLevel::ReadUncommitted.to_sql(), "READ UNCOMMITTED");
        assert_eq!(IsolationLevel::ReadCommitted.to_sql(), "READ COMMITTED");
        assert_eq!(IsolationLevel::RepeatableRead.to_sql(), "REPEATABLE READ");
        assert_eq!(IsolationLevel::Serializable.to_sql(), "SERIALIZABLE");
    }

    #[test]
    fn test_default_transaction_options() {
        let opts = TransactionOptions::default();
        assert!(opts.isolation_level.is_none());
        assert!(!opts.read_only);
        assert!(!opts.deferrable);
        assert!(opts.timeout_ms.is_none());
    }
}
