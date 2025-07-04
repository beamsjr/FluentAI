//! Integration tests for the DI container

use fluentai_di::prelude::*;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

// Test services
#[derive(Clone)]
struct Logger {
    name: String,
}

impl Logger {
    fn new(name: &str) -> Self {
        Self { name: name.to_string() }
    }
    
    fn log(&self, message: &str) {
        println!("[{}] {}", self.name, message);
    }
}

#[derive(Clone)]
struct Database {
    connection_string: String,
    logger: Arc<Logger>,
}

impl Database {
    fn new(connection_string: &str, logger: Arc<Logger>) -> Self {
        logger.log(&format!("Connecting to database: {}", connection_string));
        Self {
            connection_string: connection_string.to_string(),
            logger,
        }
    }
}

#[derive(Clone)]
struct UserService {
    database: Arc<Database>,
    call_count: Arc<AtomicUsize>,
}

impl UserService {
    fn new(database: Arc<Database>) -> Self {
        Self {
            database,
            call_count: Arc::new(AtomicUsize::new(0)),
        }
    }
    
    fn get_user(&self, id: u64) -> String {
        self.call_count.fetch_add(1, Ordering::Relaxed);
        self.database.logger.log(&format!("Getting user {}", id));
        format!("User {}", id)
    }
    
    fn call_count(&self) -> usize {
        self.call_count.load(Ordering::Relaxed)
    }
}

#[test]
fn test_singleton_registration() {
    let mut builder = ContainerBuilder::new();
    builder.register_singleton(|| Logger::new("test"));
    
    let container = builder.build();
    
    // Resolve multiple times - should get same instance
    let logger1 = container.resolve::<Logger>().unwrap();
    let logger2 = container.resolve::<Logger>().unwrap();
    
    // Both should have the same name
    assert_eq!(logger1.name, "test");
    assert_eq!(logger2.name, "test");
}

#[test]
fn test_transient_registration() {
    let counter = Arc::new(AtomicUsize::new(0));
    let counter_clone = counter.clone();
    
    let mut builder = ContainerBuilder::new();
    builder.register_transient(move || {
        let count = counter_clone.fetch_add(1, Ordering::Relaxed);
        Logger::new(&format!("logger-{}", count))
    });
    
    let container = builder.build();
    
    // Resolve multiple times - should get different instances
    let logger1 = container.resolve::<Logger>().unwrap();
    let logger2 = container.resolve::<Logger>().unwrap();
    
    // Should have different names
    assert_eq!(logger1.name, "logger-0");
    assert_eq!(logger2.name, "logger-1");
}

#[test]
fn test_dependency_injection() {
    // For this test, we'll register all services up front
    // In a real app, you might use a factory pattern or service locator
    let logger = Arc::new(Logger::new("app"));
    let database = Arc::new(Database::new("postgres://localhost/test", logger.clone()));
    let user_service = UserService::new(database);
    
    let mut builder = ContainerBuilder::new();
    builder.register_instance(user_service.clone());
    
    let container = builder.build();
    
    // Resolve user service
    let resolved_service = container.resolve::<UserService>().unwrap();
    let user = resolved_service.get_user(123);
    
    assert_eq!(user, "User 123");
    assert_eq!(resolved_service.call_count(), 1);
}

#[test]
fn test_service_not_found() {
    let container = ContainerBuilder::new().build();
    
    // Try to resolve unregistered service
    let result = container.resolve::<Logger>();
    
    assert!(result.is_err());
    match result {
        Err(DiError::ServiceNotFound { .. }) => (),
        _ => panic!("Expected ServiceNotFound error"),
    }
}

#[test]
fn test_module_registration() {
    use fluentai_di::builder::Module;
    
    struct LoggingModule;
    
    impl Module for LoggingModule {
        fn configure(&self, builder: &mut ContainerBuilder) {
            builder.register_singleton(|| Logger::new("module"));
        }
    }
    
    let container = ContainerBuilder::new()
        .add_module(LoggingModule)
        .build();
    
    let logger = container.resolve::<Logger>().unwrap();
    assert_eq!(logger.name, "module");
}

#[test]
fn test_instance_registration() {
    let logger = Logger::new("instance");
    
    let mut builder = ContainerBuilder::new();
    builder.register_instance(logger);
    
    let container = builder.build();
    let resolved = container.resolve::<Logger>().unwrap();
    
    assert_eq!(resolved.name, "instance");
}

#[cfg(feature = "async")]
#[tokio::test]
async fn test_async_container() {
    use fluentai_di::async_container::AsyncContainerBuilder;
    
    let mut builder = AsyncContainerBuilder::new();
    builder.register_async_singleton(|| async {
        // Simulate async initialization
        tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        Logger::new("async")
    });
    
    let container = builder.build();
    let logger = container.resolve::<Logger>().await.unwrap();
    
    assert_eq!(logger.name, "async");
}

#[cfg(feature = "config")]
#[test]
fn test_config_loading() {
    use fluentai_di::config::ContainerConfig;
    
    let config_toml = r#"
        [[services]]
        service_type = "Logger"
        implementation_type = "Logger"
        lifetime = "singleton"
        
        [[services]]
        service_type = "Database"
        implementation_type = "Database"
        lifetime = "singleton"
        
        [services.parameters]
        connection_string = "postgres://localhost/mydb"
    "#;
    
    let config = ContainerConfig::from_toml(config_toml).unwrap();
    
    assert_eq!(config.services.len(), 2);
    assert_eq!(config.services[0].service_type, "Logger");
    assert_eq!(config.services[1].service_type, "Database");
}