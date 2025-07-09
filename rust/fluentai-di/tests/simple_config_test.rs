//! Simple test for configuration support

#[cfg(feature = "config")]
#[test]
fn test_raw_service_registration() {
    use fluentai_di::prelude::*;
    use fluentai_di::registry::ServiceRegistry;
    use std::sync::Arc;

    // Simple test service
    #[derive(Clone)]
    struct TestService {
        value: String,
    }

    // Register a factory in the global registry
    let registry = ServiceRegistry::global();
    registry
        .register_factory("TestService", || {
            Box::new(TestService {
                value: "test".to_string(),
            }) as Box<dyn Service>
        })
        .unwrap();

    // Create a container builder and register the service
    let mut builder = ContainerBuilder::new();
    builder.register_transient_raw("TestService", || {
        Box::new(TestService {
            value: "test".to_string(),
        }) as Box<dyn Service>
    });

    let container = builder.build();

    // Resolve the raw service
    let service = container.resolve_raw("TestService").unwrap();
    assert!(service.is::<TestService>());

    // Test multiple instances for transient
    let service1 = container.resolve_raw("TestService").unwrap();
    let service2 = container.resolve_raw("TestService").unwrap();
    assert!(!Arc::ptr_eq(&service1, &service2)); // Different instances
}
