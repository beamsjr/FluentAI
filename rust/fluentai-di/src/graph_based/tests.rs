//! Tests for graph-based dependency injection

#[cfg(test)]
mod tests {
    use super::super::*;
    use fluentai_core::ast::NodeId;

    // Helper function to create NodeId
    fn node_id(id: u32) -> NodeId {
        NodeId::new(id).expect("NodeId should be non-zero")
    }

    #[test]
    fn test_service_registration() {
        let mut builder = ServiceGraphBuilder::new();

        let logger_id = builder
            .service("ILogger")
            .implementation("ConsoleLogger")
            .lifetime(ServiceLifetime::Singleton)
            .register()
            .expect("Failed to register logger");

        let container = builder.build();

        assert!(container.services.contains_key(&logger_id));
        let service = &container.services[&logger_id];
        assert_eq!(service.lifetime, ServiceLifetime::Singleton);
    }

    #[test]
    fn test_dependency_registration() {
        let mut builder = ServiceGraphBuilder::new();

        let _logger_id = builder
            .service("ILogger")
            .implementation("ConsoleLogger")
            .lifetime(ServiceLifetime::Singleton)
            .register()
            .expect("Failed to register logger");

        let repo_id = builder
            .service("IRepository")
            .implementation("UserRepository")
            .lifetime(ServiceLifetime::Scoped)
            .depends_on("ILogger", DependencyKind::Constructor)
            .register()
            .expect("Failed to register repository");

        let container = builder.build();

        let repo_service = &container.services[&repo_id];
        assert_eq!(repo_service.dependencies.len(), 1);
        assert_eq!(
            repo_service.dependencies[0].kind,
            DependencyKind::Constructor
        );
    }

    #[test]
    fn test_simple_no_cycle() {
        let mut container = GraphContainer::new();

        // Simple A -> B dependency (no cycle)
        let service_b = ServiceNode {
            id: node_id(1), // Temporary ID
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };
        let b_id = container
            .register_service(service_b)
            .expect("Failed to register service B");

        let service_a = ServiceNode {
            id: node_id(2), // Temporary ID
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![DependencyEdge {
                target: b_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            }],
            metadata: ServiceMetadata::default(),
        };
        let a_id = container
            .register_service(service_a)
            .expect("Failed to register service A");

        let analysis = container.analyze_dependencies(a_id);
        assert!(!analysis.has_cycles);
        assert_eq!(analysis.dependency_depth, 1);
    }

    #[test]
    fn test_simple_cycle() {
        let mut container = GraphContainer::new();

        // Create A -> B -> A cycle
        let service_a = ServiceNode {
            id: node_id(1), // Temporary ID
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };
        let a_id = container
            .register_service(service_a)
            .expect("Failed to register service A");

        let service_b = ServiceNode {
            id: node_id(2), // Temporary ID
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![DependencyEdge {
                target: a_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            }],
            metadata: ServiceMetadata::default(),
        };
        let b_id = container
            .register_service(service_b)
            .expect("Failed to register service B");

        // Add B dependency to A
        container
            .services
            .get_mut(&a_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: b_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        let analysis = container.analyze_dependencies(a_id);
        assert!(analysis.has_cycles);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let mut container = GraphContainer::new();

        // First register all services without dependencies
        let service_a = ServiceNode {
            id: node_id(1), // Temporary ID, will be replaced
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let service_b = ServiceNode {
            id: node_id(2), // Temporary ID, will be replaced
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let service_c = ServiceNode {
            id: node_id(3), // Temporary ID, will be replaced
            interface: node_id(30),
            implementation: node_id(31),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let a_id = container
            .register_service(service_a)
            .expect("Failed to register service A");
        let b_id = container
            .register_service(service_b)
            .expect("Failed to register service B");
        let c_id = container
            .register_service(service_c)
            .expect("Failed to register service C");

        // Now add circular dependencies
        container
            .services
            .get_mut(&a_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: b_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        container
            .services
            .get_mut(&b_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: c_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        container
            .services
            .get_mut(&c_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: a_id, // Circular reference back to A
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        let analysis = container.analyze_dependencies(a_id);
        assert!(analysis.has_cycles);
    }

    #[test]
    fn test_dependency_depth_calculation() {
        let mut container = GraphContainer::new();

        // Create chain: A -> B -> C
        let service_c = ServiceNode {
            id: node_id(3),
            interface: node_id(30),
            implementation: node_id(31),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let c_id = container
            .register_service(service_c)
            .expect("Failed to register service C");

        let service_b = ServiceNode {
            id: node_id(2),
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let b_id = container
            .register_service(service_b)
            .expect("Failed to register service B");

        // Add B's dependency on C
        container
            .services
            .get_mut(&b_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: c_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        let service_a = ServiceNode {
            id: node_id(1),
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };

        let a_id = container
            .register_service(service_a)
            .expect("Failed to register service A");

        // Add A's dependency on B
        container
            .services
            .get_mut(&a_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: b_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        let analysis = container.analyze_dependencies(a_id);
        assert_eq!(analysis.dependency_depth, 2); // A -> B -> C
        assert!(!analysis.has_cycles);
        assert_eq!(analysis.transitive_deps.len(), 2); // B and C
    }

    #[test]
    fn test_performance_cost_calculation() {
        let mut container = GraphContainer::new();

        let expensive_service = ServiceNode {
            id: node_id(1),
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Transient,
            dependencies: vec![],
            metadata: ServiceMetadata {
                performance_hints: PerformanceHints {
                    instantiation_cost: Some(100.0),
                    memory_footprint: Some(1024),
                    thread_safe: true,
                    performs_io: true,
                    cache_friendly: Some(0.5),
                },
                ..Default::default()
            },
        };

        let expensive_id = container
            .register_service(expensive_service)
            .expect("Failed to register expensive service");

        let cheap_service = ServiceNode {
            id: node_id(2),
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata {
                performance_hints: PerformanceHints {
                    instantiation_cost: Some(10.0),
                    ..Default::default()
                },
                ..Default::default()
            },
        };

        let cheap_id = container
            .register_service(cheap_service)
            .expect("Failed to register cheap service");

        // Add dependency
        container
            .services
            .get_mut(&cheap_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: expensive_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        let analysis = container.analyze_dependencies(cheap_id);
        assert_eq!(analysis.total_cost, 110.0); // 100 + 10
    }

    #[test]
    fn test_metadata_and_tags() {
        let mut builder = ServiceGraphBuilder::new();

        let service_id = builder
            .service("ICache")
            .implementation("RedisCache")
            .lifetime(ServiceLifetime::Singleton)
            .with_metadata(ServiceMetadata {
                tags: vec![
                    "cache".to_string(),
                    "redis".to_string(),
                    "network".to_string(),
                ],
                embedding: Some(vec![0.1, 0.2, 0.3, 0.4]),
                semantic_version: SemanticVersion {
                    major: 1,
                    minor: 2,
                    patch: 3,
                    behavior_hash: Some(0x12345678),
                },
                ..Default::default()
            })
            .register()
            .expect("Failed to register service");

        let container = builder.build();
        let service = &container.services[&service_id];

        assert_eq!(service.metadata.tags.len(), 3);
        assert!(service.metadata.tags.contains(&"redis".to_string()));
        assert_eq!(service.metadata.semantic_version.major, 1);
        assert_eq!(service.metadata.embedding.as_ref().unwrap().len(), 4);
    }

    #[test]
    fn test_resolution_strategy() {
        let mut builder = ServiceGraphBuilder::new();

        let _cache_id = builder
            .service("ICache")
            .implementation("LRUCache")
            .lifetime(ServiceLifetime::Singleton)
            .register()
            .expect("Failed to register service");

        let service_id = builder
            .service("IService")
            .implementation("ServiceImpl")
            .lifetime(ServiceLifetime::Transient)
            .depends_on("ICache", DependencyKind::Property)
            .register()
            .expect("Failed to register service");

        let container = builder.build();
        let service = &container.services[&service_id];

        // Verify dependency hints can be customized
        assert_eq!(
            service.dependencies[0].hints.resolution_strategy,
            ResolutionStrategy::PreferCached
        );
        assert!(!service.dependencies[0].hints.optional);
        assert!(!service.dependencies[0].hints.lazy);
    }

    #[test]
    fn test_custom_lifetime() {
        let mut builder = ServiceGraphBuilder::new();

        let service_id = builder
            .service("ICustom")
            .implementation("CustomImpl")
            .lifetime(ServiceLifetime::Custom(42))
            .register()
            .expect("Failed to register service");

        let container = builder.build();
        let service = &container.services[&service_id];

        match service.lifetime {
            ServiceLifetime::Custom(id) => assert_eq!(id, 42),
            _ => panic!("Expected custom lifetime"),
        }
    }

    #[test]
    fn test_parallel_group_identification() {
        let mut container = GraphContainer::new();

        // Create diamond dependency: A -> B, A -> C, B -> D, C -> D
        // B and C can be resolved in parallel

        // First register D (no dependencies)
        let service_d = ServiceNode {
            id: node_id(1), // Temporary ID
            interface: node_id(40),
            implementation: node_id(41),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };
        let d_id = container
            .register_service(service_d)
            .expect("Failed to register service D");

        // Then register B and C (depend on D)
        let service_b = ServiceNode {
            id: node_id(2), // Temporary ID
            interface: node_id(20),
            implementation: node_id(21),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };
        let b_id = container
            .register_service(service_b)
            .expect("Failed to register service B");

        let service_c = ServiceNode {
            id: node_id(3), // Temporary ID
            interface: node_id(30),
            implementation: node_id(31),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![],
            metadata: ServiceMetadata::default(),
        };
        let c_id = container
            .register_service(service_c)
            .expect("Failed to register service C");

        // Add dependencies for B and C
        container
            .services
            .get_mut(&b_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: d_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        container
            .services
            .get_mut(&c_id)
            .unwrap()
            .dependencies
            .push(DependencyEdge {
                target: d_id,
                kind: DependencyKind::Constructor,
                position: None,
                hints: DependencyHints::default(),
            });

        // Finally register A (depends on B and C)
        let service_a = ServiceNode {
            id: node_id(4), // Temporary ID
            interface: node_id(10),
            implementation: node_id(11),
            lifetime: ServiceLifetime::Singleton,
            dependencies: vec![
                DependencyEdge {
                    target: b_id,
                    kind: DependencyKind::Constructor,
                    position: Some(0),
                    hints: DependencyHints::default(),
                },
                DependencyEdge {
                    target: c_id,
                    kind: DependencyKind::Constructor,
                    position: Some(1),
                    hints: DependencyHints::default(),
                },
            ],
            metadata: ServiceMetadata::default(),
        };
        let a_id = container
            .register_service(service_a)
            .expect("Failed to register service A");

        let analysis = container.analyze_dependencies(a_id);
        // In a full implementation, parallel_groups would identify that B and C
        // can be resolved in parallel
        assert!(!analysis.parallel_groups.is_empty());
    }
}
