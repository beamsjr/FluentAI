//! Dependency Injection framework for FluentAi
//!
//! This crate provides a lightweight but powerful dependency injection container
//! that supports both compile-time and runtime service resolution.
//!
//! The framework now includes AI-first graph-based dependency injection that
//! integrates with FluentAi's AST structure for better analysis and optimization.

pub mod builder;
pub mod container;
pub mod error;
pub mod lifecycle;
pub mod provider;
pub mod service;

// AI-first graph-based DI
pub mod graph_based;

#[cfg(feature = "config")]
pub mod config;

#[cfg(feature = "config")]
pub mod registry;

#[cfg(feature = "async")]
pub mod async_container;

pub use builder::ContainerBuilder;
pub use container::{Container, ServiceContainer};
pub use error::{DiError, DiResult};
pub use lifecycle::{Disposable, Lifecycle};
pub use provider::{ServiceFactory, ServiceProvider};
pub use service::{Service, ServiceDescriptor, ServiceLifetime};

// Graph-based DI exports
pub use graph_based::{
    DependencyAnalysis, DependencyEdge, DependencyHints, DependencyKind, GraphContainer,
    PerformanceHints, ResolutionStrategy, ServiceGraphBuilder,
    ServiceLifetime as GraphServiceLifetime, ServiceMetadata, ServiceNode, UsageStats,
};

/// Prelude module for common imports
pub mod prelude {
    pub use crate::{
        Container, ContainerBuilder, DiError, DiResult, Service, ServiceContainer,
        ServiceDescriptor, ServiceFactory, ServiceLifetime, ServiceProvider,
    };

    #[cfg(feature = "async")]
    pub use crate::async_container::{AsyncContainer, AsyncServiceProvider};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_container() {
        let mut builder = ContainerBuilder::new();
        builder.register_singleton::<String>(|| "Hello, DI!".to_string());

        let container = builder.build();
        let greeting = container.resolve::<String>().unwrap();
        assert_eq!(greeting, "Hello, DI!");
    }
}
