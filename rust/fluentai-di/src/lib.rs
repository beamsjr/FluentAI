//! Dependency Injection framework for FluentAi
//! 
//! This crate provides a lightweight but powerful dependency injection container
//! that supports both compile-time and runtime service resolution.

pub mod container;
pub mod service;
pub mod builder;
pub mod provider;
pub mod lifecycle;
pub mod error;

#[cfg(feature = "config")]
pub mod config;

#[cfg(feature = "config")]
pub mod registry;

#[cfg(feature = "async")]
pub mod async_container;

pub use container::{Container, ServiceContainer};
pub use service::{Service, ServiceDescriptor, ServiceLifetime};
pub use builder::ContainerBuilder;
pub use provider::{ServiceProvider, ServiceFactory};
pub use lifecycle::{Lifecycle, Disposable};
pub use error::{DiError, DiResult};

/// Prelude module for common imports
pub mod prelude {
    pub use crate::{
        Container, ServiceContainer, ContainerBuilder,
        Service, ServiceDescriptor, ServiceLifetime,
        ServiceProvider, ServiceFactory,
        DiError, DiResult,
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