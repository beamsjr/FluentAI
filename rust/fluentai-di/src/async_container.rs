//! Async support for dependency injection

use crate::error::DiResult;
use crate::service::Service;
use async_trait::async_trait;
use std::any::TypeId;

/// Async service provider trait
#[async_trait]
pub trait AsyncServiceProvider: Send + Sync {
    /// Resolve a service asynchronously
    async fn resolve_async<T: Service + 'static>(&self) -> DiResult<T>;

    /// Resolve a service by type ID asynchronously
    async fn resolve_by_type_id_async(&self, type_id: TypeId) -> DiResult<Box<dyn Service>>;
}

/// Async container wrapper
pub struct AsyncContainer {
    sync_container: crate::Container,
}

impl AsyncContainer {
    /// Create a new async container from a sync container
    pub fn new(container: crate::Container) -> Self {
        Self {
            sync_container: container,
        }
    }

    /// Resolve a service asynchronously
    pub async fn resolve<T: Service + Clone + 'static>(&self) -> DiResult<T> {
        // For now, just wrap the sync resolution
        // In a real implementation, this could support async factories
        tokio::task::spawn_blocking({
            let container = self.sync_container.clone();
            move || container.resolve::<T>()
        })
        .await
        .map_err(|e| crate::error::DiError::Other(e.to_string()))?
    }
}

/// Async service factory
pub type AsyncServiceFactory = Box<
    dyn Fn() -> std::pin::Pin<
            Box<dyn std::future::Future<Output = DiResult<Box<dyn Service>>> + Send>,
        > + Send
        + Sync,
>;

/// Builder for async services
pub struct AsyncContainerBuilder {
    sync_builder: crate::builder::ContainerBuilder,
}

impl AsyncContainerBuilder {
    /// Create a new async container builder
    pub fn new() -> Self {
        Self {
            sync_builder: crate::builder::ContainerBuilder::new(),
        }
    }

    /// Register an async singleton
    pub fn register_async_singleton<T, F, Fut>(&mut self, factory: F) -> &mut Self
    where
        T: Service + Clone + 'static,
        F: Fn() -> Fut + Send + Sync + 'static,
        Fut: std::future::Future<Output = T> + Send + 'static,
    {
        // For now, we'll use blocking resolution
        // A real implementation would support true async factories
        self.sync_builder.register_singleton(move || {
            let runtime = tokio::runtime::Handle::current();
            runtime.block_on(factory())
        });
        self
    }

    /// Build the async container
    pub fn build(self) -> AsyncContainer {
        AsyncContainer::new(self.sync_builder.build())
    }
}

impl Default for AsyncContainerBuilder {
    fn default() -> Self {
        Self::new()
    }
}
