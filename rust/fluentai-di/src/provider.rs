//! Service provider interfaces and implementations

use std::any::TypeId;
use crate::error::DiResult;
use crate::service::Service;

/// Factory function for creating services
pub type ServiceFactory = Box<dyn Fn() -> DiResult<Box<dyn Service>> + Send + Sync>;

/// Basic service provider that works with Arc-wrapped services
pub trait ServiceProvider: Send + Sync {
    /// Check if a service is registered by type ID
    fn has_service_by_type_id(&self, type_id: TypeId) -> bool;
}

/// Advanced service provider with factory support
pub trait ServiceProviderWithFactory: ServiceProvider {
    /// Create a service using a factory
    fn create_service(&self, type_id: TypeId) -> DiResult<Box<dyn Service>>;
}