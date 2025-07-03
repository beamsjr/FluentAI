//! Service definitions and descriptors

use std::any::{Any, TypeId};
use std::fmt;
use std::sync::Arc;
use downcast_rs::{Downcast, impl_downcast};

/// Service lifetime determines how services are created and cached
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ServiceLifetime {
    /// A new instance is created for each request
    Transient,
    /// A single instance is created and reused within a scope
    Scoped,
    /// A single instance is created and reused for the container lifetime
    Singleton,
}

impl fmt::Display for ServiceLifetime {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ServiceLifetime::Transient => write!(f, "Transient"),
            ServiceLifetime::Scoped => write!(f, "Scoped"),
            ServiceLifetime::Singleton => write!(f, "Singleton"),
        }
    }
}

/// Trait that all services must implement
pub trait Service: Any + Send + Sync + Downcast {
    /// Get the type name of the service
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl_downcast!(Service);

/// Extension trait for Arc downcasting
pub trait ArcServiceExt {
    /// Try to downcast Arc<dyn Service> to Arc<T>
    fn downcast_arc<T: Service>(self) -> Option<Arc<T>>;
}

impl ArcServiceExt for Arc<dyn Service> {
    fn downcast_arc<T: Service>(self) -> Option<Arc<T>> {
        if self.is::<T>() {
            unsafe {
                let raw = Arc::into_raw(self);
                Some(Arc::from_raw(raw as *const T))
            }
        } else {
            None
        }
    }
}

/// Blanket implementation for all suitable types
impl<T: Any + Send + Sync> Service for T {}

/// Describes a service registration
#[derive(Clone)]
pub struct ServiceDescriptor {
    /// Type ID of the service interface
    pub service_type: TypeId,
    /// Type name for debugging
    pub service_type_name: &'static str,
    /// Type ID of the implementation
    pub implementation_type: TypeId,
    /// Implementation type name for debugging
    pub implementation_type_name: &'static str,
    /// Service lifetime
    pub lifetime: ServiceLifetime,
    /// Factory function to create the service
    pub factory: Arc<dyn Fn() -> Box<dyn Service> + Send + Sync>,
}

impl ServiceDescriptor {
    /// Create a new service descriptor
    pub fn new<TService, TImpl, F>(lifetime: ServiceLifetime, factory: F) -> Self
    where
        TService: Service + ?Sized + 'static,
        TImpl: Service + 'static,
        F: Fn() -> TImpl + Send + Sync + 'static,
    {
        ServiceDescriptor {
            service_type: TypeId::of::<TService>(),
            service_type_name: std::any::type_name::<TService>(),
            implementation_type: TypeId::of::<TImpl>(),
            implementation_type_name: std::any::type_name::<TImpl>(),
            lifetime,
            factory: Arc::new(move || Box::new(factory()) as Box<dyn Service>),
        }
    }
    
    /// Create a singleton service descriptor
    pub fn singleton<T, F>(factory: F) -> Self
    where
        T: Service + 'static,
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self::new::<T, T, F>(ServiceLifetime::Singleton, factory)
    }
    
    /// Create a transient service descriptor
    pub fn transient<T, F>(factory: F) -> Self
    where
        T: Service + 'static,
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self::new::<T, T, F>(ServiceLifetime::Transient, factory)
    }
    
    /// Create a scoped service descriptor
    pub fn scoped<T, F>(factory: F) -> Self
    where
        T: Service + 'static,
        F: Fn() -> T + Send + Sync + 'static,
    {
        Self::new::<T, T, F>(ServiceLifetime::Scoped, factory)
    }
}

impl fmt::Debug for ServiceDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("ServiceDescriptor")
            .field("service_type", &self.service_type_name)
            .field("implementation_type", &self.implementation_type_name)
            .field("lifetime", &self.lifetime)
            .finish()
    }
}