//! Service registry for configuration-based dependency injection

use crate::builder::ContainerBuilder;
use crate::error::{DiError, DiResult};
use crate::service::Service;
use std::any::TypeId;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Factory function that creates a service
pub type ServiceFactory = Arc<dyn Fn() -> Box<dyn Service> + Send + Sync>;

/// Service registry that maps type names to factory functions
pub struct ServiceRegistry {
    /// Map from type name to TypeId
    type_names: Arc<RwLock<HashMap<String, TypeId>>>,
    /// Map from type name to factory function
    factories: Arc<RwLock<HashMap<String, ServiceFactory>>>,
    /// Map from interface name to implementation name
    interfaces: Arc<RwLock<HashMap<String, String>>>,
}

impl ServiceRegistry {
    /// Create a new service registry
    pub fn new() -> Self {
        Self {
            type_names: Arc::new(RwLock::new(HashMap::new())),
            factories: Arc::new(RwLock::new(HashMap::new())),
            interfaces: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a service type
    pub fn register_type<T: Service + 'static>(&self, type_name: &str) -> DiResult<()> {
        let type_id = TypeId::of::<T>();
        self.type_names
            .write()
            .map_err(|_| DiError::LockError)?
            .insert(type_name.to_string(), type_id);
        Ok(())
    }

    /// Register a factory function for a type
    pub fn register_factory<F>(&self, type_name: &str, factory: F) -> DiResult<()>
    where
        F: Fn() -> Box<dyn Service> + Send + Sync + 'static,
    {
        self.factories
            .write()
            .map_err(|_| DiError::LockError)?
            .insert(type_name.to_string(), Arc::new(factory));
        Ok(())
    }

    /// Register an interface to implementation mapping
    pub fn register_interface(
        &self,
        interface_name: &str,
        implementation_name: &str,
    ) -> DiResult<()> {
        self.interfaces
            .write()
            .map_err(|_| DiError::LockError)?
            .insert(interface_name.to_string(), implementation_name.to_string());
        Ok(())
    }

    /// Get the implementation name for an interface
    pub fn get_implementation(&self, interface_name: &str) -> DiResult<String> {
        let interfaces = self.interfaces.read().map_err(|_| DiError::LockError)?;

        interfaces
            .get(interface_name)
            .cloned()
            .or_else(|| {
                // If not an interface, assume it's a concrete type
                Some(interface_name.to_string())
            })
            .ok_or_else(|| DiError::ServiceNotFoundByName(interface_name.to_string()))
    }

    /// Get a factory for a type name
    pub fn get_factory(&self, type_name: &str) -> DiResult<ServiceFactory> {
        let impl_name = self.get_implementation(type_name)?;

        let factories = self.factories.read().map_err(|_| DiError::LockError)?;

        factories
            .get(&impl_name)
            .cloned()
            .ok_or_else(|| DiError::ServiceNotFoundByName(impl_name))
    }

    /// Create a global registry instance
    pub fn global() -> &'static Self {
        static INSTANCE: std::sync::OnceLock<ServiceRegistry> = std::sync::OnceLock::new();
        INSTANCE.get_or_init(|| ServiceRegistry::new())
    }
}

impl Default for ServiceRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension trait for ContainerBuilder to use registry
pub trait RegistryContainerBuilderExt {
    /// Register a service using the registry
    fn register_from_registry(
        &mut self,
        service_type: &str,
        lifetime: crate::service::ServiceLifetime,
    ) -> DiResult<&mut Self>;
}

impl RegistryContainerBuilderExt for ContainerBuilder {
    fn register_from_registry(
        &mut self,
        service_type: &str,
        lifetime: crate::service::ServiceLifetime,
    ) -> DiResult<&mut Self> {
        let registry = ServiceRegistry::global();
        let factory = registry.get_factory(service_type)?;

        match lifetime {
            crate::service::ServiceLifetime::Singleton => {
                // For singleton, we need to ensure only one instance
                let instance = factory();
                self.register_instance_raw(service_type, instance);
            }
            crate::service::ServiceLifetime::Transient => {
                self.register_transient_raw(service_type, move || factory());
            }
            crate::service::ServiceLifetime::Scoped => {
                self.register_scoped_raw(service_type, move || factory());
            }
        }

        Ok(self)
    }
}

/// Macro to register services in the global registry
#[macro_export]
macro_rules! register_services {
    ($($service_type:ty => $factory:expr),* $(,)?) => {
        {
            let registry = $crate::registry::ServiceRegistry::global();
            $(
                registry.register_type::<$service_type>(stringify!($service_type))?;
                registry.register_factory(stringify!($service_type), $factory)?;
            )*
            Ok::<(), $crate::error::DiError>(())
        }
    };
}

/// Macro to register interface mappings
#[macro_export]
macro_rules! register_interfaces {
    ($($interface:ident => $implementation:ty),* $(,)?) => {
        {
            let registry = $crate::registry::ServiceRegistry::global();
            $(
                registry.register_interface(stringify!($interface), stringify!($implementation))?;
            )*
            Ok::<(), $crate::error::DiError>(())
        }
    };
}
