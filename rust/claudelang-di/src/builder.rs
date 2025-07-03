//! Container builder for fluent configuration

use crate::container::{Container, ServiceContainer};
use crate::service::{Service, ServiceDescriptor, ServiceLifetime};

/// Builder for constructing a service container
pub struct ContainerBuilder {
    container: ServiceContainer,
}

impl ContainerBuilder {
    /// Create a new container builder
    pub fn new() -> Self {
        Self {
            container: ServiceContainer::new(),
        }
    }
    
    /// Register a singleton service
    pub fn register_singleton<T>(&mut self, factory: impl Fn() -> T + Send + Sync + 'static) -> &mut Self
    where
        T: Service + 'static,
    {
        let descriptor = ServiceDescriptor::singleton(factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register a singleton service with explicit interface
    pub fn register_singleton_as<TService, TImpl>(&mut self, factory: impl Fn() -> TImpl + Send + Sync + 'static) -> &mut Self
    where
        TService: Service + ?Sized + 'static,
        TImpl: Service + 'static,
    {
        let descriptor = ServiceDescriptor::new::<TService, TImpl, _>(ServiceLifetime::Singleton, factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register a transient service
    pub fn register_transient<T>(&mut self, factory: impl Fn() -> T + Send + Sync + 'static) -> &mut Self
    where
        T: Service + 'static,
    {
        let descriptor = ServiceDescriptor::transient(factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register a transient service with explicit interface
    pub fn register_transient_as<TService, TImpl>(&mut self, factory: impl Fn() -> TImpl + Send + Sync + 'static) -> &mut Self
    where
        TService: Service + ?Sized + 'static,
        TImpl: Service + 'static,
    {
        let descriptor = ServiceDescriptor::new::<TService, TImpl, _>(ServiceLifetime::Transient, factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register a scoped service
    pub fn register_scoped<T>(&mut self, factory: impl Fn() -> T + Send + Sync + 'static) -> &mut Self
    where
        T: Service + 'static,
    {
        let descriptor = ServiceDescriptor::scoped(factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register a scoped service with explicit interface
    pub fn register_scoped_as<TService, TImpl>(&mut self, factory: impl Fn() -> TImpl + Send + Sync + 'static) -> &mut Self
    where
        TService: Service + ?Sized + 'static,
        TImpl: Service + 'static,
    {
        let descriptor = ServiceDescriptor::new::<TService, TImpl, _>(ServiceLifetime::Scoped, factory);
        self.container.register(descriptor);
        self
    }
    
    /// Register an existing instance as a singleton
    pub fn register_instance<T>(&mut self, instance: T) -> &mut Self
    where
        T: Service + Clone + 'static,
    {
        self.register_singleton(move || instance.clone())
    }
    
    /// Build the container
    pub fn build(self) -> Container {
        self.container.build()
    }
}

impl Default for ContainerBuilder {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension methods for fluent builder pattern
impl ContainerBuilder {
    /// Add multiple services using a configuration function
    pub fn add_services<F>(mut self, configure: F) -> Self
    where
        F: FnOnce(&mut Self),
    {
        configure(&mut self);
        self
    }
    
    /// Add services from another module
    pub fn add_module<M: Module>(mut self, module: M) -> Self {
        module.configure(&mut self);
        self
    }
}

/// Trait for service modules
pub trait Module {
    /// Configure services for this module
    fn configure(&self, builder: &mut ContainerBuilder);
}

/// Example module implementation
pub struct CoreModule;

impl Module for CoreModule {
    fn configure(&self, builder: &mut ContainerBuilder) {
        // Example: register core services
        builder.register_singleton(|| "Core Service".to_string());
    }
}