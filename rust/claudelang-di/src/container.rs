//! Core container implementation

use std::any::TypeId;
use std::collections::HashMap;
use std::sync::Arc;
use parking_lot::RwLock;

use crate::error::{DiError, DiResult};
use crate::service::{Service, ServiceDescriptor, ServiceLifetime, ArcServiceExt};
use crate::provider::ServiceProvider;

/// Thread-safe service container
#[derive(Clone)]
pub struct ServiceContainer {
    /// Service descriptors
    descriptors: Arc<RwLock<HashMap<TypeId, ServiceDescriptor>>>,
    /// Singleton instances
    singletons: Arc<RwLock<HashMap<TypeId, Arc<dyn Service>>>>,
    /// Parent container for hierarchical resolution
    parent: Option<Arc<ServiceContainer>>,
}

impl ServiceContainer {
    /// Create a new service container
    pub fn new() -> Self {
        Self {
            descriptors: Arc::new(RwLock::new(HashMap::new())),
            singletons: Arc::new(RwLock::new(HashMap::new())),
            parent: None,
        }
    }
    
    /// Create a child container
    pub fn create_child(parent: Arc<ServiceContainer>) -> Self {
        Self {
            descriptors: Arc::new(RwLock::new(HashMap::new())),
            singletons: Arc::new(RwLock::new(HashMap::new())),
            parent: Some(parent),
        }
    }
    
    /// Register a service descriptor
    pub fn register(&self, descriptor: ServiceDescriptor) {
        self.descriptors.write().insert(descriptor.service_type, descriptor);
    }
    
    /// Build an immutable container
    pub fn build(self) -> Container {
        Container {
            inner: Arc::new(self),
        }
    }
}

/// Immutable service container
#[derive(Clone)]
pub struct Container {
    inner: Arc<ServiceContainer>,
}

impl Container {
    /// Create a new container builder
    pub fn builder() -> crate::builder::ContainerBuilder {
        crate::builder::ContainerBuilder::new()
    }
    
    /// Resolve a service returning a cloned instance
    pub fn resolve<T: Service + Clone + 'static>(&self) -> DiResult<T> {
        let type_id = TypeId::of::<T>();
        
        // Check singletons first
        if let Some(singleton) = self.get_singleton(type_id) {
            return singleton
                .downcast_ref::<T>()
                .ok_or_else(|| DiError::ServiceNotFound {
                    service_type: std::any::type_name::<T>().to_string(),
                    type_id,
                })
                .map(|s| s.clone());
        }
        
        // Find descriptor
        let descriptor = self.find_descriptor(type_id)?;
        
        // Create instance based on lifetime
        match descriptor.lifetime {
            ServiceLifetime::Singleton => {
                // Create the instance
                let instance = (descriptor.factory)();
                
                // Try to downcast and clone before storing
                let result = instance
                    .downcast_ref::<T>()
                    .ok_or_else(|| DiError::ServiceNotFound {
                        service_type: std::any::type_name::<T>().to_string(),
                        type_id,
                    })
                    .map(|s| s.clone())?;
                
                // Store singleton
                self.inner.singletons.write().insert(type_id, Arc::from(instance));
                
                Ok(result)
            }
            ServiceLifetime::Transient => {
                let instance = (descriptor.factory)();
                instance
                    .downcast_ref::<T>()
                    .ok_or_else(|| DiError::ServiceNotFound {
                        service_type: std::any::type_name::<T>().to_string(),
                        type_id,
                    })
                    .map(|s| s.clone())
            }
            ServiceLifetime::Scoped => {
                // For now, treat scoped as transient
                // Real implementation would need a scope context
                let instance = (descriptor.factory)();
                instance
                    .downcast_ref::<T>()
                    .ok_or_else(|| DiError::ServiceNotFound {
                        service_type: std::any::type_name::<T>().to_string(),
                        type_id,
                    })
                    .map(|s| s.clone())
            }
        }
    }
    
    /// Resolve a service returning an Arc
    pub fn resolve_arc<T: Service + 'static>(&self) -> DiResult<Arc<T>> {
        let type_id = TypeId::of::<T>();
        
        // Check singletons first
        if let Some(singleton) = self.get_singleton(type_id) {
            return singleton
                .downcast_arc::<T>()
                .ok_or_else(|| DiError::ServiceNotFound {
                    service_type: std::any::type_name::<T>().to_string(),
                    type_id,
                });
        }
        
        // Find descriptor
        let descriptor = self.find_descriptor(type_id)?;
        
        // Create instance based on lifetime
        match descriptor.lifetime {
            ServiceLifetime::Singleton => {
                let instance = (descriptor.factory)();
                let arc_instance: Arc<dyn Service> = Arc::from(instance);
                
                // Store singleton
                self.inner.singletons.write().insert(type_id, arc_instance.clone());
                
                arc_instance
                    .downcast_arc::<T>()
                    .ok_or_else(|| DiError::ServiceNotFound {
                        service_type: std::any::type_name::<T>().to_string(),
                        type_id,
                    })
            }
            _ => {
                // For transient and scoped, create new Arc
                let instance = (descriptor.factory)();
                let arc_instance: Arc<dyn Service> = Arc::from(instance);
                arc_instance
                    .downcast_arc::<T>()
                    .ok_or_else(|| DiError::ServiceNotFound {
                        service_type: std::any::type_name::<T>().to_string(),
                        type_id,
                    })
            }
        }
    }
    
    /// Get a singleton if it exists
    fn get_singleton(&self, type_id: TypeId) -> Option<Arc<dyn Service>> {
        self.inner.singletons.read().get(&type_id).cloned()
    }
    
    /// Find a descriptor in this container or parent
    fn find_descriptor(&self, type_id: TypeId) -> DiResult<ServiceDescriptor> {
        if let Some(descriptor) = self.inner.descriptors.read().get(&type_id) {
            Ok(descriptor.clone())
        } else if let Some(parent) = &self.inner.parent {
            Container { inner: parent.clone() }.find_descriptor(type_id)
        } else {
            Err(DiError::ServiceNotFound {
                service_type: "Unknown".to_string(),
                type_id,
            })
        }
    }
}

impl ServiceProvider for Container {
    fn has_service_by_type_id(&self, type_id: TypeId) -> bool {
        self.inner.descriptors.read().contains_key(&type_id) ||
        self.inner.parent.as_ref().map_or(false, |p| Container { inner: p.clone() }.has_service_by_type_id(type_id))
    }
}

impl Default for Container {
    fn default() -> Self {
        Container::builder().build()
    }
}