//! Error types for the DI container

use thiserror::Error;
use std::any::TypeId;

/// Result type alias for DI operations
pub type DiResult<T> = Result<T, DiError>;

/// Errors that can occur during DI operations
#[derive(Error, Debug)]
pub enum DiError {
    /// Service not found in container
    #[error("Service not found: {service_type}")]
    ServiceNotFound {
        service_type: String,
        type_id: TypeId,
    },
    
    /// Service not found by name
    #[error("Service not found: {0}")]
    ServiceNotFoundByName(String),
    
    /// Circular dependency detected
    #[error("Circular dependency detected: {path}")]
    CircularDependency {
        path: String,
    },
    
    /// Service creation failed
    #[error("Failed to create service: {service_type}: {reason}")]
    ServiceCreationFailed {
        service_type: String,
        reason: String,
    },
    
    /// Invalid service lifetime
    #[error("Invalid service lifetime: cannot resolve {requested} from {available} scope")]
    InvalidLifetime {
        requested: String,
        available: String,
    },
    
    /// Container is locked (during resolution)
    #[error("Container is locked - possible recursive resolution")]
    ContainerLocked,
    
    /// Lock error
    #[error("Failed to acquire lock")]
    LockError,
    
    /// Configuration error
    #[cfg(feature = "config")]
    #[error("Configuration error: {0}")]
    ConfigError(String),
    
    /// Generic error
    #[error("DI error: {0}")]
    Other(String),
}