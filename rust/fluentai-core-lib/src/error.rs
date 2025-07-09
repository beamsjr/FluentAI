//! Runtime error types

use thiserror::Error;

/// Runtime error type
#[derive(Error, Debug)]
pub enum RuntimeError {
    /// VM execution error
    #[error("VM error: {0}")]
    VmError(#[from] fluentai_vm::error::VMError),

    /// Parser error
    #[error("Parse error: {0}")]
    ParseError(String),

    /// Module loading error
    #[error("Module loading error: {0}")]
    ModuleError(String),

    /// Host function error
    #[error("Host function error: {0}")]
    HostError(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// Timeout error
    #[error("Execution timeout exceeded")]
    Timeout,

    /// Security violation
    #[error("Security violation: {0}")]
    SecurityViolation(String),

    /// Resource limit exceeded
    #[error("Resource limit exceeded: {0}")]
    ResourceLimit(String),

    /// Not implemented
    #[error("Not implemented: {0}")]
    NotImplemented(String),
    
    /// Generic error
    #[error("Error: {0}")]
    Generic(String),

    /// Generic error
    #[error("{0}")]
    Other(String),
}

/// Result type alias
pub type Result<T> = std::result::Result<T, RuntimeError>;

impl RuntimeError {
    /// Create a module error
    pub fn module(msg: impl Into<String>) -> Self {
        Self::ModuleError(msg.into())
    }

    /// Create a host error
    pub fn host(msg: impl Into<String>) -> Self {
        Self::HostError(msg.into())
    }

    /// Create a configuration error
    pub fn config(msg: impl Into<String>) -> Self {
        Self::ConfigError(msg.into())
    }

    /// Create a security violation error
    pub fn security(msg: impl Into<String>) -> Self {
        Self::SecurityViolation(msg.into())
    }

    /// Create a resource limit error
    pub fn resource_limit(msg: impl Into<String>) -> Self {
        Self::ResourceLimit(msg.into())
    }

    /// Create a not implemented error
    pub fn not_implemented(msg: impl Into<String>) -> Self {
        Self::NotImplemented(msg.into())
    }

    /// Create a generic error
    pub fn other(msg: impl Into<String>) -> Self {
        Self::Other(msg.into())
    }
}

impl From<anyhow::Error> for RuntimeError {
    fn from(err: anyhow::Error) -> Self {
        Self::Generic(err.to_string())
    }
}