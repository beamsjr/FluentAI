//! SDK error types

use thiserror::Error;

/// SDK error type
#[derive(Error, Debug)]
pub enum Error {
    /// Runtime error
    #[error("Runtime error: {0}")]
    Runtime(#[from] fluentai_core_lib::RuntimeError),

    /// Type conversion error
    #[error("Type conversion error: {0}")]
    TypeError(String),

    /// Script error
    #[error("Script error: {0}")]
    ScriptError(String),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Configuration error
    #[error("Configuration error: {0}")]
    ConfigError(String),

    /// Not found error
    #[error("Not found: {0}")]
    NotFound(String),

    /// Invalid argument
    #[error("Invalid argument: {0}")]
    InvalidArgument(String),

    /// Other error
    #[error("{0}")]
    Other(String),
}

/// Result type alias
pub type Result<T> = std::result::Result<T, Error>;

impl Error {
    /// Create a type error
    pub fn type_error(msg: impl Into<String>) -> Self {
        Self::TypeError(msg.into())
    }

    /// Create a script error
    pub fn script_error(msg: impl Into<String>) -> Self {
        Self::ScriptError(msg.into())
    }

    /// Create a configuration error
    pub fn config_error(msg: impl Into<String>) -> Self {
        Self::ConfigError(msg.into())
    }

    /// Create a not found error
    pub fn not_found(msg: impl Into<String>) -> Self {
        Self::NotFound(msg.into())
    }

    /// Create an invalid argument error
    pub fn invalid_argument(msg: impl Into<String>) -> Self {
        Self::InvalidArgument(msg.into())
    }

    /// Create a generic error
    pub fn other(msg: impl Into<String>) -> Self {
        Self::Other(msg.into())
    }
}
