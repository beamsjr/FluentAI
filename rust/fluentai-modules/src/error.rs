//! Module system error types

use std::path::PathBuf;
use thiserror::Error;

/// Type alias for module system results
pub type Result<T> = std::result::Result<T, ModuleError>;

/// Errors that can occur in the module system
#[derive(Error, Debug)]
pub enum ModuleError {
    /// Module could not be found at the specified path
    #[error("Module not found: {path}")]
    ModuleNotFound {
        /// Path where the module was expected
        path: String
    },

    /// Circular dependency was detected in module imports
    #[error("Circular dependency detected: {cycle}")]
    CircularDependency {
        /// Description of the dependency cycle
        cycle: String
    },

    /// Requested export was not found in the module
    #[error("Export not found: {name} in module {module}")]
    ExportNotFound {
        /// Name of the missing export
        name: String,
        /// Module that was expected to contain the export
        module: String
    },

    /// Invalid module path was provided
    #[error("Invalid module path: {path}")]
    InvalidPath {
        /// The invalid path
        path: PathBuf
    },

    /// Module has already been loaded
    #[error("Module already loaded: {id}")]
    ModuleAlreadyLoaded {
        /// ID of the already loaded module
        id: String
    },

    /// Error occurred while importing a module
    #[error("Import error in module {module}: {message}")]
    ImportError {
        /// Module where the import error occurred
        module: String,
        /// Error message
        message: String
    },

    /// Parse error occurred while reading a module
    #[error("Parse error in module {path}: {error}")]
    ParseError {
        /// Path to the module that failed to parse
        path: PathBuf,
        /// The underlying parse error
        #[source]
        error: fluentai_parser::error::ParseError,
    },

    /// I/O error occurred while reading a module
    #[error("IO error reading module {path}: {error}")]
    IoError {
        /// Path to the module that failed to read
        path: PathBuf,
        /// The underlying I/O error
        #[source]
        error: std::io::Error,
    },

    /// Module has no exports
    #[error("Module {id} has no exports")]
    NoExports {
        /// ID of the module with no exports
        id: String
    },

    /// Module name is invalid
    #[error("Invalid module name: {name}")]
    InvalidModuleName {
        /// The invalid module name
        name: String
    },

    /// Error occurred in the module cache
    #[error("Module cache error: {message}")]
    CacheError {
        /// Cache error message
        message: String
    },
}
