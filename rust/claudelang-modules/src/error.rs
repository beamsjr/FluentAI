//! Module system error types

use std::path::PathBuf;
use thiserror::Error;

pub type Result<T> = std::result::Result<T, ModuleError>;

#[derive(Error, Debug)]
pub enum ModuleError {
    #[error("Module not found: {path}")]
    ModuleNotFound { path: String },
    
    #[error("Circular dependency detected: {cycle}")]
    CircularDependency { cycle: String },
    
    #[error("Export not found: {name} in module {module}")]
    ExportNotFound { name: String, module: String },
    
    #[error("Invalid module path: {path}")]
    InvalidPath { path: PathBuf },
    
    #[error("Module already loaded: {id}")]
    ModuleAlreadyLoaded { id: String },
    
    #[error("Import error in module {module}: {message}")]
    ImportError { module: String, message: String },
    
    #[error("Parse error in module {path}: {error}")]
    ParseError { 
        path: PathBuf, 
        #[source]
        error: claudelang_parser::error::ParseError 
    },
    
    #[error("IO error reading module {path}: {error}")]
    IoError { 
        path: PathBuf,
        #[source]
        error: std::io::Error 
    },
    
    #[error("Module {id} has no exports")]
    NoExports { id: String },
    
    #[error("Invalid module name: {name}")]
    InvalidModuleName { name: String },
    
    #[error("Module cache error: {message}")]
    CacheError { message: String },
}