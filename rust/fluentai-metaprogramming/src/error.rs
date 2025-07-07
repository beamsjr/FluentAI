//! Error types for metaprogramming

use thiserror::Error;
use fluentai_core::ast::NodeId;

#[derive(Error, Debug)]
pub enum MetaprogrammingError {
    #[error("Macro expansion error: {0}")]
    MacroExpansionError(String),
    
    #[error("Pattern matching error: {0}")]
    PatternMatchError(String),
    
    #[error("Query error: {0}")]
    QueryError(String),
    
    #[error("Transform error: {0}")]
    TransformError(String),
    
    #[error("Template error: {0}")]
    TemplateError(String),
    
    #[error("Undefined macro: {0}")]
    UndefinedMacro(String),
    
    #[error("Invalid pattern: {0}")]
    InvalidPattern(String),
    
    #[error("Circular macro dependency: {0}")]
    CircularDependency(String),
    
    #[error("Node not found: {0:?}")]
    NodeNotFound(NodeId),
    
    #[error("Type error: {0}")]
    TypeError(String),
    
    #[error("Parse error: {0}")]
    ParseError(String),
    
    #[error("Core error: {0}")]
    CoreError(#[from] fluentai_core::Error),
    
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, MetaprogrammingError>;