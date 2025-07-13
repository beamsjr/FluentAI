//! Error types for FluentAi

use thiserror::Error;

/// Main error type for FluentAi
#[derive(Error, Debug)]
pub enum Error {
    /// Parse error occurred during source code parsing
    #[error("Parse error: {0}")]
    Parse(String),

    /// Type error occurred during type checking
    #[error("Type error: {0}")]
    Type(String),

    /// Runtime error occurred during execution
    #[error("Runtime error: {0}")]
    Runtime(String),

    /// Variable was referenced but not defined
    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    /// Function called with wrong number of arguments
    #[error("Invalid arity: expected {expected}, got {got}")]
    InvalidArity { 
        /// Expected number of arguments
        expected: usize, 
        /// Actual number of arguments provided
        got: usize 
    },

    /// Effect was referenced but not defined
    #[error("Unknown effect: {0}")]
    UnknownEffect(String),

    /// Contract condition was violated
    #[error("Contract violation: {0}")]
    ContractViolation(String),

    /// AST graph has too many nodes
    #[error("Graph node ID overflow: maximum number of nodes reached")]
    GraphNodeIdOverflow,

    /// IO operation failed
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Any other error
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

/// Result type alias for FluentAi operations
pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
#[path = "error_tests.rs"]
mod tests;
