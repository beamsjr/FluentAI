//! Error types for FluentAi

use thiserror::Error;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Parse error: {0}")]
    Parse(String),

    #[error("Type error: {0}")]
    Type(String),

    #[error("Runtime error: {0}")]
    Runtime(String),

    #[error("Undefined variable: {0}")]
    UndefinedVariable(String),

    #[error("Invalid arity: expected {expected}, got {got}")]
    InvalidArity { expected: usize, got: usize },

    #[error("Unknown effect: {0}")]
    UnknownEffect(String),

    #[error("Contract violation: {0}")]
    ContractViolation(String),

    #[error("Graph node ID overflow: maximum number of nodes reached")]
    GraphNodeIdOverflow,

    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
#[path = "error_tests.rs"]
mod tests;