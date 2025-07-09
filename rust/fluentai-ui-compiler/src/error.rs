//! Error types for the UI compiler

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parse error: {0}")]
    ParseError(String),

    #[error("Codegen error: {0}")]
    CodegenError(String),

    #[error("Optimization error: {0}")]
    OptimizationError(String),

    #[error("Unsupported feature: {0}")]
    UnsupportedFeature(String),

    #[error("Invalid output format: {0}")]
    InvalidOutputFormat(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("Core error: {0}")]
    CoreError(#[from] fluentai_core::error::Error),

    #[error("Parser error: {0}")]
    ParserError(String),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, CompilerError>;
