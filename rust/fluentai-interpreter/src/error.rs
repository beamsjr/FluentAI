//! Error types for the interpreter

use std::fmt;
use thiserror::Error;

use fluentai_core::ast::NodeId;

/// Result type for interpreter operations
pub type InterpreterResult<T> = Result<T, InterpreterError>;

/// Errors that can occur during interpretation
#[derive(Debug, Error)]
pub enum InterpreterError {
    /// Variable not found
    #[error("Name error: {0}")]
    NameError(String),

    /// Type mismatch
    #[error("Type error: {0}")]
    TypeError(String),

    /// Invalid number of arguments
    #[error("Arity error: expected {expected} arguments, got {actual}")]
    ArityError { expected: usize, actual: usize },

    /// Division by zero
    #[error("Division by zero")]
    DivisionByZero,

    /// Index out of bounds
    #[error("Index out of bounds: {index} (length: {length})")]
    IndexOutOfBounds { index: i64, length: usize },

    /// Key not found
    #[error("Key not found: {0}")]
    KeyNotFound(String),

    /// Invalid operation
    #[error("Invalid operation: {0}")]
    InvalidOperation(String),

    /// Pattern match failure
    #[error("Pattern match failed: {0}")]
    PatternMatchError(String),

    /// Module not found
    #[error("Module not found: {0}")]
    ModuleNotFound(String),

    /// Export not found
    #[error("Export '{export}' not found in module '{module}'")]
    ExportNotFound { module: String, export: String },

    /// Effect error
    #[error("Effect error: {0}")]
    EffectError(String),

    /// Contract violation
    #[error("Contract violation: {0}")]
    ContractViolation(String),

    /// Async error
    #[error("Async error: {0}")]
    AsyncError(String),
    
    /// Module error
    #[error("Module error: {0}")]
    ModuleError(String),

    /// Channel error
    #[error("Channel error: {0}")]
    ChannelError(String),

    /// Stack overflow
    #[error("Stack overflow: maximum recursion depth exceeded")]
    StackOverflow,

    /// Evaluation timeout
    #[error("Evaluation timeout")]
    Timeout,

    /// Node not found in graph
    #[error("Node {0:?} not found in graph")]
    NodeNotFound(NodeId),

    /// Break outside of loop
    #[error("Break outside of loop")]
    BreakOutsideLoop,

    /// Continue outside of loop
    #[error("Continue outside of loop")]
    ContinueOutsideLoop,

    /// User interrupt
    #[error("Interrupted by user")]
    Interrupted,

    /// Generic runtime error
    #[error("Runtime error: {0}")]
    RuntimeError(String),

    /// IO error
    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    /// Other errors
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

/// Control flow signals (not really errors but use same mechanism)
#[derive(Debug, Clone)]
pub enum ControlFlow {
    /// Break from loop
    Break(Option<crate::value::Value>),
    /// Continue loop
    Continue,
    /// Return from function
    Return(crate::value::Value),
}

impl fmt::Display for ControlFlow {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ControlFlow::Break(_) => write!(f, "break"),
            ControlFlow::Continue => write!(f, "continue"),
            ControlFlow::Return(_) => write!(f, "return"),
        }
    }
}

impl std::error::Error for ControlFlow {}

/// Helper trait for converting results
pub trait IntoInterpreterError<T> {
    fn into_interpreter_error(self) -> InterpreterResult<T>;
}

impl<T, E: Into<InterpreterError>> IntoInterpreterError<T> for Result<T, E> {
    fn into_interpreter_error(self) -> InterpreterResult<T> {
        self.map_err(Into::into)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = InterpreterError::NameError("x".to_string());
        assert_eq!(err.to_string(), "Name error: x");

        let err = InterpreterError::ArityError {
            expected: 2,
            actual: 3,
        };
        assert_eq!(err.to_string(), "Arity error: expected 2 arguments, got 3");
    }
}