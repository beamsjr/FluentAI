//! Error types for value operations

use std::fmt;

/// Error type for value operations
#[derive(Debug, Clone, PartialEq)]
pub enum ValueError {
    /// Type mismatch error
    TypeError {
        expected: &'static str,
        actual: &'static str,
    },

    /// Index out of bounds
    IndexOutOfBounds { index: usize, length: usize },

    /// Key not found in map
    KeyNotFound(String),

    /// Invalid operation
    InvalidOperation(String),

    /// Conversion error
    ConversionError {
        from: &'static str,
        to: &'static str,
        reason: String,
    },

    /// Division by zero
    DivisionByZero,

    /// Function arity mismatch
    ArityMismatch { expected: usize, actual: usize },
}

impl fmt::Display for ValueError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ValueError::TypeError { expected, actual } => {
                write!(f, "Type error: expected {}, got {}", expected, actual)
            }
            ValueError::IndexOutOfBounds { index, length } => {
                write!(
                    f,
                    "Index {} out of bounds for list of length {}",
                    index, length
                )
            }
            ValueError::KeyNotFound(key) => {
                write!(f, "Key not found: {}", key)
            }
            ValueError::InvalidOperation(msg) => {
                write!(f, "Invalid operation: {}", msg)
            }
            ValueError::ConversionError { from, to, reason } => {
                write!(f, "Cannot convert {} to {}: {}", from, to, reason)
            }
            ValueError::DivisionByZero => {
                write!(f, "Division by zero")
            }
            ValueError::ArityMismatch { expected, actual } => {
                write!(f, "Function expects {} arguments, got {}", expected, actual)
            }
        }
    }
}

impl std::error::Error for ValueError {}

/// Result type for value operations
pub type ValueResult<T> = Result<T, ValueError>;
