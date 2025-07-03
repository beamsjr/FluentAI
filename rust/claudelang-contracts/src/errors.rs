//! Contract-related error types

use claudelang_core::ast::NodeId;
use thiserror::Error;

use crate::contract::ContractKind;

/// Main contract error type
#[derive(Error, Debug)]
pub enum ContractError {
    /// Contract violation during execution
    #[error("Contract violation: {0}")]
    Violation(#[from] ContractViolation),
    
    /// Error during contract verification
    #[error("Verification error: {0}")]
    VerificationError(String),
    
    /// Error parsing contract specification
    #[error("Contract parsing error: {0}")]
    ParseError(String),
    
    /// Z3 solver error
    #[error("SMT solver error: {0}")]
    SolverError(String),
    
    /// Timeout during verification
    #[error("Verification timeout after {0} seconds")]
    Timeout(u64),
    
    /// Feature not yet implemented
    #[error("Feature not implemented: {0}")]
    NotImplemented(String),
    
    /// Other error
    #[error("{0}")]
    Other(String),
}

/// Represents a contract violation
#[derive(Error, Debug, Clone)]
pub enum ContractViolation {
    /// Precondition violation
    #[error("Precondition violated{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        message
    )]
    Precondition {
        function: Option<String>,
        message: String,
        node_id: NodeId,
    },
    
    /// Postcondition violation
    #[error("Postcondition violated{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        message
    )]
    Postcondition {
        function: Option<String>,
        message: String,
        node_id: NodeId,
    },
    
    /// Invariant violation
    #[error("Invariant violated{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        message
    )]
    Invariant {
        function: Option<String>,
        message: String,
        node_id: NodeId,
    },
    
    /// Purity violation
    #[error("Purity violation in function '{}': {}", function, message)]
    Purity {
        function: String,
        message: String,
        node_id: NodeId,
    },
}

impl ContractViolation {
    /// Create a new contract violation
    pub fn new(
        kind: ContractKind,
        function: Option<String>,
        message: String,
        node_id: NodeId,
    ) -> Self {
        match kind {
            ContractKind::Precondition => Self::Precondition {
                function,
                message,
                node_id,
            },
            ContractKind::Postcondition => Self::Postcondition {
                function,
                message,
                node_id,
            },
            ContractKind::Invariant => Self::Invariant {
                function,
                message,
                node_id,
            },
        }
    }
    
    /// Get the node ID where the violation occurred
    pub fn node_id(&self) -> NodeId {
        match self {
            Self::Precondition { node_id, .. } |
            Self::Postcondition { node_id, .. } |
            Self::Invariant { node_id, .. } |
            Self::Purity { node_id, .. } => *node_id,
        }
    }
    
    /// Get the function name if available
    pub fn function_name(&self) -> Option<&str> {
        match self {
            Self::Precondition { function, .. } |
            Self::Postcondition { function, .. } |
            Self::Invariant { function, .. } => function.as_deref(),
            Self::Purity { function, .. } => Some(function),
        }
    }
}

/// Result type for contract operations
pub type ContractResult<T> = Result<T, ContractError>;