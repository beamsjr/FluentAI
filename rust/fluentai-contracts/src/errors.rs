//! Contract-related error types

use fluentai_core::ast::NodeId;
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
    
    /// Invalid expression in contract
    #[error("Invalid contract expression: {0}")]
    InvalidExpression(String),
    
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
    #[error("Precondition violated{}{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        if let Some(blame) = blame_label { format!(" [{}]", blame) } else { String::new() },
        message
    )]
    Precondition {
        function: Option<String>,
        message: String,
        node_id: NodeId,
        span: Option<(usize, usize)>,
        blame_label: Option<String>,
    },
    
    /// Postcondition violation
    #[error("Postcondition violated{}{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        if let Some(blame) = blame_label { format!(" [{}]", blame) } else { String::new() },
        message
    )]
    Postcondition {
        function: Option<String>,
        message: String,
        node_id: NodeId,
        span: Option<(usize, usize)>,
        blame_label: Option<String>,
    },
    
    /// Invariant violation
    #[error("Invariant violated{}{}: {}", 
        if let Some(func) = function { format!(" in function '{}'", func) } else { String::new() },
        if let Some(blame) = blame_label { format!(" [{}]", blame) } else { String::new() },
        message
    )]
    Invariant {
        function: Option<String>,
        message: String,
        node_id: NodeId,
        span: Option<(usize, usize)>,
        blame_label: Option<String>,
    },
    
    /// Purity violation
    #[error("Purity violation in function '{}'{}: {}", 
        function,
        if let Some(blame) = blame_label { format!(" [{}]", blame) } else { String::new() },
        message
    )]
    Purity {
        function: String,
        message: String,
        node_id: NodeId,
        span: Option<(usize, usize)>,
        blame_label: Option<String>,
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
                span: None,
                blame_label: None,
            },
            ContractKind::Postcondition => Self::Postcondition {
                function,
                message,
                node_id,
                span: None,
                blame_label: None,
            },
            ContractKind::Invariant => Self::Invariant {
                function,
                message,
                node_id,
                span: None,
                blame_label: None,
            },
        }
    }
    
    /// Create a new contract violation with span and blame information
    pub fn with_details(
        kind: ContractKind,
        function: Option<String>,
        message: String,
        node_id: NodeId,
        span: Option<(usize, usize)>,
        blame_label: Option<String>,
    ) -> Self {
        match kind {
            ContractKind::Precondition => Self::Precondition {
                function,
                message,
                node_id,
                span,
                blame_label,
            },
            ContractKind::Postcondition => Self::Postcondition {
                function,
                message,
                node_id,
                span,
                blame_label,
            },
            ContractKind::Invariant => Self::Invariant {
                function,
                message,
                node_id,
                span,
                blame_label,
            },
        }
    }
    
    /// Set the span for this violation
    pub fn with_span(mut self, span: (usize, usize)) -> Self {
        match &mut self {
            Self::Precondition { span: s, .. } |
            Self::Postcondition { span: s, .. } |
            Self::Invariant { span: s, .. } |
            Self::Purity { span: s, .. } => *s = Some(span),
        }
        self
    }
    
    /// Set the blame label for this violation
    pub fn with_blame(mut self, blame: String) -> Self {
        match &mut self {
            Self::Precondition { blame_label, .. } |
            Self::Postcondition { blame_label, .. } |
            Self::Invariant { blame_label, .. } |
            Self::Purity { blame_label, .. } => *blame_label = Some(blame),
        }
        self
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
    
    /// Get the span if available
    pub fn span(&self) -> Option<(usize, usize)> {
        match self {
            Self::Precondition { span, .. } |
            Self::Postcondition { span, .. } |
            Self::Invariant { span, .. } |
            Self::Purity { span, .. } => *span,
        }
    }
    
    /// Get the blame label if available
    pub fn blame_label(&self) -> Option<&str> {
        match self {
            Self::Precondition { blame_label, .. } |
            Self::Postcondition { blame_label, .. } |
            Self::Invariant { blame_label, .. } |
            Self::Purity { blame_label, .. } => blame_label.as_deref(),
        }
    }
}

/// Result type for contract operations
pub type ContractResult<T> = Result<T, ContractError>;