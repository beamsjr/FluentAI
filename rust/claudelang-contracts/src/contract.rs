//! Contract representation and core structures

use claudelang_core::ast::NodeId;
use serde::{Deserialize, Serialize};

/// Represents a contract specification for a function
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Contract {
    /// Name of the function this contract applies to
    pub function_name: String,
    
    /// Preconditions that must hold before function execution
    pub preconditions: Vec<ContractCondition>,
    
    /// Postconditions that must hold after function execution
    pub postconditions: Vec<ContractCondition>,
    
    /// Invariants that must hold throughout execution
    pub invariants: Vec<ContractCondition>,
    
    /// Optional complexity specification (e.g., "O(n)")
    pub complexity: Option<String>,
    
    /// Whether this function is pure (no side effects)
    pub pure: bool,
    
    /// Location information for error reporting
    pub node_id: NodeId,
}

/// A single contract condition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContractCondition {
    /// The condition expression (as an AST node ID)
    pub expression: NodeId,
    
    /// Optional message to display on violation
    pub message: Option<String>,
    
    /// Kind of condition
    pub kind: ContractKind,
}

/// Types of contract conditions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ContractKind {
    /// Precondition (requires)
    Precondition,
    
    /// Postcondition (ensures)
    Postcondition,
    
    /// Invariant
    Invariant,
}

impl Contract {
    /// Create a new empty contract for a function
    pub fn new(function_name: String, node_id: NodeId) -> Self {
        Self {
            function_name,
            preconditions: Vec::new(),
            postconditions: Vec::new(),
            invariants: Vec::new(),
            complexity: None,
            pure: false,
            node_id,
        }
    }
    
    /// Add a precondition to the contract
    pub fn add_precondition(&mut self, condition: ContractCondition) {
        self.preconditions.push(condition);
    }
    
    /// Add a postcondition to the contract
    pub fn add_postcondition(&mut self, condition: ContractCondition) {
        self.postconditions.push(condition);
    }
    
    /// Add an invariant to the contract
    pub fn add_invariant(&mut self, condition: ContractCondition) {
        self.invariants.push(condition);
    }
    
    /// Check if this contract has any conditions
    pub fn has_conditions(&self) -> bool {
        !self.preconditions.is_empty() 
            || !self.postconditions.is_empty() 
            || !self.invariants.is_empty()
    }
    
    /// Get all conditions of a specific kind
    pub fn conditions_of_kind(&self, kind: ContractKind) -> &[ContractCondition] {
        match kind {
            ContractKind::Precondition => &self.preconditions,
            ContractKind::Postcondition => &self.postconditions,
            ContractKind::Invariant => &self.invariants,
        }
    }
}

impl ContractCondition {
    /// Create a new contract condition
    pub fn new(expression: NodeId, kind: ContractKind) -> Self {
        Self {
            expression,
            message: None,
            kind,
        }
    }
    
    /// Create a condition with a custom message
    pub fn with_message(expression: NodeId, kind: ContractKind, message: String) -> Self {
        Self {
            expression,
            message: Some(message),
            kind,
        }
    }
}