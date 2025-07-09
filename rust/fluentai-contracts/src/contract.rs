//! Contract representation and core structures
//!
//! This module provides the core types for design-by-contract programming in FluentAi.
//! Contracts allow you to specify preconditions, postconditions, and invariants that
//! are verified both statically (using Z3) and at runtime.
//!
//! # Contract Semantics
//!
//! ## Preconditions (`requires`)
//! - Evaluated before function execution begins
//! - Must hold for all valid inputs to the function
//! - Caller's responsibility to ensure preconditions are met
//! - Failed preconditions indicate a bug in the calling code
//!
//! ## Postconditions (`ensures`)
//! - Evaluated after function execution completes
//! - Checked on ALL return paths (normal returns, early returns, pattern match returns)
//! - NOT checked on exceptional paths (panics, errors propagated via `?`)
//! - Can reference the return value via the special variable `result`
//! - Can reference pre-state values using `old(expr)` syntax (when implemented)
//! - Failed postconditions indicate a bug in the function implementation
//!
//! ## Invariants
//! - Object invariants: checked after every public method call on an object
//! - Loop invariants: checked before loop entry and after each iteration
//! - Must be maintained by all operations on the data structure
//! - Failed invariants indicate corruption of internal state
//!
//! # Example
//!
//! ```fluentai
//! (spec:contract factorial
//!   :requires [(>= n 0)]
//!   :ensures [(>= result 1)
//!             (or (= n 0) (= result (* n (factorial (- n 1)))))]
//!   :complexity "O(n)")
//!
//! (define (factorial n)
//!   (if (= n 0)
//!       1
//!       (* n (factorial (- n 1)))))
//! ```
//!
//! # Purity
//!
//! Functions marked as `pure` must:
//! - Have no observable side effects
//! - Be deterministic (same inputs always produce same outputs)
//! - Not perform I/O, modify global state, or allocate on the heap
//! - Only call other pure functions
//!
//! Contract expressions themselves must always be pure to ensure:
//! - Verification doesn't change program behavior
//! - Contracts can be safely disabled in production
//! - Static verification remains sound

use fluentai_core::ast::NodeId;
use serde::{Deserialize, Serialize};

/// Represents a contract specification for a function
///
/// A contract defines the behavioral specification that a function must satisfy.
/// Contracts are used for both static verification (proving correctness) and
/// runtime checking (detecting violations during execution).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Contract {
    /// Name of the function this contract applies to
    pub function_name: String,

    /// Preconditions that must hold before function execution
    ///
    /// These conditions are checked before the function body executes.
    /// Precondition failures blame the caller, not the function.
    pub preconditions: Vec<ContractCondition>,

    /// Postconditions that must hold after function execution
    ///
    /// These conditions are checked after the function returns normally.
    /// They can reference the return value via `result` and the pre-state
    /// via `old()` expressions. Postcondition failures blame the function
    /// implementation.
    pub postconditions: Vec<ContractCondition>,

    /// Invariants that must hold throughout execution
    ///
    /// For object methods: checked after each public method call
    /// For loops: checked before entry and after each iteration
    /// For data structures: must be preserved by all operations
    pub invariants: Vec<ContractCondition>,

    /// Optional complexity specification (e.g., "O(n)", "O(log n)", "O(1)")
    ///
    /// Used for performance contracts and optimization verification.
    /// Can be verified against actual execution profiles.
    pub complexity: Option<String>,

    /// Whether this function is pure (no side effects)
    ///
    /// Pure functions:
    /// - Always return the same output for the same inputs
    /// - Have no observable side effects
    /// - Can be safely memoized and optimized
    /// - Are required for use in contract expressions
    pub pure: bool,

    /// Frame condition (what this function may modify)
    ///
    /// If None, the function may modify anything (conservative).
    /// If Some, specifies exactly what the function is allowed to modify.
    pub frame_condition: Option<crate::frame_conditions::FrameCondition>,

    /// Location information for error reporting
    pub node_id: NodeId,
}

/// A single contract condition
///
/// Represents one logical assertion within a contract. The condition
/// is expressed as an AST node that evaluates to a boolean value.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ContractCondition {
    /// The condition expression (as an AST node ID)
    ///
    /// This must be a pure expression that evaluates to a boolean.
    /// The expression can reference function parameters, return values
    /// (for postconditions), and special contract functions like `old()`.
    pub expression: NodeId,

    /// Optional message to display on violation
    ///
    /// If provided, this message is shown instead of a generic violation
    /// message, helping developers understand what went wrong.
    pub message: Option<String>,

    /// Kind of condition (precondition, postcondition, or invariant)
    pub kind: ContractKind,

    /// Source location for error reporting (start, end)
    ///
    /// Character offsets in the source code where this condition appears.
    /// Used for precise error reporting in IDEs and command-line tools.
    pub span: Option<(usize, usize)>,

    /// Blame label for better error messages
    ///
    /// A short identifier that helps assign blame when contracts fail.
    /// For example: "caller", "implementation", "loop_body", etc.
    pub blame_label: Option<String>,
}

/// Types of contract conditions
///
/// Each type has different semantics for when it's checked and who
/// is blamed when it fails.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Hash)]
pub enum ContractKind {
    /// Precondition (requires)
    ///
    /// Checked before function execution. Failure indicates the caller
    /// violated the function's requirements.
    Precondition,

    /// Postcondition (ensures)
    ///
    /// Checked after function execution. Failure indicates the function
    /// implementation is incorrect.
    Postcondition,

    /// Invariant
    ///
    /// For objects: checked after every public method
    /// For loops: checked before entry and after each iteration
    /// Failure indicates corruption of internal state.
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
            frame_condition: None,
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
            span: None,
            blame_label: None,
        }
    }

    /// Create a condition with a custom message
    pub fn new_with_message(expression: NodeId, kind: ContractKind, message: String) -> Self {
        Self {
            expression,
            message: Some(message),
            kind,
            span: None,
            blame_label: None,
        }
    }

    /// Set the source span for this condition
    pub fn with_span(mut self, span: (usize, usize)) -> Self {
        self.span = Some(span);
        self
    }

    /// Set the blame label for this condition
    pub fn with_blame(mut self, blame: String) -> Self {
        self.blame_label = Some(blame);
        self
    }

    /// Add a message to this condition (builder pattern)
    pub fn with_message(mut self, message: String) -> Self {
        self.message = Some(message);
        self
    }
}

#[cfg(test)]
mod contract_examples {
    //! Examples demonstrating contract semantics
    //!
    //! These examples show how contracts work in different scenarios:
    //!
    //! ## Example 1: Simple arithmetic function
    //! ```fluentai
    //! (spec:contract safe-divide
    //!   :requires [(not (= divisor 0))]
    //!   :ensures [(= result (/ dividend divisor))]
    //!   :pure true)
    //! ```
    //!
    //! ## Example 2: Data structure invariant
    //! ```fluentai
    //! (spec:contract sorted-list-insert
    //!   :requires [(sorted? lst)]
    //!   :ensures [(sorted? result)
    //!             (= (length result) (+ 1 (length lst)))]
    //!   :invariant [(sorted? lst)])
    //! ```
    //!
    //! ## Example 3: Loop invariant
    //! ```fluentai
    //! (spec:contract sum-array
    //!   :requires [(>= n 0)]
    //!   :ensures [(= result (sum arr 0 n))]
    //!   :invariant [(and (>= i 0) (<= i n)
    //!                    (= sum (sum arr 0 i)))])
    //! ```
    //!
    //! ## Example 4: Object invariant
    //! ```fluentai
    //! (spec:contract bank-account
    //!   :invariant [(>= balance 0)
    //!               (= balance (- deposits withdrawals))])
    //! ```
    //!
    //! ## Example 5: Postcondition with old() reference
    //! ```fluentai
    //! (spec:contract increment
    //!   :ensures [(= result (+ (old counter) 1))])
    //! ```
}
