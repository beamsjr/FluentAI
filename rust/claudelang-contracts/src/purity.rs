//! Purity analysis for contract expressions
//! 
//! This module provides functionality to check whether expressions are pure,
//! meaning they have no side effects and are deterministic. This is crucial
//! for contract expressions to ensure verification soundness.

use std::collections::{HashMap, HashSet};
use claudelang_core::ast::{Graph, Node, NodeId, EffectType};
use crate::errors::{ContractError, ContractResult};

/// Analyzes expressions for purity
pub struct PurityChecker<'a> {
    /// The AST graph containing expressions
    graph: &'a Graph,
    
    /// Cache of purity results for nodes
    purity_cache: HashMap<NodeId, bool>,
    
    /// Set of known pure functions
    pure_functions: HashSet<String>,
    
    /// Set of known impure functions
    impure_functions: HashSet<String>,
}

impl<'a> PurityChecker<'a> {
    /// Create a new purity checker
    pub fn new(graph: &'a Graph) -> Self {
        let mut checker = Self {
            graph,
            purity_cache: HashMap::new(),
            pure_functions: HashSet::new(),
            impure_functions: HashSet::new(),
        };
        
        // Initialize with known pure built-in functions
        checker.register_pure_builtins();
        checker
    }
    
    /// Register built-in pure functions
    fn register_pure_builtins(&mut self) {
        // Arithmetic operations
        self.pure_functions.extend([
            "+", "-", "*", "/", "mod", "modulo", "%", "abs", "min", "max",
            "floor", "ceiling", "round", "sqrt", "pow",
        ].iter().map(|s| s.to_string()));
        
        // Comparison operations
        self.pure_functions.extend([
            "=", "==", "<", ">", "<=", ">=", "!=", "<>", "not=",
            "eq?", "equal?", "null?", "zero?", "positive?", "negative?",
            "even?", "odd?", "empty?",
        ].iter().map(|s| s.to_string()));
        
        // Logical operations
        self.pure_functions.extend([
            "and", "or", "not", "xor", "implies", "=>",
        ].iter().map(|s| s.to_string()));
        
        // List operations (non-mutating)
        self.pure_functions.extend([
            "car", "cdr", "cons", "list", "length", "append",
            "reverse", "member", "filter", "map", "fold",
            "first", "rest", "nth", "take", "drop",
        ].iter().map(|s| s.to_string()));
        
        // Type predicates
        self.pure_functions.extend([
            "number?", "integer?", "float?", "string?", "symbol?",
            "list?", "pair?", "boolean?", "procedure?",
        ].iter().map(|s| s.to_string()));
        
        // String operations (non-mutating)
        self.pure_functions.extend([
            "string-length", "string-ref", "substring", "string-append",
            "string->list", "list->string", "string=?", "string<?",
        ].iter().map(|s| s.to_string()));
        
        // Contract-specific functions
        self.pure_functions.extend([
            "old", "result", "forall", "exists", "implies", "∀", "∃",
            "range", "in", "indices",
        ].iter().map(|s| s.to_string()));
    }
    
    /// Register known impure built-in functions
    pub fn register_impure_builtins(&mut self) {
        self.impure_functions.extend([
            "set!", "define", "print", "display", "newline",
            "read", "write", "open-file", "close-file",
            "random", "current-time", "sleep",
            "make-channel", "send", "receive",
            "vector-set!", "string-set!", "set-car!", "set-cdr!",
        ].iter().map(|s| s.to_string()));
    }
    
    /// Mark a function as pure
    pub fn mark_function_pure(&mut self, name: String) {
        self.pure_functions.insert(name.clone());
        self.impure_functions.remove(&name);
    }
    
    /// Mark a function as impure
    pub fn mark_function_impure(&mut self, name: String) {
        self.impure_functions.insert(name.clone());
        self.pure_functions.remove(&name);
    }
    
    /// Check if an expression is pure
    pub fn is_pure(&mut self, node_id: NodeId) -> ContractResult<bool> {
        // Check cache first
        if let Some(&is_pure) = self.purity_cache.get(&node_id) {
            return Ok(is_pure);
        }
        
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Node {:?} not found", node_id)
            ))?;
        
        let is_pure = self.check_node_purity(node)?;
        self.purity_cache.insert(node_id, is_pure);
        Ok(is_pure)
    }
    
    /// Check purity of a specific node
    fn check_node_purity(&mut self, node: &Node) -> ContractResult<bool> {
        match node {
            // Literals are always pure
            Node::Literal(_) => Ok(true),
            
            // Variables are pure (reading doesn't cause side effects)
            Node::Variable { .. } => Ok(true),
            
            // Lambda definitions are pure (defining doesn't execute)
            Node::Lambda { .. } => Ok(true),
            
            // Applications depend on the function and arguments
            Node::Application { function, args } => {
                self.check_application_purity(*function, args)
            }
            
            // Conditionals are pure if all branches are pure
            Node::If { condition, then_branch, else_branch } => {
                Ok(self.is_pure(*condition)? 
                   && self.is_pure(*then_branch)?
                   && self.is_pure(*else_branch)?)
            }
            
            // Let bindings are pure if bindings and body are pure
            Node::Let { bindings, body } => {
                for (_, value_id) in bindings {
                    if !self.is_pure(*value_id)? {
                        return Ok(false);
                    }
                }
                self.is_pure(*body)
            }
            
            // Note: ClaudeLang doesn't have a Sequence node type
            // Sequential execution would be done through let bindings or function applications
            
            // Lists are pure if all elements are pure
            Node::List(elements) => {
                for elem in elements {
                    if !self.is_pure(*elem)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            
            // Pattern matching is pure if scrutinee and all bodies are pure
            Node::Match { expr, branches } => {
                if !self.is_pure(*expr)? {
                    return Ok(false);
                }
                for (_, body) in branches {
                    if !self.is_pure(*body)? {
                        return Ok(false);
                    }
                }
                Ok(true)
            }
            
            // Effects are impure by definition
            Node::Effect { effect_type, .. } => {
                Ok(*effect_type == EffectType::Pure)
            }
            
            // Channel operations are impure
            Node::Send { .. } | Node::Receive { .. } => Ok(false),
            
            // Contracts themselves don't execute, so they're "pure" as nodes
            Node::Contract { .. } => Ok(true),
            
            // Conservative: unknown nodes are impure
            _ => Ok(false),
        }
    }
    
    /// Check if a function application is pure
    fn check_application_purity(&mut self, function_id: NodeId, args: &[NodeId]) -> ContractResult<bool> {
        // First check if all arguments are pure
        for arg in args {
            if !self.is_pure(*arg)? {
                return Ok(false);
            }
        }
        
        // Then check the function itself
        let func_node = self.graph.get_node(function_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Function node {:?} not found", function_id)
            ))?;
        
        match func_node {
            Node::Variable { name } => {
                // Check if it's a known pure/impure function
                if self.pure_functions.contains(name) {
                    Ok(true)
                } else if self.impure_functions.contains(name) {
                    Ok(false)
                } else {
                    // Unknown function - check if it has a contract marking it as pure
                    // For now, be conservative and assume unknown functions are impure
                    Ok(false)
                }
            }
            
            Node::Lambda { params: _, body } => {
                // Lambda/closure - check if body is pure
                self.is_pure(*body)
            }
            
            _ => {
                // Not a direct function reference - be conservative
                Ok(false)
            }
        }
    }
    
    /// Validate that all contract expressions in a contract are pure
    pub fn validate_contract_purity(&mut self, contract: &crate::contract::Contract) -> ContractResult<()> {
        // Check preconditions
        for condition in &contract.preconditions {
            if !self.is_pure(condition.expression)? {
                return Err(ContractError::VerificationError(format!(
                    "Precondition contains impure expression{}",
                    condition.message.as_ref().map(|m| format!(": {}", m)).unwrap_or_default()
                )));
            }
        }
        
        // Check postconditions
        for condition in &contract.postconditions {
            if !self.is_pure(condition.expression)? {
                return Err(ContractError::VerificationError(format!(
                    "Postcondition contains impure expression{}",
                    condition.message.as_ref().map(|m| format!(": {}", m)).unwrap_or_default()
                )));
            }
        }
        
        // Check invariants
        for condition in &contract.invariants {
            if !self.is_pure(condition.expression)? {
                return Err(ContractError::VerificationError(format!(
                    "Invariant contains impure expression{}",
                    condition.message.as_ref().map(|m| format!(": {}", m)).unwrap_or_default()
                )));
            }
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_pure_arithmetic() {
        // Test would create a simple graph with arithmetic operations
        // and verify they're marked as pure
    }
    
    #[test]
    fn test_impure_side_effects() {
        // Test would create a graph with set!, print, etc.
        // and verify they're marked as impure
    }
}