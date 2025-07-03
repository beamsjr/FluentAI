//! Ghost state support for specifications
//! 
//! Ghost state allows contracts to refer to specification-only variables and
//! expressions that don't affect the runtime behavior but are useful for verification.

use std::collections::HashMap;
use claudelang_core::ast::{Graph, Node, NodeId};
use crate::{
    contract::{Contract, ContractCondition},
    errors::{ContractError, ContractResult},
};
use rustc_hash::FxHashMap;

/// Ghost state manager for contracts
pub struct GhostStateManager<'a> {
    /// The AST graph
    graph: &'a Graph,
    
    /// Ghost variables: name -> (type, initial_value)
    ghost_vars: FxHashMap<String, GhostVariable>,
    
    /// Old values: expression -> node capturing pre-state
    old_values: FxHashMap<NodeId, OldValue>,
    
    /// Ghost functions: pure functions for specifications
    ghost_functions: FxHashMap<String, GhostFunction>,
    
    /// History variables: track sequences of values
    history_vars: FxHashMap<String, HistoryVariable>,
}

/// A ghost variable used only in specifications
#[derive(Debug, Clone)]
pub struct GhostVariable {
    /// Variable name
    pub name: String,
    
    /// Type of the variable (if known)
    pub var_type: Option<String>,
    
    /// Initial value expression
    pub initial_value: Option<NodeId>,
    
    /// Whether this is a model field
    pub is_model_field: bool,
}

/// Captures the old (pre-state) value of an expression
#[derive(Debug, Clone)]
pub struct OldValue {
    /// The expression to capture
    pub expression: NodeId,
    
    /// When to capture (e.g., "pre", "loop-entry")
    pub capture_point: String,
    
    /// The captured value node
    pub captured_node: NodeId,
}

/// A ghost function for specifications
#[derive(Debug, Clone)]
pub struct GhostFunction {
    /// Function name
    pub name: String,
    
    /// Parameters
    pub params: Vec<String>,
    
    /// Body expression
    pub body: NodeId,
    
    /// Whether this is a model method
    pub is_model_method: bool,
}

/// History variable tracking value sequences
#[derive(Debug, Clone)]
pub struct HistoryVariable {
    /// Variable name
    pub name: String,
    
    /// Expression being tracked
    pub tracked_expr: NodeId,
    
    /// Maximum history length (None = unlimited)
    pub max_length: Option<usize>,
}

impl<'a> GhostStateManager<'a> {
    /// Create a new ghost state manager
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            ghost_vars: FxHashMap::default(),
            old_values: FxHashMap::default(),
            ghost_functions: FxHashMap::default(),
            history_vars: FxHashMap::default(),
        }
    }
    
    /// Add a ghost variable
    pub fn add_ghost_variable(&mut self, var: GhostVariable) {
        self.ghost_vars.insert(var.name.clone(), var);
    }
    
    /// Add an old value capture
    pub fn add_old_value(&mut self, expr: NodeId, capture_point: String) -> NodeId {
        // Create a new node to represent the captured value
        let captured = self.create_old_node(expr);
        
        let old_val = OldValue {
            expression: expr,
            capture_point,
            captured_node: captured,
        };
        
        self.old_values.insert(expr, old_val);
        captured
    }
    
    /// Create a node representing an old value
    fn create_old_node(&self, _expr: NodeId) -> NodeId {
        // In a real implementation, this would create a special node
        // that the evaluator knows to handle as a pre-state value
        NodeId::new(999999).unwrap()
    }
    
    /// Add a ghost function
    pub fn add_ghost_function(&mut self, func: GhostFunction) {
        self.ghost_functions.insert(func.name.clone(), func);
    }
    
    /// Add a history variable
    pub fn add_history_variable(&mut self, var: HistoryVariable) {
        self.history_vars.insert(var.name.clone(), var);
    }
    
    /// Process a contract to extract ghost state
    pub fn process_contract(&mut self, contract: &Contract) -> ContractResult<()> {
        // Process preconditions
        for condition in &contract.preconditions {
            self.process_condition(condition)?;
        }
        
        // Process postconditions
        for condition in &contract.postconditions {
            self.process_condition(condition)?;
        }
        
        // Process invariants
        for condition in &contract.invariants {
            self.process_condition(condition)?;
        }
        
        Ok(())
    }
    
    /// Process a contract condition for ghost state
    fn process_condition(&mut self, condition: &ContractCondition) -> ContractResult<()> {
        self.extract_ghost_state(condition.expression)
    }
    
    /// Extract ghost state from an expression
    fn extract_ghost_state(&mut self, node_id: NodeId) -> ContractResult<()> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Node {:?} not found", node_id)
            ))?;
        
        match node {
            Node::Application { function, args } => {
                // Check for special ghost state functions
                if let Some(Node::Variable { name }) = self.graph.get_node(*function) {
                    match name.as_str() {
                        "old" => {
                            // old(expr) - capture pre-state value
                            if let Some(&expr) = args.first() {
                                self.add_old_value(expr, "pre".to_string());
                            }
                        }
                        "ghost" => {
                            // ghost(var, init) - declare ghost variable
                            if args.len() >= 1 {
                                if let Some(Node::Variable { name: var_name }) = 
                                    self.graph.get_node(args[0]) {
                                    let ghost_var = GhostVariable {
                                        name: var_name.clone(),
                                        var_type: None,
                                        initial_value: args.get(1).copied(),
                                        is_model_field: false,
                                    };
                                    self.add_ghost_variable(ghost_var);
                                }
                            }
                        }
                        "history" => {
                            // history(expr, var) - track history
                            if args.len() >= 2 {
                                if let Some(Node::Variable { name: var_name }) = 
                                    self.graph.get_node(args[1]) {
                                    let history_var = HistoryVariable {
                                        name: var_name.clone(),
                                        tracked_expr: args[0],
                                        max_length: None,
                                    };
                                    self.add_history_variable(history_var);
                                }
                            }
                        }
                        _ => {}
                    }
                }
                
                // Recursively process arguments
                for arg in args {
                    self.extract_ghost_state(*arg)?;
                }
            }
            Node::Let { bindings, body } => {
                // Check for ghost bindings
                for (name, value) in bindings {
                    if name.starts_with("ghost_") {
                        let ghost_var = GhostVariable {
                            name: name.clone(),
                            var_type: None,
                            initial_value: Some(*value),
                            is_model_field: false,
                        };
                        self.add_ghost_variable(ghost_var);
                    }
                    self.extract_ghost_state(*value)?;
                }
                self.extract_ghost_state(*body)?;
            }
            Node::If { condition, then_branch, else_branch } => {
                self.extract_ghost_state(*condition)?;
                self.extract_ghost_state(*then_branch)?;
                self.extract_ghost_state(*else_branch)?;
            }
            _ => {}
        }
        
        Ok(())
    }
    
    /// Get all ghost variables
    pub fn ghost_variables(&self) -> &FxHashMap<String, GhostVariable> {
        &self.ghost_vars
    }
    
    /// Get all old values
    pub fn old_values(&self) -> &FxHashMap<NodeId, OldValue> {
        &self.old_values
    }
    
    /// Get all ghost functions
    pub fn ghost_functions(&self) -> &FxHashMap<String, GhostFunction> {
        &self.ghost_functions
    }
    
    /// Get all history variables
    pub fn history_variables(&self) -> &FxHashMap<String, HistoryVariable> {
        &self.history_vars
    }
}

/// Builder for ghost state expressions
pub struct GhostStateBuilder<'a> {
    graph: &'a mut Graph,
}

impl<'a> GhostStateBuilder<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { graph }
    }
    
    /// Build an old() expression
    pub fn old(&mut self, expr: NodeId) -> NodeId {
        let old_fn = self.graph.add_node(Node::Variable { 
            name: "old".to_string() 
        });
        self.graph.add_node(Node::Application {
            function: old_fn,
            args: vec![expr],
        })
    }
    
    /// Build a ghost variable declaration
    pub fn ghost_var(&mut self, name: &str, init: Option<NodeId>) -> NodeId {
        let ghost_fn = self.graph.add_node(Node::Variable { 
            name: "ghost".to_string() 
        });
        let var_node = self.graph.add_node(Node::Variable { 
            name: name.to_string() 
        });
        
        let mut args = vec![var_node];
        if let Some(init_expr) = init {
            args.push(init_expr);
        }
        
        self.graph.add_node(Node::Application {
            function: ghost_fn,
            args,
        })
    }
    
    /// Build a history tracking expression
    pub fn history(&mut self, tracked_expr: NodeId, var_name: &str) -> NodeId {
        let history_fn = self.graph.add_node(Node::Variable { 
            name: "history".to_string() 
        });
        let var_node = self.graph.add_node(Node::Variable { 
            name: var_name.to_string() 
        });
        
        self.graph.add_node(Node::Application {
            function: history_fn,
            args: vec![tracked_expr, var_node],
        })
    }
    
    /// Build a model field access
    pub fn model_field(&mut self, object: NodeId, field: &str) -> NodeId {
        let field_node = self.graph.add_node(Node::Variable { 
            name: format!("model_{}", field) 
        });
        let dot = self.graph.add_node(Node::Variable { 
            name: ".".to_string() 
        });
        
        self.graph.add_node(Node::Application {
            function: dot,
            args: vec![object, field_node],
        })
    }
}

/// Extensions for contracts with ghost state
impl Contract {
    /// Add a ghost variable to this contract
    pub fn add_ghost_variable(&mut self, name: String, init: Option<NodeId>) {
        // This would be stored in a new field on Contract
        // For now, we'll use a comment to indicate the intent
        let msg = format!("Ghost variable: {} = {:?}", name, init);
        if let Some(first_pre) = self.preconditions.first_mut() {
            first_pre.message = Some(msg);
        }
    }
    
    /// Check if this contract uses ghost state
    pub fn uses_ghost_state(&self) -> bool {
        // Check all conditions for ghost state usage
        self.preconditions.iter().any(|c| Self::contains_ghost_state(c)) ||
        self.postconditions.iter().any(|c| Self::contains_ghost_state(c)) ||
        self.invariants.iter().any(|c| Self::contains_ghost_state(c))
    }
    
    fn contains_ghost_state(condition: &ContractCondition) -> bool {
        // In a real implementation, would check the AST
        condition.message.as_ref()
            .map(|m| m.contains("ghost") || m.contains("old") || m.contains("history"))
            .unwrap_or(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::num::NonZeroU32;
    
    #[test]
    fn test_ghost_state_manager() {
        let graph = Graph::new();
        let mut manager = GhostStateManager::new(&graph);
        
        // Add a ghost variable
        let ghost_var = GhostVariable {
            name: "ghost_counter".to_string(),
            var_type: Some("Int".to_string()),
            initial_value: None,
            is_model_field: false,
        };
        manager.add_ghost_variable(ghost_var);
        
        assert_eq!(manager.ghost_variables().len(), 1);
        assert!(manager.ghost_variables().contains_key("ghost_counter"));
    }
    
    #[test]
    fn test_ghost_state_builder() {
        let mut graph = Graph::new();
        let mut builder = GhostStateBuilder::new(&mut graph);
        
        // Build old(x)
        let x = graph.add_node(Node::Variable { name: "x".to_string() });
        let old_x = builder.old(x);
        
        // Verify structure
        if let Some(Node::Application { function, args }) = graph.get_node(old_x) {
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                assert_eq!(name, "old");
            }
            assert_eq!(args.len(), 1);
            assert_eq!(args[0], x);
        } else {
            panic!("Expected Application node");
        }
    }
}