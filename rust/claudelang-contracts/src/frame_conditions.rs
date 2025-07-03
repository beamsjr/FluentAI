//! Frame conditions for modular reasoning
//! 
//! Frame conditions specify what a function does NOT modify, enabling
//! modular verification by limiting the scope of what needs to be checked.

use std::collections::{HashMap, HashSet};
use claudelang_core::ast::{Graph, Node, NodeId};
use crate::{
    contract::Contract,
    errors::{ContractError, ContractResult},
};
use rustc_hash::FxHashSet;

/// Manages frame conditions for contracts
pub struct FrameConditionManager<'a> {
    /// The AST graph
    graph: &'a Graph,
    
    /// Frame conditions for each function
    frame_conditions: HashMap<String, FrameCondition>,
    
    /// Global variables that can be modified
    modifiable_globals: FxHashSet<String>,
    
    /// Heap regions that can be modified
    modifiable_regions: FxHashSet<HeapRegion>,
}

/// A frame condition specifying what a function may modify
#[derive(Debug, Clone)]
pub struct FrameCondition {
    /// Variables that may be modified
    pub modifies: FxHashSet<String>,
    
    /// Object fields that may be modified
    pub modifies_fields: FxHashSet<FieldAccess>,
    
    /// Array/list indices that may be modified
    pub modifies_indices: FxHashSet<IndexAccess>,
    
    /// Heap regions that may be modified
    pub modifies_regions: FxHashSet<HeapRegion>,
    
    /// Whether this function may allocate new objects
    pub may_allocate: bool,
    
    /// Whether this function may deallocate objects
    pub may_deallocate: bool,
}

/// Represents access to an object field
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FieldAccess {
    /// Object expression (simplified to string for now)
    pub object: String,
    
    /// Field name
    pub field: String,
}

/// Represents access to an array/list index
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct IndexAccess {
    /// Array/list expression
    pub array: String,
    
    /// Index expression (simplified)
    pub index: IndexExpr,
}

/// Index expression for arrays
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum IndexExpr {
    /// Constant index
    Constant(i64),
    
    /// Variable index
    Variable(String),
    
    /// Range of indices
    Range(Box<IndexExpr>, Box<IndexExpr>),
    
    /// All indices
    All,
}

/// Heap region identifier
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HeapRegion {
    /// Named region
    Named(String),
    
    /// Region reachable from a variable
    ReachableFrom(String),
    
    /// Fresh/newly allocated region
    Fresh,
    
    /// Global heap
    Global,
}

impl<'a> FrameConditionManager<'a> {
    /// Create a new frame condition manager
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            frame_conditions: HashMap::new(),
            modifiable_globals: FxHashSet::default(),
            modifiable_regions: FxHashSet::default(),
        }
    }
    
    /// Add a frame condition for a function
    pub fn add_frame_condition(&mut self, function: String, condition: FrameCondition) {
        self.frame_conditions.insert(function, condition);
    }
    
    /// Extract frame conditions from a contract
    pub fn extract_from_contract(&mut self, contract: &Contract) -> ContractResult<FrameCondition> {
        let mut frame = FrameCondition::new();
        
        // Look for :modifies clauses in contract
        // In a real implementation, this would parse contract annotations
        
        // For now, analyze the function body to infer modifications
        if let Some(func_name) = &contract.function_name {
            self.analyze_modifications(contract.body, &mut frame)?;
            self.frame_conditions.insert(func_name.clone(), frame.clone());
        }
        
        Ok(frame)
    }
    
    /// Analyze an expression to find modifications
    fn analyze_modifications(&self, node_id: NodeId, frame: &mut FrameCondition) -> ContractResult<()> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Node {:?} not found", node_id)
            ))?;
        
        match node {
            Node::Application { function, args } => {
                // Check for mutation operations
                if let Some(Node::Variable { name }) = self.graph.get_node(*function) {
                    match name.as_str() {
                        "set!" => {
                            // Variable assignment
                            if let Some(&var_node) = args.first() {
                                if let Some(Node::Variable { name: var_name }) = self.graph.get_node(var_node) {
                                    frame.modifies.insert(var_name.clone());
                                }
                            }
                        }
                        "vector-set!" | "array-set!" => {
                            // Array modification
                            if args.len() >= 2 {
                                if let Some(Node::Variable { name: arr_name }) = self.graph.get_node(args[0]) {
                                    frame.modifies_indices.insert(IndexAccess {
                                        array: arr_name.clone(),
                                        index: self.extract_index(args[1]),
                                    });
                                }
                            }
                        }
                        "set-field!" | "." if args.len() >= 2 => {
                            // Field modification
                            if let Some(Node::Variable { name: obj_name }) = self.graph.get_node(args[0]) {
                                if let Some(Node::Variable { name: field_name }) = self.graph.get_node(args[1]) {
                                    frame.modifies_fields.insert(FieldAccess {
                                        object: obj_name.clone(),
                                        field: field_name.clone(),
                                    });
                                }
                            }
                        }
                        "cons" | "list" | "make-vector" => {
                            // Allocation
                            frame.may_allocate = true;
                        }
                        _ => {}
                    }
                }
                
                // Recursively analyze arguments
                for arg in args {
                    self.analyze_modifications(*arg, frame)?;
                }
            }
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    self.analyze_modifications(*value, frame)?;
                }
                self.analyze_modifications(*body, frame)?;
            }
            Node::If { condition, then_branch, else_branch } => {
                self.analyze_modifications(*condition, frame)?;
                self.analyze_modifications(*then_branch, frame)?;
                self.analyze_modifications(*else_branch, frame)?;
            }
            _ => {}
        }
        
        Ok(())
    }
    
    /// Extract index expression
    fn extract_index(&self, node_id: NodeId) -> IndexExpr {
        if let Some(node) = self.graph.get_node(node_id) {
            match node {
                Node::Literal(lit) => {
                    if let claudelang_core::ast::Literal::Integer(n) = lit {
                        return IndexExpr::Constant(*n);
                    }
                }
                Node::Variable { name } => {
                    return IndexExpr::Variable(name.clone());
                }
                _ => {}
            }
        }
        IndexExpr::All // Conservative default
    }
    
    /// Check if a modification is allowed by frame conditions
    pub fn check_modification_allowed(
        &self,
        function: &str,
        modification: &Modification,
    ) -> ContractResult<bool> {
        if let Some(frame) = self.frame_conditions.get(function) {
            match modification {
                Modification::Variable(var) => Ok(frame.modifies.contains(var)),
                Modification::Field(access) => Ok(frame.modifies_fields.contains(access)),
                Modification::Index(access) => Ok(frame.modifies_indices.contains(access)),
                Modification::Allocation => Ok(frame.may_allocate),
                Modification::Deallocation => Ok(frame.may_deallocate),
            }
        } else {
            // No frame condition means unrestricted modification
            Ok(true)
        }
    }
    
    /// Get frame condition for a function
    pub fn get_frame_condition(&self, function: &str) -> Option<&FrameCondition> {
        self.frame_conditions.get(function)
    }
}

/// Represents a modification
#[derive(Debug, Clone)]
pub enum Modification {
    Variable(String),
    Field(FieldAccess),
    Index(IndexAccess),
    Allocation,
    Deallocation,
}

impl FrameCondition {
    /// Create an empty frame condition
    pub fn new() -> Self {
        Self {
            modifies: FxHashSet::default(),
            modifies_fields: FxHashSet::default(),
            modifies_indices: FxHashSet::default(),
            modifies_regions: FxHashSet::default(),
            may_allocate: false,
            may_deallocate: false,
        }
    }
    
    /// Create a pure frame condition (modifies nothing)
    pub fn pure() -> Self {
        Self::new()
    }
    
    /// Check if this frame condition is pure
    pub fn is_pure(&self) -> bool {
        self.modifies.is_empty() &&
        self.modifies_fields.is_empty() &&
        self.modifies_indices.is_empty() &&
        self.modifies_regions.is_empty() &&
        !self.may_allocate &&
        !self.may_deallocate
    }
    
    /// Add a modifiable variable
    pub fn add_modifies_var(&mut self, var: String) {
        self.modifies.insert(var);
    }
    
    /// Add a modifiable field
    pub fn add_modifies_field(&mut self, object: String, field: String) {
        self.modifies_fields.insert(FieldAccess { object, field });
    }
    
    /// Add a modifiable index
    pub fn add_modifies_index(&mut self, array: String, index: IndexExpr) {
        self.modifies_indices.insert(IndexAccess { array, index });
    }
    
    /// Add a modifiable region
    pub fn add_modifies_region(&mut self, region: HeapRegion) {
        self.modifies_regions.insert(region);
    }
}

/// Builder for frame conditions
pub struct FrameConditionBuilder {
    condition: FrameCondition,
}

impl FrameConditionBuilder {
    pub fn new() -> Self {
        Self {
            condition: FrameCondition::new(),
        }
    }
    
    /// Build a frame condition that modifies specific variables
    pub fn modifies_vars(mut self, vars: Vec<String>) -> Self {
        for var in vars {
            self.condition.add_modifies_var(var);
        }
        self
    }
    
    /// Build a frame condition that modifies specific fields
    pub fn modifies_fields(mut self, fields: Vec<(String, String)>) -> Self {
        for (obj, field) in fields {
            self.condition.add_modifies_field(obj, field);
        }
        self
    }
    
    /// Allow allocation
    pub fn allows_allocation(mut self) -> Self {
        self.condition.may_allocate = true;
        self
    }
    
    /// Build the frame condition
    pub fn build(self) -> FrameCondition {
        self.condition
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_frame_condition_builder() {
        let frame = FrameConditionBuilder::new()
            .modifies_vars(vec!["x".to_string(), "y".to_string()])
            .modifies_fields(vec![("obj".to_string(), "field".to_string())])
            .allows_allocation()
            .build();
        
        assert!(frame.modifies.contains("x"));
        assert!(frame.modifies.contains("y"));
        assert_eq!(frame.modifies_fields.len(), 1);
        assert!(frame.may_allocate);
        assert!(!frame.is_pure());
    }
    
    #[test]
    fn test_pure_frame_condition() {
        let frame = FrameCondition::pure();
        assert!(frame.is_pure());
        assert!(frame.modifies.is_empty());
    }
}