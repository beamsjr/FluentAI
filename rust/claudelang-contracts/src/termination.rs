//! Termination analysis for recursive contracts
//! 
//! This module provides functionality to prove that recursive functions terminate,
//! which is essential for sound contract verification.

use std::collections::{HashMap, HashSet};
use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use crate::{
    contract::Contract,
    errors::{ContractError, ContractResult},
};
use rustc_hash::FxHashMap;

/// Analyzes termination of recursive functions
pub struct TerminationChecker<'a> {
    /// The AST graph
    graph: &'a Graph,
    
    /// Call graph: function -> functions it calls
    call_graph: FxHashMap<String, HashSet<String>>,
    
    /// Recursive functions detected
    recursive_functions: HashSet<String>,
    
    /// Termination measures for functions
    termination_measures: FxHashMap<String, TerminationMeasure>,
    
    /// Cache of termination analysis results
    termination_cache: FxHashMap<String, TerminationResult>,
}

/// A termination measure (ranking function)
#[derive(Debug, Clone)]
pub struct TerminationMeasure {
    /// The expression that measures progress
    pub measure_expr: NodeId,
    
    /// Variables that the measure depends on
    pub depends_on: Vec<String>,
    
    /// Whether this measure is lexicographic
    pub is_lexicographic: bool,
}

/// Result of termination analysis
#[derive(Debug, Clone, PartialEq)]
pub enum TerminationResult {
    /// Function provably terminates
    Terminates { reason: String },
    
    /// Function may not terminate
    MayNotTerminate { reason: String },
    
    /// Cannot determine termination
    Unknown { reason: String },
}

impl<'a> TerminationChecker<'a> {
    /// Create a new termination checker
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            call_graph: FxHashMap::default(),
            recursive_functions: HashSet::new(),
            termination_measures: FxHashMap::default(),
            termination_cache: FxHashMap::default(),
        }
    }
    
    /// Analyze termination for a contract
    pub fn analyze_contract(&mut self, contract: &Contract) -> ContractResult<TerminationResult> {
        let func_name = contract.function_name.as_ref()
            .ok_or_else(|| ContractError::VerificationError(
                "Contract must have associated function".to_string()
            ))?;
        
        // Check cache
        if let Some(result) = self.termination_cache.get(func_name) {
            return Ok(result.clone());
        }
        
        // Build call graph if not already done
        if self.call_graph.is_empty() {
            self.build_call_graph()?;
        }
        
        // Check if function is recursive
        if !self.is_recursive(func_name) {
            let result = TerminationResult::Terminates {
                reason: "Non-recursive function always terminates".to_string()
            };
            self.termination_cache.insert(func_name.clone(), result.clone());
            return Ok(result);
        }
        
        // Analyze recursive function
        let result = self.analyze_recursive_function(func_name, contract)?;
        self.termination_cache.insert(func_name.clone(), result.clone());
        Ok(result)
    }
    
    /// Build the call graph from the AST
    fn build_call_graph(&mut self) -> ContractResult<()> {
        // This is a simplified implementation
        // In a real system, we'd traverse the entire AST
        Ok(())
    }
    
    /// Check if a function is recursive
    fn is_recursive(&self, func_name: &str) -> bool {
        // Check for direct recursion
        if let Some(calls) = self.call_graph.get(func_name) {
            if calls.contains(func_name) {
                return true;
            }
            
            // Check for mutual recursion
            let mut visited = HashSet::new();
            self.has_recursive_path(func_name, func_name, &mut visited)
        } else {
            false
        }
    }
    
    /// Check if there's a recursive path in the call graph
    fn has_recursive_path(&self, start: &str, target: &str, visited: &mut HashSet<String>) -> bool {
        if visited.contains(start) {
            return false;
        }
        visited.insert(start.to_string());
        
        if let Some(calls) = self.call_graph.get(start) {
            for called in calls {
                if called == target {
                    return true;
                }
                if self.has_recursive_path(called, target, visited) {
                    return true;
                }
            }
        }
        false
    }
    
    /// Analyze a recursive function for termination
    fn analyze_recursive_function(&mut self, func_name: &str, contract: &Contract) -> ContractResult<TerminationResult> {
        // Try different termination proof strategies
        
        // 1. Look for explicit termination measure in contract
        if let Some(measure) = self.extract_termination_measure(contract)? {
            return self.verify_termination_measure(func_name, &measure, contract);
        }
        
        // 2. Try to infer termination from recursive structure
        if let Some(result) = self.infer_structural_termination(func_name, contract.body)? {
            return Ok(result);
        }
        
        // 3. Try to find decreasing parameter
        if let Some(result) = self.find_decreasing_parameter(func_name, contract.body)? {
            return Ok(result);
        }
        
        // Cannot prove termination
        Ok(TerminationResult::Unknown {
            reason: "Could not find termination measure or prove structural termination".to_string()
        })
    }
    
    /// Extract termination measure from contract if specified
    fn extract_termination_measure(&self, contract: &Contract) -> ContractResult<Option<TerminationMeasure>> {
        // Look for :terminates-by clause in contract
        // This would be a future enhancement to the contract syntax
        Ok(None)
    }
    
    /// Verify that a termination measure actually decreases
    fn verify_termination_measure(
        &self,
        func_name: &str,
        measure: &TerminationMeasure,
        contract: &Contract,
    ) -> ContractResult<TerminationResult> {
        // Verify that:
        // 1. Measure is well-founded (e.g., natural number)
        // 2. Measure strictly decreases on recursive calls
        // 3. Measure has a lower bound
        
        Ok(TerminationResult::Unknown {
            reason: "Termination measure verification not yet implemented".to_string()
        })
    }
    
    /// Try to infer termination from structural recursion
    fn infer_structural_termination(&self, func_name: &str, body: NodeId) -> ContractResult<Option<TerminationResult>> {
        // Look for patterns like:
        // - List recursion with cdr
        // - Number recursion with (- n 1)
        // - Tree recursion on subtrees
        
        if let Some(node) = self.graph.get_node(body) {
            if let Some(pattern) = self.detect_structural_pattern(node)? {
                return Ok(Some(TerminationResult::Terminates {
                    reason: format!("Structural recursion on {}", pattern)
                }));
            }
        }
        
        Ok(None)
    }
    
    /// Detect structural recursion patterns
    fn detect_structural_pattern(&self, node: &Node) -> ContractResult<Option<String>> {
        match node {
            Node::If { condition, then_branch, else_branch } => {
                // Check for base case pattern
                if self.is_base_case(*condition)? {
                    // Check if recursive call is on smaller structure
                    if self.has_structural_recursive_call(*else_branch)? {
                        return Ok(Some("list/tree structure".to_string()));
                    }
                }
            }
            Node::Match { expr: _, branches } => {
                // Pattern matching often indicates structural recursion
                for (pattern, _body) in branches {
                    if self.is_structural_pattern(*pattern)? {
                        return Ok(Some("pattern match".to_string()));
                    }
                }
            }
            _ => {}
        }
        Ok(None)
    }
    
    /// Check if a condition represents a base case
    fn is_base_case(&self, condition: NodeId) -> ContractResult<bool> {
        if let Some(node) = self.graph.get_node(condition) {
            match node {
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = self.graph.get_node(*function) {
                        // Common base case predicates
                        if matches!(name.as_str(), "null?" | "empty?" | "zero?" | "=" | "<=") {
                            return Ok(true);
                        }
                    }
                }
                _ => {}
            }
        }
        Ok(false)
    }
    
    /// Check for structural recursive call
    fn has_structural_recursive_call(&self, node_id: NodeId) -> ContractResult<bool> {
        // This would analyze the recursive call to ensure it's on a smaller structure
        // For now, simplified implementation
        Ok(false)
    }
    
    /// Check if a pattern is structural
    fn is_structural_pattern(&self, pattern: NodeId) -> ContractResult<bool> {
        // Check for cons patterns, number patterns, etc.
        Ok(false)
    }
    
    /// Try to find a decreasing parameter
    fn find_decreasing_parameter(&self, func_name: &str, body: NodeId) -> ContractResult<Option<TerminationResult>> {
        // Look for parameters that decrease in recursive calls
        // E.g., factorial(n-1), fibonacci(n-1), etc.
        
        let params = self.extract_parameters(func_name)?;
        for param in params {
            if self.parameter_decreases(&param, body)? {
                return Ok(Some(TerminationResult::Terminates {
                    reason: format!("Parameter '{}' decreases on recursive calls", param)
                }));
            }
        }
        
        Ok(None)
    }
    
    /// Extract function parameters
    fn extract_parameters(&self, func_name: &str) -> ContractResult<Vec<String>> {
        // This would extract from the function definition
        Ok(vec![])
    }
    
    /// Check if a parameter decreases in recursive calls
    fn parameter_decreases(&self, param: &str, body: NodeId) -> ContractResult<bool> {
        // Analyze recursive calls to see if param is smaller
        Ok(false)
    }
    
    /// Add a user-provided termination measure
    pub fn add_termination_measure(&mut self, func_name: String, measure: TerminationMeasure) {
        self.termination_measures.insert(func_name, measure);
        self.termination_cache.remove(&func_name); // Invalidate cache
    }
}

/// Helper to build termination measures
pub struct TerminationMeasureBuilder<'a> {
    graph: &'a mut Graph,
}

impl<'a> TerminationMeasureBuilder<'a> {
    pub fn new(graph: &'a mut Graph) -> Self {
        Self { graph }
    }
    
    /// Build a simple numeric measure
    pub fn numeric_measure(&mut self, param: &str) -> TerminationMeasure {
        let var = self.graph.add_node(Node::Variable { name: param.to_string() });
        TerminationMeasure {
            measure_expr: var,
            depends_on: vec![param.to_string()],
            is_lexicographic: false,
        }
    }
    
    /// Build a list length measure
    pub fn list_length_measure(&mut self, param: &str) -> TerminationMeasure {
        let var = self.graph.add_node(Node::Variable { name: param.to_string() });
        let length_fn = self.graph.add_node(Node::Variable { name: "length".to_string() });
        let measure = self.graph.add_node(Node::Application {
            function: length_fn,
            args: vec![var],
        });
        
        TerminationMeasure {
            measure_expr: measure,
            depends_on: vec![param.to_string()],
            is_lexicographic: false,
        }
    }
    
    /// Build a lexicographic measure for multiple parameters
    pub fn lexicographic_measure(&mut self, params: Vec<(&str, NodeId)>) -> TerminationMeasure {
        let list_items: Vec<NodeId> = params.iter().map(|(_, expr)| *expr).collect();
        let measure = self.graph.add_node(Node::List(list_items));
        
        TerminationMeasure {
            measure_expr: measure,
            depends_on: params.into_iter().map(|(p, _)| p.to_string()).collect(),
            is_lexicographic: true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::num::NonZeroU32;
    
    #[test]
    fn test_non_recursive_termination() {
        let graph = Graph::new();
        let mut checker = TerminationChecker::new(&graph);
        
        let contract = Contract::new(
            "add".to_string(),
            NodeId::new(NonZeroU32::new(1).unwrap())
        );
        
        let result = checker.analyze_contract(&contract);
        assert!(matches!(result, Ok(TerminationResult::Terminates { .. })));
    }
    
    #[test]
    fn test_termination_measure_builder() {
        let mut graph = Graph::new();
        let mut builder = TerminationMeasureBuilder::new(&mut graph);
        
        let measure = builder.numeric_measure("n");
        assert_eq!(measure.depends_on, vec!["n"]);
        assert!(!measure.is_lexicographic);
        
        let list_measure = builder.list_length_measure("lst");
        assert_eq!(list_measure.depends_on, vec!["lst"]);
    }
}