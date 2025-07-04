//! Incremental verification support
//! 
//! This module provides functionality to track dependencies between contracts
//! and functions, enabling incremental re-verification only when dependencies change.

use std::collections::{HashMap, HashSet};
use rustc_hash::{FxHashMap, FxHashSet};
use fluentai_core::ast::{Graph, Node, NodeId};
use crate::{
    contract::Contract,
    errors::{ContractError, ContractResult},
    static_verification::{StaticVerifier, VerificationResult},
};

/// Tracks dependencies and manages incremental verification
pub struct IncrementalVerifier<'a> {
    /// The AST graph
    graph: &'a Graph,
    
    /// Function dependency graph: function -> functions it calls
    function_deps: FxHashMap<String, HashSet<String>>,
    
    /// Reverse dependency graph: function -> functions that call it
    reverse_deps: FxHashMap<String, HashSet<String>>,
    
    /// Contract dependency graph: contract -> functions it references
    contract_deps: FxHashMap<String, HashSet<String>>,
    
    /// Function to contracts mapping
    function_contracts: FxHashMap<String, HashSet<String>>,
    
    /// Hash of function bodies for change detection
    function_hashes: FxHashMap<String, u64>,
    
    /// Cached verification results
    verification_cache: FxHashMap<String, VerificationResult>,
    
    /// Functions that have changed since last verification
    changed_functions: HashSet<String>,
}

impl<'a> IncrementalVerifier<'a> {
    /// Create a new incremental verifier
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            function_deps: FxHashMap::default(),
            reverse_deps: FxHashMap::default(),
            contract_deps: FxHashMap::default(),
            function_contracts: FxHashMap::default(),
            function_hashes: FxHashMap::default(),
            verification_cache: FxHashMap::default(),
            changed_functions: HashSet::new(),
        }
    }
    
    /// Build dependency graph from the AST
    pub fn build_dependencies(&mut self, contracts: &HashMap<String, Contract>) -> ContractResult<()> {
        // First, analyze all function definitions
        self.analyze_functions()?;
        
        // Then, analyze contracts
        for (name, contract) in contracts {
            self.analyze_contract(name, contract)?;
        }
        
        Ok(())
    }
    
    /// Analyze function definitions to build dependency graph
    fn analyze_functions(&mut self) -> ContractResult<()> {
        // In a real implementation, this would traverse the AST to find:
        // 1. All function definitions
        // 2. Function calls within each function
        // 3. Build the dependency graph
        
        // For now, we'll implement a simplified version
        // that demonstrates the concept
        
        Ok(())
    }
    
    /// Analyze a contract to find function dependencies
    fn analyze_contract(&mut self, name: &str, contract: &Contract) -> ContractResult<()> {
        let mut deps = HashSet::new();
        
        // Analyze preconditions
        for condition in &contract.preconditions {
            self.collect_function_refs(condition.expression, &mut deps)?;
        }
        
        // Analyze postconditions
        for condition in &contract.postconditions {
            self.collect_function_refs(condition.expression, &mut deps)?;
        }
        
        // Analyze invariants
        for condition in &contract.invariants {
            self.collect_function_refs(condition.expression, &mut deps)?;
        }
        
        self.contract_deps.insert(name.to_string(), deps.clone());
        
        // Update function to contracts mapping
        let func_name = &contract.function_name;
        self.function_contracts
            .entry(func_name.clone())
            .or_insert_with(HashSet::new)
            .insert(name.to_string());
        
        Ok(())
    }
    
    /// Collect function references from an expression
    fn collect_function_refs(&self, node_id: NodeId, refs: &mut HashSet<String>) -> ContractResult<()> {
        let node = self.graph.get_node(node_id)
            .ok_or_else(|| ContractError::VerificationError(
                format!("Node {:?} not found", node_id)
            ))?;
        
        match node {
            Node::Variable { name } => {
                // Check if this is a function reference
                // In a real implementation, we'd need symbol resolution
                refs.insert(name.clone());
            }
            Node::Application { function, args } => {
                self.collect_function_refs(*function, refs)?;
                for arg in args {
                    self.collect_function_refs(*arg, refs)?;
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                self.collect_function_refs(*condition, refs)?;
                self.collect_function_refs(*then_branch, refs)?;
                self.collect_function_refs(*else_branch, refs)?;
            }
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    self.collect_function_refs(*value, refs)?;
                }
                self.collect_function_refs(*body, refs)?;
            }
            Node::Lambda { body, .. } => {
                self.collect_function_refs(*body, refs)?;
            }
            Node::List(elements) => {
                for elem in elements {
                    self.collect_function_refs(*elem, refs)?;
                }
            }
            Node::Match { expr, branches } => {
                self.collect_function_refs(*expr, refs)?;
                for (_, body) in branches {
                    self.collect_function_refs(*body, refs)?;
                }
            }
            _ => {} // Other node types don't contain function references
        }
        
        Ok(())
    }
    
    /// Mark a function as changed
    pub fn mark_function_changed(&mut self, function_name: &str) {
        self.changed_functions.insert(function_name.to_string());
        
        // Transitively mark all dependent functions as potentially affected
        if let Some(dependents) = self.reverse_deps.get(function_name) {
            for dep in dependents.clone() {
                self.changed_functions.insert(dep);
            }
        }
    }
    
    /// Get contracts that need re-verification
    pub fn get_contracts_to_verify(&self) -> HashSet<String> {
        let mut contracts_to_verify = HashSet::new();
        
        // Direct contracts of changed functions
        for func in &self.changed_functions {
            if let Some(contracts) = self.function_contracts.get(func) {
                contracts_to_verify.extend(contracts.clone());
            }
        }
        
        // Contracts that depend on changed functions
        for (contract_name, deps) in &self.contract_deps {
            if deps.intersection(&self.changed_functions).next().is_some() {
                contracts_to_verify.insert(contract_name.clone());
            }
        }
        
        contracts_to_verify
    }
    
    /// Verify only the contracts that need re-verification
    pub fn verify_incremental(
        &mut self,
        contracts: &HashMap<String, Contract>,
        static_verifier: &mut StaticVerifier,
    ) -> ContractResult<HashMap<String, VerificationResult>> {
        let contracts_to_verify = self.get_contracts_to_verify();
        let mut results = HashMap::new();
        
        // Copy cached results for unchanged contracts
        for (name, result) in &self.verification_cache {
            if !contracts_to_verify.contains(name.as_str()) {
                results.insert(name.clone(), result.clone());
            }
        }
        
        // Verify changed contracts
        for contract_name in contracts_to_verify {
            if let Some(contract) = contracts.get(&contract_name) {
                let result = static_verifier.verify_contract(contract)?;
                
                // Update cache
                self.verification_cache.insert(contract_name.clone(), result.clone());
                results.insert(contract_name.clone(), result);
            }
        }
        
        // Clear changed functions after verification
        self.changed_functions.clear();
        
        Ok(results)
    }
    
    /// Update function hash for change detection
    pub fn update_function_hash(&mut self, function_name: &str, body_node: NodeId) {
        // Compute hash of the function body
        // In a real implementation, this would be a proper AST hash
        let hash = self.compute_node_hash(body_node);
        
        if let Some(&old_hash) = self.function_hashes.get(function_name) {
            if old_hash != hash {
                self.mark_function_changed(function_name);
            }
        }
        
        self.function_hashes.insert(function_name.to_string(), hash);
    }
    
    /// Compute hash of an AST node (simplified)
    fn compute_node_hash(&self, node_id: NodeId) -> u64 {
        use std::hash::{Hash, Hasher};
        use std::collections::hash_map::DefaultHasher;
        
        let mut hasher = DefaultHasher::new();
        node_id.hash(&mut hasher);
        
        // In a real implementation, we'd recursively hash the entire subtree
        hasher.finish()
    }
    
    /// Get verification statistics
    pub fn get_stats(&self) -> IncrementalStats {
        IncrementalStats {
            total_functions: self.function_deps.len(),
            total_contracts: self.contract_deps.len(),
            changed_functions: self.changed_functions.len(),
            cached_results: self.verification_cache.len(),
        }
    }
}

/// Statistics about incremental verification
#[derive(Debug, Clone)]
pub struct IncrementalStats {
    pub total_functions: usize,
    pub total_contracts: usize,
    pub changed_functions: usize,
    pub cached_results: usize,
}

/// Dependency analyzer for more detailed analysis
pub struct DependencyAnalyzer<'a> {
    graph: &'a Graph,
    visited: HashSet<NodeId>,
}

impl<'a> DependencyAnalyzer<'a> {
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            visited: HashSet::new(),
        }
    }
    
    /// Find all functions called by a given function
    pub fn analyze_function_calls(&mut self, function_body: NodeId) -> HashSet<String> {
        self.visited.clear();
        let mut calls = HashSet::new();
        self.collect_calls(function_body, &mut calls);
        calls
    }
    
    fn collect_calls(&mut self, node_id: NodeId, calls: &mut HashSet<String>) {
        if self.visited.contains(&node_id) {
            return;
        }
        self.visited.insert(node_id);
        
        if let Some(node) = self.graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    // Check if this is a direct function call
                    if let Some(Node::Variable { name }) = self.graph.get_node(*function) {
                        calls.insert(name.clone());
                    }
                    self.collect_calls(*function, calls);
                    for arg in args {
                        self.collect_calls(*arg, calls);
                    }
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.collect_calls(*condition, calls);
                    self.collect_calls(*then_branch, calls);
                    self.collect_calls(*else_branch, calls);
                }
                Node::Let { bindings, body } => {
                    for (_, value) in bindings {
                        self.collect_calls(*value, calls);
                    }
                    self.collect_calls(*body, calls);
                }
                Node::Lambda { body, .. } => {
                    self.collect_calls(*body, calls);
                }
                Node::List(elements) => {
                    for elem in elements {
                        self.collect_calls(*elem, calls);
                    }
                }
                Node::Match { expr, branches } => {
                    self.collect_calls(*expr, calls);
                    for (_, body) in branches {
                        self.collect_calls(*body, calls);
                    }
                }
                _ => {}
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_incremental_verifier() {
        let graph = Graph::new();
        let mut verifier = IncrementalVerifier::new(&graph);
        
        // Mark a function as changed
        verifier.mark_function_changed("foo");
        assert!(verifier.changed_functions.contains("foo"));
        
        // Test stats
        let stats = verifier.get_stats();
        assert_eq!(stats.changed_functions, 1);
    }
}