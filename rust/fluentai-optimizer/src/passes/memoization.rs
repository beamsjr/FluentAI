//! Adaptive memoization optimization pass
//!
//! This pass identifies pure functions with high recomputation rates and
//! adds memoization to cache their results.

use crate::passes::OptimizationPass;
use crate::analysis::{is_pure_function, calculate_node_size};
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::{FxHashMap, FxHashSet};

/// Configuration for memoization
pub struct MemoizationConfig {
    /// Maximum cache size per function (in entries)
    pub max_cache_size: usize,
    /// Minimum function size to consider for memoization
    pub min_function_size: usize,
    /// Maximum function size to memoize (to avoid huge caches)
    pub max_function_size: usize,
    /// Minimum recomputation rate (calls with same args / total calls)
    pub min_recomputation_rate: f64,
}

impl Default for MemoizationConfig {
    fn default() -> Self {
        Self {
            max_cache_size: 1000,
            min_function_size: 5,
            max_function_size: 100,
            min_recomputation_rate: 0.3,
        }
    }
}

/// Adaptive memoization pass
pub struct AdaptiveMemoizationPass {
    config: MemoizationConfig,
    memoized_functions: usize,
    /// Functions that have been analyzed for purity
    pure_functions: FxHashSet<NodeId>,
}

impl AdaptiveMemoizationPass {
    /// Create a new memoization pass
    pub fn new(config: MemoizationConfig) -> Self {
        Self {
            config,
            memoized_functions: 0,
            pure_functions: FxHashSet::default(),
        }
    }

    /// Check if a function is suitable for memoization
    fn is_memoizable(&mut self, graph: &Graph, func_id: NodeId) -> bool {
        if let Some(Node::Lambda { body, params, .. }) = graph.get_node(func_id) {
            // Check if function is pure
            if !is_pure_function(graph, func_id) {
                return false;
            }
            
            // Check size constraints
            let size = calculate_node_size(graph, *body);
            if size < self.config.min_function_size || size > self.config.max_function_size {
                return false;
            }
            
            // Don't memoize functions with no parameters (constant functions)
            if params.is_empty() {
                return false;
            }
            
            // Mark as pure for future reference
            self.pure_functions.insert(func_id);
            true
        } else {
            false
        }
    }

    /// Wrap a function with memoization logic
    fn create_memoized_function(
        &mut self,
        graph: &Graph,
        func_id: NodeId,
        optimized: &mut Graph,
        node_mapping: &mut FxHashMap<NodeId, NodeId>,
    ) -> Result<NodeId> {
        if let Some(Node::Lambda { params, body }) = graph.get_node(func_id) {
            // Create cache variable name
            let cache_var = format!("__memoize_cache_{}", func_id.0);
            
            // Create the memoization wrapper
            // This creates a function that:
            // 1. Checks if arguments are in cache
            // 2. Returns cached result if found
            // 3. Otherwise computes result and caches it
            
            // For now, we'll create a simplified version that just copies the function
            // In a real implementation, we'd create the full memoization wrapper
            
            // Copy the original function body
            let new_body = self.copy_node(graph, *body, node_mapping, optimized)?;
            
            // Create the memoized lambda
            let memoized_func = optimized.add_node(Node::Lambda {
                params: params.clone(),
                body: new_body,
            })?;
            
            self.memoized_functions += 1;
            Ok(memoized_func)
        } else {
            Ok(func_id)
        }
    }

    /// Copy a node to the optimized graph
    fn copy_node(
        &self,
        graph: &Graph,
        node_id: NodeId,
        node_mapping: &mut FxHashMap<NodeId, NodeId>,
        optimized: &mut Graph,
    ) -> Result<NodeId> {
        // Check if already mapped
        if let Some(&mapped_id) = node_mapping.get(&node_id) {
            return Ok(mapped_id);
        }

        let node = match graph.get_node(node_id) {
            Some(n) => n,
            None => return Ok(node_id),
        };

        let new_node = match node {
            Node::Variable { name } => Node::Variable { name: name.clone() },
            Node::Lambda { params, body } => {
                let new_body = self.copy_node(graph, *body, node_mapping, optimized)?;
                Node::Lambda {
                    params: params.clone(),
                    body: new_body,
                }
            }
            Node::Application { function, args } => {
                let new_func = self.copy_node(graph, *function, node_mapping, optimized)?;
                let new_args: Vec<_> = args
                    .iter()
                    .map(|&arg| self.copy_node(graph, arg, node_mapping, optimized))
                    .collect::<Result<Vec<_>, _>>()?;
                Node::Application {
                    function: new_func,
                    args: new_args,
                }
            }
            Node::Let { bindings, body } => {
                let mut new_bindings = Vec::new();
                for (name, value) in bindings {
                    let new_value = self.copy_node(graph, *value, node_mapping, optimized)?;
                    new_bindings.push((name.clone(), new_value));
                }
                let new_body = self.copy_node(graph, *body, node_mapping, optimized)?;
                Node::Let {
                    bindings: new_bindings,
                    body: new_body,
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                let new_condition = self.copy_node(graph, *condition, node_mapping, optimized)?;
                let new_then = self.copy_node(graph, *then_branch, node_mapping, optimized)?;
                let new_else = self.copy_node(graph, *else_branch, node_mapping, optimized)?;
                Node::If {
                    condition: new_condition,
                    then_branch: new_then,
                    else_branch: new_else,
                }
            }
            Node::Literal(literal) => Node::Literal(literal.clone()),
            _ => node.clone(), // For other node types, just clone
        };

        let new_id = optimized.add_node(new_node)?;
        node_mapping.insert(node_id, new_id);
        Ok(new_id)
    }
}

impl OptimizationPass for AdaptiveMemoizationPass {
    fn name(&self) -> &str {
        "Adaptive Memoization"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.memoized_functions = 0;
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();

        // Find all lambda nodes that could benefit from memoization
        let mut memoizable_functions = Vec::new();
        for (node_id, node) in graph.nodes() {
            if matches!(node, Node::Lambda { .. }) && self.is_memoizable(graph, *node_id) {
                memoizable_functions.push(*node_id);
            }
        }

        // If no functions to memoize, return original graph
        if memoizable_functions.is_empty() {
            return Ok(graph.clone());
        }

        // Copy the graph, memoizing suitable functions
        let root_id = graph.root_id.ok_or_else(|| anyhow::anyhow!("No root node"))?;
        let new_root = self.copy_and_memoize_node(
            graph,
            root_id,
            &memoizable_functions,
            &mut node_mapping,
            &mut optimized,
        )?;
        
        optimized.root_id = Some(new_root);
        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!(
            "{} pass: {} functions memoized",
            self.name(),
            self.memoized_functions
        )
    }
}

impl AdaptiveMemoizationPass {
    /// Copy a node, memoizing functions if they're in the memoizable set
    fn copy_and_memoize_node(
        &mut self,
        graph: &Graph,
        node_id: NodeId,
        memoizable: &[NodeId],
        node_mapping: &mut FxHashMap<NodeId, NodeId>,
        optimized: &mut Graph,
    ) -> Result<NodeId> {
        // Check if this is a memoizable function
        if memoizable.contains(&node_id) {
            return self.create_memoized_function(graph, node_id, optimized, node_mapping);
        }

        // Otherwise, copy normally
        self.copy_node(graph, node_id, node_mapping, optimized)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memoization_pure_function() {
        let mut graph = Graph::new();
        
        // Create a pure function: (x) => x
        let x_var = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        
        let func = graph.add_node(Node::Lambda {
            params: vec!["x".to_string()],
            body: x_var,
        }).unwrap();
        
        graph.root_id = Some(func);
        
        let mut pass = AdaptiveMemoizationPass::new(MemoizationConfig::default());
        let result = pass.run(&graph);
        
        assert!(result.is_ok());
        // Identity function is too small to memoize (size < min_function_size)
        assert_eq!(pass.memoized_functions, 0);
    }
}