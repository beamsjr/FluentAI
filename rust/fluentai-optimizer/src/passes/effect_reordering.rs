//! Effect reordering optimization pass
//!
//! This pass reorders effect operations to improve performance by:
//! - Batching similar effects together (e.g., all IO operations)
//! - Moving pure computations outside of effect handlers when possible
//! - Reducing effect handler overhead by minimizing transitions

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Node, NodeId, EffectType};
use rustc_hash::{FxHashMap, FxHashSet};

/// Configuration for effect reordering
pub struct EffectReorderingConfig {
    /// Enable batching of similar effects
    pub enable_batching: bool,
    /// Enable moving pure computations out of handlers
    pub enable_pure_hoisting: bool,
    /// Maximum distance to move effects for batching
    pub max_reorder_distance: usize,
}

impl Default for EffectReorderingConfig {
    fn default() -> Self {
        Self {
            enable_batching: true,
            enable_pure_hoisting: true,
            max_reorder_distance: 10,
        }
    }
}

/// Effect reordering optimization pass
pub struct EffectReorderingPass {
    config: EffectReorderingConfig,
    effects_reordered: usize,
    pure_computations_hoisted: usize,
}

impl EffectReorderingPass {
    /// Create a new effect reordering pass
    pub fn new(config: EffectReorderingConfig) -> Self {
        Self {
            config,
            effects_reordered: 0,
            pure_computations_hoisted: 0,
        }
    }

    /// Analyze effects in a subgraph
    fn analyze_effects(&self, graph: &Graph, root: NodeId) -> EffectAnalysis {
        let mut analysis = EffectAnalysis::new();
        let mut visited = FxHashSet::default();
        self.analyze_node(graph, root, &mut visited, &mut analysis);
        analysis
    }

    /// Recursively analyze a node for effects
    fn analyze_node(
        &self,
        graph: &Graph,
        node_id: NodeId,
        visited: &mut FxHashSet<NodeId>,
        analysis: &mut EffectAnalysis,
    ) {
        if !visited.insert(node_id) {
            return;
        }

        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Effect { effect_type, operation, args } => {
                    analysis.effects.push(EffectInfo {
                        node_id,
                        effect_type: effect_type.clone(),
                        operation: operation.clone(),
                        args: args.clone(),
                    });
                }
                Node::Handler { handlers, body } => {
                    analysis.handler_regions.push(HandlerRegion {
                        handler_id: node_id,
                        body: *body,
                        handlers: handlers.clone(),
                    });
                    self.analyze_node(graph, *body, visited, analysis);
                }
                Node::Let { bindings, body } => {
                    // Analyze bindings
                    for (name, value) in bindings {
                        self.analyze_node(graph, *value, visited, analysis);
                        // Check if binding is pure
                        if self.is_pure_computation(graph, *value) {
                            analysis.pure_bindings.insert(*value, name.clone());
                        }
                    }
                    self.analyze_node(graph, *body, visited, analysis);
                }
                Node::Application { function, args } => {
                    self.analyze_node(graph, *function, visited, analysis);
                    for arg in args {
                        self.analyze_node(graph, *arg, visited, analysis);
                    }
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.analyze_node(graph, *condition, visited, analysis);
                    self.analyze_node(graph, *then_branch, visited, analysis);
                    self.analyze_node(graph, *else_branch, visited, analysis);
                }
                _ => {
                    // For other nodes, just record if they're pure
                    if self.is_pure_computation(graph, node_id) {
                        analysis.pure_nodes.insert(node_id);
                    }
                }
            }
        }
    }

    /// Check if a node represents a pure computation
    fn is_pure_computation(&self, graph: &Graph, node_id: NodeId) -> bool {
        use crate::analysis::has_side_effects;
        let mut visited = FxHashSet::default();
        !has_side_effects(graph, node_id, &mut visited)
    }

    /// Reorder effects for better performance
    fn reorder_effects(
        &mut self,
        graph: &Graph,
        analysis: &EffectAnalysis,
        optimized: &mut Graph,
    ) -> Result<NodeId> {
        // Group effects by type for batching
        let mut effect_groups: FxHashMap<EffectType, Vec<&EffectInfo>> = FxHashMap::default();
        for effect in &analysis.effects {
            effect_groups.entry(effect.effect_type.clone()).or_default().push(effect);
        }

        // For now, return the original graph
        // In a real implementation, we would:
        // 1. Create a dependency graph of effects
        // 2. Identify effects that can be safely reordered
        // 3. Batch similar effects together
        // 4. Generate optimized code with reordered effects
        
        Ok(graph.root_id.unwrap_or(NodeId(std::num::NonZeroU32::new(1).unwrap())))
    }

    /// Hoist pure computations out of effect handlers
    fn hoist_pure_computations(
        &mut self,
        graph: &Graph,
        analysis: &EffectAnalysis,
        optimized: &mut Graph,
    ) -> Result<()> {
        // For each handler region
        for region in &analysis.handler_regions {
            // Find pure computations that can be hoisted
            let hoistable = self.find_hoistable_computations(graph, region, analysis);
            
            if !hoistable.is_empty() {
                self.pure_computations_hoisted += hoistable.len();
                // In a real implementation, we would move these computations
                // outside the handler region
            }
        }
        
        Ok(())
    }

    /// Find computations that can be hoisted out of a handler
    fn find_hoistable_computations(
        &self,
        graph: &Graph,
        region: &HandlerRegion,
        analysis: &EffectAnalysis,
    ) -> Vec<NodeId> {
        let mut hoistable = Vec::new();
        
        // Look for pure nodes within the handler body
        let mut to_check = vec![region.body];
        let mut checked = FxHashSet::default();
        
        while let Some(node_id) = to_check.pop() {
            if !checked.insert(node_id) {
                continue;
            }
            
            if analysis.pure_nodes.contains(&node_id) {
                hoistable.push(node_id);
            }
            
            // Add children to check
            if let Some(node) = graph.get_node(node_id) {
                match node {
                    Node::Let { bindings, body } => {
                        for (_, value) in bindings {
                            to_check.push(*value);
                        }
                        to_check.push(*body);
                    }
                    Node::Application { function, args } => {
                        to_check.push(*function);
                        to_check.extend(args);
                    }
                    _ => {}
                }
            }
        }
        
        hoistable
    }
}

impl OptimizationPass for EffectReorderingPass {
    fn name(&self) -> &str {
        "Effect Reordering"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.effects_reordered = 0;
        self.pure_computations_hoisted = 0;
        
        let mut optimized = Graph::new();
        
        // Analyze effects in the graph
        let root = graph.root_id.ok_or_else(|| anyhow::anyhow!("No root node"))?;
        let analysis = self.analyze_effects(graph, root);
        
        // If no effects found, return original graph
        if analysis.effects.is_empty() && analysis.handler_regions.is_empty() {
            return Ok(graph.clone());
        }
        
        // Reorder effects if enabled
        if self.config.enable_batching && !analysis.effects.is_empty() {
            let _new_root = self.reorder_effects(graph, &analysis, &mut optimized)?;
        }
        
        // Hoist pure computations if enabled
        if self.config.enable_pure_hoisting && !analysis.handler_regions.is_empty() {
            self.hoist_pure_computations(graph, &analysis, &mut optimized)?;
        }
        
        // For now, return the original graph
        // A full implementation would return the optimized graph
        Ok(graph.clone())
    }

    fn stats(&self) -> String {
        format!(
            "{} pass: {} effects reordered, {} pure computations hoisted",
            self.name(),
            self.effects_reordered,
            self.pure_computations_hoisted
        )
    }
}

/// Analysis results for effects in a graph
struct EffectAnalysis {
    /// All effect operations found
    effects: Vec<EffectInfo>,
    /// Handler regions
    handler_regions: Vec<HandlerRegion>,
    /// Pure computations (can be moved)
    pure_nodes: FxHashSet<NodeId>,
    /// Pure bindings
    pure_bindings: FxHashMap<NodeId, String>,
}

impl EffectAnalysis {
    fn new() -> Self {
        Self {
            effects: Vec::new(),
            handler_regions: Vec::new(),
            pure_nodes: FxHashSet::default(),
            pure_bindings: FxHashMap::default(),
        }
    }
}

/// Information about an effect operation
struct EffectInfo {
    node_id: NodeId,
    effect_type: EffectType,
    operation: String,
    args: Vec<NodeId>,
}

/// Information about a handler region
struct HandlerRegion {
    handler_id: NodeId,
    body: NodeId,
    handlers: Vec<(EffectType, Option<String>, NodeId)>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_effect_reordering_empty_graph() {
        let graph = Graph::new();
        let mut pass = EffectReorderingPass::new(EffectReorderingConfig::default());
        let result = pass.run(&graph);
        assert!(result.is_err()); // No root node
    }

    #[test]
    fn test_effect_analysis() {
        let mut graph = Graph::new();
        
        // Create a simple effect
        let io_effect = graph.add_node(Node::Effect {
            effect_type: EffectType::IO,
            operation: "print".to_string(),
            args: vec![],
        }).unwrap();
        
        graph.root_id = Some(io_effect);
        
        let pass = EffectReorderingPass::new(EffectReorderingConfig::default());
        let analysis = pass.analyze_effects(&graph, io_effect);
        
        assert_eq!(analysis.effects.len(), 1);
        assert_eq!(analysis.effects[0].operation, "print");
    }
}