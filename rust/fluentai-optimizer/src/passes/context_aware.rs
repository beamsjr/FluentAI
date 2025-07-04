//! Context-aware optimization pass that uses ContextMemory

use crate::passes::OptimizationPass;
use fluentai_core::ast::{Graph, Node, NodeId, PerformanceHint, PerformanceHintType};
use anyhow::Result;
use std::collections::HashSet;
use rustc_hash::FxHashMap;

/// Context-aware optimization pass
/// 
/// This pass uses information from ContextMemory (usage statistics, performance hints, etc.)
/// to make better optimization decisions.
pub struct ContextAwarePass;

impl ContextAwarePass {
    pub fn new() -> Self {
        Self
    }
    
    /// Check if a function should be inlined based on context
    fn should_inline(&self, graph: &Graph, node_id: NodeId) -> bool {
        if let Some(context) = graph.get_context_memory(node_id) {
            // Check performance hints
            for hint in &context.performance_hints {
                if matches!(hint.hint_type, fluentai_core::ast::PerformanceHintType::ShouldInline) {
                    return hint.confidence > 0.7;
                }
            }
            
            // Check usage statistics
            let stats = &context.usage_stats;
            
            // Inline hot functions that are small
            if stats.is_hot_path && stats.execution_count > 1000 {
                // Estimate function size (would need better heuristics)
                if let Some(node) = graph.get_node(node_id) {
                    if let Node::Lambda { body, .. } = node {
                        let size = self.estimate_node_size(graph, *body);
                        return size < 50; // Inline small hot functions
                    }
                }
            }
            
            // Don't inline functions with high error rates
            if stats.error_count > 0 && stats.execution_count > 0 {
                let error_rate = stats.error_count as f64 / stats.execution_count as f64;
                if error_rate > 0.1 {
                    return false;
                }
            }
        }
        
        false
    }
    
    /// Check if a loop should be unrolled based on context
    fn should_unroll(&self, graph: &Graph, node_id: NodeId) -> bool {
        if let Some(context) = graph.get_context_memory(node_id) {
            // Check performance hints
            for hint in &context.performance_hints {
                if matches!(hint.hint_type, fluentai_core::ast::PerformanceHintType::ShouldUnroll) {
                    return hint.confidence > 0.7;
                }
            }
            
            // Check if it's a hot loop with predictable iterations
            let stats = &context.usage_stats;
            if stats.is_hot_path {
                // Look for semantic tags that indicate fixed iterations
                if context.semantic_tags.contains(&"fixed-iterations".to_string()) {
                    return true;
                }
            }
        }
        
        false
    }
    
    /// Check if operations can be vectorized based on context
    fn can_vectorize(&self, graph: &Graph, node_id: NodeId) -> bool {
        if let Some(context) = graph.get_context_memory(node_id) {
            // Check performance hints
            for hint in &context.performance_hints {
                if matches!(hint.hint_type, fluentai_core::ast::PerformanceHintType::CanVectorize) {
                    return hint.confidence > 0.8;
                }
            }
            
            // Check semantic tags
            if context.semantic_tags.contains(&"simd-compatible".to_string()) {
                return true;
            }
        }
        
        false
    }
    
    /// Estimate the size of a node subtree
    fn estimate_node_size(&self, graph: &Graph, node_id: NodeId) -> usize {
        let mut size = 0;
        let mut visited = HashSet::new();
        self.count_nodes(graph, node_id, &mut visited, &mut size);
        size
    }
    
    fn count_nodes(&self, graph: &Graph, node_id: NodeId, visited: &mut HashSet<NodeId>, size: &mut usize) {
        if !visited.insert(node_id) {
            return;
        }
        
        *size += 1;
        
        if let Some(node) = graph.get_node(node_id) {
            match node {
                Node::Application { function, args } => {
                    self.count_nodes(graph, *function, visited, size);
                    for arg in args {
                        self.count_nodes(graph, *arg, visited, size);
                    }
                }
                Node::Lambda { params: _, body } => {
                    self.count_nodes(graph, *body, visited, size);
                }
                Node::If { condition, then_branch, else_branch } => {
                    self.count_nodes(graph, *condition, visited, size);
                    self.count_nodes(graph, *then_branch, visited, size);
                    self.count_nodes(graph, *else_branch, visited, size);
                }
                Node::Let { bindings, body } | Node::Letrec { bindings, body } => {
                    for (_, value) in bindings {
                        self.count_nodes(graph, *value, visited, size);
                    }
                    self.count_nodes(graph, *body, visited, size);
                }
                _ => {}
            }
        }
    }
    
    /// Apply context-aware optimizations to a specific node
    fn optimize_node(&self, graph: &mut Graph, node_id: NodeId) -> bool {
        let mut changed = false;
        
        if let Some(node) = graph.get_node(node_id).cloned() {
            match &node {
                Node::Lambda { .. } => {
                    if self.should_inline(graph, node_id) {
                        // Mark for inlining by adding metadata
                        let metadata = graph.metadata_mut(node_id);
                        if let Some(context) = &mut metadata.context_memory {
                            context.performance_hints.push(PerformanceHint {
                                hint_type: PerformanceHintType::ShouldInline,
                                confidence: 0.9,
                                context: Some("Function is small and frequently called".to_string()),
                            });
                            changed = true;
                        } else {
                            metadata.context_memory = Some(fluentai_core::ast::ContextMemory {
                                embedding_id: None,
                                usage_stats: Default::default(),
                                rationale: None,
                                performance_hints: vec![PerformanceHint {
                                    hint_type: PerformanceHintType::ShouldInline,
                                    confidence: 0.9,
                                    context: Some("Function is small and frequently called".to_string()),
                                }],
                                semantic_tags: vec![],
                                last_modified: None,
                            });
                            changed = true;
                        }
                    }
                }
                Node::Application { function, .. } => {
                    // Check if this is a map/filter that can be vectorized
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        if (name == "map" || name == "filter") && self.can_vectorize(graph, node_id) {
                            // Mark for vectorization
                            let metadata = graph.metadata_mut(node_id);
                            if let Some(context) = &mut metadata.context_memory {
                                context.performance_hints.push(PerformanceHint {
                                    hint_type: PerformanceHintType::CanVectorize,
                                    confidence: 0.85,
                                    context: Some("Array operation can be vectorized".to_string()),
                                });
                                changed = true;
                            } else {
                                metadata.context_memory = Some(fluentai_core::ast::ContextMemory {
                                    embedding_id: None,
                                    usage_stats: Default::default(),
                                    rationale: None,
                                    performance_hints: vec![PerformanceHint {
                                        hint_type: PerformanceHintType::CanVectorize,
                                        confidence: 0.85,
                                        context: Some("Array operation can be vectorized".to_string()),
                                    }],
                                    semantic_tags: vec![],
                                    last_modified: None,
                                });
                                changed = true;
                            }
                        }
                    }
                }
                _ => {}
            }
        }
        
        changed
    }
}

impl OptimizationPass for ContextAwarePass {
    fn name(&self) -> &str {
        "context-aware"
    }
    
    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        
        // First pass: Copy all nodes and apply optimizations
        for (node_id, node) in &graph.nodes {
            let new_node = node.clone();
            let new_id = optimized.add_node(new_node);
            node_mapping.insert(*node_id, new_id);
            
            // Copy metadata
            if let Some(metadata) = graph.get_metadata(*node_id) {
                optimized.set_metadata(new_id, metadata.clone());
            }
            
            // Copy context memory
            if let Some(context) = graph.get_context_memory(*node_id) {
                optimized.set_context_memory(new_id, context.clone());
            }
        }
        
        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }
        
        // Second pass: Apply context-aware optimizations
        let node_ids: Vec<_> = optimized.nodes.keys().cloned().collect();
        for node_id in node_ids {
            self.optimize_node(&mut optimized, node_id);
        }
        
        // Third pass: Update semantic tags based on optimizations applied
        for (node_id, _) in optimized.nodes.clone() {
            if let Some(context_memory) = optimized.get_context_memory(node_id) {
                if context_memory.performance_hints.iter().any(|h| matches!(h.hint_type, PerformanceHintType::CanVectorize)) {
                    // Update context memory to reflect vectorization
                    if let Some(mut context) = optimized.get_context_memory(node_id).cloned() {
                        if !context.semantic_tags.contains(&"vectorized".to_string()) {
                            context.semantic_tags.push("vectorized".to_string());
                            optimized.set_context_memory(node_id, context);
                        }
                    }
                }
            }
        }
        
        Ok(optimized)
    }
}