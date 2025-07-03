//! Common subexpression elimination pass

use claudelang_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashMap;
use anyhow::Result;
use crate::passes::OptimizationPass;
use crate::analysis::EffectAnalysis;

/// Common subexpression elimination pass
pub struct CommonSubexpressionEliminationPass {
    eliminated_count: usize,
}

impl CommonSubexpressionEliminationPass {
    /// Create new CSE pass
    pub fn new() -> Self {
        Self { eliminated_count: 0 }
    }

    /// Generate a canonical key for a node
    fn node_key(&self, node: &Node, graph: &Graph) -> String {
        match node {
            Node::Literal(lit) => format!("lit:{:?}", lit),
            Node::Variable { name } => format!("var:{}", name),
            Node::Application { function, args } => {
                // Include function name for better matching
                let func_key = if let Some(Node::Variable { name }) = graph.get_node(*function) {
                    name.clone()
                } else {
                    format!("{:?}", function)
                };
                format!("app:{}:{:?}", func_key, args)
            }
            Node::List(items) => format!("list:{:?}", items),
            _ => format!("{:?}", node),
        }
    }

    /// Check if nodes are structurally equal
    fn nodes_equal(&self, node1: &Node, node2: &Node) -> bool {
        match (node1, node2) {
            (Node::Literal(l1), Node::Literal(l2)) => l1 == l2,
            (Node::Variable { name: n1 }, Node::Variable { name: n2 }) => n1 == n2,
            (Node::Application { function: f1, args: a1 }, 
             Node::Application { function: f2, args: a2 }) => f1 == f2 && a1 == a2,
            (Node::List(i1), Node::List(i2)) => i1 == i2,
            _ => false,
        }
    }
}

impl OptimizationPass for CommonSubexpressionEliminationPass {
    fn name(&self) -> &str {
        "Common Subexpression Elimination"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.eliminated_count = 0;
        
        // Perform effect analysis
        let effect_analysis = EffectAnalysis::analyze(graph);
        
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();
        let mut expr_cache: FxHashMap<String, NodeId> = FxHashMap::default();

        // Process nodes in some order
        let nodes: Vec<_> = graph.nodes.iter().collect();
        
        for (node_id, node) in nodes {
            // Only eliminate pure expressions
            if effect_analysis.pure_nodes.contains(node_id) {
                let key = self.node_key(node, graph);
                
                // Check if we've seen this expression before
                if let Some(&existing_id) = expr_cache.get(&key) {
                    // Verify they're really equal
                    if let Some(existing_node) = optimized.get_node(existing_id) {
                        if self.nodes_equal(node, existing_node) {
                            // Reuse existing node
                            node_mapping.insert(*node_id, existing_id);
                            self.eliminated_count += 1;
                            continue;
                        }
                    }
                }
                
                // Add new expression
                let mapped_node = map_node_refs(node, &node_mapping);
                let new_id = optimized.add_node(mapped_node);
                node_mapping.insert(*node_id, new_id);
                expr_cache.insert(key, new_id);
            } else {
                // Non-pure expressions can't be eliminated
                let mapped_node = map_node_refs(node, &node_mapping);
                let new_id = optimized.add_node(mapped_node);
                node_mapping.insert(*node_id, new_id);
            }
        }

        // Update root
        if let Some(root) = graph.root_id {
            optimized.root_id = node_mapping.get(&root).copied();
        }

        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!("{} pass: {} expressions eliminated", self.name(), self.eliminated_count)
    }
}

/// Map node references through the mapping
fn map_node_refs(node: &Node, mapping: &FxHashMap<NodeId, NodeId>) -> Node {
    match node {
        Node::Application { function, args } => {
            Node::Application {
                function: mapping.get(function).copied().unwrap_or(*function),
                args: args.iter()
                    .map(|id| mapping.get(id).copied().unwrap_or(*id))
                    .collect(),
            }
        }
        Node::Lambda { params, body } => {
            Node::Lambda {
                params: params.clone(),
                body: mapping.get(body).copied().unwrap_or(*body),
            }
        }
        Node::Let { bindings, body } => {
            Node::Let {
                bindings: bindings.iter()
                    .map(|(name, id)| {
                        (name.clone(), mapping.get(id).copied().unwrap_or(*id))
                    })
                    .collect(),
                body: mapping.get(body).copied().unwrap_or(*body),
            }
        }
        Node::If { condition, then_branch, else_branch } => {
            Node::If {
                condition: mapping.get(condition).copied().unwrap_or(*condition),
                then_branch: mapping.get(then_branch).copied().unwrap_or(*then_branch),
                else_branch: mapping.get(else_branch).copied().unwrap_or(*else_branch),
            }
        }
        Node::List(items) => {
            Node::List(
                items.iter()
                    .map(|id| mapping.get(id).copied().unwrap_or(*id))
                    .collect()
            )
        }
        _ => node.clone(),
    }
}