//! Constant folding optimization pass

use claudelang_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::FxHashMap;
use anyhow::Result;
use crate::passes::OptimizationPass;

/// Constant folding pass
pub struct ConstantFoldingPass {
    folded_count: usize,
}

impl ConstantFoldingPass {
    /// Create new constant folding pass
    pub fn new() -> Self {
        Self { folded_count: 0 }
    }
}

impl OptimizationPass for ConstantFoldingPass {
    fn name(&self) -> &str {
        "Constant Folding"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.folded_count = 0;
        let mut optimized = Graph::new();
        let mut node_mapping = FxHashMap::default();

        // Process all nodes
        for (node_id, node) in &graph.nodes {
            if let Some(folded) = fold_constants(graph, &node_mapping, node) {
                let new_id = optimized.add_node(folded);
                node_mapping.insert(*node_id, new_id);
                self.folded_count += 1;
            } else {
                let mapped = map_node_refs(node, &node_mapping);
                let new_id = optimized.add_node(mapped);
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
        format!("{} pass: {} constants folded", self.name(), self.folded_count)
    }
}

/// Try to fold constants in a node
fn fold_constants(graph: &Graph, mapping: &FxHashMap<NodeId, NodeId>, node: &Node) -> Option<Node> {
    match node {
        Node::Application { function, args } => {
            // Get mapped function
            let func_id = mapping.get(function).copied().unwrap_or(*function);
            
            if let Some(Node::Variable { name }) = graph.get_node(func_id) {
                // Check if all arguments are literals
                let mut literals = Vec::new();
                for arg_id in args {
                    let mapped_id = mapping.get(arg_id).copied().unwrap_or(*arg_id);
                    match graph.get_node(mapped_id) {
                        Some(Node::Literal(lit)) => literals.push(lit.clone()),
                        _ => return None,
                    }
                }
                
                // Try to evaluate
                evaluate_builtin(name, &literals)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Evaluate built-in functions
fn evaluate_builtin(name: &str, args: &[Literal]) -> Option<Node> {
    use Literal::*;
    
    let result = match (name, args) {
        // Arithmetic
        ("+", [Integer(a), Integer(b)]) => Integer(a + b),
        ("-", [Integer(a), Integer(b)]) => Integer(a - b),
        ("*", [Integer(a), Integer(b)]) => Integer(a * b),
        ("/", [Integer(a), Integer(b)]) if *b != 0 => Integer(a / b),
        
        // Comparison
        ("<", [Integer(a), Integer(b)]) => Boolean(a < b),
        (">", [Integer(a), Integer(b)]) => Boolean(a > b),
        ("=", [Integer(a), Integer(b)]) => Boolean(a == b),
        
        // Boolean
        ("and", [Boolean(a), Boolean(b)]) => Boolean(*a && *b),
        ("or", [Boolean(a), Boolean(b)]) => Boolean(*a || *b),
        ("not", [Boolean(a)]) => Boolean(!a),
        
        _ => return None,
    };
    
    Some(Node::Literal(result))
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
        _ => node.clone(),
    }
}