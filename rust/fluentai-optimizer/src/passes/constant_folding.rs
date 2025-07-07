//! Constant folding optimization pass

use fluentai_core::ast::{Graph, Node, Literal};
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
        let mut optimized = graph.clone();
        
        // Keep folding until no more changes
        loop {
            let mut changed = false;
            let nodes: Vec<_> = optimized.nodes.keys().copied().collect();
            
            for node_id in nodes {
                if let Some(node) = optimized.get_node(node_id).cloned() {
                    if let Some(folded) = fold_constants_in_optimized(&optimized, &node) {
                        optimized.nodes.insert(node_id, folded);
                        self.folded_count += 1;
                        changed = true;
                    }
                }
            }
            
            if !changed {
                break;
            }
        }
        
        // Special case: if the root is now a literal, return just that
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(lit)) = optimized.get_node(root_id) {
                let mut result = Graph::new();
                let new_root = result.add_node(Node::Literal(lit.clone()))?;
                result.root_id = Some(new_root);
                return Ok(result);
            }
        }

        Ok(optimized)
    }

    fn stats(&self) -> String {
        format!("{} pass: {} constants folded", self.name(), self.folded_count)
    }
}

/// Try to fold constants in a node within the optimized graph
fn fold_constants_in_optimized(graph: &Graph, node: &Node) -> Option<Node> {
    match node {
        Node::Application { function, args } => {
            // Look at the function in the graph
            if let Some(Node::Variable { name }) = graph.get_node(*function) {
                // Check if all arguments are literals
                let mut literals = Vec::new();
                for arg_id in args {
                    match graph.get_node(*arg_id) {
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
        ("mod", [Integer(a), Integer(b)]) if *b != 0 => Integer(a % b),
        
        // Comparison
        ("<", [Integer(a), Integer(b)]) => Boolean(a < b),
        (">", [Integer(a), Integer(b)]) => Boolean(a > b),
        ("=", [Integer(a), Integer(b)]) => Boolean(a == b),
        
        // Boolean
        ("and", [Boolean(a), Boolean(b)]) => Boolean(*a && *b),
        ("or", [Boolean(a), Boolean(b)]) => Boolean(*a || *b),
        ("not", [Boolean(a)]) => Boolean(!a),
        
        // String operations
        ("str-concat", [String(a), String(b)]) => String(format!("{}{}", a, b)),
        
        _ => return None,
    };
    
    Some(Node::Literal(result))
}

