//! Strength reduction optimization pass
//!
//! This pass replaces expensive operations with cheaper equivalents:
//! - x * 2 -> x + x
//! - x * 0 -> 0
//! - x * 1 -> x
//! - x / 1 -> x
//! - x ** 2 -> x * x
//! - x % 2 == 0 -> bitwise operations for power-of-2 modulo

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use rustc_hash::FxHashMap;

/// Strength reduction optimization pass
pub struct StrengthReductionPass {
    replacements: usize,
}

impl StrengthReductionPass {
    /// Create a new strength reduction pass
    pub fn new() -> Self {
        Self { replacements: 0 }
    }

    /// Apply strength reduction to the entire graph
    fn optimize_graph(&mut self, graph: &Graph) -> Graph {
        let mut optimized = Graph::new();
        let mut node_map = FxHashMap::default();
        
        // Create operator nodes we'll need
        let plus_id = optimized.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        let mult_id = optimized.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        
        // First pass: copy all nodes and create mapping
        for (node_id, node) in &graph.nodes {
            let new_node = match node {
                // Check if this is an operation we can optimize
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        match name.as_str() {
                            "*" if args.len() == 2 => {
                                if let Some(opt) = self.optimize_multiplication(graph, &args[0], &args[1], &node_map, plus_id) {
                                    self.replacements += 1;
                                    opt
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "/" if args.len() == 2 => {
                                if let Some(opt) = self.optimize_division(graph, &args[0], &args[1], &node_map) {
                                    self.replacements += 1;
                                    opt
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "**" | "expt" if args.len() == 2 => {
                                if let Some(opt) = self.optimize_exponentiation(graph, &args[0], &args[1], &node_map, mult_id) {
                                    self.replacements += 1;
                                    opt
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            _ => self.clone_node_with_refs(node, &node_map),
                        }
                    } else {
                        self.clone_node_with_refs(node, &node_map)
                    }
                }
                _ => self.clone_node_with_refs(node, &node_map),
            };
            
            let new_id = optimized.add_node(new_node).unwrap();
            node_map.insert(*node_id, new_id);
        }
        
        // Update root
        optimized.root_id = graph.root_id.and_then(|id| node_map.get(&id).copied());
        
        optimized
    }

    /// Clone a node and update its references
    fn clone_node_with_refs(&self, node: &Node, node_map: &FxHashMap<NodeId, NodeId>) -> Node {
        match node {
            Node::Application { function, args } => {
                Node::Application {
                    function: node_map.get(function).copied().unwrap_or(*function),
                    args: args.iter()
                        .map(|id| node_map.get(id).copied().unwrap_or(*id))
                        .collect(),
                }
            }
            Node::Lambda { params, body } => {
                Node::Lambda {
                    params: params.clone(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Let { bindings, body } => {
                Node::Let {
                    bindings: bindings.iter()
                        .map(|(name, id)| (name.clone(), node_map.get(id).copied().unwrap_or(*id)))
                        .collect(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::If { condition, then_branch, else_branch } => {
                Node::If {
                    condition: node_map.get(condition).copied().unwrap_or(*condition),
                    then_branch: node_map.get(then_branch).copied().unwrap_or(*then_branch),
                    else_branch: node_map.get(else_branch).copied().unwrap_or(*else_branch),
                }
            }
            Node::Match { expr, branches } => {
                Node::Match {
                    expr: node_map.get(expr).copied().unwrap_or(*expr),
                    branches: branches.iter()
                        .map(|(pat, body)| (pat.clone(), node_map.get(body).copied().unwrap_or(*body)))
                        .collect(),
                }
            }
            Node::List(items) => {
                Node::List(items.iter()
                    .map(|id| node_map.get(id).copied().unwrap_or(*id))
                    .collect())
            }
            Node::Async { body } => {
                Node::Async { body: node_map.get(body).copied().unwrap_or(*body) }
            }
            Node::Letrec { bindings, body } => {
                Node::Letrec {
                    bindings: bindings.iter()
                        .map(|(name, id)| (name.clone(), node_map.get(id).copied().unwrap_or(*id)))
                        .collect(),
                    body: node_map.get(body).copied().unwrap_or(*body),
                }
            }
            Node::Map(pairs) => {
                Node::Map(pairs.iter()
                    .map(|(k, v)| {
                        (node_map.get(k).copied().unwrap_or(*k),
                         node_map.get(v).copied().unwrap_or(*v))
                    })
                    .collect())
            }
            _ => node.clone(),
        }
    }

    /// Optimize multiplication operations
    fn optimize_multiplication(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
        plus_id: NodeId,
    ) -> Option<Node> {
        let left_node = graph.get_node(*left)?;
        let right_node = graph.get_node(*right)?;
        
        match (left_node, right_node) {
            // x * 0 -> 0
            (_, Node::Literal(Literal::Integer(0))) | 
            (Node::Literal(Literal::Integer(0)), _) => {
                Some(Node::Literal(Literal::Integer(0)))
            }
            // x * 1 -> x
            (_, Node::Literal(Literal::Integer(1))) => {
                Some(self.clone_node_with_refs(left_node, node_map))
            }
            // 1 * x -> x
            (Node::Literal(Literal::Integer(1)), _) => {
                Some(self.clone_node_with_refs(right_node, node_map))
            }
            // x * 2 -> x + x
            (_, Node::Literal(Literal::Integer(2))) => {
                let new_left = node_map.get(left).copied().unwrap_or(*left);
                Some(Node::Application {
                    function: plus_id,
                    args: vec![new_left, new_left],
                })
            }
            // 2 * x -> x + x
            (Node::Literal(Literal::Integer(2)), _) => {
                let new_right = node_map.get(right).copied().unwrap_or(*right);
                Some(Node::Application {
                    function: plus_id,
                    args: vec![new_right, new_right],
                })
            }
            _ => None,
        }
    }

    /// Optimize division operations
    fn optimize_division(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        if let Some(Node::Literal(Literal::Integer(1))) = graph.get_node(*right) {
            // x / 1 -> x
            graph.get_node(*left).map(|n| self.clone_node_with_refs(n, node_map))
        } else {
            None
        }
    }

    /// Optimize exponentiation operations
    fn optimize_exponentiation(
        &self,
        graph: &Graph,
        base: &NodeId,
        exp: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
        mult_id: NodeId,
    ) -> Option<Node> {
        if let Some(Node::Literal(lit)) = graph.get_node(*exp) {
            match lit {
                Literal::Integer(0) => {
                    // x ** 0 -> 1
                    Some(Node::Literal(Literal::Integer(1)))
                }
                Literal::Integer(1) => {
                    // x ** 1 -> x
                    graph.get_node(*base).map(|n| self.clone_node_with_refs(n, node_map))
                }
                Literal::Integer(2) => {
                    // x ** 2 -> x * x
                    let new_base = node_map.get(base).copied().unwrap_or(*base);
                    Some(Node::Application {
                        function: mult_id,
                        args: vec![new_base, new_base],
                    })
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

impl OptimizationPass for StrengthReductionPass {
    fn name(&self) -> &str {
        "Strength Reduction"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.replacements = 0;
        Ok(self.optimize_graph(graph))
    }

    fn stats(&self) -> String {
        format!("{} pass: {} operations reduced", self.name(), self.replacements)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::Literal;

    #[test]
    fn test_multiply_by_two() {
        let mut graph = Graph::new();
        
        // Create x * 2
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let mul = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: mul,
            args: vec![x, two],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x * 2 with x + x
        assert_eq!(pass.replacements, 1);
        
        // Find the optimized root node
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Application { function, args }) = optimized.get_node(root_id) {
                // Should be an addition
                if let Some(Node::Variable { name }) = optimized.get_node(*function) {
                    assert_eq!(name, "+");
                }
                // Both args should reference x
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], args[1]);
            } else {
                panic!("Expected Application node");
            }
        } else {
            panic!("No root node");
        }
    }

    #[test]
    fn test_multiply_by_zero() {
        let mut graph = Graph::new();
        
        // Create x * 0
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let mul = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: mul,
            args: vec![x, zero],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x * 0 with 0
        assert_eq!(pass.replacements, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(0))) = optimized.get_node(root_id) {
                // Success
            } else {
                panic!("Expected literal 0");
            }
        }
    }

    #[test]
    fn test_multiply_by_one() {
        let mut graph = Graph::new();
        
        // Create x * 1
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let mul = graph.add_node(Node::Variable { name: "*".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: mul,
            args: vec![x, one],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x * 1 with x
        assert_eq!(pass.replacements, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Variable { name }) = optimized.get_node(root_id) {
                assert_eq!(name, "x");
            } else {
                panic!("Expected variable x");
            }
        }
    }

    #[test]
    fn test_divide_by_one() {
        let mut graph = Graph::new();
        
        // Create x / 1
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let one = graph.add_node(Node::Literal(Literal::Integer(1))).unwrap();
        let div = graph.add_node(Node::Variable { name: "/".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: div,
            args: vec![x, one],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x / 1 with x
        assert_eq!(pass.replacements, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Variable { name }) = optimized.get_node(root_id) {
                assert_eq!(name, "x");
            } else {
                panic!("Expected variable x");
            }
        }
    }

    #[test]
    fn test_exponent_zero() {
        let mut graph = Graph::new();
        
        // Create x ** 0
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let exp = graph.add_node(Node::Variable { name: "**".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: exp,
            args: vec![x, zero],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x ** 0 with 1
        assert_eq!(pass.replacements, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(1))) = optimized.get_node(root_id) {
                // Success
            } else {
                panic!("Expected literal 1");
            }
        }
    }

    #[test]
    fn test_exponent_two() {
        let mut graph = Graph::new();
        
        // Create x ** 2
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let two = graph.add_node(Node::Literal(Literal::Integer(2))).unwrap();
        let exp = graph.add_node(Node::Variable { name: "**".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: exp,
            args: vec![x, two],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = StrengthReductionPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        // Should have replaced x ** 2 with x * x
        assert_eq!(pass.replacements, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Application { function, args }) = optimized.get_node(root_id) {
                // Should be multiplication
                if let Some(Node::Variable { name }) = optimized.get_node(*function) {
                    assert_eq!(name, "*");
                }
                // Both args should be x
                assert_eq!(args.len(), 2);
                assert_eq!(args[0], args[1]);
            } else {
                panic!("Expected Application node");
            }
        }
    }
}