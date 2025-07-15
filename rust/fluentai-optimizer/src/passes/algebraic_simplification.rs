//! Algebraic simplification optimization pass
//!
//! This pass applies algebraic identities to simplify expressions:
//! - x + 0 -> x, 0 + x -> x
//! - x - 0 -> x
//! - x - x -> 0
//! - x / x -> 1 (when x != 0)
//! - x && true -> x, true && x -> x
//! - x && false -> false, false && x -> false
//! - x || true -> true, true || x -> true
//! - x || false -> x, false || x -> x
//! - !true -> false, !false -> true

use crate::passes::OptimizationPass;
use anyhow::Result;
use fluentai_core::ast::{Graph, Literal, Node, NodeId};
use rustc_hash::FxHashMap;

/// Algebraic simplification optimization pass
pub struct AlgebraicSimplificationPass {
    simplifications: usize,
}

impl AlgebraicSimplificationPass {
    /// Create a new algebraic simplification pass
    pub fn new() -> Self {
        Self { simplifications: 0 }
    }

    /// Apply algebraic simplifications to the entire graph
    fn simplify_graph(&mut self, graph: &Graph) -> Graph {
        let mut optimized = Graph::new();
        let mut node_map = FxHashMap::default();
        
        // First pass: copy all nodes and create mapping
        for (node_id, node) in &graph.nodes {
            let new_node = match node {
                // Check if this is an operation we can simplify
                Node::Application { function, args } => {
                    if let Some(Node::Variable { name }) = graph.get_node(*function) {
                        match name.as_str() {
                            "+" if args.len() == 2 => {
                                if let Some(simplified) = self.simplify_addition(graph, &args[0], &args[1], &node_map) {
                                    self.simplifications += 1;
                                    simplified
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "-" if args.len() == 2 => {
                                if let Some(simplified) = self.simplify_subtraction(graph, &args[0], &args[1], &node_map) {
                                    self.simplifications += 1;
                                    simplified
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "/" if args.len() == 2 => {
                                if let Some(simplified) = self.simplify_division(graph, &args[0], &args[1], &node_map) {
                                    self.simplifications += 1;
                                    simplified
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "&&" | "and" if args.len() == 2 => {
                                if let Some(simplified) = self.simplify_and(graph, &args[0], &args[1], &node_map) {
                                    self.simplifications += 1;
                                    simplified
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "||" | "or" if args.len() == 2 => {
                                if let Some(simplified) = self.simplify_or(graph, &args[0], &args[1], &node_map) {
                                    self.simplifications += 1;
                                    simplified
                                } else {
                                    self.clone_node_with_refs(node, &node_map)
                                }
                            }
                            "!" | "not" if args.len() == 1 => {
                                if let Some(simplified) = self.simplify_not(graph, &args[0], &node_map) {
                                    self.simplifications += 1;
                                    simplified
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

    /// Check if two nodes are the same variable
    fn are_same_variable(&self, graph: &Graph, id1: &NodeId, id2: &NodeId) -> bool {
        if id1 == id2 {
            return true;
        }
        
        match (graph.get_node(*id1), graph.get_node(*id2)) {
            (Some(Node::Variable { name: name1 }), Some(Node::Variable { name: name2 })) => {
                name1 == name2
            }
            _ => false,
        }
    }

    /// Simplify addition operations
    fn simplify_addition(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        let left_node = graph.get_node(*left)?;
        let right_node = graph.get_node(*right)?;
        
        match (left_node, right_node) {
            // x + 0 -> x
            (_, Node::Literal(Literal::Integer(0))) => {
                Some(self.clone_node_with_refs(left_node, node_map))
            }
            // 0 + x -> x
            (Node::Literal(Literal::Integer(0)), _) => {
                Some(self.clone_node_with_refs(right_node, node_map))
            }
            _ => None,
        }
    }

    /// Simplify subtraction operations
    fn simplify_subtraction(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        let left_node = graph.get_node(*left)?;
        let right_node = graph.get_node(*right)?;
        
        // x - x -> 0
        if self.are_same_variable(graph, left, right) {
            return Some(Node::Literal(Literal::Integer(0)));
        }
        
        match (left_node, right_node) {
            // x - 0 -> x
            (_, Node::Literal(Literal::Integer(0))) => {
                Some(self.clone_node_with_refs(left_node, node_map))
            }
            _ => None,
        }
    }

    /// Simplify division operations
    fn simplify_division(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        // x / x -> 1 (assuming x != 0)
        if self.are_same_variable(graph, left, right) {
            return Some(Node::Literal(Literal::Integer(1)));
        }
        
        None
    }

    /// Simplify logical AND operations
    fn simplify_and(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        let left_node = graph.get_node(*left)?;
        let right_node = graph.get_node(*right)?;
        
        match (left_node, right_node) {
            // x && true -> x
            (_, Node::Literal(Literal::Boolean(true))) => {
                Some(self.clone_node_with_refs(left_node, node_map))
            }
            // true && x -> x
            (Node::Literal(Literal::Boolean(true)), _) => {
                Some(self.clone_node_with_refs(right_node, node_map))
            }
            // x && false -> false
            (_, Node::Literal(Literal::Boolean(false))) |
            // false && x -> false
            (Node::Literal(Literal::Boolean(false)), _) => {
                Some(Node::Literal(Literal::Boolean(false)))
            }
            _ => None,
        }
    }

    /// Simplify logical OR operations
    fn simplify_or(
        &self,
        graph: &Graph,
        left: &NodeId,
        right: &NodeId,
        node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        let left_node = graph.get_node(*left)?;
        let right_node = graph.get_node(*right)?;
        
        match (left_node, right_node) {
            // x || false -> x
            (_, Node::Literal(Literal::Boolean(false))) => {
                Some(self.clone_node_with_refs(left_node, node_map))
            }
            // false || x -> x
            (Node::Literal(Literal::Boolean(false)), _) => {
                Some(self.clone_node_with_refs(right_node, node_map))
            }
            // x || true -> true
            (_, Node::Literal(Literal::Boolean(true))) |
            // true || x -> true
            (Node::Literal(Literal::Boolean(true)), _) => {
                Some(Node::Literal(Literal::Boolean(true)))
            }
            _ => None,
        }
    }

    /// Simplify NOT operations
    fn simplify_not(
        &self,
        graph: &Graph,
        arg: &NodeId,
        _node_map: &FxHashMap<NodeId, NodeId>,
    ) -> Option<Node> {
        if let Some(Node::Literal(lit)) = graph.get_node(*arg) {
            match lit {
                Literal::Boolean(true) => Some(Node::Literal(Literal::Boolean(false))),
                Literal::Boolean(false) => Some(Node::Literal(Literal::Boolean(true))),
                _ => None,
            }
        } else {
            None
        }
    }
}

impl OptimizationPass for AlgebraicSimplificationPass {
    fn name(&self) -> &str {
        "Algebraic Simplification"
    }

    fn run(&mut self, graph: &Graph) -> Result<Graph> {
        self.simplifications = 0;
        Ok(self.simplify_graph(graph))
    }

    fn stats(&self) -> String {
        format!("{} pass: {} expressions simplified", self.name(), self.simplifications)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::Literal;

    #[test]
    fn test_add_zero() {
        let mut graph = Graph::new();
        
        // Create x + 0
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let zero = graph.add_node(Node::Literal(Literal::Integer(0))).unwrap();
        let plus = graph.add_node(Node::Variable { name: "+".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: plus,
            args: vec![x, zero],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Variable { name }) = optimized.get_node(root_id) {
                assert_eq!(name, "x");
            } else {
                panic!("Expected variable x");
            }
        }
    }

    #[test]
    fn test_subtract_self() {
        let mut graph = Graph::new();
        
        // Create x - x
        let x1 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let x2 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let minus = graph.add_node(Node::Variable { name: "-".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: minus,
            args: vec![x1, x2],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(0))) = optimized.get_node(root_id) {
                // Success
            } else {
                panic!("Expected literal 0");
            }
        }
    }

    #[test]
    fn test_and_true() {
        let mut graph = Graph::new();
        
        // Create x && true
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let true_val = graph.add_node(Node::Literal(Literal::Boolean(true))).unwrap();
        let and = graph.add_node(Node::Variable { name: "&&".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: and,
            args: vec![x, true_val],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Variable { name }) = optimized.get_node(root_id) {
                assert_eq!(name, "x");
            } else {
                panic!("Expected variable x");
            }
        }
    }

    #[test]
    fn test_or_false() {
        let mut graph = Graph::new();
        
        // Create x || false
        let x = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let false_val = graph.add_node(Node::Literal(Literal::Boolean(false))).unwrap();
        let or = graph.add_node(Node::Variable { name: "||".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: or,
            args: vec![x, false_val],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Variable { name }) = optimized.get_node(root_id) {
                assert_eq!(name, "x");
            } else {
                panic!("Expected variable x");
            }
        }
    }

    #[test]
    fn test_not_true() {
        let mut graph = Graph::new();
        
        // Create !true
        let true_val = graph.add_node(Node::Literal(Literal::Boolean(true))).unwrap();
        let not = graph.add_node(Node::Variable { name: "!".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: not,
            args: vec![true_val],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(Literal::Boolean(false))) = optimized.get_node(root_id) {
                // Success
            } else {
                panic!("Expected literal false");
            }
        }
    }

    #[test]
    fn test_divide_self() {
        let mut graph = Graph::new();
        
        // Create x / x
        let x1 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let x2 = graph.add_node(Node::Variable { name: "x".to_string() }).unwrap();
        let div = graph.add_node(Node::Variable { name: "/".to_string() }).unwrap();
        let app = graph.add_node(Node::Application {
            function: div,
            args: vec![x1, x2],
        }).unwrap();
        
        graph.root_id = Some(app);
        
        let mut pass = AlgebraicSimplificationPass::new();
        let optimized = pass.run(&graph).unwrap();
        
        assert_eq!(pass.simplifications, 1);
        
        if let Some(root_id) = optimized.root_id {
            if let Some(Node::Literal(Literal::Integer(1))) = optimized.get_node(root_id) {
                // Success
            } else {
                panic!("Expected literal 1");
            }
        }
    }
}