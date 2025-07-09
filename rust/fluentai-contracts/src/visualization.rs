//! Visualization of symbolic execution paths
//!
//! This module provides tools to visualize symbolic execution trees
//! in various formats for debugging and analysis.

use crate::errors::{ContractError, ContractResult};
use crate::symbolic_execution::{PathConstraint, SymbolicState, SymbolicValue};
use std::collections::HashMap;
use std::fmt::Write;

/// A node in the symbolic execution tree
#[derive(Debug, Clone)]
pub struct ExecutionNode {
    pub id: usize,
    pub parent_id: Option<usize>,
    pub constraint: Option<PathConstraint>,
    pub state: SymbolicState,
    pub children: Vec<usize>,
    pub is_leaf: bool,
    pub is_satisfiable: Option<bool>,
}

/// Symbolic execution tree structure
#[derive(Debug)]
pub struct ExecutionTree {
    nodes: HashMap<usize, ExecutionNode>,
    root_id: usize,
    next_id: usize,
}

impl ExecutionTree {
    /// Create a new execution tree
    pub fn new() -> Self {
        let root_state = SymbolicState::new();
        let root_node = ExecutionNode {
            id: 0,
            parent_id: None,
            constraint: None,
            state: root_state,
            children: Vec::new(),
            is_leaf: false,
            is_satisfiable: Some(true),
        };

        let mut nodes = HashMap::new();
        nodes.insert(0, root_node);

        Self {
            nodes,
            root_id: 0,
            next_id: 1,
        }
    }

    /// Add a new node to the tree
    pub fn add_node(
        &mut self,
        parent_id: usize,
        constraint: PathConstraint,
        state: SymbolicState,
    ) -> ContractResult<usize> {
        let parent = self
            .nodes
            .get_mut(&parent_id)
            .ok_or_else(|| ContractError::Other(format!("Parent node {} not found", parent_id)))?;

        let node_id = self.next_id;
        self.next_id += 1;

        parent.children.push(node_id);
        parent.is_leaf = false;

        let node = ExecutionNode {
            id: node_id,
            parent_id: Some(parent_id),
            constraint: Some(constraint),
            state,
            children: Vec::new(),
            is_leaf: true,
            is_satisfiable: None,
        };

        self.nodes.insert(node_id, node);
        Ok(node_id)
    }

    /// Mark a node's satisfiability
    pub fn set_satisfiability(&mut self, node_id: usize, is_sat: bool) -> ContractResult<()> {
        let node = self
            .nodes
            .get_mut(&node_id)
            .ok_or_else(|| ContractError::Other(format!("Node {} not found", node_id)))?;
        node.is_satisfiable = Some(is_sat);
        Ok(())
    }

    /// Generate DOT format visualization
    pub fn to_dot(&self) -> String {
        let mut dot = String::new();
        writeln!(&mut dot, "digraph SymbolicExecutionTree {{").unwrap();
        writeln!(&mut dot, "  rankdir=TB;").unwrap();
        writeln!(&mut dot, "  node [shape=box, style=rounded];").unwrap();

        // Write nodes
        for (id, node) in &self.nodes {
            let label = self.format_node_label(node);
            let color = match node.is_satisfiable {
                Some(true) => "green",
                Some(false) => "red",
                None => "gray",
            };
            let style = if node.is_leaf { "filled" } else { "rounded" };

            writeln!(
                &mut dot,
                "  n{} [label=\"{}\", color={}, style=\"{}\"];",
                id, label, color, style
            )
            .unwrap();
        }

        // Write edges
        for (id, node) in &self.nodes {
            if let Some(parent_id) = node.parent_id {
                let edge_label = if let Some(constraint) = &node.constraint {
                    self.format_constraint_label(constraint)
                } else {
                    String::new()
                };

                writeln!(
                    &mut dot,
                    "  n{} -> n{} [label=\"{}\"];",
                    parent_id, id, edge_label
                )
                .unwrap();
            }
        }

        writeln!(&mut dot, "}}").unwrap();
        dot
    }

    /// Generate Mermaid format visualization
    pub fn to_mermaid(&self) -> String {
        let mut mermaid = String::new();
        writeln!(&mut mermaid, "graph TD").unwrap();

        // Write nodes
        for (id, node) in &self.nodes {
            let label = self.format_node_label(node);
            let shape = if node.is_leaf { "([{}])" } else { "[{}]" };

            writeln!(&mut mermaid, "  n{}{}", id, shape.replace("{}", &label)).unwrap();

            // Add styling based on satisfiability
            match node.is_satisfiable {
                Some(true) => writeln!(&mut mermaid, "  style n{} fill:#9f9", id).unwrap(),
                Some(false) => writeln!(&mut mermaid, "  style n{} fill:#f99", id).unwrap(),
                None => writeln!(&mut mermaid, "  style n{} fill:#ccc", id).unwrap(),
            }
        }

        // Write edges
        for (id, node) in &self.nodes {
            if let Some(parent_id) = node.parent_id {
                let edge_label = if let Some(constraint) = &node.constraint {
                    format!("|{}|", self.format_constraint_label(constraint))
                } else {
                    String::new()
                };

                writeln!(&mut mermaid, "  n{} -->{}n{}", parent_id, edge_label, id).unwrap();
            }
        }

        mermaid
    }

    /// Generate ASCII tree visualization
    pub fn to_ascii(&self) -> String {
        let mut output = String::new();
        self.write_ascii_node(&mut output, self.root_id, "", true);
        output
    }

    fn write_ascii_node(&self, output: &mut String, node_id: usize, prefix: &str, is_last: bool) {
        let node = &self.nodes[&node_id];

        // Write branch
        write!(output, "{}", prefix).unwrap();
        if node.parent_id.is_some() {
            write!(output, "{}", if is_last { "└── " } else { "├── " }).unwrap();
        }

        // Write node info
        let sat_marker = match node.is_satisfiable {
            Some(true) => "✓",
            Some(false) => "✗",
            None => "?",
        };

        writeln!(
            output,
            "[{}] Node {} {}",
            sat_marker,
            node_id,
            self.format_node_summary(node)
        )
        .unwrap();

        // Write constraint if present
        if let Some(constraint) = &node.constraint {
            let new_prefix = format!("{}{}    ", prefix, if is_last { " " } else { "│" });
            writeln!(
                output,
                "{}└─ {}",
                new_prefix,
                self.format_constraint_label(constraint)
            )
            .unwrap();
        }

        // Recurse to children
        let child_count = node.children.len();
        for (i, &child_id) in node.children.iter().enumerate() {
            let is_last_child = i == child_count - 1;
            let new_prefix = format!("{}{}", prefix, if is_last { "    " } else { "│   " });
            self.write_ascii_node(output, child_id, &new_prefix, is_last_child);
        }
    }

    /// Format a node label for visualization
    fn format_node_label(&self, node: &ExecutionNode) -> String {
        format!(
            "Node {}\\n{} constraints\\n{} bindings",
            node.id,
            node.state.path_constraints.len(),
            node.state.bindings.len()
        )
    }

    /// Format a node summary
    fn format_node_summary(&self, node: &ExecutionNode) -> String {
        format!(
            "(constraints: {}, vars: {})",
            node.state.path_constraints.len(),
            node.state.bindings.len()
        )
    }

    /// Format a constraint label
    fn format_constraint_label(&self, constraint: &PathConstraint) -> String {
        let constraint_str = self.format_symbolic_value(&constraint.constraint);
        if constraint.expected {
            constraint_str
        } else {
            format!("¬({})", constraint_str)
        }
    }

    /// Format a symbolic value for display
    fn format_symbolic_value(&self, value: &SymbolicValue) -> String {
        match value {
            SymbolicValue::Concrete(lit) => format!("{:?}", lit),
            SymbolicValue::Symbolic { name, .. } => name.clone(),
            SymbolicValue::BinOp { op, left, right } => {
                format!(
                    "({} {} {})",
                    self.format_symbolic_value(left),
                    op,
                    self.format_symbolic_value(right)
                )
            }
            SymbolicValue::UnaryOp { op, operand } => {
                format!("({} {})", op, self.format_symbolic_value(operand))
            }
            _ => "...".to_string(),
        }
    }

    /// Get statistics about the execution tree
    pub fn get_statistics(&self) -> TreeStatistics {
        let total_nodes = self.nodes.len();
        let leaf_nodes = self.nodes.values().filter(|n| n.is_leaf).count();
        let satisfiable_paths = self
            .nodes
            .values()
            .filter(|n| n.is_leaf && n.is_satisfiable == Some(true))
            .count();
        let unsatisfiable_paths = self
            .nodes
            .values()
            .filter(|n| n.is_leaf && n.is_satisfiable == Some(false))
            .count();

        let max_depth = self.compute_max_depth();
        let avg_constraints = if leaf_nodes > 0 {
            self.nodes
                .values()
                .filter(|n| n.is_leaf)
                .map(|n| n.state.path_constraints.len())
                .sum::<usize>() as f64
                / leaf_nodes as f64
        } else {
            0.0
        };

        TreeStatistics {
            total_nodes,
            leaf_nodes,
            satisfiable_paths,
            unsatisfiable_paths,
            max_depth,
            avg_constraints_per_path: avg_constraints,
        }
    }

    /// Compute the maximum depth of the tree
    fn compute_max_depth(&self) -> usize {
        let mut max_depth = 0;

        for node in self.nodes.values() {
            let depth = self.compute_node_depth(node.id);
            max_depth = max_depth.max(depth);
        }

        max_depth
    }

    /// Compute the depth of a specific node
    fn compute_node_depth(&self, node_id: usize) -> usize {
        let mut depth = 0;
        let mut current_id = Some(node_id);

        while let Some(id) = current_id {
            if let Some(node) = self.nodes.get(&id) {
                current_id = node.parent_id;
                if current_id.is_some() {
                    depth += 1;
                }
            } else {
                break;
            }
        }

        depth
    }
}

/// Statistics about the execution tree
#[derive(Debug)]
pub struct TreeStatistics {
    pub total_nodes: usize,
    pub leaf_nodes: usize,
    pub satisfiable_paths: usize,
    pub unsatisfiable_paths: usize,
    pub max_depth: usize,
    pub avg_constraints_per_path: f64,
}

/// Builder for creating execution trees from symbolic states
pub struct TreeBuilder {
    tree: ExecutionTree,
    state_to_node: HashMap<u64, usize>,
}

impl TreeBuilder {
    /// Create a new tree builder
    pub fn new() -> Self {
        Self {
            tree: ExecutionTree::new(),
            state_to_node: HashMap::new(),
        }
    }

    /// Build a tree from a collection of symbolic states
    pub fn build_from_states(states: Vec<SymbolicState>) -> ContractResult<ExecutionTree> {
        let mut builder = Self::new();

        for state in states {
            builder.add_state(state)?;
        }

        Ok(builder.tree)
    }

    /// Add a state to the tree
    fn add_state(&mut self, state: SymbolicState) -> ContractResult<()> {
        // Find the parent node based on constraint prefix
        let parent_id = self.find_parent_node(&state)?;

        // Add the new constraint
        if let Some(last_constraint) = state.path_constraints.last() {
            let node_id = self
                .tree
                .add_node(parent_id, last_constraint.clone(), state.clone())?;

            // Hash the state for future lookups
            let state_hash = self.hash_state(&state);
            self.state_to_node.insert(state_hash, node_id);
        }

        Ok(())
    }

    /// Find the parent node for a given state
    fn find_parent_node(&self, state: &SymbolicState) -> ContractResult<usize> {
        if state.path_constraints.is_empty() {
            return Ok(self.tree.root_id);
        }

        // Try to find a node with all but the last constraint
        let parent_constraints = &state.path_constraints[..state.path_constraints.len() - 1];
        let parent_hash = self.hash_constraints(parent_constraints);

        if let Some(&parent_id) = self.state_to_node.get(&parent_hash) {
            Ok(parent_id)
        } else {
            // Default to root if parent not found
            Ok(self.tree.root_id)
        }
    }

    /// Hash a state for identification
    fn hash_state(&self, state: &SymbolicState) -> u64 {
        self.hash_constraints(&state.path_constraints)
    }

    /// Hash a list of constraints
    fn hash_constraints(&self, constraints: &[PathConstraint]) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};

        let mut hasher = DefaultHasher::new();
        for constraint in constraints {
            format!("{:?}", constraint).hash(&mut hasher);
        }
        hasher.finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbolic_execution::ListOperation;
    use fluentai_core::ast::Literal;

    #[test]
    fn test_tree_construction() {
        let mut tree = ExecutionTree::new();

        // Add a branch
        let mut state1 = SymbolicState::new();
        state1.add_constraint(
            SymbolicValue::BinOp {
                op: ">".to_string(),
                left: Box::new(SymbolicValue::Symbolic {
                    name: "x".to_string(),
                    ty: None,
                }),
                right: Box::new(SymbolicValue::Concrete(Literal::Integer(0))),
            },
            true,
        );

        let node1 = tree
            .add_node(0, state1.path_constraints[0].clone(), state1)
            .unwrap();
        tree.set_satisfiability(node1, true).unwrap();

        // Generate visualizations
        let dot = tree.to_dot();
        assert!(dot.contains("digraph SymbolicExecutionTree"));

        let mermaid = tree.to_mermaid();
        assert!(mermaid.contains("graph TD"));

        let ascii = tree.to_ascii();
        assert!(ascii.contains("Node 0"));

        // Check statistics
        let stats = tree.get_statistics();
        assert_eq!(stats.total_nodes, 2);
        assert_eq!(stats.leaf_nodes, 1);
        assert_eq!(stats.satisfiable_paths, 1);
    }
}
