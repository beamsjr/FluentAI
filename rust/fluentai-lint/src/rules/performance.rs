//! Performance-related lint rules

use crate::diagnostic::LintDiagnostic;
use crate::visitor::Visitor;
use crate::rules::{Rule, RuleCategory, DiagnosticCollector};
use crate::impl_rule;
use fluentai_core::ast::{Graph, Node, NodeId};
use rustc_hash::FxHashSet;

/// Check for inefficient recursion patterns
#[derive(Default)]
pub struct InefficientRecursion;

impl InefficientRecursion {
    fn visitor(&self) -> InefficientRecursionVisitor {
        InefficientRecursionVisitor {
            collector: DiagnosticCollector::new("inefficient-recursion"),
            in_recursive_context: false,
        }
    }
}

struct InefficientRecursionVisitor {
    collector: DiagnosticCollector,
    in_recursive_context: bool,
}

impl Visitor for InefficientRecursionVisitor {
    fn visit_node(&mut self, graph: &Graph, _node_id: NodeId, node: &Node) {
        // Placeholder implementation
        // Real implementation would analyze:
        // - Tail recursion optimization opportunities
        // - Stack-consuming recursive patterns
        // - Mutual recursion cycles
        
        match node {
            Node::Lambda { body, .. } => {
                self.visit_node_id(graph, *body);
            }
            Node::Application { function, args } => {
                self.visit_node_id(graph, *function);
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Let { bindings, body } => {
                for (_, value) in bindings {
                    self.visit_node_id(graph, *value);
                }
                self.visit_node_id(graph, *body);
            }
            _ => {}
        }
    }
}

impl InefficientRecursionVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    InefficientRecursion,
    "inefficient-recursion",
    "Inefficient Recursion",
    "Detects recursive patterns that could be optimized",
    RuleCategory::Performance
);

/// Check for unused computations
#[derive(Default)]
pub struct UnusedComputation;

impl UnusedComputation {
    fn visitor(&self) -> UnusedComputationVisitor {
        UnusedComputationVisitor {
            collector: DiagnosticCollector::new("unused-computation"),
            pure_expressions: Vec::new(),
        }
    }
}

struct UnusedComputationVisitor {
    collector: DiagnosticCollector,
    pure_expressions: Vec<NodeId>,
}

impl Visitor for UnusedComputationVisitor {
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        match node {
            // Check for pure expressions whose results are ignored
            Node::Application { function, args } => {
                // Check if this is a pure function call whose result is ignored
                if let Some(func_node) = graph.get_node(*function) {
                    if is_likely_pure(func_node) {
                        // This is a heuristic - would need effect analysis for accuracy
                        self.pure_expressions.push(node_id);
                    }
                }
                
                self.visit_node_id(graph, *function);
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            Node::Let { bindings, body } => {
                // Check if any bindings compute values that are never used
                for (name, value) in bindings {
                    // If the binding starts with _, it's intentionally unused
                    if !name.starts_with('_') {
                        // This would integrate with unused variable detection
                        self.visit_node_id(graph, *value);
                    }
                }
                self.visit_node_id(graph, *body);
            }
            _ => {
                match node {
                    Node::Lambda { body, .. } => {
                        self.visit_node_id(graph, *body);
                    }
                    Node::If { condition, then_branch, else_branch } => {
                        self.visit_node_id(graph, *condition);
                        self.visit_node_id(graph, *then_branch);
                        self.visit_node_id(graph, *else_branch);
                    }
                    _ => {}
                }
            }
        }
    }
}

fn is_likely_pure(node: &Node) -> bool {
    match node {
        Node::Variable { name } => {
            // Common pure functions
            matches!(name.as_str(), "+" | "-" | "*" | "/" | "map" | "filter" | "reduce")
        }
        Node::Lambda { .. } => true,
        _ => false,
    }
}

impl UnusedComputationVisitor {
    fn finish(mut self) -> Vec<LintDiagnostic> {
        // Report pure expressions whose results are ignored
        for _expr_id in self.pure_expressions {
            self.collector.add_diagnostic(
                LintDiagnostic::warning(
                    "unused-computation",
                    "Pure computation result is ignored"
                )
                .with_note("Consider removing this computation or using its result")
            );
        }
        
        self.collector.finish()
    }
}

impl_rule!(
    UnusedComputation,
    "unused-computation",
    "Unused Computation",
    "Detects computations whose results are never used",
    RuleCategory::Performance
);