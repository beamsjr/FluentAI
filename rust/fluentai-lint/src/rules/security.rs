//! Security-related lint rules

use crate::diagnostic::LintDiagnostic;
use crate::visitor::Visitor;
use crate::rules::{Rule, RuleCategory, DiagnosticCollector};
use crate::impl_rule;
use fluentai_core::ast::{Graph, Node, NodeId, EffectType};

/// Check for potentially unsafe effects
#[derive(Default)]
pub struct UnsafeEffects;

impl UnsafeEffects {
    fn visitor(&self) -> UnsafeEffectsVisitor {
        UnsafeEffectsVisitor {
            collector: DiagnosticCollector::new("unsafe-effects"),
        }
    }
}

struct UnsafeEffectsVisitor {
    collector: DiagnosticCollector,
}

impl Visitor for UnsafeEffectsVisitor {
    fn visit_node(&mut self, graph: &Graph, _node_id: NodeId, node: &Node) {
        match node {
            Node::Effect { effect_type, operation, args } => {
                // Check for unsafe effect patterns
                match effect_type {
                    EffectType::IO => {
                        if operation.contains("exec") || operation.contains("system") {
                            self.collector.add_diagnostic(
                                LintDiagnostic::error(
                                    "unsafe-effects",
                                    format!("Potentially dangerous IO operation: {}", operation)
                                )
                                .with_note("System command execution can be a security risk")
                            );
                        }
                    }
                    EffectType::Network => {
                        // Check for unvalidated network operations
                        if args.is_empty() {
                            self.collector.add_diagnostic(
                                LintDiagnostic::warning(
                                    "unsafe-effects",
                                    "Network operation without explicit parameters"
                                )
                            );
                        }
                    }
                    _ => {}
                }
                
                // Continue traversal
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            _ => {
                // Continue traversal
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

impl UnsafeEffectsVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    UnsafeEffects,
    "unsafe-effects",
    "Unsafe Effects",
    "Detects potentially dangerous effect operations",
    RuleCategory::Security
);

/// Check for unvalidated input usage
#[derive(Default)]
pub struct UnvalidatedInput;

impl UnvalidatedInput {
    fn visitor(&self) -> UnvalidatedInputVisitor {
        UnvalidatedInputVisitor {
            collector: DiagnosticCollector::new("unvalidated-input"),
            input_sources: Vec::new(),
        }
    }
}

struct UnvalidatedInputVisitor {
    collector: DiagnosticCollector,
    input_sources: Vec<NodeId>,
}

impl Visitor for UnvalidatedInputVisitor {
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        match node {
            Node::Effect { effect_type, operation, .. } => {
                // Track input sources
                if matches!(effect_type, EffectType::IO) && operation.contains("read") {
                    self.input_sources.push(node_id);
                }
                
                // Check for direct usage of input in dangerous operations
                if matches!(effect_type, EffectType::IO | EffectType::Network) {
                    // This is a simplified check - real implementation would do data flow analysis
                    self.collector.add_diagnostic(
                        LintDiagnostic::warning(
                            "unvalidated-input",
                            "Ensure user input is validated before use in effects"
                        )
                        .with_note("Consider adding input validation")
                    );
                }
            }
            _ => {}
        }
        
        // Continue traversal
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
            Node::If { condition, then_branch, else_branch } => {
                self.visit_node_id(graph, *condition);
                self.visit_node_id(graph, *then_branch);
                self.visit_node_id(graph, *else_branch);
            }
            Node::Effect { args, .. } => {
                for arg in args {
                    self.visit_node_id(graph, *arg);
                }
            }
            _ => {}
        }
    }
}

impl UnvalidatedInputVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    UnvalidatedInput,
    "unvalidated-input",
    "Unvalidated Input",
    "Detects potential usage of unvalidated user input",
    RuleCategory::Security
);