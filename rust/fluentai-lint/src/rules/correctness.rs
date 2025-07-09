//! Correctness-related lint rules

use crate::diagnostic::{LintDiagnostic, Suggestion};
use crate::impl_rule;
use crate::rules::{DiagnosticCollector, Rule, RuleCategory};
use crate::visitor::Visitor;
use fluentai_core::ast::{Graph, Node, NodeId};

/// Check for unused variables
#[derive(Default)]
pub struct UnusedVariables;

impl UnusedVariables {
    fn visitor(&self) -> UnusedVariablesVisitor {
        UnusedVariablesVisitor {
            collector: DiagnosticCollector::new("unused-variables"),
            bindings: Vec::new(),
            current_scope: 0,
        }
    }
}

struct UnusedVariablesVisitor {
    collector: DiagnosticCollector,
    bindings: Vec<(String, NodeId, usize, bool)>, // (name, node_id, scope, used)
    current_scope: usize,
}

impl Visitor for UnusedVariablesVisitor {
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        match node {
            Node::Let { bindings, body } => {
                self.current_scope += 1;
                let scope = self.current_scope;

                // Register bindings
                for (name, value) in bindings {
                    self.bindings.push((name.clone(), node_id, scope, false));
                    self.visit_node_id(graph, *value);
                }

                // Visit body
                self.visit_node_id(graph, *body);

                // Check for unused bindings in this scope
                let unused: Vec<_> = self
                    .bindings
                    .iter()
                    .filter(|(_, _, s, used)| *s == scope && !used)
                    .map(|(name, _, _, _)| name.clone())
                    .collect();

                for name in unused {
                    if !name.starts_with('_') {
                        self.collector.add_diagnostic(
                            LintDiagnostic::warning(
                                "unused-variables",
                                format!("Variable '{}' is never used", name)
                            )
                            .with_suggestion(Suggestion {
                                message: format!("Prefix with underscore to indicate it's intentionally unused: _{}", name),
                                replacements: vec![],
                            })
                        );
                    }
                }

                self.current_scope -= 1;
            }
            Node::Variable { name } => {
                // Mark variable as used
                for binding in &mut self.bindings {
                    if binding.0 == *name && binding.2 <= self.current_scope {
                        binding.3 = true;
                    }
                }
            }
            Node::Lambda { params, body } => {
                self.current_scope += 1;
                let scope = self.current_scope;

                // Register parameters
                for param in params {
                    self.bindings.push((param.clone(), node_id, scope, false));
                }

                self.visit_node_id(graph, *body);

                // Check unused parameters
                let unused: Vec<_> = self
                    .bindings
                    .iter()
                    .filter(|(_, _, s, used)| *s == scope && !used)
                    .map(|(name, _, _, _)| name.clone())
                    .collect();

                for name in unused {
                    if !name.starts_with('_') {
                        self.collector.add_diagnostic(LintDiagnostic::warning(
                            "unused-variables",
                            format!("Parameter '{}' is never used", name),
                        ));
                    }
                }

                self.current_scope -= 1;
            }
            _ => {
                // Continue traversal for other nodes
                match node {
                    Node::Application { function, args } => {
                        self.visit_node_id(graph, *function);
                        for arg in args {
                            self.visit_node_id(graph, *arg);
                        }
                    }
                    Node::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
                        self.visit_node_id(graph, *condition);
                        self.visit_node_id(graph, *then_branch);
                        self.visit_node_id(graph, *else_branch);
                    }
                    Node::List(items) => {
                        for item in items {
                            self.visit_node_id(graph, *item);
                        }
                    }
                    _ => {}
                }
            }
        }
    }
}

impl UnusedVariablesVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    UnusedVariables,
    "unused-variables",
    "Unused Variables",
    "Detects variables that are declared but never used",
    RuleCategory::Correctness
);

/// Check for unreachable code
#[derive(Default)]
pub struct UnreachableCode;

impl UnreachableCode {
    fn visitor(&self) -> UnreachableCodeVisitor {
        UnreachableCodeVisitor {
            collector: DiagnosticCollector::new("unreachable-code"),
        }
    }
}

struct UnreachableCodeVisitor {
    collector: DiagnosticCollector,
}

impl Visitor for UnreachableCodeVisitor {
    fn visit_node(&mut self, graph: &Graph, _node_id: NodeId, node: &Node) {
        match node {
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                // Check for constant conditions
                if let Some(Node::Literal(lit)) = graph.get_node(*condition) {
                    match lit {
                        fluentai_core::ast::Literal::Boolean(true) => {
                            self.collector.add_diagnostic(LintDiagnostic::warning(
                                "unreachable-code",
                                "Else branch is unreachable due to constant true condition",
                            ));
                        }
                        fluentai_core::ast::Literal::Boolean(false) => {
                            self.collector.add_diagnostic(LintDiagnostic::warning(
                                "unreachable-code",
                                "Then branch is unreachable due to constant false condition",
                            ));
                        }
                        _ => {}
                    }
                }

                self.visit_node_id(graph, *condition);
                self.visit_node_id(graph, *then_branch);
                self.visit_node_id(graph, *else_branch);
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
                    _ => {}
                }
            }
        }
    }
}

impl UnreachableCodeVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    UnreachableCode,
    "unreachable-code",
    "Unreachable Code",
    "Detects code that can never be executed",
    RuleCategory::Correctness
);

/// Check for type mismatches (placeholder)
#[derive(Default)]
pub struct TypeMismatch;

impl TypeMismatch {
    fn visitor(&self) -> TypeMismatchVisitor {
        TypeMismatchVisitor {
            collector: DiagnosticCollector::new("type-mismatch"),
        }
    }
}

struct TypeMismatchVisitor {
    collector: DiagnosticCollector,
}

impl Visitor for TypeMismatchVisitor {
    fn visit_node(&mut self, graph: &Graph, _node_id: NodeId, node: &Node) {
        // Type checking would require type inference
        // This is a placeholder for future implementation
        match node {
            Node::Application { function, args } => {
                // Check for obvious mismatches like calling a non-function
                if let Some(func_node) = graph.get_node(*function) {
                    match func_node {
                        Node::Literal(_) | Node::List(_) => {
                            self.collector.add_diagnostic(LintDiagnostic::error(
                                "type-mismatch",
                                "Attempting to call a non-function value",
                            ));
                        }
                        _ => {}
                    }
                }

                self.visit_node_id(graph, *function);
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
                    Node::Let { bindings, body } => {
                        for (_, value) in bindings {
                            self.visit_node_id(graph, *value);
                        }
                        self.visit_node_id(graph, *body);
                    }
                    Node::If {
                        condition,
                        then_branch,
                        else_branch,
                    } => {
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

impl TypeMismatchVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    TypeMismatch,
    "type-mismatch",
    "Type Mismatch",
    "Detects type errors and mismatches",
    RuleCategory::Correctness
);
