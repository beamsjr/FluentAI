//! Style-related lint rules

use crate::diagnostic::LintDiagnostic;
use crate::visitor::Visitor;
use crate::rules::{Rule, RuleCategory, DiagnosticCollector};
use crate::impl_rule;
use fluentai_core::ast::{Graph, Node, NodeId};
use regex::Regex;
use once_cell::sync::Lazy;

/// Check naming conventions for variables and functions
#[derive(Default)]
pub struct NamingConventions {
    allow_snake_case: bool,
    allow_kebab_case: bool,
}

struct NamingVisitor {
    collector: DiagnosticCollector,
    allow_snake_case: bool,
    allow_kebab_case: bool,
}

impl NamingConventions {
    fn visitor(&self) -> NamingVisitor {
        NamingVisitor {
            collector: DiagnosticCollector::new("naming-conventions"),
            allow_snake_case: self.allow_snake_case,
            allow_kebab_case: self.allow_kebab_case,
        }
    }
}

impl Visitor for NamingVisitor {
    fn visit_node(&mut self, graph: &Graph, _node_id: NodeId, node: &Node) {
        match node {
            Node::Variable { name } => {
                if !is_valid_name(name, self.allow_snake_case, self.allow_kebab_case) {
                    self.collector.add_diagnostic(
                        LintDiagnostic::warning(
                            "naming-conventions",
                            format!("Variable '{}' does not follow naming conventions", name)
                        )
                        .with_note("FluentAi uses kebab-case for identifiers by default")
                    );
                }
            }
            Node::Lambda { params, body } => {
                for param in params {
                    if !is_valid_name(param, self.allow_snake_case, self.allow_kebab_case) {
                        self.collector.add_diagnostic(
                            LintDiagnostic::warning(
                                "naming-conventions",
                                format!("Parameter '{}' does not follow naming conventions", param)
                            )
                        );
                    }
                }
                self.visit_node_id(graph, *body);
            }
            _ => {
                // Continue traversal
                match node {
                    Node::Application { function, args } => {
                        self.visit_node_id(graph, *function);
                        for arg in args {
                            self.visit_node_id(graph, *arg);
                        }
                    }
                    Node::Let { bindings, body } => {
                        for (name, value) in bindings {
                            if !is_valid_name(name, self.allow_snake_case, self.allow_kebab_case) {
                                self.collector.add_diagnostic(
                                    LintDiagnostic::warning(
                                        "naming-conventions",
                                        format!("Binding '{}' does not follow naming conventions", name)
                                    )
                                );
                            }
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

impl NamingVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

fn is_valid_name(name: &str, allow_snake: bool, allow_kebab: bool) -> bool {
    static KEBAB_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[a-z][a-z0-9-]*$").unwrap());
    static SNAKE_RE: Lazy<Regex> = Lazy::new(|| Regex::new(r"^[a-z][a-z0-9_]*$").unwrap());
    
    // Special cases
    if name.starts_with('_') || name == "*" {
        return true;
    }
    
    if allow_kebab && KEBAB_RE.is_match(name) {
        return true;
    }
    
    if allow_snake && SNAKE_RE.is_match(name) {
        return true;
    }
    
    // Default to kebab-case
    KEBAB_RE.is_match(name)
}

impl_rule!(
    NamingConventions,
    "naming-conventions",
    "Naming Conventions",
    "Enforces consistent naming conventions for identifiers",
    RuleCategory::Style
);

/// Check line length
#[derive(Default)]
pub struct LineLength {
    max_length: usize,
}

impl LineLength {
    fn visitor(&self) -> LineLengthVisitor {
        LineLengthVisitor {
            collector: DiagnosticCollector::new("line-length"),
            _max_length: if self.max_length == 0 { 100 } else { self.max_length },
        }
    }
}

struct LineLengthVisitor {
    collector: DiagnosticCollector,
    _max_length: usize,
}

impl Visitor for LineLengthVisitor {
    fn visit_node(&mut self, _graph: &Graph, _node_id: NodeId, _node: &Node) {
        // Line length checking would require source location info
        // This is a placeholder - real implementation would check source text
    }
}

impl LineLengthVisitor {
    fn finish(self) -> Vec<LintDiagnostic> {
        self.collector.finish()
    }
}

impl_rule!(
    LineLength,
    "line-length",
    "Line Length",
    "Enforces maximum line length",
    RuleCategory::Style
);

/// Check for unused imports
#[derive(Default)]
pub struct UnusedImports;

impl UnusedImports {
    fn visitor(&self) -> UnusedImportsVisitor {
        UnusedImportsVisitor {
            collector: DiagnosticCollector::new("unused-imports"),
            imports: Vec::new(),
            used_names: Vec::new(),
        }
    }
}

struct UnusedImportsVisitor {
    collector: DiagnosticCollector,
    imports: Vec<(NodeId, Vec<String>)>,
    used_names: Vec<String>,
}

impl Visitor for UnusedImportsVisitor {
    fn visit_node(&mut self, graph: &Graph, node_id: NodeId, node: &Node) {
        match node {
            Node::Import { import_list, .. } => {
                let names: Vec<String> = import_list.iter()
                    .map(|item| item.alias.as_ref().unwrap_or(&item.name).clone())
                    .collect();
                self.imports.push((node_id, names));
            }
            Node::Variable { name } => {
                self.used_names.push(name.clone());
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
            Node::List(items) => {
                for item in items {
                    self.visit_node_id(graph, *item);
                }
            }
            _ => {}
        }
    }
}

impl UnusedImportsVisitor {
    fn finish(mut self) -> Vec<LintDiagnostic> {
        // Check which imports are unused
        for (_node_id, import_names) in self.imports {
            for name in import_names {
                if !self.used_names.contains(&name) {
                    self.collector.add_diagnostic(
                        LintDiagnostic::warning(
                            "unused-imports",
                            format!("Unused import: '{}'", name)
                        )
                    );
                }
            }
        }
        
        self.collector.finish()
    }
}

impl_rule!(
    UnusedImports,
    "unused-imports",
    "Unused Imports",
    "Detects unused import statements",
    RuleCategory::Style
);