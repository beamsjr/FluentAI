//! Lint rules for FluentAi

use crate::diagnostic::LintDiagnostic;
use fluentai_core::ast::Graph;
use serde::{Deserialize, Serialize};
use std::any::Any;

// Rule modules
pub mod correctness;
pub mod performance;
pub mod security;
pub mod style;

/// Category of lint rules
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum RuleCategory {
    /// Style and formatting rules
    Style,
    /// Correctness and bug-finding rules
    Correctness,
    /// Performance optimization rules
    Performance,
    /// Security-related rules
    Security,
    /// Complexity and maintainability rules
    Complexity,
}

/// Trait for lint rules
pub trait Rule: Send + Sync {
    /// Unique identifier for the rule
    fn id(&self) -> &'static str;

    /// Human-readable name
    fn name(&self) -> &'static str;

    /// Description of what the rule checks
    fn description(&self) -> &'static str;

    /// Category of the rule
    fn category(&self) -> RuleCategory;

    /// Check the graph and return diagnostics
    fn check(&self, graph: &Graph) -> Vec<LintDiagnostic>;

    /// Get configuration schema (if any)
    fn config_schema(&self) -> Option<serde_json::Value> {
        None
    }

    /// Configure the rule with a value
    fn configure(&mut self, _config: &toml::Value) -> anyhow::Result<()> {
        Ok(())
    }

    /// Convert to Any for downcasting
    fn as_any(&self) -> &dyn Any;
}

/// Helper macro to implement common Rule trait methods
#[macro_export]
macro_rules! impl_rule {
    ($name:ident, $id:expr, $display_name:expr, $desc:expr, $category:expr) => {
        impl Rule for $name {
            fn id(&self) -> &'static str {
                $id
            }

            fn name(&self) -> &'static str {
                $display_name
            }

            fn description(&self) -> &'static str {
                $desc
            }

            fn category(&self) -> RuleCategory {
                $category
            }

            fn as_any(&self) -> &dyn std::any::Any {
                self
            }

            fn check(&self, graph: &Graph) -> Vec<LintDiagnostic> {
                let mut visitor = self.visitor();
                visitor.visit_graph(graph);
                visitor.finish()
            }
        }
    };
}

/// Base visitor for collecting diagnostics
pub struct DiagnosticCollector {
    pub diagnostics: Vec<LintDiagnostic>,
    pub rule_id: String,
}

impl DiagnosticCollector {
    pub fn new(rule_id: impl Into<String>) -> Self {
        Self {
            diagnostics: Vec::new(),
            rule_id: rule_id.into(),
        }
    }

    pub fn add_diagnostic(&mut self, diagnostic: LintDiagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn finish(self) -> Vec<LintDiagnostic> {
        self.diagnostics
    }
}

/// Get all built-in rules
pub fn get_builtin_rules() -> Vec<Box<dyn Rule>> {
    let mut rules: Vec<Box<dyn Rule>> = Vec::new();

    // Style rules
    rules.push(Box::new(style::NamingConventions::default()));
    rules.push(Box::new(style::LineLength::default()));
    rules.push(Box::new(style::UnusedImports::default()));

    // Correctness rules
    rules.push(Box::new(correctness::UnusedVariables::default()));
    rules.push(Box::new(correctness::UnreachableCode::default()));
    rules.push(Box::new(correctness::TypeMismatch::default()));

    // Performance rules
    rules.push(Box::new(performance::InefficientRecursion::default()));
    rules.push(Box::new(performance::UnusedComputation::default()));

    // Security rules
    rules.push(Box::new(security::UnsafeEffects::default()));
    rules.push(Box::new(security::UnvalidatedInput::default()));

    rules
}

/// Rule registry for managing available rules
pub struct RuleRegistry {
    rules: Vec<Box<dyn Rule>>,
}

impl RuleRegistry {
    pub fn new() -> Self {
        Self {
            rules: get_builtin_rules(),
        }
    }

    pub fn with_builtin_rules() -> Self {
        Self::new()
    }

    pub fn add_rule(&mut self, rule: Box<dyn Rule>) {
        self.rules.push(rule);
    }

    pub fn get_rule(&self, id: &str) -> Option<&dyn Rule> {
        self.rules.iter().find(|r| r.id() == id).map(|r| r.as_ref())
    }

    pub fn get_rules_by_category(&self, category: RuleCategory) -> Vec<&dyn Rule> {
        self.rules
            .iter()
            .filter(|r| r.category() == category)
            .map(|r| r.as_ref())
            .collect()
    }

    pub fn all_rules(&self) -> &[Box<dyn Rule>] {
        &self.rules
    }
}
