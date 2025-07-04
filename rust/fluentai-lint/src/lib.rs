//! FluentAi Linter
//!
//! A comprehensive linting framework for FluentAi code that provides:
//! - Built-in rules for common patterns and issues
//! - Extensible rule system
//! - Rich diagnostic output with suggestions
//! - Configuration via .fluentai-lint.toml

pub mod rules;
pub mod diagnostic;
pub mod config;
pub mod engine;
pub mod visitor;

pub use engine::{LintEngine, LintResult};
pub use config::{LintConfig, RuleLevel};
pub use diagnostic::{LintDiagnostic, DiagnosticKind, Suggestion};
pub use rules::{Rule, RuleCategory};