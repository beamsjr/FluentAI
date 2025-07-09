//! FluentAi Linter
//!
//! A comprehensive linting framework for FluentAi code that provides:
//! - Built-in rules for common patterns and issues
//! - Extensible rule system
//! - Rich diagnostic output with suggestions
//! - Configuration via .fluentai-lint.toml

pub mod config;
pub mod diagnostic;
pub mod engine;
pub mod rules;
pub mod visitor;

pub use config::{LintConfig, RuleLevel};
pub use diagnostic::{DiagnosticKind, LintDiagnostic, Suggestion};
pub use engine::{LintEngine, LintResult};
pub use rules::{Rule, RuleCategory};
