//! ClaudeLang Interactive REPL
//!
//! This crate provides an interactive Read-Eval-Print Loop for ClaudeLang
//! with features like syntax highlighting, auto-completion, and multiple
//! execution modes.

pub mod repl;
pub mod completer;
pub mod highlighter;
pub mod commands;
pub mod history;
pub mod environment;
pub mod error;

pub use repl::{Repl, ReplConfig};
pub use environment::{ReplEnvironment, ExecutionMode};
pub use error::{ReplError, ReplResult};

// Re-export key dependencies
pub use rustyline;