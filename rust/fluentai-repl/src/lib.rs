//! FluentAi Interactive REPL
//!
//! This crate provides an interactive Read-Eval-Print Loop for FluentAi
//! with features like syntax highlighting, auto-completion, and multiple
//! execution modes.

pub mod commands;
pub mod completer;
pub mod environment;
pub mod error;
pub mod highlighter;
pub mod history;
pub mod repl;

pub use environment::{ExecutionMode, ReplEnvironment};
pub use error::{ReplError, ReplResult};
pub use repl::{Repl, ReplConfig};

// Re-export key dependencies
pub use rustyline;
