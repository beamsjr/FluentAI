//! Error types for the REPL

use thiserror::Error;
use claudelang_interpreter::InterpreterError;
use claudelang_parser::ParseError;
use rustyline::error::ReadlineError;
use std::io;

/// Result type for REPL operations
pub type ReplResult<T> = Result<T, ReplError>;

/// REPL-specific errors
#[derive(Debug, Error)]
pub enum ReplError {
    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] io::Error),

    /// Parse error
    #[error("Parse error: {0}")]
    Parse(#[from] ParseError),

    /// Interpreter error
    #[error("Interpreter error: {0}")]
    Interpreter(#[from] InterpreterError),

    /// Command error
    #[error("Command error: {0}")]
    Command(String),

    /// Configuration error
    #[error("Configuration error: {0}")]
    Config(String),

    /// History error
    #[error("History error: {0}")]
    History(String),

    /// Completion error
    #[error("Completion error: {0}")]
    Completion(String),

    /// Highlight error
    #[error("Highlight error: {0}")]
    Highlight(String),

    /// Debug error
    #[error("Debug error: {0}")]
    Debug(String),

    /// Mode switch error
    #[error("Mode switch error: {0}")]
    ModeSwitch(String),

    /// General error
    #[error("{0}")]
    General(String),
    
    /// Readline error
    #[error("Readline error: {0}")]
    Readline(#[from] ReadlineError),
}

impl From<String> for ReplError {
    fn from(s: String) -> Self {
        ReplError::General(s)
    }
}

impl From<&str> for ReplError {
    fn from(s: &str) -> Self {
        ReplError::General(s.to_string())
    }
}