//! Parser error types

use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum ParseError {
    #[error("Unexpected token at position {position}: expected {expected}, found {found}")]
    UnexpectedToken {
        position: usize,
        expected: String,
        found: String,
    },
    
    #[error("Unexpected end of input")]
    UnexpectedEof,
    
    #[error("Invalid number literal: {0}")]
    InvalidNumber(String),
    
    #[error("Invalid escape sequence in string: {0}")]
    InvalidEscape(String),
    
    #[error("Unclosed delimiter: {0}")]
    UnclosedDelimiter(String),
    
    #[error("Invalid syntax: {0}")]
    InvalidSyntax(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    UnexpectedToken,
    UnexpectedEof,
    InvalidNumber,
    InvalidEscape,
    UnclosedDelimiter,
    InvalidSyntax,
}

impl ParseError {
    pub fn kind(&self) -> ErrorKind {
        match self {
            ParseError::UnexpectedToken { .. } => ErrorKind::UnexpectedToken,
            ParseError::UnexpectedEof => ErrorKind::UnexpectedEof,
            ParseError::InvalidNumber(_) => ErrorKind::InvalidNumber,
            ParseError::InvalidEscape(_) => ErrorKind::InvalidEscape,
            ParseError::UnclosedDelimiter(_) => ErrorKind::UnclosedDelimiter,
            ParseError::InvalidSyntax(_) => ErrorKind::InvalidSyntax,
        }
    }
}