//! High-performance FLC (Fluent Lambda Chain) parser for FluentAi
//!
//! This parser is designed for maximum performance and clarity.

pub mod error;
pub mod flc_lexer;
pub mod flc_parser;
pub mod flc_error;

#[cfg(test)]
mod flc_parser_tests;

#[cfg(test)]
mod lib_tests;

#[cfg(test)]
mod test_assignment_debug;

pub use error::{ErrorKind, ParseError};

use fluentai_core::ast::Graph;

/// Parse FluentAi source code into an AST graph
pub fn parse(source: &str) -> Result<Graph, ParseError> {
    parse_flc(source)
}

/// Parse FLC syntax into an AST graph
pub fn parse_flc(source: &str) -> Result<Graph, ParseError> {
    let parser = flc_parser::Parser::new(source);
    parser.parse().map_err(|e| ParseError::InvalidSyntax(e.to_string()))
}

/// Parse with custom allocator for better performance
pub fn parse_with_arena<'a>(
    source: &'a str,
    _arena: &'a bumpalo::Bump,
) -> Result<Graph, ParseError> {
    // FLC parser doesn't use arena allocator yet, just parse normally
    parse(source)
}
