//! High-performance S-expression parser for FluentAi
//!
//! This parser is designed for maximum performance:
//! - Zero-copy parsing where possible
//! - Arena allocation for AST nodes
//! - Optimized lexer using logos
//! - Minimal allocations

pub mod lexer;
pub mod parser;
pub mod error;

#[cfg(test)]
mod integration_tests;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_coverage_tests;

pub use parser::{Parser, ParseResult};
pub use error::{ParseError, ErrorKind};

use fluentai_core::ast::Graph;

/// Parse FluentAi source code into an AST graph
pub fn parse(source: &str) -> Result<Graph, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse()
}

/// Parse with custom allocator for better performance
pub fn parse_with_arena<'a>(source: &'a str, arena: &'a bumpalo::Bump) -> Result<Graph, ParseError> {
    let mut parser = Parser::with_arena(source, arena);
    parser.parse()
}