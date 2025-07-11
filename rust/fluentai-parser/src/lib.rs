//! High-performance S-expression parser for FluentAi
//!
//! This parser is designed for maximum performance:
//! - Zero-copy parsing where possible
//! - Arena allocation for AST nodes
//! - Optimized lexer using logos
//! - Minimal allocations

pub mod error;
pub mod flc_lexer;
pub mod iterative_parser;
pub mod lexer;
pub mod parser;
pub mod threaded_parser;

#[cfg(test)]
mod integration_tests;

#[cfg(test)]
mod parser_tests;

#[cfg(test)]
mod lexer_tests;

#[cfg(test)]
mod parser_coverage_tests;

#[cfg(test)]
mod parser_depth_tests;

#[cfg(test)]
mod iterative_integration_tests;

#[cfg(test)]
mod parsing_strategies_test;

#[cfg(test)]
mod lib_tests;

pub use error::{ErrorKind, ParseError};
pub use iterative_parser::IterativeParser;
pub use parser::{ParseResult, Parser};
pub use threaded_parser::{
    parse_threaded, parse_with_stack_and_depth, parse_with_stack_size, ThreadedParserConfig,
};

use fluentai_core::ast::Graph;

/// Parse FluentAi source code into an AST graph
pub fn parse(source: &str) -> Result<Graph, ParseError> {
    let mut parser = Parser::new(source);
    parser.parse()
}

/// Parse with custom allocator for better performance
pub fn parse_with_arena<'a>(
    source: &'a str,
    arena: &'a bumpalo::Bump,
) -> Result<Graph, ParseError> {
    let mut parser = Parser::with_arena(source, arena);
    parser.parse()
}

/// Parse with custom depth limit to prevent stack overflow
pub fn parse_with_depth_limit(source: &str, max_depth: usize) -> Result<Graph, ParseError> {
    let mut parser = Parser::new(source).with_max_depth(max_depth);
    parser.parse()
}

/// Parse using iterative parser for extremely deep nesting
/// This parser uses an explicit stack instead of recursion
pub fn parse_iterative(source: &str) -> Result<Graph, ParseError> {
    let parser = IterativeParser::new(source);
    parser.parse()
}

/// Parse iteratively with custom depth limit
pub fn parse_iterative_with_depth(source: &str, max_depth: usize) -> Result<Graph, ParseError> {
    let parser = IterativeParser::new(source).with_max_depth(max_depth);
    parser.parse()
}
