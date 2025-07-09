//! Threaded parser with configurable stack size
//!
//! This module provides a way to run the parser in a separate thread
//! with a custom stack size, allowing parsing of deeply nested structures
//! that would otherwise overflow the default thread stack.

use crate::error::ParseError;
use crate::parser::Parser;
use fluentai_core::ast::Graph;
use std::sync::mpsc;
use std::thread;

/// Configuration for threaded parsing
#[derive(Debug, Clone)]
pub struct ThreadedParserConfig {
    /// Stack size in bytes for the parser thread
    pub stack_size: usize,
    /// Maximum parsing depth
    pub max_depth: usize,
    /// Thread name (for debugging)
    pub thread_name: String,
}

impl Default for ThreadedParserConfig {
    fn default() -> Self {
        Self {
            stack_size: 8 * 1024 * 1024, // 8MB default
            max_depth: 5000,
            thread_name: "fluentai-parser".to_string(),
        }
    }
}

impl ThreadedParserConfig {
    /// Create a new configuration with the specified stack size
    pub fn with_stack_size(mut self, size: usize) -> Self {
        self.stack_size = size;
        self
    }

    /// Set the maximum parsing depth
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Set the thread name
    pub fn with_thread_name(mut self, name: String) -> Self {
        self.thread_name = name;
        self
    }
}

/// Parse source code in a separate thread with custom stack size
pub fn parse_threaded(source: String, config: ThreadedParserConfig) -> Result<Graph, ParseError> {
    let (tx, rx) = mpsc::channel();

    let builder = thread::Builder::new()
        .name(config.thread_name)
        .stack_size(config.stack_size);

    let handle = builder
        .spawn(move || {
            let result = Parser::new(&source)
                .with_max_depth(config.max_depth)
                .parse();
            let _ = tx.send(result);
        })
        .map_err(|e| ParseError::InvalidSyntax(format!("Failed to spawn parser thread: {}", e)))?;

    // Wait for the parser to complete
    handle
        .join()
        .map_err(|_| ParseError::InvalidSyntax("Parser thread panicked".to_string()))?;

    // Get the result from the channel
    rx.recv()
        .map_err(|_| ParseError::InvalidSyntax("Failed to receive parser result".to_string()))?
}

/// Convenience function for parsing with a specific stack size
pub fn parse_with_stack_size(source: &str, stack_size: usize) -> Result<Graph, ParseError> {
    let config = ThreadedParserConfig::default().with_stack_size(stack_size);
    parse_threaded(source.to_string(), config)
}

/// Parse with both custom stack size and depth limit
pub fn parse_with_stack_and_depth(
    source: &str,
    stack_size: usize,
    max_depth: usize,
) -> Result<Graph, ParseError> {
    let config = ThreadedParserConfig::default()
        .with_stack_size(stack_size)
        .with_max_depth(max_depth);
    parse_threaded(source.to_string(), config)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_threaded_parser_simple() {
        let result = parse_with_stack_size("(+ 1 2)", 1024 * 1024);
        assert!(result.is_ok());
    }

    #[test]
    fn test_threaded_parser_custom_config() {
        let config = ThreadedParserConfig::default()
            .with_stack_size(2 * 1024 * 1024)
            .with_max_depth(100)
            .with_thread_name("test-parser".to_string());

        let result = parse_threaded("(list 1 2 3)".to_string(), config);
        assert!(result.is_ok());
    }

    #[test]
    fn test_threaded_parser_deep_nesting() {
        // Create a deeply nested expression
        let mut expr = String::new();
        let depth = 500;

        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }

        // Parse with large stack and adequate depth limit
        // Note: Each nesting level requires multiple parser calls
        let result = parse_with_stack_and_depth(&expr, 16 * 1024 * 1024, 2500);

        if let Err(ref e) = result {
            println!("Parse error: {:?}", e);
        }

        assert!(result.is_ok(), "Failed to parse: {:?}", result);
    }

    #[test]
    fn test_threaded_parser_respects_depth_limit() {
        // Create a deeply nested expression
        let mut expr = String::new();
        let depth = 200;

        for _ in 0..depth {
            expr.push_str("(+ ");
        }
        expr.push('1');
        for _ in 0..depth {
            expr.push_str(" 2)");
        }

        // Parse with depth limit lower than actual depth
        let result = parse_with_stack_and_depth(&expr, 8 * 1024 * 1024, 100);
        assert!(matches!(result, Err(ParseError::MaxDepthExceeded { .. })));
    }

    #[test]
    fn test_threaded_parser_handles_errors() {
        let result = parse_with_stack_size("(+ 1", 1024 * 1024);
        assert!(result.is_err());
    }
}
