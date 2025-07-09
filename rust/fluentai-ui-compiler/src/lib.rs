//! UI Compiler for FluentAi
//!
//! This module compiles FluentAi UI code to JavaScript that can run in browsers.
//! It supports various output formats including standalone JS, React components,
//! and Web Components.

pub mod codegen;
pub mod compiler;
// Modules to be implemented later
// pub mod optimizer;
// pub mod runtime;
pub mod error;

pub use compiler::{CompilerOptions, OutputFormat, UICompiler};
pub use error::{CompilerError, Result};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_creation() {
        let compiler = UICompiler::new(CompilerOptions::default());
        assert!(compiler.is_ok());
    }
}
