//! Native code generation backend for FluentAI
//!
//! This crate provides ahead-of-time (AOT) compilation of FluentAI programs
//! to native executables and libraries. It builds on the JIT infrastructure
//! but generates standalone binaries rather than runtime-compiled code.

#![warn(missing_docs)]

use anyhow::Result;
use fluentai_core::ast::Graph;
use std::path::Path;

pub mod compiler;
pub mod linker;
pub mod target;
pub mod value_abi;

pub use compiler::NativeCompiler;
pub use target::{CompilationTarget, OutputFormat};

/// Options for native code generation
#[derive(Debug, Clone)]
pub struct CodegenOptions {
    /// Target triple (e.g., "x86_64-unknown-linux-gnu")
    pub target: String,
    
    /// Optimization level (0-3)
    pub opt_level: u8,
    
    /// Output format
    pub output_format: OutputFormat,
    
    /// Enable debug information
    pub debug_info: bool,
    
    /// Enable link-time optimization
    pub lto: bool,
    
    /// Additional compiler flags
    pub flags: Vec<String>,
}

impl Default for CodegenOptions {
    fn default() -> Self {
        Self {
            target: target_lexicon::HOST.to_string(),
            opt_level: 2,
            output_format: OutputFormat::Executable,
            debug_info: cfg!(debug_assertions),
            lto: false,
            flags: Vec::new(),
        }
    }
}

/// Main entry point for native code generation
pub fn compile_to_native(
    ast: &Graph,
    output_path: &Path,
    options: CodegenOptions,
) -> Result<()> {
    let compiler = NativeCompiler::new(options)?;
    compiler.compile(ast, output_path)
}

/// Compile multiple modules and link them together
pub fn compile_and_link(
    modules: Vec<(&str, &Graph)>,
    output_path: &Path,
    options: CodegenOptions,
) -> Result<()> {
    let compiler = NativeCompiler::new(options)?;
    compiler.compile_modules(modules, output_path)
}