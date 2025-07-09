//! FluentAI Core Library
//!
//! This crate provides the complete FluentAI runtime that gets embedded
//! into compiled applications. It includes:
//! - Full VM for bytecode execution
//! - JIT compiler for dynamic optimization
//! - Effect handlers and standard library
//! - Garbage collection
//! - Module loading system
//! 
//! Applications can be compiled as:
//! 1. Native code with embedded runtime (for dynamic features)
//! 2. Bytecode with embedded VM (smaller, portable)
//! 3. Hybrid (AOT compiled hot paths + bytecode for dynamic code)

pub mod aot;
pub mod config;
pub mod context;
pub mod embed;
pub mod engine;
pub mod error;
pub mod host;
pub mod loader;
pub mod module;

pub use aot::{AotCompiler, AotOptions, OutputFormat};
pub use config::{RuntimeConfig, ExecutionMode};
pub use context::RuntimeContext;
pub use engine::RuntimeEngine;
pub use error::{RuntimeError, Result};
pub use host::{HostFunction, HostRegistry};
pub use module::{Module, CompiledModule};

/// Re-export core types
pub use fluentai_core::value::Value;

/// Runtime version
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Prelude module for common imports
pub mod prelude {
    pub use crate::{
        RuntimeConfig, RuntimeContext, RuntimeEngine,
        RuntimeError, Result,
        Module, CompiledModule,
        HostFunction, HostRegistry,
    };
    pub use crate::Value;
}