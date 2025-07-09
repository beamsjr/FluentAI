//! FluentAI Runtime Library
//!
//! This crate provides an embeddable runtime for executing FluentAI programs.
//! It includes the VM, module loading, host function registration, and 
//! configuration management.

pub mod config;
pub mod context;
pub mod engine;
pub mod error;
pub mod host;
pub mod loader;
pub mod module;

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