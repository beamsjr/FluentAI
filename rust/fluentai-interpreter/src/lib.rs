//! FluentAi Tree-Walking Interpreter
//!
//! This crate provides a direct AST interpretation engine for FluentAi
//! with support for debugging, provenance tracking, and multiple execution modes.

pub mod async_runtime;
pub mod debug;
pub mod environment;
pub mod error;
pub mod interpreter;
pub mod provenance;
pub mod value;

pub use debug::{Breakpoint, DebugMode, StepMode};
pub use environment::{Binding, Environment};
pub use error::{InterpreterError, InterpreterResult};
pub use interpreter::{ExecutionMode, Interpreter, InterpreterOptions};
pub use provenance::{ProvenanceInfo, SourceLocation};
pub use value::{Closure, Value};

// Re-export commonly used types
pub use fluentai_core::ast::{Graph, Node, NodeId};
