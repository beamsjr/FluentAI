//! FluentAi Tree-Walking Interpreter
//! 
//! This crate provides a direct AST interpretation engine for FluentAi
//! with support for debugging, provenance tracking, and multiple execution modes.

pub mod value;
pub mod environment;
pub mod interpreter;
pub mod error;
pub mod debug;
pub mod provenance;
pub mod async_runtime;

pub use value::{Value, Closure};
pub use environment::{Environment, Binding};
pub use interpreter::{Interpreter, InterpreterOptions, ExecutionMode};
pub use error::{InterpreterError, InterpreterResult};
pub use debug::{DebugMode, Breakpoint, StepMode};
pub use provenance::{ProvenanceInfo, SourceLocation};

// Re-export commonly used types
pub use fluentai_core::ast::{Graph, Node, NodeId};