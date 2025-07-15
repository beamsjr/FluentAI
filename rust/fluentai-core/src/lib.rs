//! Core types and data structures for FluentAi
//!
//! This crate provides the fundamental building blocks used throughout
//! the FluentAi implementation, including:
//! - AST representation
//! - Value representation
//! - Error types
//! - Documentation system

#![warn(missing_docs)]

pub mod ast;
pub mod documentation;
pub mod error;
pub mod thread_pool;
pub mod traits;
pub mod value;
pub mod work_stealing_scheduler;

pub use ast::{AstHashMap, AstHashSet};
pub use error::{Error, Result};
pub use thread_pool::{ThreadPool, ThreadPoolBuilder, ThreadPoolConfig};
