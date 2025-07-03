//! Core types and data structures for FluentAi
//!
//! This crate provides the fundamental building blocks used throughout
//! the FluentAi implementation, including:
//! - AST representation
//! - Effect system types
//! - Value representation
//! - Error types

pub mod ast;
pub mod documentation;
pub mod effects;
pub mod error;
pub mod value;
pub mod traits;

pub use error::{Error, Result};