//! Core types and data structures for ClaudeLang
//!
//! This crate provides the fundamental building blocks used throughout
//! the ClaudeLang implementation, including:
//! - AST representation
//! - Effect system types
//! - Value representation
//! - Error types

pub mod ast;
pub mod documentation;
pub mod effects;
pub mod error;
pub mod value;

pub use error::{Error, Result};