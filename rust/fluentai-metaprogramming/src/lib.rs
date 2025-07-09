//! FluentAi Metaprogramming System
//!
//! This crate provides:
//! - Macro system for compile-time code generation
//! - Graph query language for pattern matching on AST
//! - Code transformation and rewriting
//! - Template-based code generation

pub mod error;
pub mod macros;
pub mod patterns;
pub mod query;
pub mod template;
pub mod transform;

pub use error::{MetaprogrammingError, Result};
pub use macros::{MacroDefinition, MacroExpander};
pub use patterns::Pattern;
pub use query::{GraphQuery, QueryResult};
pub use template::{Template, TemplateEngine};
pub use transform::{TransformRule, Transformer};
