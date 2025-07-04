//! FluentAi Metaprogramming System
//!
//! This crate provides:
//! - Macro system for compile-time code generation
//! - Graph query language for pattern matching on AST
//! - Code transformation and rewriting
//! - Template-based code generation

pub mod macros;
pub mod query;
pub mod transform;
pub mod template;
pub mod patterns;
pub mod error;

pub use macros::{MacroExpander, MacroDefinition};
pub use query::{GraphQuery, QueryResult};
pub use patterns::Pattern;
pub use transform::{Transformer, TransformRule};
pub use template::{Template, TemplateEngine};
pub use error::{MetaprogrammingError, Result};