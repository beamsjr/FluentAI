//! Documentation system for FluentAi

pub mod traits;
pub mod impls;
pub mod registry;
pub mod builtins;

#[cfg(test)]
mod tests;

pub use traits::{Documentation, DocumentedNode, DocumentationCategory, DocumentationVisibility, OperatorDoc, KeywordDoc, BuiltinDoc, Associativity, UserFacingFeature};
pub use registry::DocumentationRegistry;