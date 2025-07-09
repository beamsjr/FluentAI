//! Documentation system for FluentAi

pub mod builtins;
pub mod impls;
pub mod registry;
pub mod traits;

#[cfg(test)]
mod tests;

pub use registry::DocumentationRegistry;
pub use traits::{
    Associativity, BuiltinDoc, Documentation, DocumentationCategory, DocumentationVisibility,
    DocumentedNode, KeywordDoc, OperatorDoc, UserFacingFeature,
};
