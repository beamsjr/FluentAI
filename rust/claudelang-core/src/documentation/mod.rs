//! Documentation system for ClaudeLang

pub mod traits;
pub mod impls;
pub mod registry;

#[cfg(test)]
mod tests;

pub use traits::{Documentation, DocumentedNode, DocumentationCategory, OperatorDoc, KeywordDoc, BuiltinDoc, Associativity};
pub use registry::DocumentationRegistry;