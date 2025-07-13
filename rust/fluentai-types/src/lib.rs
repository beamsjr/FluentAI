//! FluentAi Type System
//!
//! This crate implements a complete type system for FluentAi including:
//!
//! - **Type Representation**: Basic types, ADTs, effect types, probabilistic and temporal types
//! - **Type Inference**: Hindley-Milner type inference with let-polymorphism
//! - **Type Checking**: Validation with readable error messages
//! - **Effect Tracking**: Automatic tracking of side effects
//!
//! # Example
//!
//! ```no_run
//! use fluentai_types::{TypeChecker, TypeEnvironment};
//! use fluentai_parser::parse_flc;
//!
//! let code = "(lambda (x) (+ x 1))";
//! let graph = parse_flc(code).unwrap();
//!
//! let mut checker = TypeChecker::new();
//! let result = checker.check(&graph);
//!
//! if result.success {
//!     for (node_id, ty) in result.types {
//!         println!("Node {:?}: {}", node_id, ty);
//!     }
//! } else {
//!     for error in result.errors {
//!         eprintln!("{}", error);
//!     }
//! }
//! ```

#![warn(missing_docs)]

pub mod checker;
pub mod environment;
pub mod inference;
pub mod types;
pub mod unification;

// Re-export main types
pub use checker::{TypeCheckError, TypeCheckResult, TypeCheckWarning, TypeChecker};
pub use environment::{TypeEnvironment, TypeEnvironmentBuilder};
pub use inference::{TypeError, TypeInferencer};
pub use types::{
    EffectTypeWrapper, FunctionType, ListType, PrimitiveType, RecordType, TemporalType, TupleType,
    TypeConstraint, TypeKind, TypeVariable, TypedValue, UncertainType, VariantType,
};
pub use unification::{Substitution, UnificationError, Unifier};

use anyhow::Result;
use fluentai_core::ast::Graph;
use rustc_hash::FxHashMap;

/// Quick type inference for a graph
pub fn infer_types(graph: &Graph) -> Result<FxHashMap<fluentai_core::ast::NodeId, TypedValue>> {
    let mut inferencer = TypeInferencer::new();
    inferencer.infer_graph(graph)
}

/// Quick type checking for a graph
pub fn type_check(graph: &Graph) -> TypeCheckResult {
    let mut checker = TypeChecker::new();
    checker.check(graph)
}

/// Create a basic type environment with primitives
pub fn basic_env() -> TypeEnvironment {
    TypeEnvironment::new()
}

#[cfg(test)]
mod inference_tests;

#[cfg(test)]
mod unification_tests;

#[cfg(test)]
mod checker_tests;

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_parser::parse_flc;

    #[test]
    fn test_basic_type_inference() {
        let code = "1 + 2";
        let graph = parse_flc(code).unwrap();
        let types = infer_types(&graph).unwrap();

        assert!(!types.is_empty());
        if let Some(root_id) = graph.root_id {
            let root_type = &types[&root_id];
            assert_eq!(root_type.to_string(), "Int");
        }
    }

    #[test]
    fn test_function_type_inference() {
        let code = "(x, y) => x + y";
        let graph = parse_flc(code).unwrap();
        let types = infer_types(&graph).unwrap();

        if let Some(root_id) = graph.root_id {
            let root_type = &types[&root_id];
            // Should be a function type
            assert_eq!(root_type.kind(), TypeKind::Function);
        }
    }

    #[test]
    fn test_type_checking() {
        let code = "if (true) { 1 } else { 2 }";
        let graph = parse_flc(code).unwrap();
        let result = type_check(&graph);

        assert!(result.success);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_type_error_detection() {
        let code = "1 + \"hello\"";
        let graph = parse_flc(code).unwrap();
        let result = type_check(&graph);

        assert!(!result.success);
        assert!(!result.errors.is_empty());
    }
}
