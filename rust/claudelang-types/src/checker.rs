//! Type checking with readable error messages

use crate::{
    environment::TypeEnvironment,
    inference::{TypeInferencer, TypeError},
    types::*,
};
use claudelang_core::ast::{EffectType, Graph, NodeId};
use rustc_hash::FxHashMap;
use std::collections::HashSet;

/// Type checker that provides readable error messages
pub struct TypeChecker {
    /// Type inferencer
    inferencer: TypeInferencer,
    /// Source locations for error reporting
    locations: FxHashMap<NodeId, SourceLocation>,
    /// Type annotations from source
    annotations: FxHashMap<NodeId, TypedValue>,
}

/// Source location for error reporting
#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl SourceLocation {
    pub fn unknown() -> Self {
        Self {
            file: "<unknown>".to_string(),
            line: 0,
            column: 0,
        }
    }
}

/// Type checking result
#[derive(Debug)]
pub struct TypeCheckResult {
    /// Inferred types for all nodes
    pub types: FxHashMap<NodeId, TypedValue>,
    /// Type errors with locations
    pub errors: Vec<TypeCheckError>,
    /// Warnings
    pub warnings: Vec<TypeCheckWarning>,
    /// Overall success
    pub success: bool,
}

/// Type check error with source location
#[derive(Debug, Clone)]
pub struct TypeCheckError {
    pub location: SourceLocation,
    pub error: TypeError,
    pub context: String,
}

impl std::fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: error: {}\n  {}",
            self.location.file,
            self.location.line,
            self.location.column,
            self.error,
            self.context
        )
    }
}

/// Type check warning
#[derive(Debug, Clone)]
pub struct TypeCheckWarning {
    pub location: SourceLocation,
    pub message: String,
}

impl std::fmt::Display for TypeCheckWarning {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}:{}:{}: warning: {}",
            self.location.file,
            self.location.line,
            self.location.column,
            self.message
        )
    }
}

impl TypeChecker {
    /// Create a new type checker
    pub fn new() -> Self {
        Self {
            inferencer: TypeInferencer::new(),
            locations: FxHashMap::default(),
            annotations: FxHashMap::default(),
        }
    }

    /// Create with a custom environment
    pub fn with_env(env: TypeEnvironment) -> Self {
        Self {
            inferencer: TypeInferencer::with_env(env),
            locations: FxHashMap::default(),
            annotations: FxHashMap::default(),
        }
    }

    /// Add source location for a node
    pub fn add_location(&mut self, node_id: NodeId, location: SourceLocation) {
        self.locations.insert(node_id, location);
    }

    /// Add type annotation for a node
    pub fn add_annotation(&mut self, node_id: NodeId, ty: TypedValue) {
        self.annotations.insert(node_id, ty);
    }

    /// Type check a graph
    pub fn check(&mut self, graph: &Graph) -> TypeCheckResult {
        // Run type inference
        let types = match self.inferencer.infer_graph(graph) {
            Ok(types) => types,
            Err(e) => {
                return TypeCheckResult {
                    types: FxHashMap::default(),
                    errors: vec![TypeCheckError {
                        location: SourceLocation::unknown(),
                        error: TypeError::UnificationFailure(
                            "Type inference failed".to_string(),
                            e.to_string(),
                        ),
                        context: "During type inference".to_string(),
                    }],
                    warnings: Vec::new(),
                    success: false,
                };
            }
        };

        // Check annotations match inferred types
        let mut errors = Vec::new();
        for (node_id, annotated_type) in &self.annotations {
            if let Some(inferred_type) = types.get(node_id) {
                if !self.types_compatible(annotated_type, inferred_type) {
                    errors.push(TypeCheckError {
                        location: self.get_location(*node_id),
                        error: TypeError::TypeMismatch {
                            expected: annotated_type.to_string(),
                            found: inferred_type.to_string(),
                        },
                        context: "Type annotation doesn't match inferred type".to_string(),
                    });
                }
            }
        }

        // Convert inference errors to type check errors
        for error in self.inferencer.errors() {
            errors.push(TypeCheckError {
                location: SourceLocation::unknown(),
                error: error.clone(),
                context: String::new(),
            });
        }

        // Generate warnings
        let warnings = self.generate_warnings(&types, graph);

        let success = errors.is_empty();
        
        TypeCheckResult {
            types,
            errors,
            warnings,
            success,
        }
    }

    /// Check effects against allowed set
    pub fn check_effects(
        &self,
        _graph: &Graph,
        types: &FxHashMap<NodeId, TypedValue>,
        allowed_effects: &HashSet<EffectType>,
    ) -> Vec<TypeCheckError> {
        let mut errors = Vec::new();

        // Collect all effects used
        let mut used_effects = HashSet::new();
        for ty in types.values() {
            used_effects.extend(ty.effects.iter().copied());
        }

        // Remove Pure effect as it's always allowed
        used_effects.remove(&EffectType::Pure);

        // Check for forbidden effects
        let forbidden: HashSet<_> = used_effects.difference(allowed_effects).copied().collect();
        
        if !forbidden.is_empty() {
            let effect_list = forbidden.iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join(", ");

            errors.push(TypeCheckError {
                location: SourceLocation::unknown(),
                error: TypeError::EffectConstraintViolation(
                    format!("Forbidden effects used: {}", effect_list)
                ),
                context: "Effects must be explicitly allowed".to_string(),
            });
        }

        errors
    }

    /// Check if two types are compatible (accounting for subtyping)
    fn types_compatible(&self, expected: &TypedValue, actual: &TypedValue) -> bool {
        // For now, just check exact equality
        // Full implementation would handle subtyping
        expected == actual
    }

    /// Get location for a node
    fn get_location(&self, node_id: NodeId) -> SourceLocation {
        self.locations.get(&node_id)
            .cloned()
            .unwrap_or_else(SourceLocation::unknown)
    }

    /// Generate warnings for potential issues
    fn generate_warnings(
        &self,
        types: &FxHashMap<NodeId, TypedValue>,
        _graph: &Graph,
    ) -> Vec<TypeCheckWarning> {
        let mut warnings = Vec::new();

        for (node_id, ty) in types {
            // Warn about unused type variables
            if let TypedValueInner::Variable(var) = &ty.inner {
                if var.constraints.is_empty() {
                    warnings.push(TypeCheckWarning {
                        location: self.get_location(*node_id),
                        message: format!(
                            "Type variable '{}' has no constraints and could be any type",
                            var.name
                        ),
                    });
                }
            }

            // Warn about implicit effects
            if !ty.is_pure() && !ty.effects.is_empty() {
                let effect_list = ty.effects.iter()
                    .filter(|e| **e != EffectType::Pure)
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");

                warnings.push(TypeCheckWarning {
                    location: self.get_location(*node_id),
                    message: format!("Expression has side effects: {}", effect_list),
                });
            }
        }

        warnings
    }

    /// Format type error with context
    pub fn format_error(&self, error: &TypeCheckError, graph: &Graph) -> String {
        let mut result = error.to_string();

        // Add source context if available
        if let Some(node) = graph.get_node(NodeId(error.location.line as u32)) {
            result.push_str(&format!("\n  in expression: {:?}", node));
        }

        result
    }

    /// Get inferred type for a node
    pub fn get_type(&self, _node_id: NodeId) -> Option<TypedValue> {
        // Return None for now, as we don't have direct access to node_types
        // In a full implementation, we'd store the types from the last check
        None
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for type checker with annotations
pub struct TypeCheckerBuilder {
    checker: TypeChecker,
}

impl TypeCheckerBuilder {
    pub fn new() -> Self {
        Self {
            checker: TypeChecker::new(),
        }
    }

    pub fn with_env(env: TypeEnvironment) -> Self {
        Self {
            checker: TypeChecker::with_env(env),
        }
    }

    pub fn with_location(mut self, node_id: NodeId, location: SourceLocation) -> Self {
        self.checker.add_location(node_id, location);
        self
    }

    pub fn with_annotation(mut self, node_id: NodeId, ty: TypedValue) -> Self {
        self.checker.add_annotation(node_id, ty);
        self
    }

    pub fn build(self) -> TypeChecker {
        self.checker
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use claudelang_parser::parse;

    fn check_code(code: &str) -> TypeCheckResult {
        let graph = parse(code).unwrap();
        let mut checker = TypeChecker::new();
        checker.check(&graph)
    }

    #[test]
    fn test_successful_type_check() {
        let result = check_code("(+ 1 2)");
        assert!(result.success);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_type_error() {
        let result = check_code("(+ 1 \"hello\")");
        assert!(!result.success);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_effect_checking() {
        let graph = parse("(print \"hello\")").unwrap();
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);
        
        // Check with no IO allowed
        let allowed = HashSet::new();
        let effect_errors = checker.check_effects(&graph, &result.types, &allowed);
        assert!(!effect_errors.is_empty());
        
        // Check with IO allowed
        let mut allowed = HashSet::new();
        allowed.insert(EffectType::IO);
        let effect_errors = checker.check_effects(&graph, &result.types, &allowed);
        assert!(effect_errors.is_empty());
    }

    #[test]
    fn test_warnings() {
        let result = check_code("(lambda (x) x)");
        // Should warn about unconstrained type variable
        assert!(!result.warnings.is_empty());
    }
}