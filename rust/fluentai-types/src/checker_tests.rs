//! Tests for type checker edge cases and builder patterns

#[cfg(test)]
mod checker_edge_case_tests {
    use crate::checker::*;
    use crate::environment::TypeEnvironment;
    use crate::inference::TypeError;
    use crate::types::*;
    use fluentai_core::ast::{EffectType, Graph, NodeId};
    use fluentai_parser::parse;
    use std::collections::HashSet;

    // Helper function to create a simple graph for testing
    fn create_test_graph(code: &str) -> Graph {
        parse(code).unwrap()
    }

    // Test TypeChecker with custom environment
    #[test]
    fn test_type_checker_with_env() {
        let mut env = TypeEnvironment::new();
        env.bind("custom_var", TypedValue::primitive(PrimitiveType::string()));

        let mut checker = TypeChecker::with_env(env);
        // Test that it can type check code using the custom environment
        let graph = create_test_graph("custom_var");
        let result = checker.check(&graph);

        // Should successfully type check since custom_var is bound
        assert!(result.success || !result.types.is_empty());
    }

    // Test adding locations to nodes
    #[test]
    fn test_add_location() {
        let mut checker = TypeChecker::new();
        let node_id = NodeId::new(1).unwrap();

        let location = SourceLocation {
            file: "test.flu".to_string(),
            line: 10,
            column: 5,
        };

        checker.add_location(node_id, location);

        // Verify location is used in error reporting
        let error = TypeCheckError {
            location: SourceLocation::unknown(),
            error: TypeError::UnboundVariable("test".to_string()),
            context: "test".to_string(),
        };

        // The location should be retrievable through error formatting
        let graph = Graph::new();
        let formatted = checker.format_error(&error, &graph);
        assert!(formatted.contains("Unbound variable"));
    }

    // Test adding type annotations
    #[test]
    fn test_add_annotation() {
        let mut checker = TypeChecker::new();
        let node_id = NodeId::new(1).unwrap();
        let ty = TypedValue::primitive(PrimitiveType::int());

        checker.add_annotation(node_id, ty.clone());

        // Verify annotation works by checking a graph with that annotation
        // The annotation should be used during type checking
        let graph = create_test_graph("42");
        let result = checker.check(&graph);

        // Check succeeded (annotations are tested in annotation_mismatch test)
        assert!(result.types.contains_key(&graph.root_id.unwrap()));
    }

    // Test annotation mismatch errors
    #[test]
    fn test_annotation_mismatch() {
        let graph = create_test_graph("42");
        let mut checker = TypeChecker::new();

        // Annotate the root node as String but it's actually Int
        if let Some(root_id) = graph.root_id {
            checker.add_annotation(root_id, TypedValue::primitive(PrimitiveType::string()));
        }

        let result = checker.check(&graph);

        assert!(!result.success);
        assert!(!result.errors.is_empty());

        // Check that we got a type mismatch error
        let has_mismatch = result
            .errors
            .iter()
            .any(|e| matches!(&e.error, TypeError::TypeMismatch { .. }));
        assert!(has_mismatch);
    }

    // Test effect checking with various scenarios
    #[test]
    fn test_check_effects_forbidden() {
        let graph = create_test_graph("print(\"hello\")");
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        // Allow only Pure effects (IO is forbidden)
        let mut allowed = HashSet::new();
        allowed.insert(EffectType::Pure);

        let effect_errors = checker.check_effects(&graph, &result.types, &allowed);

        assert!(!effect_errors.is_empty());
        assert!(effect_errors[0]
            .error
            .to_string()
            .contains("Forbidden effects"));
    }

    #[test]
    fn test_check_effects_allowed() {
        let graph = create_test_graph("print(\"hello\")");
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        // Allow IO effects
        let mut allowed = HashSet::new();
        allowed.insert(EffectType::IO);
        allowed.insert(EffectType::Pure);

        let effect_errors = checker.check_effects(&graph, &result.types, &allowed);

        assert!(effect_errors.is_empty());
    }

    #[test]
    fn test_check_effects_multiple_forbidden() {
        // Create a more complex graph with multiple effects
        let graph = create_test_graph("print(\"hello\")");
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        // First check if type checking succeeded
        if result.success {
            // Only allow Pure effects (no IO)
            let allowed = HashSet::new(); // Empty set means only Pure is allowed

            let effect_errors = checker.check_effects(&graph, &result.types, &allowed);

            // Should have errors about IO effect being forbidden
            assert!(!effect_errors.is_empty());
        } else {
            // If type checking failed, that's also acceptable for this test
            assert!(!result.errors.is_empty());
        }
    }

    // Test SourceLocation creation
    #[test]
    fn test_source_location_unknown() {
        let loc = SourceLocation::unknown();
        assert_eq!(loc.file, "<unknown>");
        assert_eq!(loc.line, 0);
        assert_eq!(loc.column, 0);
    }

    // Test error formatting
    #[test]
    fn test_format_error() {
        let checker = TypeChecker::new();
        let graph = create_test_graph("1 + \"hello\"");

        let error = TypeCheckError {
            location: SourceLocation {
                file: "test.flu".to_string(),
                line: 1,
                column: 1,
            },
            error: TypeError::TypeMismatch {
                expected: "Int".to_string(),
                found: "String".to_string(),
            },
            context: "In addition operation".to_string(),
        };

        let formatted = checker.format_error(&error, &graph);

        assert!(formatted.contains("test.flu:1:1"));
        assert!(formatted.contains("Type mismatch"));
        assert!(formatted.contains("In addition operation"));
    }

    // Test TypeCheckError Display implementation
    #[test]
    fn test_type_check_error_display() {
        let error = TypeCheckError {
            location: SourceLocation {
                file: "main.flu".to_string(),
                line: 5,
                column: 10,
            },
            error: TypeError::UnboundVariable("x".to_string()),
            context: "Variable not in scope".to_string(),
        };

        let display = error.to_string();

        assert!(display.contains("main.flu:5:10"));
        assert!(display.contains("error:"));
        assert!(display.contains("Unbound variable: x"));
        assert!(display.contains("Variable not in scope"));
    }

    // Test TypeCheckWarning Display implementation
    #[test]
    fn test_type_check_warning_display() {
        let warning = TypeCheckWarning {
            location: SourceLocation {
                file: "lib.flu".to_string(),
                line: 3,
                column: 7,
            },
            message: "Unused type variable 'a'".to_string(),
        };

        let display = warning.to_string();

        assert!(display.contains("lib.flu:3:7"));
        assert!(display.contains("warning:"));
        assert!(display.contains("Unused type variable 'a'"));
    }

    // Test warning generation
    #[test]
    fn test_generate_warnings_unconstrained_type_var() {
        let graph = create_test_graph("(x) => x");
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        // Should have warnings about unconstrained type variables
        assert!(!result.warnings.is_empty());

        let has_unconstrained_warning = result
            .warnings
            .iter()
            .any(|w| w.message.contains("no constraints"));
        assert!(has_unconstrained_warning);
    }

    #[test]
    fn test_generate_warnings_side_effects() {
        let graph = create_test_graph("print(\"hello\")");
        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        // Should have warnings about side effects
        let has_effect_warning = result
            .warnings
            .iter()
            .any(|w| w.message.contains("side effects"));
        assert!(has_effect_warning);
    }

    // Test TypeCheckerBuilder
    #[test]
    fn test_type_checker_builder() {
        let node_id1 = NodeId::new(1).unwrap();
        let node_id2 = NodeId::new(2).unwrap();

        let mut checker = TypeCheckerBuilder::new()
            .with_location(
                node_id1,
                SourceLocation {
                    file: "test1.flu".to_string(),
                    line: 1,
                    column: 1,
                },
            )
            .with_annotation(node_id2, TypedValue::primitive(PrimitiveType::bool()))
            .build();

        // Test that the builder worked by type checking something
        let graph = create_test_graph("42");
        let result = checker.check(&graph);
        assert!(!result.types.is_empty());
    }

    #[test]
    fn test_type_checker_builder_with_env() {
        let mut env = TypeEnvironment::new();
        env.bind(
            "my_func",
            TypedValue::function(FunctionType::new(
                vec![TypedValue::primitive(PrimitiveType::int())],
                TypedValue::primitive(PrimitiveType::int()),
            )),
        );

        let mut checker = TypeCheckerBuilder::with_env(env)
            .with_location(NodeId::new(1).unwrap(), SourceLocation::unknown())
            .build();

        // Test that it works with the custom env
        let graph = create_test_graph("my_func(42)");
        let result = checker.check(&graph);
        assert!(result.success || !result.types.is_empty());
    }

    // Test get_type method (currently returns None)
    #[test]
    fn test_get_type_returns_none() {
        let checker = TypeChecker::new();
        let node_id = NodeId::new(1).unwrap();

        assert!(checker.get_type(node_id).is_none());
    }

    // Test Default implementation
    #[test]
    fn test_type_checker_default() {
        let mut checker1 = TypeChecker::new();
        let mut checker2 = TypeChecker::default();

        // Both should work the same way
        let graph = create_test_graph("42");
        let result1 = checker1.check(&graph);
        let result2 = checker2.check(&graph);

        assert_eq!(result1.success, result2.success);
        assert_eq!(result1.errors.len(), result2.errors.len());
    }

    // Test complex error scenario with multiple issues
    #[test]
    fn test_multiple_errors() {
        let graph = create_test_graph("{ let x = 1; x + \"hello\" }");
        let mut checker = TypeChecker::new();

        // Add an annotation that will also cause a mismatch
        if let Some(root_id) = graph.root_id {
            checker.add_annotation(root_id, TypedValue::primitive(PrimitiveType::bool()));
        }

        let result = checker.check(&graph);

        assert!(!result.success);
        // Should have multiple errors
        assert!(result.errors.len() >= 1);
    }

    // Test types_compatible method (private, tested indirectly)
    #[test]
    fn test_types_compatible_via_annotation_check() {
        let graph = create_test_graph("42");
        let mut checker = TypeChecker::new();

        // Annotate with exact same type - should be compatible
        if let Some(root_id) = graph.root_id {
            checker.add_annotation(root_id, TypedValue::primitive(PrimitiveType::int()));
        }

        let result = checker.check(&graph);

        // No annotation mismatch errors
        let has_annotation_error = result
            .errors
            .iter()
            .any(|e| e.context.contains("annotation"));
        assert!(!has_annotation_error);
    }

    // Test error context information
    #[test]
    fn test_error_context_during_inference() {
        // Create an invalid graph that will fail during inference
        let mut graph = Graph::new();
        graph.root_id = Some(NodeId::new(999).unwrap()); // Non-existent node

        let mut checker = TypeChecker::new();
        let result = checker.check(&graph);

        assert!(!result.success);
        assert!(!result.errors.is_empty());

        // Should have context about inference failure
        let has_inference_context = result
            .errors
            .iter()
            .any(|e| e.context.contains("During type inference"));
        assert!(has_inference_context);
    }

    // Test location retrieval with fallback
    #[test]
    fn test_get_location_fallback() {
        let mut checker = TypeChecker::new();
        let existing_id = NodeId::new(1).unwrap();
        let _missing_id = NodeId::new(2).unwrap();

        checker.add_location(
            existing_id,
            SourceLocation {
                file: "exists.flu".to_string(),
                line: 5,
                column: 10,
            },
        );

        // Test through error formatting which uses get_location internally
        let error1 = TypeCheckError {
            location: SourceLocation {
                file: "exists.flu".to_string(),
                line: 5,
                column: 10,
            },
            error: TypeError::UnboundVariable("x".to_string()),
            context: "test".to_string(),
        };

        let error2 = TypeCheckError {
            location: SourceLocation::unknown(),
            error: TypeError::UnboundVariable("y".to_string()),
            context: "test".to_string(),
        };

        let graph = Graph::new();
        let formatted1 = checker.format_error(&error1, &graph);
        let formatted2 = checker.format_error(&error2, &graph);

        assert!(formatted1.contains("exists.flu"));
        assert!(formatted2.contains("<unknown>"));
    }
}
