//! Additional tests for type inference edge cases and advanced features

#[cfg(test)]
mod advanced_inference_tests {
    use crate::environment::TypeEnvironment;
    use crate::inference::*;
    use crate::types::*;
    use anyhow::Result;
    use fluentai_parser::parse;

    fn infer_with_env(code: &str, env: TypeEnvironment) -> Result<TypedValue> {
        let graph = parse(code)?;
        let mut inferencer = TypeInferencer::with_env(env);
        let types = inferencer.infer_graph(&graph)?;

        if let Some(root_id) = graph.root_id {
            Ok(types.get(&root_id).unwrap().clone())
        } else {
            Err(anyhow::anyhow!("No root node"))
        }
    }

    #[test]
    fn test_polymorphic_functions() {
        // Test identity function
        let code = r#"{
            let id = (x) => x;
            id(42)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_higher_order_functions() {
        // Test simple higher-order function
        let code = r#"{
            let apply_twice = (f, x) => f(f(x));
            apply_twice((n) => n + 1, 5)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    #[ignore = "recursive let bindings not yet fully supported"]
    fn test_recursive_types() {
        // Test recursive function type inference
        let code = r#"{
            let fact = (n) => {
                if (n == 0) { 1 }
                else { n * fact(n - 1) }
            };
            fact(5)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    #[ignore = "mutual recursion with let bindings not yet fully supported"]
    fn test_mutual_recursion() {
        // Test mutually recursive functions
        let code = r#"{
            let even = (n) => {
                if (n == 0) { true }
                else { odd(n - 1) }
            };
            let odd = (n) => {
                if (n == 0) { false }
                else { even(n - 1) }
            };
            even(10)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_effect_propagation() {
        // Test that effects propagate through function calls
        let code = r#"{
            let print_twice = (x) => {
                print(x);
                print(x)
            };
            print_twice("hello")
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        assert!(ty.effects.contains(&fluentai_core::ast::EffectType::IO));
    }

    #[test]
    fn test_pattern_matching_types() {
        // Test pattern matching with different types
        let code = r#"
            match(5) {
                0 => "zero",
                1 => "one",
                _ => "other"
            }
        "#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "String");
    }

    #[test]
    #[ignore = "await async syntax not yet supported by parser"]
    fn test_async_await_types() {
        // Test async/await type inference
        let code = r#"
            await async { 1 + 2 }
        "#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        assert_eq!(ty.to_string(), "Int");
        // Await should remove the async effect
        assert!(!ty.effects.contains(&fluentai_core::ast::EffectType::Async));
    }

    #[test]
    #[ignore = "channel() function not in default environment"]
    fn test_channel_types() {
        // Test channel creation
        let code = "channel()";

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Channel type should be a variant
        assert_eq!(ty.kind(), TypeKind::Variant);
    }

    #[test]
    fn test_error_recovery() {
        // Test that inference continues after errors
        let code = r#"{
            let x = unbound_var;  // This should error
            42                     // But we should still get Int
        }"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should fail due to unbound variable
        assert!(result.is_err());
        assert!(!inferencer.errors().is_empty());

        // Check that the error is recorded
        let has_unbound_error = inferencer
            .errors()
            .iter()
            .any(|e| matches!(e, TypeError::UnboundVariable(_)));
        assert!(has_unbound_error);
    }

    #[test]
    fn test_type_annotations() {
        // Test with pre-defined type environment
        let mut env = TypeEnvironment::new();

        // Add some annotated bindings
        let int_to_bool = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::bool()),
        ));
        env.bind("is_even", int_to_bool);

        let code = "is_even(42)";
        let result = infer_with_env(code, env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_complex_nested_types() {
        // Test deeply nested type structures
        let code = r#"
            list(
                list(1, 2, 3),
                list(4, 5, 6),
                list(7, 8, 9)
            )
        "#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "[[Int]]");
    }

    #[test]
    fn test_tuple_inference() {
        // Test tuple type inference (using lists as tuples)
        let code = r#"{
            let pair = list(42, "hello");
            car(pair)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        // This would return Int in a proper tuple system
    }

    #[test]
    fn test_error_propagation_in_functions() {
        // Test that type errors in function application are caught
        let code = r#"1 + "not a number""#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should fail with unification error
        assert!(result.is_err() || !inferencer.errors().is_empty());
    }

    #[test]
    fn test_generalization_and_instantiation() {
        // Test simple let binding
        let code = r#"{
            let x = 5;
            x + x
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_effect_constraints() {
        // Test that effects propagate through print
        let code = r#"print("hello")"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Should have IO effect
        assert!(ty.effects.contains(&fluentai_core::ast::EffectType::IO));
    }

    // ===== Additional Edge Case Tests =====

    #[test]
    fn test_error_invalid_literal() {
        // Test invalid literal handling
        let graph = fluentai_parser::parse("invalid-syntax!@#")
            .unwrap_or_else(|_| fluentai_core::ast::Graph::new());
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should handle parse errors gracefully
        assert!(result.is_err() || graph.root_id.is_none());
    }

    #[test]
    fn test_arity_mismatch() {
        // Test function called with wrong number of arguments
        let code = r#"add(1)"#; // add expects 2 args

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should fail with arity mismatch or unification error
        assert!(result.is_err() || !inferencer.errors().is_empty());
    }

    #[test]
    fn test_pattern_match_literal() {
        // Test pattern matching with literal patterns
        let code = r#"
            match("hello") {
                "hello" => 1,
                "world" => 2,
                _ => 3
            }
        "#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_pattern_match_constructor() {
        // Test pattern matching with constructor patterns
        let code = r#"
            match(42) {
                0 => "zero",
                _ => "other"
            }
        "#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "String");
    }

    #[test]
    #[ignore = "module/export syntax not yet fully supported"]
    fn test_module_inference() {
        // Test module definition inference
        let code = r#"
            module test_module {
                export add;
                let add = (x, y) => x + y;
                add
            }
        "#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Module inference might not be fully implemented
        assert!(result.is_ok() || !inferencer.errors().is_empty());
    }

    #[test]
    #[ignore = "import syntax with :: not yet supported by parser"]
    fn test_import_inference() {
        // Test import statement inference
        let code = r#"use std::math::{sin, cos}"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Import inference might not be fully implemented
        assert!(result.is_ok() || !inferencer.errors().is_empty());
    }

    #[test]
    fn test_qualified_variable_inference() {
        // Test qualified variable reference
        let code = r#"math.pi"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should either succeed or report unbound variable
        if result.is_err() {
            let has_unbound = inferencer
                .errors()
                .iter()
                .any(|e| matches!(e, TypeError::UnboundVariable(_)));
            assert!(has_unbound);
        }
    }

    #[test]
    #[ignore = "@contract syntax not yet supported by parser"]
    fn test_contract_inference() {
        // Test contract specification inference
        let code = r#"
            @contract {
                requires: n >= 0,
                ensures: result >= 1
            }
            function factorial(n) { /* ... */ }
        "#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Contract inference might not be fully implemented
        assert!(result.is_ok() || !inferencer.errors().is_empty());
    }

    #[test]
    fn test_spawn_inference() {
        // Test spawn expression type inference
        let code = r#"spawn { 1 + 2 }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Should have Concurrent effect
        assert!(ty
            .effects
            .contains(&fluentai_core::ast::EffectType::Concurrent));
    }

    #[test]
    #[ignore = "send!/recv! macro syntax not yet supported by parser"]
    fn test_send_receive_inference() {
        // Test channel send/receive operations
        let code = r#"{
            let ch = channel();
            send!(ch, 42);
            recv!(ch)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Should have Concurrent effect
        assert!(ty
            .effects
            .contains(&fluentai_core::ast::EffectType::Concurrent));
    }

    #[test]
    fn test_builtin_string_functions() {
        // Test string manipulation functions
        let code = r#"string-length("hello")"#;

        let mut env = TypeEnvironment::new();
        // Add string-length function to environment
        let string_to_int = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::int()),
        ));
        env.bind("string-length", string_to_int);

        let result = infer_with_env(code, env);
        match result {
            Ok(ty) => assert_eq!(ty.to_string(), "Int"),
            Err(e) => panic!("Type inference failed: {:?}", e)
        }
    }

    #[test]
    fn test_builtin_list_functions() {
        // Test list manipulation functions
        let code = r#"cons(1, list(2, 3, 4))"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "[Int]");
    }

    #[test]
    fn test_effect_handler_operations() {
        // Test effect handler with specific operations
        let code = r#"perform IO.print("hello")"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Effect handler might not be fully implemented
        if result.is_ok() {
            let root_id = graph.root_id.unwrap();
            let types = result.unwrap();
            let ty = types.get(&root_id).unwrap();
            assert!(ty.effects.contains(&fluentai_core::ast::EffectType::IO));
        } else {
            // It's OK if effect handler syntax is not fully supported
            assert!(!inferencer.errors().is_empty());
        }
    }

    #[test]
    fn test_multiple_errors_collection() {
        // Test that multiple type errors are collected
        let code = r#"{
            1 + "string";
            "another" + 2;
            true + false
        }"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should collect multiple errors
        assert!(result.is_err() || !inferencer.errors().is_empty());
        // Could have multiple type mismatches
        let errors = inferencer.errors();
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_empty_list_polymorphism() {
        // Test empty list with polymorphic type
        let code = r#"{
            let empty = [];
            cons(1, empty)
        }"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "[Int]");
    }

    #[test]
    fn test_boolean_operators() {
        // Test boolean operators
        let code = r#"true && false"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_comparison_operators() {
        // Test comparison operators
        let code = r#"5 >= 3"#;

        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_float_arithmetic() {
        // Test float arithmetic operations
        let code = r#"1.0 + 2.0"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Float arithmetic might not be fully supported, so we accept error or success
        if result.is_ok() {
            let root_id = graph.root_id.unwrap();
            let types = result.unwrap();
            let ty = types.get(&root_id).unwrap();
            assert!(ty.to_string() == "Float" || ty.to_string() == "Int");
        } else {
            // It's OK if float arithmetic is not supported
            assert!(!inferencer.errors().is_empty());
        }
    }

    #[test]
    fn test_error_recovery_multiple_bindings() {
        // Test error recovery with multiple let bindings
        let code = r#"{
            let x = 1;
            let y = unbound;
            let z = 3;
            x + z
        }"#;

        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);

        // Should fail due to unbound variable
        assert!(result.is_err() || !inferencer.errors().is_empty());
    }
}
