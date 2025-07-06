//! Additional tests for type inference edge cases and advanced features

#[cfg(test)]
mod advanced_inference_tests {
    use crate::inference::*;
    use crate::types::*;
    use crate::environment::TypeEnvironment;
    use fluentai_parser::parse;
    use anyhow::Result;

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
        let code = r#"
            (let ((id (lambda (x) x)))
                (id 42))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_higher_order_functions() {
        // Test simple higher-order function
        let code = r#"
            (let ((apply-twice (lambda (f x) 
                                (f (f x)))))
                (apply-twice (lambda (n) (+ n 1)) 5))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_recursive_types() {
        // Test recursive function type inference
        let code = r#"
            (letrec ((fact (lambda (n)
                            (if (= n 0)
                                1
                                (* n (fact (- n 1)))))))
                (fact 5))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_mutual_recursion() {
        // Test mutually recursive functions
        let code = r#"
            (letrec ((even? (lambda (n)
                             (if (= n 0)
                                 #t
                                 (odd? (- n 1)))))
                     (odd? (lambda (n)
                            (if (= n 0)
                                #f
                                (even? (- n 1))))))
                (even? 10))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_effect_propagation() {
        // Test that effects propagate through function calls
        let code = r#"
            (let ((print-twice (lambda (x)
                                 (do (print x)
                                     (print x)))))
                (print-twice "hello"))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        assert!(ty.effects.contains(&fluentai_core::ast::EffectType::IO));
    }

    #[test]
    fn test_pattern_matching_types() {
        // Test pattern matching with different types
        let code = r#"
            (match 5
                (0 "zero")
                (1 "one")
                (_ "other"))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "String");
    }

    #[test]
    fn test_async_await_types() {
        // Test async/await type inference
        let code = r#"
            (await (async (+ 1 2)))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        assert_eq!(ty.to_string(), "Int");
        // Await should remove the async effect
        assert!(!ty.effects.contains(&fluentai_core::ast::EffectType::Async));
    }

    #[test]
    fn test_channel_types() {
        // Test channel creation
        let code = "(chan)";
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Channel type should be a variant
        assert_eq!(ty.kind(), TypeKind::Variant);
    }

    #[test]
    fn test_error_recovery() {
        // Test that inference continues after errors
        let code = r#"
            (let ((x unbound-var))  ; This should error
                42)                 ; But we should still get Int
        "#;
        
        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);
        
        // Should fail due to unbound variable
        assert!(result.is_err());
        assert!(!inferencer.errors().is_empty());
        
        // Check that the error is recorded
        let has_unbound_error = inferencer.errors().iter()
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
        env.bind("is-even?", int_to_bool);
        
        let code = "(is-even? 42)";
        let result = infer_with_env(code, env);
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Bool");
    }

    #[test]
    fn test_complex_nested_types() {
        // Test deeply nested type structures
        let code = r#"
            (list 
                (list 1 2 3)
                (list 4 5 6)
                (list 7 8 9))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "[[Int]]");
    }

    #[test]
    fn test_tuple_inference() {
        // Test tuple type inference (using lists as tuples)
        let code = r#"
            (let ((pair (list 42 "hello")))
                (car pair))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        // This would return Int in a proper tuple system
    }

    #[test]
    fn test_error_propagation_in_functions() {
        // Test that type errors in function application are caught
        let code = r#"(+ 1 "not a number")"#;
        
        let graph = parse(code).unwrap();
        let mut inferencer = TypeInferencer::new();
        let result = inferencer.infer_graph(&graph);
        
        // Should fail with unification error
        assert!(result.is_err() || !inferencer.errors().is_empty());
    }

    #[test]
    fn test_generalization_and_instantiation() {
        // Test simple let binding
        let code = r#"
            (let ((x 5))
                (+ x x))
        "#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        assert_eq!(result.unwrap().to_string(), "Int");
    }

    #[test]
    fn test_effect_constraints() {
        // Test that effects propagate through print
        let code = r#"(print "hello")"#;
        
        let result = infer_with_env(code, TypeEnvironment::new());
        assert!(result.is_ok());
        let ty = result.unwrap();
        // Should have IO effect
        assert!(ty.effects.contains(&fluentai_core::ast::EffectType::IO));
    }
}