//! Additional tests for type unification edge cases

#[cfg(test)]
mod advanced_unification_tests {
    use crate::unification::*;
    use crate::types::*;
    use rustc_hash::FxHashMap;

    #[test]
    fn test_complex_substitution() {
        let mut unifier = Unifier::new();
        
        // Create type variables
        let t1 = TypedValue::variable(TypeVariable::new("T1"));
        let t2 = TypedValue::variable(TypeVariable::new("T2"));
        let t3 = TypedValue::variable(TypeVariable::new("T3"));
        
        // Create complex type: T1 -> (T2 -> T3)
        let inner_func = TypedValue::function(FunctionType::new(
            vec![t2.clone()],
            t3.clone(),
        ));
        let outer_func = TypedValue::function(FunctionType::new(
            vec![t1.clone()],
            inner_func,
        ));
        
        // Create concrete type: Int -> (String -> Bool)
        let concrete_inner = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::bool()),
        ));
        let concrete_outer = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            concrete_inner,
        ));
        
        // Unify
        let subst = unifier.unify(&outer_func, &concrete_outer).unwrap();
        
        // Check substitutions
        let t1_result = subst.apply_type(&t1);
        let t2_result = subst.apply_type(&t2);
        let t3_result = subst.apply_type(&t3);
        
        assert_eq!(t1_result.to_string(), "Int");
        assert_eq!(t2_result.to_string(), "String");
        assert_eq!(t3_result.to_string(), "Bool");
    }

    #[test]
    fn test_record_unification() {
        let mut unifier = Unifier::new();
        
        // Create record types
        let mut fields1 = FxHashMap::default();
        fields1.insert("x".to_string(), TypedValue::primitive(PrimitiveType::int()));
        fields1.insert("y".to_string(), TypedValue::primitive(PrimitiveType::string()));
        let record1 = TypedValue::record(RecordType { fields: fields1 });
        
        let mut fields2 = FxHashMap::default();
        fields2.insert("x".to_string(), TypedValue::primitive(PrimitiveType::int()));
        fields2.insert("y".to_string(), TypedValue::primitive(PrimitiveType::string()));
        let record2 = TypedValue::record(RecordType { fields: fields2 });
        
        // Should unify
        assert!(unifier.unify(&record1, &record2).is_ok());
        
        // Different field names shouldn't unify
        let mut fields3 = FxHashMap::default();
        fields3.insert("a".to_string(), TypedValue::primitive(PrimitiveType::int()));
        fields3.insert("b".to_string(), TypedValue::primitive(PrimitiveType::string()));
        let record3 = TypedValue::record(RecordType { fields: fields3 });
        
        let mut unifier2 = Unifier::new();
        assert!(unifier2.unify(&record1, &record3).is_err());
    }

    #[test]
    fn test_variant_unification() {
        let mut unifier = Unifier::new();
        
        // Create variant types
        let variant1 = TypedValue::variant(
            VariantType::new()
                .with_variant("Some", Some(TypedValue::primitive(PrimitiveType::int())))
                .with_variant("None", None)
        );
        
        let variant2 = TypedValue::variant(
            VariantType::new()
                .with_variant("Some", Some(TypedValue::primitive(PrimitiveType::int())))
                .with_variant("None", None)
        );
        
        // Should unify
        assert!(unifier.unify(&variant1, &variant2).is_ok());
        
        // Different payload types shouldn't unify
        let variant3 = TypedValue::variant(
            VariantType::new()
                .with_variant("Some", Some(TypedValue::primitive(PrimitiveType::string())))
                .with_variant("None", None)
        );
        
        let mut unifier2 = Unifier::new();
        assert!(unifier2.unify(&variant1, &variant3).is_err());
    }

    #[test]
    fn test_substitution_composition() {
        let mut subst1 = Substitution::new();
        let mut subst2 = Substitution::new();
        
        // T1 -> Int
        subst1.insert("T1".to_string(), TypedValue::primitive(PrimitiveType::int()));
        
        // T2 -> T1
        subst2.insert("T2".to_string(), TypedValue::variable(TypeVariable::new("T1")));
        
        // Compose substitutions
        subst2.compose(&subst1);
        
        // T2 should now map to Int
        let t2 = TypedValue::variable(TypeVariable::new("T2"));
        let result = subst2.apply_type(&t2);
        assert_eq!(result.to_string(), "Int");
    }

    #[test]
    fn test_deep_occurs_check() {
        let mut unifier = Unifier::new();
        
        // Create a deeply nested type that references itself
        let t = TypedValue::variable(TypeVariable::new("T"));
        let list_t = TypedValue::list(ListType::new(t.clone()));
        let func_returning_list = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            list_t,
        ));
        
        // Should fail occurs check when trying to unify T with function returning [T]
        assert!(unifier.unify(&t, &func_returning_list).is_err());
    }

    #[test]
    fn test_effect_type_unification() {
        let mut unifier = Unifier::new();
        
        // Create effect types
        let io_effect = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::IO,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::string()))),
        });
        
        let io_effect2 = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::IO,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::string()))),
        });
        
        // Same effects should unify
        assert!(unifier.unify(&io_effect, &io_effect2).is_ok());
        
        // Different effect kinds shouldn't unify
        let async_effect = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::Async,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::string()))),
        });
        
        let mut unifier2 = Unifier::new();
        assert!(unifier2.unify(&io_effect, &async_effect).is_err());
    }

    #[test]
    fn test_uncertain_type_unification() {
        let mut unifier = Unifier::new();
        
        let base = TypedValue::primitive(PrimitiveType::float());
        let uncertain1 = TypedValue::uncertain(UncertainType {
            base_type: Box::new(base.clone()),
            confidence: 0.9,
            distribution: "normal".to_string(),
        });
        
        let uncertain2 = TypedValue::uncertain(UncertainType {
            base_type: Box::new(base),
            confidence: 0.8, // Different confidence
            distribution: "normal".to_string(),
        });
        
        // Should unify based on base type (ignoring confidence for now)
        assert!(unifier.unify(&uncertain1, &uncertain2).is_ok());
    }

    #[test]
    fn test_temporal_type_unification() {
        let mut unifier = Unifier::new();
        
        let base = TypedValue::primitive(PrimitiveType::int());
        let temporal1 = TypedValue::temporal(TemporalType {
            base_type: Box::new(base.clone()),
            constraint: "after(0)".to_string(),
        });
        
        let temporal2 = TypedValue::temporal(TemporalType {
            base_type: Box::new(base),
            constraint: "before(100)".to_string(), // Different constraint
        });
        
        // Should unify based on base type (ignoring constraint for now)
        assert!(unifier.unify(&temporal1, &temporal2).is_ok());
    }

    #[test]
    fn test_polymorphic_list_unification() {
        let mut unifier = Unifier::new();
        
        // Create polymorphic list type: [a] where a is a type variable
        let a = TypedValue::variable(TypeVariable::new("a"));
        let list_a = TypedValue::list(ListType::new(a.clone()));
        
        // Create another polymorphic list: [b]
        let b = TypedValue::variable(TypeVariable::new("b"));
        let list_b = TypedValue::list(ListType::new(b));
        
        // They should unify, making a = b
        let subst = unifier.unify(&list_a, &list_b).unwrap();
        
        // After unification, both should have the same element type
        let result_a = subst.apply_type(&list_a);
        let result_b = subst.apply_type(&list_b);
        assert_eq!(result_a.to_string(), result_b.to_string());
    }

    #[test]
    fn test_function_with_effects_unification() {
        let mut unifier = Unifier::new();
        
        // Create function types with effects
        let pure_func = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int()),
        ));
        
        let mut io_func = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int()),
        ));
        io_func.effects.insert(fluentai_core::ast::EffectType::IO);
        
        // Currently, effects are not part of the type equality
        // So these should unify (though in a full effect system they might not)
        assert!(unifier.unify(&pure_func, &io_func).is_ok());
    }

    #[test]
    fn test_nested_polymorphic_unification() {
        let mut unifier = Unifier::new();
        
        // Create type: (a -> b) -> [a] -> [b]
        let a = TypedValue::variable(TypeVariable::new("a"));
        let b = TypedValue::variable(TypeVariable::new("b"));
        
        let mapper = TypedValue::function(FunctionType::new(vec![a.clone()], b.clone()));
        let list_a = TypedValue::list(ListType::new(a.clone()));
        let list_b = TypedValue::list(ListType::new(b.clone()));
        
        let map_type = TypedValue::function(FunctionType::new(
            vec![mapper, list_a],
            list_b,
        ));
        
        // Create concrete type: (Int -> String) -> [Int] -> [String]
        let int_to_string = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::string()),
        ));
        let int_list = TypedValue::list(ListType::new(TypedValue::primitive(PrimitiveType::int())));
        let string_list = TypedValue::list(ListType::new(TypedValue::primitive(PrimitiveType::string())));
        
        let concrete_map = TypedValue::function(FunctionType::new(
            vec![int_to_string, int_list],
            string_list,
        ));
        
        // Should unify
        let subst = unifier.unify(&map_type, &concrete_map).unwrap();
        
        // Check that a -> Int and b -> String
        let a_result = subst.apply_type(&a);
        let b_result = subst.apply_type(&b);
        assert_eq!(a_result.to_string(), "Int");
        assert_eq!(b_result.to_string(), "String");
    }

    // ===== Additional Edge Case Tests =====

    #[test]
    fn test_substitution_contains_var() {
        let mut subst = Substitution::new();
        subst.insert("T".to_string(), TypedValue::primitive(PrimitiveType::int()));
        
        assert!(subst.contains_var("T"));
        assert!(!subst.contains_var("U"));
    }

    #[test]
    fn test_substitution_default() {
        let subst1 = Substitution::new();
        let subst2 = Substitution::default();
        
        // Both should be empty
        assert!(!subst1.contains_var("T"));
        assert!(!subst2.contains_var("T"));
    }

    #[test]
    fn test_unifier_default() {
        let unifier1 = Unifier::new();
        let unifier2 = Unifier::default();
        
        // Both should have empty substitutions
        assert!(!unifier1.substitution().contains_var("T"));
        assert!(!unifier2.substitution().contains_var("T"));
    }

    #[test]
    fn test_tuple_unification() {
        let mut unifier = Unifier::new();
        
        // Create tuple types
        let tuple1 = TypedValue::tuple(TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
            TypedValue::primitive(PrimitiveType::string()),
        ]));
        
        let tuple2 = TypedValue::tuple(TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
            TypedValue::primitive(PrimitiveType::string()),
        ]));
        
        // Should unify
        assert!(unifier.unify(&tuple1, &tuple2).is_ok());
        
        // Different element types shouldn't unify
        let tuple3 = TypedValue::tuple(TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::bool()),
            TypedValue::primitive(PrimitiveType::string()),
        ]));
        
        let mut unifier2 = Unifier::new();
        assert!(unifier2.unify(&tuple1, &tuple3).is_err());
        
        // Different arities shouldn't unify
        let tuple4 = TypedValue::tuple(TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
        ]));
        
        let mut unifier3 = Unifier::new();
        let result = unifier3.unify(&tuple1, &tuple4);
        assert!(result.is_err());
        match result {
            Err(UnificationError::ArityMismatch { expected, found }) => {
                assert_eq!(expected, 2);
                assert_eq!(found, 1);
            }
            _ => panic!("Expected ArityMismatch error"),
        }
    }

    #[test]
    fn test_record_field_mismatch() {
        let mut unifier = Unifier::new();
        
        // Create records with different number of fields
        let mut fields1 = FxHashMap::default();
        fields1.insert("x".to_string(), TypedValue::primitive(PrimitiveType::int()));
        let record1 = TypedValue::record(RecordType { fields: fields1 });
        
        let mut fields2 = FxHashMap::default();
        fields2.insert("x".to_string(), TypedValue::primitive(PrimitiveType::int()));
        fields2.insert("y".to_string(), TypedValue::primitive(PrimitiveType::string()));
        let record2 = TypedValue::record(RecordType { fields: fields2 });
        
        // Should fail with RecordFieldMismatch
        let result = unifier.unify(&record1, &record2);
        assert!(matches!(result, Err(UnificationError::RecordFieldMismatch)));
    }

    #[test]
    fn test_variant_payload_mismatch() {
        let mut unifier = Unifier::new();
        
        // Create variants with mismatched payloads
        let variant1 = TypedValue::variant(
            VariantType::new()
                .with_variant("Some", Some(TypedValue::primitive(PrimitiveType::int())))
                .with_variant("None", None)
        );
        
        let variant2 = TypedValue::variant(
            VariantType::new()
                .with_variant("Some", None) // No payload where one is expected
                .with_variant("None", None)
        );
        
        // Should fail with VariantMismatch
        let result = unifier.unify(&variant1, &variant2);
        assert!(matches!(result, Err(UnificationError::VariantMismatch)));
    }

    #[test]
    fn test_variant_tag_mismatch() {
        let mut unifier = Unifier::new();
        
        // Create variants with different tags
        let variant1 = TypedValue::variant(
            VariantType::new()
                .with_variant("Ok", Some(TypedValue::primitive(PrimitiveType::int())))
                .with_variant("Err", Some(TypedValue::primitive(PrimitiveType::string())))
        );
        
        let variant2 = TypedValue::variant(
            VariantType::new()
                .with_variant("Success", Some(TypedValue::primitive(PrimitiveType::int())))
                .with_variant("Failure", Some(TypedValue::primitive(PrimitiveType::string())))
        );
        
        // Should fail with VariantMismatch
        assert!(unifier.unify(&variant1, &variant2).is_err());
    }

    #[test]
    fn test_effect_payload_mismatch() {
        let mut unifier = Unifier::new();
        
        // Create effect types with mismatched payloads
        let effect1 = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::IO,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::string()))),
        });
        
        let effect2 = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::IO,
            payload_type: None, // No payload where one is expected
        });
        
        // Should fail
        assert!(unifier.unify(&effect1, &effect2).is_err());
    }

    #[test]
    fn test_kind_mismatch_error() {
        let mut unifier = Unifier::new();
        
        let int_type = TypedValue::primitive(PrimitiveType::int());
        let list_type = TypedValue::list(ListType::new(TypedValue::primitive(PrimitiveType::int())));
        
        // Should fail with KindMismatch
        let result = unifier.unify(&int_type, &list_type);
        assert!(result.is_err());
        match result {
            Err(UnificationError::KindMismatch { expected, found }) => {
                assert_eq!(expected, TypeKind::Primitive);
                assert_eq!(found, TypeKind::List);
            }
            _ => panic!("Expected KindMismatch error"),
        }
    }

    #[test]
    fn test_complex_occurs_check_in_record() {
        let mut unifier = Unifier::new();
        
        // Create record with self-referential field
        let t = TypedValue::variable(TypeVariable::new("T"));
        let mut fields = FxHashMap::default();
        fields.insert("self".to_string(), t.clone());
        let record = TypedValue::record(RecordType { fields });
        
        // Should fail occurs check
        assert!(unifier.unify(&t, &record).is_err());
    }

    #[test]
    fn test_complex_occurs_check_in_variant() {
        let mut unifier = Unifier::new();
        
        // Create variant with self-referential payload
        let t = TypedValue::variable(TypeVariable::new("T"));
        let variant = TypedValue::variant(
            VariantType::new()
                .with_variant("Recursive", Some(t.clone()))
        );
        
        // Should fail occurs check
        assert!(unifier.unify(&t, &variant).is_err());
    }

    #[test]
    fn test_occurs_check_in_uncertain_type() {
        let mut unifier = Unifier::new();
        
        let t = TypedValue::variable(TypeVariable::new("T"));
        let uncertain = TypedValue::uncertain(UncertainType {
            base_type: Box::new(t.clone()),
            confidence: 0.9,
            distribution: "normal".to_string(),
        });
        
        // Should fail occurs check
        assert!(unifier.unify(&t, &uncertain).is_err());
    }

    #[test]
    fn test_occurs_check_in_temporal_type() {
        let mut unifier = Unifier::new();
        
        let t = TypedValue::variable(TypeVariable::new("T"));
        let temporal = TypedValue::temporal(TemporalType {
            base_type: Box::new(t.clone()),
            constraint: "always".to_string(),
        });
        
        // Should fail occurs check
        assert!(unifier.unify(&t, &temporal).is_err());
    }

    #[test]
    fn test_occurs_check_in_effect_type() {
        let mut unifier = Unifier::new();
        
        let t = TypedValue::variable(TypeVariable::new("T"));
        let effect = TypedValue::effect(EffectTypeWrapper {
            effect_kind: fluentai_core::ast::EffectType::IO,
            payload_type: Some(Box::new(t.clone())),
        });
        
        // Should fail occurs check
        assert!(unifier.unify(&t, &effect).is_err());
    }

    #[test]
    fn test_substitution_application_on_functions() {
        let mut subst = Substitution::new();
        subst.insert("T".to_string(), TypedValue::primitive(PrimitiveType::int()));
        
        // Create a function with type variable
        let func = TypedValue::function(FunctionType::new(
            vec![TypedValue::variable(TypeVariable::new("T"))],
            TypedValue::variable(TypeVariable::new("T")),
        ));
        
        // Apply substitution
        let result = subst.apply_type(&func);
        
        // Check that T was replaced with Int
        match &result.inner {
            TypedValueInner::Function(f) => {
                assert_eq!(f.params[0].to_string(), "Int");
                assert_eq!(f.result.to_string(), "Int");
            }
            _ => panic!("Expected function type"),
        }
    }

    #[test]
    fn test_error_display() {
        // Test error display implementations
        let err1 = UnificationError::TypeMismatch("Int".to_string(), "String".to_string());
        assert!(err1.to_string().contains("Cannot unify Int with String"));
        
        let err2 = UnificationError::OccursCheck("T".to_string(), "[T]".to_string());
        assert!(err2.to_string().contains("Occurs check failed: T occurs in [T]"));
        
        let err3 = UnificationError::KindMismatch {
            expected: TypeKind::Function,
            found: TypeKind::List,
        };
        assert!(err3.to_string().contains("Kind mismatch"));
        
        let err4 = UnificationError::ArityMismatch {
            expected: 2,
            found: 3,
        };
        assert!(err4.to_string().contains("Arity mismatch: expected 2 arguments, found 3"));
        
        let err5 = UnificationError::RecordFieldMismatch;
        assert!(err5.to_string().contains("Record field mismatch"));
        
        let err6 = UnificationError::VariantMismatch;
        assert!(err6.to_string().contains("Variant mismatch"));
    }

    #[test]
    fn test_unify_with_type_constraints() {
        let mut unifier = Unifier::new();
        
        // Create type variables with constraints
        let t1 = TypedValue::variable(
            TypeVariable::new("T").with_constraint(TypeConstraint::Numeric)
        );
        let t2 = TypedValue::variable(
            TypeVariable::new("U").with_constraint(TypeConstraint::Comparable)
        );
        
        // Currently constraints are not checked during unification
        // But they should unify as variables
        assert!(unifier.unify(&t1, &t2).is_ok());
    }
}