//! Additional edge case tests for types module

#[cfg(test)]
mod types_edge_tests {
    use crate::types::*;
    use fluentai_core::ast::EffectType;
    use rustc_hash::FxHashMap;
    use std::collections::HashSet;

    // ===== EffectTypeWrapper Tests =====

    #[test]
    fn test_effect_type_wrapper_creation() {
        let effect = EffectTypeWrapper {
            effect_kind: EffectType::IO,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::string()))),
        };
        
        let typed_val = TypedValue::effect(effect);
        assert!(matches!(typed_val.inner, TypedValueInner::Effect(_)));
        assert!(typed_val.effects.contains(&EffectType::IO));
    }

    #[test]
    fn test_effect_type_wrapper_no_payload() {
        let effect = EffectTypeWrapper {
            effect_kind: EffectType::Concurrent,
            payload_type: None,
        };
        
        let typed_val = TypedValue::effect(effect);
        assert!(typed_val.effects.contains(&EffectType::Concurrent));
    }

    #[test]
    fn test_effect_type_wrapper_display() {
        let effect = EffectTypeWrapper {
            effect_kind: EffectType::Async,
            payload_type: Some(Box::new(TypedValue::primitive(PrimitiveType::int()))),
        };
        
        let typed_val = TypedValue::effect(effect);
        let display_str = typed_val.to_string();
        assert!(display_str.contains("Async"));
    }

    // ===== UncertainType Tests =====

    #[test]
    fn test_uncertain_type_creation() {
        let uncertain = UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::float())),
            confidence: 0.95,
            distribution: "normal".to_string(),
        };
        
        let typed_val = TypedValue::uncertain(uncertain);
        assert!(matches!(typed_val.inner, TypedValueInner::Uncertain(_)));
    }

    #[test]
    fn test_uncertain_type_with_effects() {
        let mut base = TypedValue::primitive(PrimitiveType::float());
        base.effects.insert(EffectType::State);
        
        let uncertain = UncertainType {
            base_type: Box::new(base),
            confidence: 0.8,
            distribution: "uniform".to_string(),
        };
        
        let typed_val = TypedValue::uncertain(uncertain);
        assert!(typed_val.effects.contains(&EffectType::State));
    }

    #[test]
    fn test_uncertain_type_display() {
        let uncertain = UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::int())),
            confidence: 0.99,
            distribution: "poisson".to_string(),
        };
        
        let typed_val = TypedValue::uncertain(uncertain);
        let display_str = typed_val.to_string();
        assert!(display_str.contains("Uncertain"));
    }

    // ===== TemporalType Tests =====

    #[test]
    fn test_temporal_type_creation() {
        let temporal = TemporalType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::int())),
            constraint: "after(0)".to_string(),
        };
        
        let typed_val = TypedValue::temporal(temporal);
        assert!(matches!(typed_val.inner, TypedValueInner::Temporal(_)));
    }

    #[test]
    fn test_temporal_type_with_effects() {
        let mut base = TypedValue::primitive(PrimitiveType::string());
        base.effects.insert(EffectType::IO);
        
        let temporal = TemporalType {
            base_type: Box::new(base),
            constraint: "eventually".to_string(),
        };
        
        let typed_val = TypedValue::temporal(temporal);
        assert!(typed_val.effects.contains(&EffectType::IO));
    }

    #[test]
    fn test_temporal_type_complex_constraint() {
        let temporal = TemporalType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::bool())),
            constraint: "within(5s) && after(now)".to_string(),
        };
        
        let typed_val = TypedValue::temporal(temporal);
        if let TypedValueInner::Temporal(t) = &typed_val.inner {
            assert!(t.constraint.contains("within"));
            assert!(t.constraint.contains("after"));
        } else {
            panic!("Expected Temporal type");
        }
    }

    // ===== TypeConstraint Edge Cases =====

    #[test]
    fn test_type_constraint_has_trait() {
        let constraint = TypeConstraint::HasTrait("Serializable".to_string());
        assert_eq!(constraint.to_string(), "Serializable");
    }

    #[test]
    fn test_type_constraint_subtype_of() {
        let constraint = TypeConstraint::SubtypeOf("Animal".to_string());
        assert_eq!(constraint.to_string(), "<: Animal");
    }

    #[test]
    fn test_type_variable_multiple_constraints() {
        let var = TypeVariable::new("T")
            .with_constraint(TypeConstraint::Numeric)
            .with_constraint(TypeConstraint::Comparable)
            .with_constraint(TypeConstraint::HasTrait("Clone".to_string()));
        
        assert_eq!(var.constraints.len(), 3);
        let display = var.to_string();
        assert!(display.contains("Num"));
        assert!(display.contains("Ord"));
        assert!(display.contains("Clone"));
    }

    // ===== Complex Type Combinations =====

    #[test]
    fn test_list_of_uncertain_types() {
        let uncertain = TypedValue::uncertain(UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::float())),
            confidence: 0.9,
            distribution: "normal".to_string(),
        });
        
        let list = TypedValue::list(ListType::new(uncertain));
        assert!(matches!(list.inner, TypedValueInner::List(_)));
    }

    #[test]
    fn test_temporal_function_type() {
        let func = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::bool()),
        ));
        
        let temporal = TypedValue::temporal(TemporalType {
            base_type: Box::new(func),
            constraint: "always".to_string(),
        });
        
        assert!(matches!(temporal.inner, TypedValueInner::Temporal(_)));
    }

    #[test]
    fn test_record_with_uncertain_fields() {
        let mut fields = FxHashMap::default();
        
        fields.insert("temperature".to_string(), TypedValue::uncertain(UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::float())),
            confidence: 0.95,
            distribution: "normal".to_string(),
        }));
        
        fields.insert("pressure".to_string(), TypedValue::uncertain(UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::float())),
            confidence: 0.90,
            distribution: "uniform".to_string(),
        }));
        
        let record = TypedValue::record(RecordType { fields });
        assert!(matches!(record.inner, TypedValueInner::Record(_)));
    }

    // ===== TypedValue Methods =====

    #[test]
    fn test_typed_value_kind() {
        let primitive = TypedValue::primitive(PrimitiveType::int());
        assert_eq!(primitive.kind(), TypeKind::Primitive);
        
        let func = TypedValue::function(FunctionType::new(vec![], TypedValue::primitive(PrimitiveType::unit())));
        assert_eq!(func.kind(), TypeKind::Function);
        
        let list = TypedValue::list(ListType::new(TypedValue::primitive(PrimitiveType::string())));
        assert_eq!(list.kind(), TypeKind::List);
    }

    #[test]
    fn test_typed_value_is_pure() {
        let pure = TypedValue::primitive(PrimitiveType::int());
        assert!(pure.is_pure());
        
        let mut with_io = TypedValue::primitive(PrimitiveType::string());
        with_io.effects.insert(EffectType::IO);
        assert!(!with_io.is_pure());
        
        let mut with_pure_effect = TypedValue::primitive(PrimitiveType::bool());
        with_pure_effect.effects.insert(EffectType::Pure);
        assert!(with_pure_effect.is_pure());
    }

    #[test]
    fn test_typed_value_with_effects() {
        let mut effects = HashSet::new();
        effects.insert(EffectType::IO);
        effects.insert(EffectType::State);
        
        let typed_val = TypedValue::primitive(PrimitiveType::unit()).with_effects(effects.clone());
        assert_eq!(typed_val.effects, effects);
    }

    // ===== Empty/Edge Cases =====

    #[test]
    fn test_empty_variant_type() {
        let variant = VariantType::new();
        let typed_val = TypedValue::variant(variant);
        
        if let TypedValueInner::Variant(v) = &typed_val.inner {
            assert!(v.variants.is_empty());
        } else {
            panic!("Expected Variant type");
        }
    }

    #[test]
    fn test_function_with_empty_params() {
        let func = FunctionType::new(vec![], TypedValue::primitive(PrimitiveType::unit()));
        let typed_val = TypedValue::function(func);
        
        if let TypedValueInner::Function(f) = &typed_val.inner {
            assert!(f.params.is_empty());
        } else {
            panic!("Expected Function type");
        }
    }

    #[test]
    fn test_type_variable_no_constraints() {
        let var = TypeVariable::new("U");
        assert!(var.constraints.is_empty());
        assert_eq!(var.to_string(), "U");
    }

    // ===== Display Implementations =====

    #[test]
    fn test_complex_type_display() {
        // List of functions
        let func = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::string()),
        ));
        let list = TypedValue::list(ListType::new(func));
        
        let display = list.to_string();
        assert!(display.contains("["));
        assert!(display.contains("]"));
        assert!(display.contains("→"));
    }

    #[test]
    fn test_nested_uncertain_temporal_type() {
        let uncertain = TypedValue::uncertain(UncertainType {
            base_type: Box::new(TypedValue::primitive(PrimitiveType::float())),
            confidence: 0.9,
            distribution: "normal".to_string(),
        });
        
        let temporal = TypedValue::temporal(TemporalType {
            base_type: Box::new(uncertain),
            constraint: "eventually".to_string(),
        });
        
        assert!(matches!(temporal.inner, TypedValueInner::Temporal(_)));
        if let TypedValueInner::Temporal(t) = &temporal.inner {
            assert!(matches!(t.base_type.inner, TypedValueInner::Uncertain(_)));
        }
    }

    // ===== Effect Propagation Tests =====

    #[test]
    fn test_variant_effect_propagation() {
        let mut io_type = TypedValue::primitive(PrimitiveType::string());
        io_type.effects.insert(EffectType::IO);
        
        let variant = VariantType::new()
            .with_variant("Ok", Some(TypedValue::primitive(PrimitiveType::int())))
            .with_variant("Err", Some(io_type));
        
        let typed_val = TypedValue::variant(variant);
        assert!(typed_val.effects.contains(&EffectType::IO));
    }

    #[test]
    fn test_deeply_nested_effect_propagation() {
        let mut base = TypedValue::primitive(PrimitiveType::int());
        base.effects.insert(EffectType::State);
        
        let list = TypedValue::list(ListType::new(base));
        let tuple = TypedValue::tuple(TupleType::new(vec![list, TypedValue::primitive(PrimitiveType::bool())]));
        
        assert!(tuple.effects.contains(&EffectType::State));
    }
}