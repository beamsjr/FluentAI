#[cfg(test)]
mod tests {
    use crate::types::*;
    use fluentai_core::ast::EffectType;
    use rustc_hash::FxHashMap;
    use std::collections::HashSet;
    
    // ===== TypeKind Tests =====
    
    #[test]
    fn test_type_kind_display() {
        assert_eq!(TypeKind::Primitive.to_string(), "Primitive");
        assert_eq!(TypeKind::Function.to_string(), "Function");
        assert_eq!(TypeKind::Tuple.to_string(), "Tuple");
        assert_eq!(TypeKind::List.to_string(), "List");
        assert_eq!(TypeKind::Record.to_string(), "Record");
        assert_eq!(TypeKind::Variant.to_string(), "Variant");
        assert_eq!(TypeKind::Effect.to_string(), "Effect");
        assert_eq!(TypeKind::Uncertain.to_string(), "Uncertain");
        assert_eq!(TypeKind::Temporal.to_string(), "Temporal");
        assert_eq!(TypeKind::TypeVariable.to_string(), "TypeVariable");
    }
    
    #[test]
    fn test_type_kind_equality() {
        assert_eq!(TypeKind::Primitive, TypeKind::Primitive);
        assert_ne!(TypeKind::Primitive, TypeKind::Function);
        assert_ne!(TypeKind::List, TypeKind::Tuple);
    }
    
    // ===== Type Struct Tests =====
    
    #[test]
    fn test_type_default() {
        let ty = Type::default();
        assert_eq!(ty.kind, TypeKind::TypeVariable);
        assert!(ty.effects.is_empty());
        assert!(ty.metadata.is_empty());
    }
    
    #[test]
    fn test_type_with_metadata() {
        let mut ty = Type::default();
        ty.metadata.insert("source".to_string(), "user".to_string());
        ty.metadata.insert("line".to_string(), "42".to_string());
        
        assert_eq!(ty.metadata.get("source"), Some(&"user".to_string()));
        assert_eq!(ty.metadata.get("line"), Some(&"42".to_string()));
        assert_eq!(ty.metadata.get("unknown"), None);
    }
    
    // ===== PrimitiveType Tests =====
    
    #[test]
    fn test_primitive_constructors() {
        assert_eq!(PrimitiveType::int().name, "Int");
        assert_eq!(PrimitiveType::float().name, "Float");
        assert_eq!(PrimitiveType::string().name, "String");
        assert_eq!(PrimitiveType::bool().name, "Bool");
        assert_eq!(PrimitiveType::unit().name, "Unit");
    }
    
    #[test]
    fn test_primitive_custom() {
        let char_type = PrimitiveType::new("Char");
        assert_eq!(char_type.name, "Char");
        assert_eq!(char_type.to_string(), "Char");
    }
    
    #[test]
    fn test_primitive_display() {
        assert_eq!(PrimitiveType::int().to_string(), "Int");
        assert_eq!(PrimitiveType::new("CustomType").to_string(), "CustomType");
    }
    
    // ===== FunctionType Tests =====
    
    #[test]
    fn test_function_type_creation() {
        let func = FunctionType::new(
            vec![
                TypedValue::primitive(PrimitiveType::int()),
                TypedValue::primitive(PrimitiveType::string()),
            ],
            TypedValue::primitive(PrimitiveType::bool()),
        );
        
        assert_eq!(func.params.len(), 2);
        assert!(func.is_pure);
        assert_eq!(func.result.to_string(), "Bool");
    }
    
    #[test]
    fn test_function_with_effects() {
        let func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::unit()),
        );
        
        let mut effects = HashSet::new();
        effects.insert(EffectType::IO);
        
        let typed_func = func.with_effects(effects.clone());
        assert_eq!(typed_func.effects, effects);
        assert_eq!(typed_func.kind(), TypeKind::Function);
    }
    
    #[test]
    fn test_function_display_simple() {
        let func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int()),
        );
        assert_eq!(func.to_string(), "Int → Int");
    }
    
    #[test]
    fn test_function_display_multiple_params() {
        let func = FunctionType::new(
            vec![
                TypedValue::primitive(PrimitiveType::int()),
                TypedValue::primitive(PrimitiveType::string()),
                TypedValue::primitive(PrimitiveType::bool()),
            ],
            TypedValue::primitive(PrimitiveType::float()),
        );
        assert_eq!(func.to_string(), "Int → String → Bool → Float");
    }
    
    #[test]
    fn test_function_no_params() {
        let func = FunctionType::new(
            vec![],
            TypedValue::primitive(PrimitiveType::int()),
        );
        assert_eq!(func.to_string(), " → Int");
    }
    
    // ===== TupleType Tests =====
    
    #[test]
    fn test_tuple_creation() {
        let tuple = TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
            TypedValue::primitive(PrimitiveType::string()),
        ]);
        
        assert_eq!(tuple.elements.len(), 2);
        assert_eq!(tuple.to_string(), "(Int, String)");
    }
    
    #[test]
    fn test_empty_tuple() {
        let tuple = TupleType::new(vec![]);
        assert_eq!(tuple.elements.len(), 0);
        assert_eq!(tuple.to_string(), "()");
    }
    
    #[test]
    fn test_single_element_tuple() {
        let tuple = TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::bool()),
        ]);
        assert_eq!(tuple.to_string(), "(Bool)");
    }
    
    // ===== ListType Tests =====
    
    #[test]
    fn test_list_creation() {
        let list = ListType::new(TypedValue::primitive(PrimitiveType::int()));
        assert_eq!(list.element_type.to_string(), "Int");
        assert_eq!(list.to_string(), "[Int]");
    }
    
    #[test]
    fn test_nested_list() {
        let inner_list = ListType::new(TypedValue::primitive(PrimitiveType::int()));
        let outer_list = ListType::new(TypedValue::list(inner_list));
        assert_eq!(outer_list.to_string(), "[[Int]]");
    }
    
    // ===== RecordType Tests =====
    
    #[test]
    fn test_record_creation() {
        let record = RecordType::new()
            .with_field("name", TypedValue::primitive(PrimitiveType::string()))
            .with_field("age", TypedValue::primitive(PrimitiveType::int()));
        
        assert_eq!(record.fields.len(), 2);
        assert!(record.fields.contains_key("name"));
        assert!(record.fields.contains_key("age"));
    }
    
    #[test]
    fn test_empty_record() {
        let record = RecordType::new();
        assert_eq!(record.fields.len(), 0);
        assert_eq!(record.to_string(), "{}");
    }
    
    #[test]
    fn test_record_field_access() {
        let record = RecordType::new()
            .with_field("x", TypedValue::primitive(PrimitiveType::float()));
        
        assert!(record.fields.get("x").is_some());
        assert!(record.fields.get("y").is_none());
    }
    
    // ===== VariantType Tests =====
    
    #[test]
    fn test_variant_creation() {
        let variant = VariantType::new()
            .with_variant("None", None)
            .with_variant("Some", Some(TypedValue::primitive(PrimitiveType::int())));
        
        assert_eq!(variant.variants.len(), 2);
    }
    
    #[test]
    fn test_variant_simple_enum() {
        let color = VariantType::new()
            .with_variant("Red", None)
            .with_variant("Green", None)
            .with_variant("Blue", None);
        
        assert_eq!(color.variants.len(), 3);
        assert!(color.variants.get("Red").unwrap().is_none());
    }
    
    // ===== TypeVariable Tests =====
    
    #[test]
    fn test_type_variable_creation() {
        let var = TypeVariable::new("a");
        assert_eq!(var.name, "a");
        assert!(var.constraints.is_empty());
        assert_eq!(var.to_string(), "a");
    }
    
    #[test]
    fn test_type_variable_with_constraints() {
        let var = TypeVariable::new("T")
            .with_constraint(TypeConstraint::Numeric)
            .with_constraint(TypeConstraint::Comparable);
        
        assert_eq!(var.constraints.len(), 2);
        assert!(var.to_string().contains("where T:"));
    }
    
    #[test]
    fn test_type_constraint_display() {
        assert_eq!(TypeConstraint::Numeric.to_string(), "Num");
        assert_eq!(TypeConstraint::Comparable.to_string(), "Ord");
        assert_eq!(TypeConstraint::HasTrait("Show".to_string()).to_string(), "Show");
        assert_eq!(TypeConstraint::SubtypeOf("Animal".to_string()).to_string(), "<: Animal");
    }
    
    // ===== TypedValue Tests =====
    
    #[test]
    fn test_typed_value_primitive() {
        let int_val = TypedValue::primitive(PrimitiveType::int());
        assert_eq!(int_val.kind(), TypeKind::Primitive);
        assert!(int_val.is_pure());
        assert_eq!(int_val.to_string(), "Int");
    }
    
    #[test]
    fn test_typed_value_with_effects() {
        let mut effects = HashSet::new();
        effects.insert(EffectType::IO);
        effects.insert(EffectType::State);
        
        let mut val = TypedValue::primitive(PrimitiveType::string());
        val.effects = effects.clone();
        
        assert!(!val.is_pure());
        assert_eq!(val.effects.len(), 2);
    }
    
    #[test]
    fn test_typed_value_pure_with_pure_effect() {
        let mut effects = HashSet::new();
        effects.insert(EffectType::Pure);
        
        let mut val = TypedValue::primitive(PrimitiveType::int());
        val.effects = effects;
        
        assert!(val.is_pure());
    }
    
    #[test]
    fn test_typed_value_function() {
        let func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int()),
        );
        
        let typed_func = TypedValue::function(func);
        assert_eq!(typed_func.kind(), TypeKind::Function);
        assert!(typed_func.is_pure());
    }
    
    #[test]
    fn test_typed_value_list() {
        let list = ListType::new(TypedValue::primitive(PrimitiveType::string()));
        let typed_list = TypedValue::list(list);
        
        assert_eq!(typed_list.kind(), TypeKind::List);
        assert_eq!(typed_list.to_string(), "[String]");
    }
    
    #[test]
    fn test_typed_value_tuple() {
        let tuple = TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
            TypedValue::primitive(PrimitiveType::bool()),
        ]);
        let typed_tuple = TypedValue::tuple(tuple);
        
        assert_eq!(typed_tuple.kind(), TypeKind::Tuple);
        assert_eq!(typed_tuple.to_string(), "(Int, Bool)");
    }
    
    #[test]
    fn test_typed_value_record() {
        let record = RecordType::new()
            .with_field("x", TypedValue::primitive(PrimitiveType::float()))
            .with_field("y", TypedValue::primitive(PrimitiveType::float()));
        
        let typed_record = TypedValue::record(record);
        
        assert_eq!(typed_record.kind(), TypeKind::Record);
    }
    
    #[test]
    fn test_typed_value_variant() {
        let variant = VariantType::new()
            .with_variant("Ok", Some(TypedValue::primitive(PrimitiveType::int())))
            .with_variant("Err", Some(TypedValue::primitive(PrimitiveType::string())));
        
        let typed_variant = TypedValue::variant(variant);
        
        assert_eq!(typed_variant.kind(), TypeKind::Variant);
    }
    
    #[test]
    fn test_typed_value_variable() {
        let var = TypeVariable::new("T");
        let typed_var = TypedValue::variable(var);
        
        assert_eq!(typed_var.kind(), TypeKind::TypeVariable);
        assert_eq!(typed_var.to_string(), "T");
    }
    
    // ===== Complex Type Tests =====
    
    #[test]
    fn test_function_returning_function() {
        // (Int → Int) → (Int → Int)
        let inner_func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int()),
        );
        
        let outer_func = FunctionType::new(
            vec![TypedValue::function(inner_func.clone())],
            TypedValue::function(inner_func),
        );
        
        let typed = TypedValue::function(outer_func);
        assert_eq!(typed.kind(), TypeKind::Function);
    }
    
    #[test]
    fn test_list_of_tuples() {
        // [(Int, String)]
        let tuple = TupleType::new(vec![
            TypedValue::primitive(PrimitiveType::int()),
            TypedValue::primitive(PrimitiveType::string()),
        ]);
        
        let list = ListType::new(TypedValue::tuple(tuple));
        let typed = TypedValue::list(list);
        
        assert_eq!(typed.to_string(), "[(Int, String)]");
    }
    
    #[test]
    fn test_record_with_function_field() {
        let handler = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::unit()),
        );
        
        let record = RecordType::new()
            .with_field("handler", TypedValue::function(handler))
            .with_field("name", TypedValue::primitive(PrimitiveType::string()));
        
        let typed = TypedValue::record(record);
        
        assert_eq!(typed.kind(), TypeKind::Record);
    }
    
    // ===== Effect Propagation Tests =====
    
    #[test]
    fn test_effect_propagation_in_function() {
        let mut param_effects = HashSet::new();
        param_effects.insert(EffectType::IO);
        
        let mut param = TypedValue::primitive(PrimitiveType::string());
        param.effects = param_effects.clone();
        
        let func = FunctionType::new(
            vec![param],
            TypedValue::primitive(PrimitiveType::unit()),
        );
        
        let typed_func = TypedValue::function(func);
        // Function should collect effects from parameters
        assert!(!typed_func.effects.is_empty());
    }
    
    #[test]
    fn test_effect_propagation_in_list() {
        let mut element_effects = HashSet::new();
        element_effects.insert(EffectType::State);
        
        let mut element = TypedValue::primitive(PrimitiveType::int());
        element.effects = element_effects.clone();
        
        let list = ListType::new(element);
        let typed_list = TypedValue::list(list);
        
        // List should propagate element effects
        assert_eq!(typed_list.effects, element_effects);
    }
    
    // ===== Edge Cases =====
    
    #[test]
    fn test_deeply_nested_types() {
        // [[[Int]]]
        let level1 = ListType::new(TypedValue::primitive(PrimitiveType::int()));
        let level2 = ListType::new(TypedValue::list(level1));
        let level3 = ListType::new(TypedValue::list(level2));
        
        let typed = TypedValue::list(level3);
        assert_eq!(typed.to_string(), "[[[Int]]]");
    }
    
    #[test]
    fn test_empty_variant() {
        let variant = VariantType::new();
        let typed = TypedValue::variant(variant);
        
        assert_eq!(typed.kind(), TypeKind::Variant);
    }
    
    #[test]
    fn test_type_equality() {
        let int1 = TypedValue::primitive(PrimitiveType::int());
        let int2 = TypedValue::primitive(PrimitiveType::int());
        let float = TypedValue::primitive(PrimitiveType::float());
        
        assert_eq!(int1, int2);
        assert_ne!(int1, float);
    }
}