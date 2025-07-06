#[cfg(test)]
mod tests {
    use crate::environment::{TypeEnvironment, TypeEnvironmentBuilder};
    use crate::types::{PrimitiveType, FunctionType, ListType, TypedValue, RecordType};
    use fluentai_core::ast::EffectType;
    use std::collections::HashSet;
    use rustc_hash::FxHashMap;
    
    // ===== Basic Environment Tests =====
    
    #[test]
    fn test_new_environment() {
        let env = TypeEnvironment::new();
        
        // Should have primitive types pre-loaded
        assert!(env.lookup("Int").is_some());
        assert!(env.lookup("Float").is_some());
        assert!(env.lookup("String").is_some());
        assert!(env.lookup("Bool").is_some());
        assert!(env.lookup("Unit").is_some());
    }
    
    #[test]
    fn test_primitive_types_correct() {
        let env = TypeEnvironment::new();
        
        assert_eq!(env.lookup("Int").unwrap().to_string(), "Int");
        assert_eq!(env.lookup("Float").unwrap().to_string(), "Float");
        assert_eq!(env.lookup("String").unwrap().to_string(), "String");
        assert_eq!(env.lookup("Bool").unwrap().to_string(), "Bool");
        assert_eq!(env.lookup("Unit").unwrap().to_string(), "Unit");
    }
    
    // ===== Binding Tests =====
    
    #[test]
    fn test_bind_and_lookup() {
        let mut env = TypeEnvironment::new();
        let int_type = TypedValue::primitive(PrimitiveType::int());
        
        env.bind("x", int_type.clone());
        
        assert!(env.lookup("x").is_some());
        assert_eq!(env.lookup("x").unwrap(), &int_type);
    }
    
    #[test]
    fn test_bind_overwrites() {
        let mut env = TypeEnvironment::new();
        
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        env.bind("x", TypedValue::primitive(PrimitiveType::string()));
        
        assert_eq!(env.lookup("x").unwrap().to_string(), "String");
    }
    
    #[test]
    fn test_lookup_nonexistent() {
        let env = TypeEnvironment::new();
        assert!(env.lookup("undefined_var").is_none());
    }
    
    // ===== Scope Tests =====
    
    #[test]
    fn test_push_pop_scope() {
        let mut env = TypeEnvironment::new();
        
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        env.push_scope();
        env.bind("y", TypedValue::primitive(PrimitiveType::string()));
        
        // Both should be visible
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());
        
        env.pop_scope();
        
        // Only x should remain
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_none());
    }
    
    #[test]
    fn test_shadowing_in_nested_scope() {
        let mut env = TypeEnvironment::new();
        
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        assert_eq!(env.lookup("x").unwrap().to_string(), "Int");
        
        env.push_scope();
        env.bind("x", TypedValue::primitive(PrimitiveType::string()));
        assert_eq!(env.lookup("x").unwrap().to_string(), "String");
        
        env.pop_scope();
        assert_eq!(env.lookup("x").unwrap().to_string(), "Int");
    }
    
    #[test]
    fn test_multiple_scopes() {
        let mut env = TypeEnvironment::new();
        
        env.bind("a", TypedValue::primitive(PrimitiveType::int()));
        
        env.push_scope();
        env.bind("b", TypedValue::primitive(PrimitiveType::string()));
        
        env.push_scope();
        env.bind("c", TypedValue::primitive(PrimitiveType::bool()));
        
        // All should be visible
        assert!(env.lookup("a").is_some());
        assert!(env.lookup("b").is_some());
        assert!(env.lookup("c").is_some());
        
        env.pop_scope();
        assert!(env.lookup("a").is_some());
        assert!(env.lookup("b").is_some());
        assert!(env.lookup("c").is_none());
        
        env.pop_scope();
        assert!(env.lookup("a").is_some());
        assert!(env.lookup("b").is_none());
        assert!(env.lookup("c").is_none());
    }
    
    #[test]
    fn test_pop_scope_minimum() {
        let mut env = TypeEnvironment::new();
        
        // Should maintain at least one scope
        env.pop_scope();
        env.pop_scope();
        env.pop_scope();
        
        // Should still be able to bind
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        assert!(env.lookup("x").is_some());
    }
    
    // ===== Type Variable Tests =====
    
    #[test]
    fn test_fresh_type_var() {
        let mut env = TypeEnvironment::new();
        
        let var1 = env.fresh_type_var("T");
        let var2 = env.fresh_type_var("T");
        let var3 = env.fresh_type_var("U");
        
        assert_eq!(var1.name, "T0");
        assert_eq!(var2.name, "T1");
        assert_eq!(var3.name, "U2");
    }
    
    #[test]
    fn test_fresh_type() {
        let mut env = TypeEnvironment::new();
        
        let ty1 = env.fresh_type("a");
        let ty2 = env.fresh_type("a");
        
        assert_ne!(ty1.to_string(), ty2.to_string());
    }
    
    // ===== Type Definition Tests =====
    
    #[test]
    fn test_define_type() {
        let mut env = TypeEnvironment::new();
        
        let list_int = TypedValue::list(ListType::new(
            TypedValue::primitive(PrimitiveType::int())
        ));
        
        env.define_type("IntList", list_int.clone());
        
        assert!(env.lookup("IntList").is_some());
        assert_eq!(env.lookup("IntList").unwrap(), &list_int);
    }
    
    #[test]
    fn test_type_definitions_persist_across_scopes() {
        let mut env = TypeEnvironment::new();
        
        env.define_type("MyType", TypedValue::primitive(PrimitiveType::string()));
        
        env.push_scope();
        assert!(env.lookup("MyType").is_some());
        
        env.pop_scope();
        assert!(env.lookup("MyType").is_some());
    }
    
    // ===== Current Bindings Tests =====
    
    #[test]
    fn test_current_bindings() {
        let mut env = TypeEnvironment::new();
        
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        env.bind("y", TypedValue::primitive(PrimitiveType::string()));
        
        let bindings = env.current_bindings();
        
        // Should include user bindings and primitives
        assert!(bindings.iter().any(|(n, _)| n == "x"));
        assert!(bindings.iter().any(|(n, _)| n == "y"));
        assert!(bindings.iter().any(|(n, _)| n == "Int"));
    }
    
    #[test]
    fn test_current_bindings_with_shadowing() {
        let mut env = TypeEnvironment::new();
        
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        env.push_scope();
        env.bind("x", TypedValue::primitive(PrimitiveType::string()));
        
        let bindings = env.current_bindings();
        
        // Should only have one "x" binding (the inner one)
        let x_bindings: Vec<_> = bindings.iter()
            .filter(|(n, _)| n == "x")
            .collect();
        
        assert_eq!(x_bindings.len(), 1);
        assert_eq!(x_bindings[0].1.to_string(), "String");
    }
    
    // ===== Environment Builder Tests =====
    
    #[test]
    fn test_environment_builder_basic() {
        let env = TypeEnvironmentBuilder::new()
            .with_binding("x", TypedValue::primitive(PrimitiveType::int()))
            .with_binding("y", TypedValue::primitive(PrimitiveType::string()))
            .build();
        
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());
        assert_eq!(env.lookup("x").unwrap().to_string(), "Int");
        assert_eq!(env.lookup("y").unwrap().to_string(), "String");
    }
    
    #[test]
    fn test_environment_builder_with_type_def() {
        let list_type = TypedValue::list(ListType::new(
            TypedValue::primitive(PrimitiveType::float())
        ));
        
        let env = TypeEnvironmentBuilder::new()
            .with_type_def("FloatList", list_type.clone())
            .build();
        
        assert!(env.lookup("FloatList").is_some());
        assert_eq!(env.lookup("FloatList").unwrap(), &list_type);
    }
    
    #[test]
    fn test_environment_builder_multiple_bindings() {
        let env = TypeEnvironmentBuilder::new()
            .with_binding("x", TypedValue::primitive(PrimitiveType::int()))
            .with_binding("y", TypedValue::primitive(PrimitiveType::float()))
            .build();
        
        // Both bindings should be present
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("y").is_some());
        assert_eq!(env.lookup("x").unwrap().to_string(), "Int");
        assert_eq!(env.lookup("y").unwrap().to_string(), "Float");
    }
    
    #[test]
    fn test_environment_builder_chain() {
        let func_type = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::int())
        );
        
        let env = TypeEnvironmentBuilder::new()
            .with_binding("x", TypedValue::primitive(PrimitiveType::int()))
            .with_binding("f", TypedValue::function(func_type))
            .with_type_def("Number", TypedValue::primitive(PrimitiveType::float()))
            .build();
        
        assert!(env.lookup("x").is_some());
        assert!(env.lookup("f").is_some());
        assert!(env.lookup("Number").is_some());
    }
    
    // ===== Complex Scenarios =====
    
    #[test]
    fn test_function_type_binding() {
        let mut env = TypeEnvironment::new();
        
        let func = FunctionType::new(
            vec![
                TypedValue::primitive(PrimitiveType::int()),
                TypedValue::primitive(PrimitiveType::int())
            ],
            TypedValue::primitive(PrimitiveType::int())
        );
        
        env.bind("add", TypedValue::function(func));
        
        let add_type = env.lookup("add").unwrap();
        assert_eq!(add_type.to_string(), "Int → Int → Int");
    }
    
    #[test]
    fn test_polymorphic_type_binding() {
        let mut env = TypeEnvironment::new();
        
        let type_var = env.fresh_type("a");
        let list_type = TypedValue::list(ListType::new(type_var));
        
        env.bind("empty_list", list_type);
        
        assert!(env.lookup("empty_list").is_some());
    }
    
    #[test]
    fn test_effect_types_in_environment() {
        let mut env = TypeEnvironment::new();
        
        let io_func = FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::string())],
            TypedValue::primitive(PrimitiveType::unit())
        );
        
        let mut effects = HashSet::new();
        effects.insert(EffectType::IO);
        
        let typed_func = io_func.with_effects(effects);
        env.bind("print", typed_func);
        
        let print_type = env.lookup("print").unwrap();
        assert!(!print_type.is_pure());
    }
    
    #[test]
    fn test_nested_scope_complex() {
        let mut env = TypeEnvironment::new();
        
        // Global scope
        env.bind("global", TypedValue::primitive(PrimitiveType::int()));
        
        env.push_scope();
        // Function scope
        env.bind("param", TypedValue::primitive(PrimitiveType::string()));
        
        env.push_scope();
        // Let binding scope
        env.bind("local", TypedValue::primitive(PrimitiveType::bool()));
        env.bind("param", TypedValue::primitive(PrimitiveType::float())); // Shadow
        
        // Check visibility
        assert_eq!(env.lookup("global").unwrap().to_string(), "Int");
        assert_eq!(env.lookup("param").unwrap().to_string(), "Float");
        assert_eq!(env.lookup("local").unwrap().to_string(), "Bool");
        
        env.pop_scope();
        
        // Local should be gone, param should revert
        assert_eq!(env.lookup("param").unwrap().to_string(), "String");
        assert!(env.lookup("local").is_none());
    }
    
    // ===== Thread Safety Tests =====
    
    #[test]
    fn test_clone_environment() {
        let mut env1 = TypeEnvironment::new();
        env1.bind("x", TypedValue::primitive(PrimitiveType::int()));
        
        let mut env2 = env1.clone();
        env2.bind("y", TypedValue::primitive(PrimitiveType::string()));
        
        // Changes to env2 shouldn't affect env1
        assert!(env1.lookup("x").is_some());
        assert!(env1.lookup("y").is_none());
        
        assert!(env2.lookup("x").is_some());
        assert!(env2.lookup("y").is_some());
    }
    
    #[test]
    fn test_fresh_vars_after_clone() {
        let mut env1 = TypeEnvironment::new();
        let var1 = env1.fresh_type_var("T");
        
        let mut env2 = env1.clone();
        let var2 = env2.fresh_type_var("T");
        
        // Should continue from the same counter
        assert_eq!(var1.name, "T0");
        assert_eq!(var2.name, "T1");
    }

    // ===== Additional Edge Case Tests =====

    #[test]
    fn test_environment_default() {
        let env1 = TypeEnvironment::new();
        let env2 = TypeEnvironment::default();
        
        // Both should have the same primitive types
        assert_eq!(env1.lookup("Int"), env2.lookup("Int"));
        assert_eq!(env1.lookup("String"), env2.lookup("String"));
    }

    #[test]
    fn test_child_environment() {
        let mut parent = TypeEnvironment::new();
        parent.bind("x", TypedValue::primitive(PrimitiveType::int()));
        
        let mut child = parent.child();
        
        // Child should have parent's bindings
        assert!(child.lookup("x").is_some());
        assert_eq!(child.lookup("x").unwrap().to_string(), "Int");
        
        // Child should have one more scope than parent
        assert_eq!(child.scope_depth(), parent.scope_depth() + 1);
        
        // Binding in child shouldn't affect parent
        child.bind("y", TypedValue::primitive(PrimitiveType::string()));
        assert!(child.lookup("y").is_some());
        assert!(parent.lookup("y").is_none());
    }

    #[test]
    fn test_type_definition_lookup() {
        let mut env = TypeEnvironment::new();
        
        // Define a custom type alias
        let my_list = TypedValue::list(ListType::new(
            TypedValue::primitive(PrimitiveType::int())
        ));
        env.define_type("IntList", my_list.clone());
        
        // Should be able to look it up via regular lookup
        assert!(env.lookup("IntList").is_some());
        assert_eq!(env.lookup("IntList").unwrap(), &my_list);
        
        // Should not find undefined types
        assert!(env.lookup("UndefinedType").is_none());
    }

    #[test]
    fn test_scope_depth() {
        let mut env = TypeEnvironment::new();
        
        // Initial scope depth
        let initial_depth = env.scope_depth();
        
        // Push some scopes
        env.push_scope();
        assert_eq!(env.scope_depth(), initial_depth + 1);
        
        env.push_scope();
        assert_eq!(env.scope_depth(), initial_depth + 2);
        
        // Pop scopes
        env.pop_scope();
        assert_eq!(env.scope_depth(), initial_depth + 1);
        
        env.pop_scope();
        assert_eq!(env.scope_depth(), initial_depth);
    }

    #[test]
    fn test_type_variables_are_unique() {
        let mut env1 = TypeEnvironment::new();
        let mut env2 = TypeEnvironment::new();
        
        // Fresh type variables from same environment should be unique
        let t1 = env1.fresh_type("T");
        let t2 = env1.fresh_type("T");
        
        // Even with same prefix, they should be different
        assert_ne!(t1.to_string(), t2.to_string());
        
        // Different environments start with same counter, so they would produce same names
        let t3 = env2.fresh_type("T");
        assert_eq!(t3.to_string(), "T0"); // Same as t1 was
    }

    #[test]
    fn test_complex_type_definitions() {
        let mut env = TypeEnvironment::new();
        
        // Define a function type
        let int_to_string = TypedValue::function(FunctionType::new(
            vec![TypedValue::primitive(PrimitiveType::int())],
            TypedValue::primitive(PrimitiveType::string()),
        ));
        env.define_type("IntToString", int_to_string.clone());
        
        // Define a record type
        let mut fields = FxHashMap::default();
        fields.insert("x".to_string(), TypedValue::primitive(PrimitiveType::float()));
        fields.insert("y".to_string(), TypedValue::primitive(PrimitiveType::float()));
        let point = TypedValue::record(RecordType { fields });
        env.define_type("Point", point.clone());
        
        // Both should be retrievable
        assert_eq!(env.lookup("IntToString"), Some(&int_to_string));
        assert_eq!(env.lookup("Point"), Some(&point));
    }

    #[test]
    fn test_pop_scope_beyond_initial() {
        let mut env = TypeEnvironment::new();
        
        // Should have at least one scope initially
        let initial_depth = env.scope_depth();
        assert!(initial_depth > 0);
        
        // Pop all scopes except the last one
        for _ in 1..initial_depth {
            env.pop_scope();
        }
        
        // Should still have one scope
        assert_eq!(env.scope_depth(), 1);
        
        // Popping the last scope should have no effect (maintains at least 1)
        env.pop_scope();
        assert_eq!(env.scope_depth(), 1); // Maintains at least 1 scope
    }

    #[test]
    fn test_environment_builder_default() {
        let builder = TypeEnvironmentBuilder::new();
        let env = builder.build();
        
        // Should have primitive types from default environment
        assert!(env.lookup("Int").is_some());
        assert!(env.lookup("String").is_some());
        assert!(env.lookup("Bool").is_some());
    }
}