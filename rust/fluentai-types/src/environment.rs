//! Type environment for managing type bindings and scopes

use crate::types::{PrimitiveType, TypeVariable, TypedValue};
use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicU32, Ordering};

/// Type environment for type checking and inference
#[derive(Debug, Clone)]
pub struct TypeEnvironment {
    /// Stack of binding scopes
    scopes: Vec<FxHashMap<String, TypedValue>>,
    /// Type variable counter for generating fresh variables
    type_var_counter: u32,
    /// Global type definitions
    type_definitions: FxHashMap<String, TypedValue>,
}

impl TypeEnvironment {
    /// Create a new type environment with primitive types
    pub fn new() -> Self {
        let mut env = Self {
            scopes: vec![FxHashMap::default()],
            type_var_counter: 0,
            type_definitions: FxHashMap::default(),
        };
        env.init_primitives();
        env
    }

    /// Initialize primitive types
    fn init_primitives(&mut self) {
        // Add primitive types to the global scope
        self.bind("Int", TypedValue::primitive(PrimitiveType::int()));
        self.bind("Float", TypedValue::primitive(PrimitiveType::float()));
        self.bind("String", TypedValue::primitive(PrimitiveType::string()));
        self.bind("Bool", TypedValue::primitive(PrimitiveType::bool()));
        self.bind("Unit", TypedValue::primitive(PrimitiveType::unit()));
    }

    /// Bind a name to a type in the current scope
    pub fn bind(&mut self, name: impl Into<String>, ty: TypedValue) {
        if let Some(current_scope) = self.scopes.last_mut() {
            current_scope.insert(name.into(), ty);
        }
    }

    /// Look up a type by name, searching from innermost to outermost scope
    pub fn lookup(&self, name: &str) -> Option<&TypedValue> {
        // Search from innermost to outermost scope
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
            }
        }
        
        // Check global type definitions
        self.type_definitions.get(name)
    }

    /// Push a new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    /// Pop the current scope
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Generate a fresh type variable
    pub fn fresh_type_var(&mut self, prefix: &str) -> TypeVariable {
        let name = format!("{}{}", prefix, self.type_var_counter);
        self.type_var_counter += 1;
        TypeVariable::new(name)
    }

    /// Generate a fresh typed value with a type variable
    pub fn fresh_type(&mut self, prefix: &str) -> TypedValue {
        TypedValue::variable(self.fresh_type_var(prefix))
    }

    /// Add a global type definition
    pub fn define_type(&mut self, name: impl Into<String>, ty: TypedValue) {
        self.type_definitions.insert(name.into(), ty);
    }

    /// Get all bindings in the current scope
    pub fn current_bindings(&self) -> Vec<(String, TypedValue)> {
        let mut bindings = Vec::new();
        
        // Collect bindings from all scopes, innermost first
        for scope in self.scopes.iter().rev() {
            for (name, ty) in scope {
                // Only add if not already present (inner scopes shadow outer)
                if !bindings.iter().any(|(n, _)| n == name) {
                    bindings.push((name.clone(), ty.clone()));
                }
            }
        }
        
        bindings
    }

    /// Create a child environment with a new scope
    pub fn child(&self) -> Self {
        let mut child = self.clone();
        child.push_scope();
        child
    }

    /// Get the number of scopes
    pub fn scope_depth(&self) -> usize {
        self.scopes.len()
    }
}

impl Default for TypeEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Global type variable counter for ensuring uniqueness across environments
static GLOBAL_TYPE_VAR_COUNTER: AtomicU32 = AtomicU32::new(0);

/// Type environment builder for more complex setups
pub struct TypeEnvironmentBuilder {
    env: TypeEnvironment,
}

impl TypeEnvironmentBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            env: TypeEnvironment::new(),
        }
    }

    /// Add a type binding
    pub fn with_binding(mut self, name: impl Into<String>, ty: TypedValue) -> Self {
        self.env.bind(name, ty);
        self
    }

    /// Add a type definition
    pub fn with_type_def(mut self, name: impl Into<String>, ty: TypedValue) -> Self {
        self.env.define_type(name, ty);
        self
    }

    /// Build the environment
    pub fn build(self) -> TypeEnvironment {
        self.env
    }
}

/// Generate a globally unique type variable
pub fn fresh_global_type_var(prefix: &str) -> TypeVariable {
    let counter = GLOBAL_TYPE_VAR_COUNTER.fetch_add(1, Ordering::SeqCst);
    TypeVariable::new(format!("{}{}", prefix, counter))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::types::{FunctionType, ListType};

    #[test]
    fn test_environment_basics() {
        let mut env = TypeEnvironment::new();
        
        // Primitive types should be available
        assert!(env.lookup("Int").is_some());
        assert!(env.lookup("String").is_some());
        
        // Add a binding
        let list_type = TypedValue::list(ListType::new(
            TypedValue::primitive(PrimitiveType::int())
        ));
        env.bind("xs", list_type.clone());
        
        // Should be able to look it up
        assert_eq!(env.lookup("xs"), Some(&list_type));
    }

    #[test]
    fn test_scoping() {
        let mut env = TypeEnvironment::new();
        
        // Add binding in outer scope
        env.bind("x", TypedValue::primitive(PrimitiveType::int()));
        
        // Push new scope
        env.push_scope();
        
        // Should still see outer binding
        assert!(env.lookup("x").is_some());
        
        // Shadow with new binding
        env.bind("x", TypedValue::primitive(PrimitiveType::string()));
        
        // Should see inner binding
        if let Some(ty) = env.lookup("x") {
            match &ty.inner {
                crate::types::TypedValueInner::Primitive(p) => {
                    assert_eq!(p.name, "String");
                }
                _ => panic!("Expected primitive type"),
            }
        }
        
        // Pop scope
        env.pop_scope();
        
        // Should see outer binding again
        if let Some(ty) = env.lookup("x") {
            match &ty.inner {
                crate::types::TypedValueInner::Primitive(p) => {
                    assert_eq!(p.name, "Int");
                }
                _ => panic!("Expected primitive type"),
            }
        }
    }

    #[test]
    fn test_fresh_type_vars() {
        let mut env = TypeEnvironment::new();
        
        let var1 = env.fresh_type_var("T");
        let var2 = env.fresh_type_var("T");
        
        // Should generate different names
        assert_ne!(var1.name, var2.name);
        assert!(var1.name.starts_with("T"));
        assert!(var2.name.starts_with("T"));
    }

    #[test]
    fn test_global_type_vars() {
        let var1 = fresh_global_type_var("G");
        let var2 = fresh_global_type_var("G");
        
        // Should be globally unique
        assert_ne!(var1.name, var2.name);
    }
}

#[cfg(test)]
#[path = "environment_tests.rs"]
mod environment_tests;