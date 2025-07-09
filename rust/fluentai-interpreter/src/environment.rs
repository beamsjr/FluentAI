//! Environment management for lexical scoping

use indexmap::IndexMap;
use rustc_hash::FxHashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::error::{InterpreterError, InterpreterResult};
use crate::value::Value;

/// A binding in the environment
#[derive(Debug, Clone)]
pub struct Binding {
    /// The value
    pub value: Value,
    /// Whether this binding is mutable
    pub mutable: bool,
    /// Whether this binding is exported (for modules)
    pub exported: bool,
}

/// Environment for variable bindings with lexical scoping
#[derive(Debug, Clone)]
pub struct Environment {
    /// Inner mutable state
    inner: Rc<RefCell<EnvironmentInner>>,
}

#[derive(Debug)]
struct EnvironmentInner {
    /// Variable bindings in this scope
    bindings: FxHashMap<String, Binding>,
    /// Parent environment (for lexical scoping)
    parent: Option<Environment>,
    /// Module namespace if this is a module environment
    module_name: Option<String>,
    /// Cache for fast lookups (inline caching)
    lookup_cache: FxHashMap<String, Value>,
}

impl Environment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvironmentInner {
                bindings: FxHashMap::default(),
                parent: None,
                module_name: None,
                lookup_cache: FxHashMap::default(),
            })),
        }
    }

    /// Create a new environment with a parent
    pub fn with_parent(parent: Environment) -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvironmentInner {
                bindings: FxHashMap::default(),
                parent: Some(parent),
                module_name: None,
                lookup_cache: FxHashMap::default(),
            })),
        }
    }

    /// Create a module environment
    pub fn for_module(name: String, parent: Environment) -> Self {
        Self {
            inner: Rc::new(RefCell::new(EnvironmentInner {
                bindings: FxHashMap::default(),
                parent: Some(parent),
                module_name: Some(name),
                lookup_cache: FxHashMap::default(),
            })),
        }
    }

    /// Extend this environment with a new child scope
    pub fn extend(&self) -> Self {
        Self::with_parent(self.clone())
    }

    /// Bind a variable to a value
    pub fn bind(&self, name: String, value: Value) -> InterpreterResult<()> {
        self.bind_with_options(name, value, false, false)
    }

    /// Bind a variable with options
    pub fn bind_with_options(
        &self,
        name: String,
        value: Value,
        mutable: bool,
        exported: bool,
    ) -> InterpreterResult<()> {
        let mut inner = self.inner.borrow_mut();

        // Check if already bound in this scope
        if inner.bindings.contains_key(&name) {
            return Err(InterpreterError::NameError(format!(
                "Variable '{}' already defined in this scope",
                name
            )));
        }

        // Clear cache entry if exists
        inner.lookup_cache.remove(&name);

        inner.bindings.insert(
            name,
            Binding {
                value,
                mutable,
                exported,
            },
        );

        Ok(())
    }

    /// Define a variable (for contract predicates - allows rebinding)
    pub fn define(&self, name: String, value: Value) {
        let mut inner = self.inner.borrow_mut();
        inner.lookup_cache.remove(&name);
        inner.bindings.insert(
            name,
            Binding {
                value,
                mutable: true,
                exported: false,
            },
        );
    }

    /// Update an existing binding
    pub fn update(&self, name: &str, value: Value) -> InterpreterResult<()> {
        let mut inner = self.inner.borrow_mut();

        // Try to update in current scope
        if let Some(binding) = inner.bindings.get_mut(name) {
            if !binding.mutable {
                return Err(InterpreterError::NameError(format!(
                    "Cannot mutate immutable binding '{}'",
                    name
                )));
            }
            binding.value = value;
            inner.lookup_cache.remove(name);
            return Ok(());
        }

        // Try parent scope
        if let Some(parent) = inner.parent.clone() {
            drop(inner); // Release borrow
            parent.update(name, value)
        } else {
            Err(InterpreterError::NameError(format!(
                "Undefined variable '{}'",
                name
            )))
        }
    }

    /// Look up a variable
    pub fn lookup(&self, name: &str) -> Option<Value> {
        let mut inner = self.inner.borrow_mut();

        // Check cache first
        if let Some(value) = inner.lookup_cache.get(name) {
            return Some(value.clone());
        }

        // Check current scope
        if let Some(binding) = inner.bindings.get(name) {
            let value = binding.value.clone();
            // Cache the lookup
            inner.lookup_cache.insert(name.to_string(), value.clone());
            return Some(value);
        }

        // Check parent scope
        if let Some(parent) = inner.parent.clone() {
            drop(inner); // Release borrow
            parent.lookup(name)
        } else {
            None
        }
    }

    /// Get all exported bindings (for modules)
    pub fn get_exports(&self) -> IndexMap<String, Value> {
        let inner = self.inner.borrow();
        let mut exports = IndexMap::new();

        for (name, binding) in &inner.bindings {
            if binding.exported {
                exports.insert(name.clone(), binding.value.clone());
            }
        }

        exports
    }

    /// Get module name if this is a module environment
    pub fn module_name(&self) -> Option<String> {
        self.inner.borrow().module_name.clone()
    }

    /// Clear the lookup cache (useful after bulk updates)
    pub fn clear_cache(&self) {
        self.inner.borrow_mut().lookup_cache.clear();
    }

    /// Get the size of this environment (for debugging)
    pub fn size(&self) -> usize {
        self.inner.borrow().bindings.len()
    }

    /// Get the depth of the environment chain
    pub fn depth(&self) -> usize {
        let inner = self.inner.borrow();
        match &inner.parent {
            Some(parent) => 1 + parent.depth(),
            None => 1,
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::ValueData;

    #[test]
    fn test_basic_binding() {
        let env = Environment::new();
        let val = Value::new(ValueData::Integer(42));

        env.bind("x".to_string(), val.clone()).unwrap();
        let retrieved = env.lookup("x").unwrap();

        assert_eq!(retrieved.to_integer(), Some(42));
    }

    #[test]
    fn test_scoping() {
        let parent = Environment::new();
        parent
            .bind("x".to_string(), Value::new(ValueData::Integer(1)))
            .unwrap();

        let child = parent.extend();
        child
            .bind("x".to_string(), Value::new(ValueData::Integer(2)))
            .unwrap();

        assert_eq!(child.lookup("x").unwrap().to_integer(), Some(2));
        assert_eq!(parent.lookup("x").unwrap().to_integer(), Some(1));
    }

    #[test]
    fn test_update_mutable() {
        let env = Environment::new();
        env.bind_with_options(
            "x".to_string(),
            Value::new(ValueData::Integer(1)),
            true,
            false,
        )
        .unwrap();

        env.update("x", Value::new(ValueData::Integer(2))).unwrap();
        assert_eq!(env.lookup("x").unwrap().to_integer(), Some(2));
    }

    #[test]
    fn test_update_immutable_fails() {
        let env = Environment::new();
        env.bind("x".to_string(), Value::new(ValueData::Integer(1)))
            .unwrap();

        let result = env.update("x", Value::new(ValueData::Integer(2)));
        assert!(result.is_err());
    }
}
