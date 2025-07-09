//! Module execution environments

use crate::ModuleInfo;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Represents a value in the module environment
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ModuleValue {
    /// A simple value (will be replaced with actual VM value type)
    Value(serde_json::Value),

    /// A reference to an exported value from another module
    ModuleRef {
        module_id: String,
        export_name: String,
    },
}

/// Environment for module execution
#[derive(Debug, Clone)]
pub struct ModuleEnvironment {
    /// Current module being executed
    pub current_module: Option<Arc<ModuleInfo>>,

    /// Bindings in this environment
    bindings: FxHashMap<String, ModuleValue>,

    /// Imported modules (module alias -> module info)
    imports: FxHashMap<String, Arc<ModuleInfo>>,

    /// Parent environment (for nested scopes)
    parent: Option<Box<ModuleEnvironment>>,
}

impl ModuleEnvironment {
    /// Create a new empty environment
    pub fn new() -> Self {
        Self {
            current_module: None,
            bindings: FxHashMap::default(),
            imports: FxHashMap::default(),
            parent: None,
        }
    }

    /// Create a new environment for a module
    pub fn for_module(module: Arc<ModuleInfo>) -> Self {
        Self {
            current_module: Some(module),
            bindings: FxHashMap::default(),
            imports: FxHashMap::default(),
            parent: None,
        }
    }

    /// Create a child environment
    pub fn child(&self) -> Self {
        Self {
            current_module: self.current_module.clone(),
            bindings: FxHashMap::default(),
            imports: self.imports.clone(),
            parent: Some(Box::new(self.clone())),
        }
    }

    /// Define a binding in this environment
    pub fn define(&mut self, name: String, value: ModuleValue) {
        self.bindings.insert(name, value);
    }

    /// Look up a binding in this environment or parent environments
    pub fn lookup(&self, name: &str) -> Option<&ModuleValue> {
        self.bindings
            .get(name)
            .or_else(|| self.parent.as_ref().and_then(|p| p.lookup(name)))
    }

    /// Look up a qualified name (module.name)
    pub fn lookup_qualified(&self, module_name: &str, var_name: &str) -> Option<ModuleValue> {
        // Check if it's an imported module
        if let Some(module) = self.imports.get(module_name) {
            // Check if the variable is exported
            if module.exports.contains(&var_name.to_string()) {
                // Return a module reference
                return Some(ModuleValue::ModuleRef {
                    module_id: module.id.clone(),
                    export_name: var_name.to_string(),
                });
            }
        }
        None
    }

    /// Add an imported module
    pub fn add_import(&mut self, alias: String, module: Arc<ModuleInfo>) {
        self.imports.insert(alias, module);
    }

    /// Get all imported modules
    pub fn imports(&self) -> &FxHashMap<String, Arc<ModuleInfo>> {
        &self.imports
    }

    /// Get all bindings in this environment (not including parent)
    pub fn bindings(&self) -> &FxHashMap<String, ModuleValue> {
        &self.bindings
    }

    /// Check if a name is defined in this environment (not including parent)
    pub fn is_defined(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    /// Get only the exported bindings from this environment
    pub fn exported_bindings(&self) -> FxHashMap<String, ModuleValue> {
        if let Some(module) = &self.current_module {
            self.bindings
                .iter()
                .filter(|(name, _)| module.exports.contains(name))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect()
        } else {
            FxHashMap::default()
        }
    }
}

impl Default for ModuleEnvironment {
    fn default() -> Self {
        Self::new()
    }
}

/// Module-aware binding resolver
pub struct BindingResolver {
    /// Stack of environments
    environments: Vec<ModuleEnvironment>,
}

impl BindingResolver {
    /// Create a new resolver with an initial environment
    pub fn new(initial: ModuleEnvironment) -> Self {
        Self {
            environments: vec![initial],
        }
    }

    /// Push a new environment onto the stack
    pub fn push(&mut self, env: ModuleEnvironment) {
        self.environments.push(env);
    }

    /// Pop an environment from the stack
    pub fn pop(&mut self) -> Option<ModuleEnvironment> {
        if self.environments.len() > 1 {
            self.environments.pop()
        } else {
            None
        }
    }

    /// Get the current environment
    pub fn current(&self) -> Option<&ModuleEnvironment> {
        self.environments.last()
    }

    /// Get the current environment mutably
    pub fn current_mut(&mut self) -> Option<&mut ModuleEnvironment> {
        self.environments.last_mut()
    }

    /// Resolve a binding in the current environment
    pub fn resolve(&self, name: &str) -> Option<&ModuleValue> {
        self.current()?.lookup(name)
    }

    /// Resolve a qualified binding in the current environment
    pub fn resolve_qualified(&self, module: &str, name: &str) -> Option<ModuleValue> {
        self.current()?.lookup_qualified(module, name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{Graph, NodeId};
    use std::path::PathBuf;

    fn create_test_module(id: &str, exports: Vec<String>) -> Arc<ModuleInfo> {
        Arc::new(ModuleInfo {
            id: id.to_string(),
            name: id.to_string(),
            path: PathBuf::from(format!("{}.ai", id)),
            graph: Graph::new(),
            root: NodeId::new(1).unwrap(),
            exports,
            dependencies: vec![],
            metadata: FxHashMap::default(),
        })
    }

    #[test]
    fn test_environment_basic() {
        let mut env = ModuleEnvironment::new();

        env.define("x".to_string(), ModuleValue::Value(serde_json::json!(42)));

        assert!(env.is_defined("x"));
        assert!(matches!(env.lookup("x"), Some(ModuleValue::Value(_))));
        assert!(env.lookup("y").is_none());
    }

    #[test]
    fn test_environment_child() {
        let mut parent = ModuleEnvironment::new();
        parent.define("x".to_string(), ModuleValue::Value(serde_json::json!(10)));

        let mut child = parent.child();
        child.define("y".to_string(), ModuleValue::Value(serde_json::json!(20)));

        // Child can see both bindings
        assert!(child.lookup("x").is_some());
        assert!(child.lookup("y").is_some());

        // Parent can only see its own
        assert!(parent.lookup("x").is_some());
        assert!(parent.lookup("y").is_none());
    }

    #[test]
    fn test_module_imports() {
        let math_module = create_test_module("math", vec!["sin".to_string(), "cos".to_string()]);

        let mut env = ModuleEnvironment::new();
        env.add_import("math".to_string(), math_module);

        // Can look up exported functions
        assert!(env.lookup_qualified("math", "sin").is_some());
        assert!(env.lookup_qualified("math", "cos").is_some());

        // Cannot look up non-exported functions
        assert!(env.lookup_qualified("math", "internal_fn").is_none());
    }
}
