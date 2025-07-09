//! Host function registration and management

use fluentai_core::value::Value;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use crate::error::{Result, RuntimeError};

/// Host function signature
pub type HostFn = Arc<dyn Fn(&[Value]) -> Result<Value> + Send + Sync>;

/// Host function metadata
#[derive(Clone)]
pub struct HostFunction {
    /// Function name
    pub name: String,
    /// Module name (optional)
    pub module: Option<String>,
    /// Number of parameters
    pub arity: usize,
    /// Variable arity (accepts any number of args >= arity)
    pub variadic: bool,
    /// Function implementation
    pub func: HostFn,
    /// Documentation
    pub doc: Option<String>,
}

impl fmt::Debug for HostFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HostFunction")
            .field("name", &self.name)
            .field("module", &self.module)
            .field("arity", &self.arity)
            .field("variadic", &self.variadic)
            .field("doc", &self.doc)
            .finish()
    }
}

impl HostFunction {
    /// Create a new host function
    pub fn new(
        name: impl Into<String>,
        arity: usize,
        func: impl Fn(&[Value]) -> Result<Value> + Send + Sync + 'static,
    ) -> Self {
        Self {
            name: name.into(),
            module: None,
            arity,
            variadic: false,
            func: Arc::new(func),
            doc: None,
        }
    }

    /// Set the module name
    pub fn with_module(mut self, module: impl Into<String>) -> Self {
        self.module = Some(module.into());
        self
    }

    /// Set variadic flag
    pub fn variadic(mut self) -> Self {
        self.variadic = true;
        self
    }

    /// Set documentation
    pub fn with_doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }

    /// Get the fully qualified name
    pub fn qualified_name(&self) -> String {
        match &self.module {
            Some(module) => format!("{}/{}", module, self.name),
            None => self.name.clone(),
        }
    }

    /// Call the host function
    pub fn call(&self, args: &[Value]) -> Result<Value> {
        // Check arity
        if !self.variadic && args.len() != self.arity {
            return Err(RuntimeError::host(format!(
                "Function '{}' expects {} arguments, got {}",
                self.qualified_name(),
                self.arity,
                args.len()
            )));
        } else if self.variadic && args.len() < self.arity {
            return Err(RuntimeError::host(format!(
                "Function '{}' expects at least {} arguments, got {}",
                self.qualified_name(),
                self.arity,
                args.len()
            )));
        }

        // Call the function
        (self.func)(args)
    }
}

/// Host function registry
#[derive(Default)]
pub struct HostRegistry {
    /// Registered functions by name
    functions: RwLock<HashMap<String, HostFunction>>,
}

impl HostRegistry {
    /// Create a new host registry
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a host function
    pub fn register(&self, func: HostFunction) -> Result<()> {
        let name = func.qualified_name();
        let mut functions = self.functions.write();

        if functions.contains_key(&name) {
            return Err(RuntimeError::host(format!(
                "Function '{}' is already registered",
                name
            )));
        }

        functions.insert(name, func);
        Ok(())
    }

    /// Register multiple host functions
    pub fn register_all(&self, funcs: impl IntoIterator<Item = HostFunction>) -> Result<()> {
        for func in funcs {
            self.register(func)?;
        }
        Ok(())
    }

    /// Get a host function by name
    pub fn get(&self, name: &str) -> Option<HostFunction> {
        self.functions.read().get(name).cloned()
    }

    /// Get a host function by module and name
    pub fn get_qualified(&self, module: &str, name: &str) -> Option<HostFunction> {
        let qualified = format!("{}/{}", module, name);
        self.get(&qualified)
    }

    /// Check if a function is registered
    pub fn contains(&self, name: &str) -> bool {
        self.functions.read().contains_key(name)
    }

    /// Get all registered functions
    pub fn all(&self) -> Vec<HostFunction> {
        self.functions.read().values().cloned().collect()
    }

    /// Get all functions in a module
    pub fn module_functions(&self, module: &str) -> Vec<HostFunction> {
        self.functions
            .read()
            .values()
            .filter(|f| f.module.as_deref() == Some(module))
            .cloned()
            .collect()
    }

    /// Clear all registered functions
    pub fn clear(&self) {
        self.functions.write().clear();
    }
}

/// Builder for host functions
pub struct HostFunctionBuilder {
    name: String,
    module: Option<String>,
    arity: usize,
    variadic: bool,
    doc: Option<String>,
}

impl HostFunctionBuilder {
    /// Create a new builder
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            module: None,
            arity: 0,
            variadic: false,
            doc: None,
        }
    }

    /// Set the module
    pub fn module(mut self, module: impl Into<String>) -> Self {
        self.module = Some(module.into());
        self
    }

    /// Set the arity
    pub fn arity(mut self, arity: usize) -> Self {
        self.arity = arity;
        self
    }

    /// Set variadic
    pub fn variadic(mut self) -> Self {
        self.variadic = true;
        self
    }

    /// Set documentation
    pub fn doc(mut self, doc: impl Into<String>) -> Self {
        self.doc = Some(doc.into());
        self
    }

    /// Build the host function
    pub fn build<F>(self, func: F) -> HostFunction
    where
        F: Fn(&[Value]) -> Result<Value> + Send + Sync + 'static,
    {
        HostFunction {
            name: self.name,
            module: self.module,
            arity: self.arity,
            variadic: self.variadic,
            func: Arc::new(func),
            doc: self.doc,
        }
    }
}

/// Macro for easily defining host functions
#[macro_export]
macro_rules! host_function {
    ($name:expr, |$args:ident| $body:expr) => {
        HostFunction::new($name, 0, |$args| $body).variadic()
    };
    ($name:expr, $arity:expr, |$args:ident| $body:expr) => {
        HostFunction::new($name, $arity, |$args| $body)
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::value::Value;

    #[test]
    fn test_host_function() {
        let func = HostFunction::new("add", 2, |args| match (&args[0], &args[1]) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            _ => Err(RuntimeError::host("Expected two numbers")),
        });

        assert_eq!(func.name, "add");
        assert_eq!(func.arity, 2);
        assert!(!func.variadic);

        // Test successful call
        let result = func
            .call(&[Value::Float(1.0), Value::Float(2.0)])
            .unwrap();
        assert_eq!(result, Value::Float(3.0));

        // Test arity check
        assert!(func.call(&[Value::Float(1.0)]).is_err());
    }

    #[test]
    fn test_host_registry() {
        let registry = HostRegistry::new();

        let add = HostFunction::new("add", 2, |args| match (&args[0], &args[1]) {
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            _ => Err(RuntimeError::host("Expected two numbers")),
        })
        .with_module("math");

        registry.register(add).unwrap();

        assert!(registry.contains("math/add"));
        assert!(registry.get_qualified("math", "add").is_some());

        let funcs = registry.module_functions("math");
        assert_eq!(funcs.len(), 1);
    }
}
