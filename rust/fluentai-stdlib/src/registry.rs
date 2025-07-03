//! Function registry for the FluentAi standard library

use rustc_hash::FxHashMap;
use crate::value::Value;
use fluentai_core::ast::EffectType;
use anyhow::Result;
use parking_lot::RwLock;
use std::sync::Arc;

/// A standard library function with metadata
#[derive(Clone)]
pub struct StdlibFunction {
    /// The function implementation
    pub func: fn(&[Value]) -> Result<Value>,
    
    /// The function's name
    pub name: String,
    
    /// Minimum number of arguments
    pub min_args: usize,
    
    /// Maximum number of arguments (None for variadic)
    pub max_args: Option<usize>,
    
    /// Effects this function may perform
    pub effects: Vec<EffectType>,
    
    /// Documentation string
    pub doc: String,
}

impl StdlibFunction {
    /// Create a new pure function (no effects)
    pub fn pure(
        name: impl Into<String>,
        func: fn(&[Value]) -> Result<Value>,
        min_args: usize,
        max_args: Option<usize>,
        doc: impl Into<String>,
    ) -> Self {
        Self {
            func,
            name: name.into(),
            min_args,
            max_args,
            effects: vec![],
            doc: doc.into(),
        }
    }
    
    /// Create a new effectful function
    pub fn effectful(
        name: impl Into<String>,
        func: fn(&[Value]) -> Result<Value>,
        min_args: usize,
        max_args: Option<usize>,
        effects: Vec<EffectType>,
        doc: impl Into<String>,
    ) -> Self {
        Self {
            func,
            name: name.into(),
            min_args,
            max_args,
            effects,
            doc: doc.into(),
        }
    }
    
    /// Validate argument count
    pub fn validate_args(&self, arg_count: usize) -> Result<()> {
        if arg_count < self.min_args {
            anyhow::bail!(
                "{}: expected at least {} arguments, got {}",
                self.name, self.min_args, arg_count
            );
        }
        
        if let Some(max) = self.max_args {
            if arg_count > max {
                anyhow::bail!(
                    "{}: expected at most {} arguments, got {}",
                    self.name, max, arg_count
                );
            }
        }
        
        Ok(())
    }
    
    /// Call the function with argument validation
    pub fn call(&self, args: &[Value]) -> Result<Value> {
        self.validate_args(args.len())?;
        (self.func)(args)
    }
}

/// Registry for standard library functions
#[derive(Clone)]
pub struct StdlibRegistry {
    functions: Arc<RwLock<FxHashMap<String, StdlibFunction>>>,
}

impl StdlibRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            functions: Arc::new(RwLock::new(FxHashMap::default())),
        }
    }
    
    /// Register a function
    pub fn register(&mut self, func: StdlibFunction) {
        self.functions.write().insert(func.name.clone(), func);
    }
    
    /// Register multiple functions at once
    pub fn register_all(&mut self, funcs: Vec<StdlibFunction>) {
        let mut functions = self.functions.write();
        for func in funcs {
            functions.insert(func.name.clone(), func);
        }
    }
    
    /// Look up a function by name
    pub fn get(&self, name: &str) -> Option<StdlibFunction> {
        self.functions.read().get(name).cloned()
    }
    
    /// Get all function names
    pub fn function_names(&self) -> Vec<String> {
        self.functions.read().keys().cloned().collect()
    }
    
    /// Check if a function exists
    pub fn contains(&self, name: &str) -> bool {
        self.functions.read().contains_key(name)
    }
}

impl Default for StdlibRegistry {
    fn default() -> Self {
        Self::new()
    }
}