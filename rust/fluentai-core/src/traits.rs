//! Core traits for dependency injection and extensibility

use std::sync::Arc;
use anyhow::Result;
use crate::ast::{Graph, NodeId};

/// Trait for module loading
pub trait ModuleLoader: Send + Sync {
    /// Load a module by name
    fn load_module(&self, name: &str) -> Result<Arc<Graph>>;
    
    /// Check if a module exists
    fn module_exists(&self, name: &str) -> bool;
    
    /// Get the search paths for modules
    fn search_paths(&self) -> &[String];
}

/// Trait for standard library registry
pub trait StdlibProvider: Send + Sync {
    /// Check if a function exists
    fn has_function(&self, name: &str) -> bool;
    
    /// Get the arity of a function
    fn get_arity(&self, name: &str) -> Option<(usize, Option<usize>)>;
    
    /// Check if a function is pure
    fn is_pure(&self, name: &str) -> bool;
}

/// Trait for effect handling
pub trait EffectHandler: Send + Sync {
    /// Get the name of the effect this handler manages
    fn effect_name(&self) -> &str;
    
    /// Check if this handler can handle an operation
    fn can_handle(&self, operation: &str) -> bool;
    
    /// Get the list of operations this handler supports
    fn supported_operations(&self) -> Vec<String>;
}

/// Trait for effect context
pub trait EffectContextProvider: Send + Sync {
    /// Register an effect handler
    fn register_handler(&self, handler: Box<dyn EffectHandler>) -> Result<()>;
    
    /// Get a handler by effect name
    fn get_handler(&self, effect_name: &str) -> Option<Arc<dyn EffectHandler>>;
    
    /// Check if an effect is registered
    fn has_effect(&self, effect_name: &str) -> bool;
}

/// Trait for optimization passes
pub trait OptimizationPass: Send + Sync {
    /// Get the name of this optimization pass
    fn name(&self) -> &str;
    
    /// Run the optimization on a graph
    fn optimize(&self, graph: &Graph) -> Result<Graph>;
    
    /// Check if this pass should run at a given optimization level
    fn should_run_at_level(&self, level: OptimizationLevel) -> bool;
}

/// Optimization levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum OptimizationLevel {
    /// No optimizations
    None = 0,
    /// Basic optimizations (constant folding, dead code elimination)
    Basic = 1,
    /// Standard optimizations (inlining, CSE)
    Standard = 2,
    /// Aggressive optimizations (loop optimizations, advanced inlining)
    Aggressive = 3,
}

/// Trait for type checking
pub trait TypeChecker: Send + Sync {
    /// Type check a graph
    fn check(&self, graph: &Graph) -> Result<()>;
    
    /// Get the type of a node
    fn get_type(&self, graph: &Graph, node_id: NodeId) -> Result<String>;
}

/// Trait for code generation backends
pub trait CodeGenerator: Send + Sync {
    /// Get the name of this backend
    fn name(&self) -> &str;
    
    /// Generate code from a graph
    fn generate(&self, graph: &Graph) -> Result<Vec<u8>>;
    
    /// Get the file extension for generated code
    fn file_extension(&self) -> &str;
}

/// Trait for diagnostic reporting
pub trait DiagnosticReporter: Send + Sync {
    /// Report an error
    fn report_error(&self, message: &str, location: Option<NodeId>);
    
    /// Report a warning
    fn report_warning(&self, message: &str, location: Option<NodeId>);
    
    /// Report an info message
    fn report_info(&self, message: &str, location: Option<NodeId>);
    
    /// Check if there were any errors
    fn has_errors(&self) -> bool;
    
    /// Get the number of errors
    fn error_count(&self) -> usize;
}

#[cfg(test)]
#[path = "traits_tests.rs"]
mod tests;