//! Bridge for higher-order functions between stdlib and VM
//! 
//! This module provides the infrastructure for stdlib functions to call
//! VM functions (like in map, filter, fold operations).

use crate::value::Value;
use anyhow::Result;

/// Trait for VM callback interface
/// This allows the stdlib to call back into the VM for function execution
pub trait VMCallback: Send + Sync {
    /// Call a VM function with given arguments
    fn call_function(&mut self, func: &Value, args: &[Value]) -> Result<Value>;
}

/// Dummy implementation for when VM callback is not available
pub struct NoOpVMCallback;

impl VMCallback for NoOpVMCallback {
    fn call_function(&mut self, _func: &Value, _args: &[Value]) -> Result<Value> {
        Err(anyhow::anyhow!("VM callback not available - higher-order functions require VM integration"))
    }
}

/// Context for stdlib functions that need VM integration
pub struct StdlibContext {
    pub vm_callback: Option<Box<dyn VMCallback>>,
}

impl Default for StdlibContext {
    fn default() -> Self {
        Self {
            vm_callback: None,
        }
    }
}

impl StdlibContext {
    /// Create a new context with VM callback
    pub fn with_callback(callback: Box<dyn VMCallback>) -> Self {
        Self {
            vm_callback: Some(callback),
        }
    }
    
    /// Call a VM function
    pub fn call_function(&mut self, func: &Value, args: &[Value]) -> Result<Value> {
        if let Some(callback) = &mut self.vm_callback {
            callback.call_function(func, args)
        } else {
            Err(anyhow::anyhow!("No VM callback available"))
        }
    }
}