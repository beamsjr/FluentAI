//! Bridge for higher-order functions between stdlib and VM
//!
//! This module provides the infrastructure for stdlib functions to call
//! VM functions (like in map, filter, fold operations).

use crate::value::Value;
use anyhow::Result;
use fluentai_effects::EffectContext;
use std::sync::Arc;

/// Trait for VM callback interface
/// This allows the stdlib to call back into the VM for function execution
pub trait VMCallback: Send + Sync {
    /// Call a VM function with given arguments
    fn call_function(&mut self, func: &Value, args: &[Value]) -> Result<Value>;

    /// Get the current effect context
    fn effect_context(&self) -> Arc<EffectContext>;

    /// Call a VM function with a specific effect context
    fn call_function_with_context(
        &mut self,
        func: &Value,
        args: &[Value],
        context: Arc<EffectContext>,
    ) -> Result<Value> {
        // Default implementation just calls the regular function
        // VMs can override this to properly propagate the context
        self.call_function(func, args)
    }
    
    /// Send a message to a channel
    fn send_to_channel(&mut self, channel_id: u64, message: Value) -> Result<()> {
        // Default implementation returns an error
        Err(anyhow::anyhow!("Channel operations not supported by this VM"))
    }
    
    /// Send a message to an actor
    fn send_to_actor(&mut self, actor_id: u64, message: Value) -> Result<()> {
        // Default implementation returns an error
        Err(anyhow::anyhow!("Actor operations not supported by this VM"))
    }
}

/// Dummy implementation for when VM callback is not available
pub struct NoOpVMCallback;

impl VMCallback for NoOpVMCallback {
    fn call_function(&mut self, _func: &Value, _args: &[Value]) -> Result<Value> {
        Err(anyhow::anyhow!(
            "VM callback not available - higher-order functions require VM integration"
        ))
    }

    fn effect_context(&self) -> Arc<EffectContext> {
        // Return a default context when no VM is available
        Arc::new(EffectContext::default())
    }
}

/// Context for stdlib functions that need VM integration and effect handling
pub struct StdlibContext {
    pub vm_callback: Option<Box<dyn VMCallback>>,
    /// Override effect context (if not provided, uses VM's context)
    pub effect_context_override: Option<Arc<EffectContext>>,
}

impl Default for StdlibContext {
    fn default() -> Self {
        Self {
            vm_callback: None,
            effect_context_override: None,
        }
    }
}

impl StdlibContext {
    /// Create a new context with VM callback
    pub fn with_callback(callback: Box<dyn VMCallback>) -> Self {
        Self {
            vm_callback: Some(callback),
            effect_context_override: None,
        }
    }

    /// Create a new context with VM callback and effect context
    pub fn with_callback_and_effects(
        callback: Box<dyn VMCallback>,
        effect_context: Arc<EffectContext>,
    ) -> Self {
        Self {
            vm_callback: Some(callback),
            effect_context_override: Some(effect_context),
        }
    }

    /// Get the current effect context
    pub fn effect_context(&self) -> Arc<EffectContext> {
        if let Some(context) = &self.effect_context_override {
            context.clone()
        } else if let Some(callback) = &self.vm_callback {
            callback.effect_context()
        } else {
            Arc::new(EffectContext::default())
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

    /// Call a VM function with the current effect context
    pub fn call_function_with_effects(&mut self, func: &Value, args: &[Value]) -> Result<Value> {
        let context = self.effect_context();
        if let Some(callback) = &mut self.vm_callback {
            callback.call_function_with_context(func, args, context)
        } else {
            Err(anyhow::anyhow!("No VM callback available"))
        }
    }
}
