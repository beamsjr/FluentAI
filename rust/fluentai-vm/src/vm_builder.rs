//! Builder pattern for creating VM instances safely in async contexts

use crate::{Bytecode, SecurityPolicy, VM};
use fluentai_effects::{EffectContext, EffectRuntime};
use fluentai_modules::ModuleLoader;
use fluentai_stdlib::StdlibRegistry;
use std::sync::Arc;

/// Builder for creating VM instances
pub struct VMBuilder {
    bytecode: Bytecode,
    effect_runtime: Option<Arc<EffectRuntime>>,
    effect_context: Option<Arc<EffectContext>>,
    stdlib: Option<StdlibRegistry>,
    module_loader: Option<ModuleLoader>,
    security_policy: Option<SecurityPolicy>,
    sandbox_mode: bool,
    trace: bool,
}

impl VMBuilder {
    /// Create a new VM builder with empty bytecode
    pub fn new() -> Self {
        Self {
            bytecode: Bytecode::new(),
            effect_runtime: None,
            effect_context: None,
            stdlib: None,
            module_loader: None,
            security_policy: None,
            sandbox_mode: false,
            trace: false,
        }
    }

    /// Set the bytecode
    pub fn with_bytecode(mut self, bytecode: Bytecode) -> Self {
        self.bytecode = bytecode;
        self
    }

    /// Set the effect runtime (useful when running in async context)
    pub fn with_effect_runtime(mut self, runtime: Arc<EffectRuntime>) -> Self {
        self.effect_runtime = Some(runtime);
        self
    }

    /// Set the effect context
    pub fn with_effect_context(mut self, context: Arc<EffectContext>) -> Self {
        self.effect_context = Some(context);
        self
    }

    /// Set the stdlib registry
    pub fn with_stdlib(mut self, stdlib: StdlibRegistry) -> Self {
        self.stdlib = Some(stdlib);
        self
    }

    /// Set the module loader
    pub fn with_module_loader(mut self, loader: ModuleLoader) -> Self {
        self.module_loader = Some(loader);
        self
    }

    /// Set the security policy
    pub fn with_security_policy(mut self, policy: SecurityPolicy) -> Self {
        self.security_policy = Some(policy);
        self
    }

    /// Enable sandbox mode
    pub fn with_sandbox_mode(mut self) -> Self {
        self.sandbox_mode = true;
        self
    }

    /// Enable trace mode
    pub fn with_trace(mut self) -> Self {
        self.trace = true;
        self
    }

    /// Build the VM instance
    /// This can be safely called from async context if effect_runtime is provided
    pub fn build(self) -> anyhow::Result<VM> {
        // If no effect runtime is provided and we need one, this will panic in async context
        // Caller should provide runtime when in async context
        let mut vm = VM::new(self.bytecode);

        if let Some(runtime) = self.effect_runtime {
            vm.set_effect_runtime(runtime);
        }

        if let Some(context) = self.effect_context {
            vm.set_effect_context(context);
        }

        if let Some(stdlib) = self.stdlib {
            vm.set_stdlib_registry(stdlib);
        }

        if let Some(loader) = self.module_loader {
            vm.set_module_loader(loader);
        }

        if let Some(policy) = self.security_policy {
            vm.with_security_policy(policy);
        }

        if self.sandbox_mode {
            vm.with_sandbox_security();
        }

        if self.trace {
            vm.enable_trace();
        }

        Ok(vm)
    }
}
