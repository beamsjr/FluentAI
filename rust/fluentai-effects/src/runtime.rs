//! Effect runtime for FluentAi
//!
//! Provides async runtime support for effects
//!
//! # Safety Note
//!
//! The `handle()` and `block_on()` methods will panic if the runtime is not properly initialized.
//! Use `try_handle()` and `try_block_on()` for non-panicking alternatives.

use std::sync::Arc;
use tokio::runtime::{Handle, Runtime};

pub struct EffectRuntime {
    runtime: Option<Arc<Runtime>>,
    handle: Option<Handle>,
}

impl EffectRuntime {
    pub fn new() -> Result<Self, std::io::Error> {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .enable_all()
            .build()?;

        Ok(Self {
            runtime: Some(Arc::new(runtime)),
            handle: None,
        })
    }

    /// Create an EffectRuntime that uses the current tokio runtime handle
    /// This is safe to use in async contexts
    pub fn from_current() -> Self {
        Self {
            runtime: None,
            handle: Some(Handle::current()),
        }
    }

    pub fn handle(&self) -> Handle {
        if let Some(handle) = &self.handle {
            handle.clone()
        } else if let Some(runtime) = &self.runtime {
            runtime.handle().clone()
        } else {
            panic!("EffectRuntime has no handle")
        }
    }

    /// Try to get handle without panicking
    pub fn try_handle(&self) -> Option<Handle> {
        if let Some(handle) = &self.handle {
            Some(handle.clone())
        } else if let Some(runtime) = &self.runtime {
            Some(runtime.handle().clone())
        } else {
            None
        }
    }

    pub fn block_on<F: std::future::Future>(&self, future: F) -> F::Output {
        if let Some(runtime) = &self.runtime {
            runtime.block_on(future)
        } else {
            panic!("Cannot block_on without owned runtime")
        }
    }

    /// Try to block on a future without panicking
    pub fn try_block_on<F: std::future::Future>(&self, future: F) -> Option<F::Output> {
        if let Some(runtime) = &self.runtime {
            Some(runtime.block_on(future))
        } else {
            None
        }
    }

    pub fn spawn<F>(&self, future: F) -> tokio::task::JoinHandle<F::Output>
    where
        F: std::future::Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.handle().spawn(future)
    }
}

impl Default for EffectRuntime {
    fn default() -> Self {
        // Try to use current runtime if we're in async context
        if let Ok(handle) = Handle::try_current() {
            Self {
                runtime: None,
                handle: Some(handle),
            }
        } else {
            // Create new runtime if not in async context
            Self::new().expect("Failed to create runtime")
        }
    }
}

#[derive(Clone)]
pub struct RuntimeHandle {
    handle: Handle,
}

impl RuntimeHandle {
    pub fn new(runtime: &EffectRuntime) -> Self {
        Self {
            handle: runtime.handle(),
        }
    }

    /// Try to create a RuntimeHandle without panicking
    pub fn try_new(runtime: &EffectRuntime) -> Option<Self> {
        runtime.try_handle().map(|handle| Self { handle })
    }

    pub fn spawn<F>(&self, future: F) -> tokio::task::JoinHandle<F::Output>
    where
        F: std::future::Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.handle.spawn(future)
    }

    pub fn enter(&self) -> tokio::runtime::EnterGuard<'_> {
        self.handle.enter()
    }
}
