//! Async runtime management for FluentAi

use tokio::runtime::{Runtime, Handle};
use std::sync::Arc;
use anyhow::Result;

pub struct EffectRuntime {
    runtime: Arc<Runtime>,
}

impl EffectRuntime {
    pub fn new() -> Result<Self> {
        let runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(4)
            .enable_all()
            .build()?;
        
        Ok(Self {
            runtime: Arc::new(runtime),
        })
    }
    
    pub fn handle(&self) -> Handle {
        self.runtime.handle().clone()
    }
    
    pub fn block_on<F: std::future::Future>(&self, future: F) -> F::Output {
        self.runtime.block_on(future)
    }
    
    pub fn spawn<F>(&self, future: F) -> tokio::task::JoinHandle<F::Output>
    where
        F: std::future::Future + Send + 'static,
        F::Output: Send + 'static,
    {
        self.runtime.spawn(future)
    }
}

impl Default for EffectRuntime {
    fn default() -> Self {
        Self::new().expect("Failed to create runtime")
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