//! Service lifecycle management

use crate::error::DiResult;

/// Trait for services that need lifecycle management
pub trait Lifecycle {
    /// Called when the service is created
    fn on_create(&mut self) -> DiResult<()> {
        Ok(())
    }
    
    /// Called before the service is disposed
    fn on_dispose(&mut self) -> DiResult<()> {
        Ok(())
    }
}

/// Trait for services that need explicit disposal
pub trait Disposable {
    /// Dispose of any resources held by the service
    fn dispose(&mut self);
}

// Lifecycle trait has default implementations, so no blanket impl needed

/// Scope guard for automatic disposal
pub struct DisposableScope {
    disposables: Vec<Box<dyn Disposable + Send>>,
}

impl DisposableScope {
    /// Create a new disposable scope
    pub fn new() -> Self {
        Self {
            disposables: Vec::new(),
        }
    }
    
    /// Add a disposable to the scope
    pub fn add<T: Disposable + Send + 'static>(&mut self, disposable: T) {
        self.disposables.push(Box::new(disposable));
    }
    
    /// Manually dispose all resources
    pub fn dispose(&mut self) {
        for mut disposable in self.disposables.drain(..) {
            disposable.dispose();
        }
    }
}

impl Drop for DisposableScope {
    fn drop(&mut self) {
        self.dispose();
    }
}

impl Default for DisposableScope {
    fn default() -> Self {
        Self::new()
    }
}