//! Continuation support for async execution
//!
//! This module provides the infrastructure for suspending and resuming
//! VM execution at await points, enabling proper async/await support.

use crate::error::{VMError, VMResult};
use crate::vm::CallFrame;
use fluentai_core::value::Value;
use rustc_hash::FxHashMap;
use std::fmt;

/// Represents a suspended execution state that can be resumed later
#[derive(Debug, Clone)]
pub struct Continuation {
    /// The call stack at the suspension point
    pub call_stack: Vec<CallFrame>,
    /// The value stack at the suspension point
    pub value_stack: Vec<Value>,
    /// Local variables at the suspension point
    pub locals: Vec<Value>,
    /// The instruction pointer to resume at
    pub resume_ip: usize,
    /// The chunk ID to resume in
    pub resume_chunk: usize,
    /// Any error handlers that were active
    pub error_handlers: Vec<ErrorHandlerFrame>,
}

/// Represents an active error handler at suspension time
#[derive(Debug, Clone)]
pub struct ErrorHandlerFrame {
    pub catch_ip: usize,
    pub finally_ip: Option<usize>,
    pub stack_depth: usize,
}

/// Unique identifier for a continuation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ContinuationId(pub u64);

impl fmt::Display for ContinuationId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Continuation#{}", self.0)
    }
}

/// Manages continuations for the VM
pub struct ContinuationManager {
    /// Map of continuation IDs to continuations
    continuations: FxHashMap<ContinuationId, Continuation>,
    /// Next continuation ID to allocate
    next_id: u64,
}

impl ContinuationManager {
    /// Create a new continuation manager
    pub fn new() -> Self {
        Self {
            continuations: FxHashMap::default(),
            next_id: 1,
        }
    }

    /// Create a continuation from the current VM state
    pub fn create_continuation(
        &mut self,
        call_stack: Vec<CallFrame>,
        value_stack: Vec<Value>,
        locals: Vec<Value>,
        resume_ip: usize,
        resume_chunk: usize,
        error_handlers: Vec<ErrorHandlerFrame>,
    ) -> ContinuationId {
        let id = ContinuationId(self.next_id);
        self.next_id += 1;

        let continuation = Continuation {
            call_stack,
            value_stack,
            locals,
            resume_ip,
            resume_chunk,
            error_handlers,
        };

        self.continuations.insert(id, continuation);
        id
    }

    /// Get a continuation by ID
    pub fn get(&self, id: ContinuationId) -> Option<&Continuation> {
        self.continuations.get(&id)
    }

    /// Remove and return a continuation
    pub fn take(&mut self, id: ContinuationId) -> Option<Continuation> {
        self.continuations.remove(&id)
    }

    /// Check if a continuation exists
    pub fn contains(&self, id: ContinuationId) -> bool {
        self.continuations.contains_key(&id)
    }

    /// Get the number of active continuations
    pub fn count(&self) -> usize {
        self.continuations.len()
    }

    /// Clear all continuations (useful for cleanup)
    pub fn clear(&mut self) {
        self.continuations.clear();
    }
}

impl Default for ContinuationManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Extension trait for VM to support continuations
pub trait ContinuationSupport {
    /// Suspend the current execution and create a continuation
    fn suspend_execution(&mut self, resume_offset: isize) -> VMResult<ContinuationId>;
    
    /// Resume execution from a continuation
    fn resume_continuation(&mut self, continuation_id: ContinuationId, resume_value: Value) -> VMResult<()>;
    
    /// Check if the VM can be suspended at the current point
    fn can_suspend(&self) -> bool;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_continuation_manager() {
        let mut manager = ContinuationManager::new();
        
        // Create a continuation
        let id = manager.create_continuation(
            vec![],
            vec![],
            vec![],
            0,
            0,
            vec![],
        );
        
        assert!(manager.contains(id));
        assert_eq!(manager.count(), 1);
        
        // Take the continuation
        let cont = manager.take(id);
        assert!(cont.is_some());
        assert!(!manager.contains(id));
        assert_eq!(manager.count(), 0);
    }

    #[test]
    fn test_continuation_id_display() {
        let id = ContinuationId(42);
        assert_eq!(format!("{}", id), "Continuation#42");
    }
}