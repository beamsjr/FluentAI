//! Promise executor for async VM operations
//!
//! This module provides the infrastructure for executing promise bodies
//! in separate tasks, enabling true asynchronous execution.

use crate::error::{VMError, VMResult};
use crate::safety::PromiseId;
use crate::vm::VM;
use fluentai_bytecode::Bytecode;
use fluentai_core::value::Value;
use std::sync::Arc;
use tokio::sync::oneshot;
use tokio::task::JoinHandle;

/// Manages promise execution in separate tasks
pub struct PromiseExecutor {
    /// Shared bytecode for all executors
    bytecode: Arc<Bytecode>,
    /// Active promise tasks
    tasks: Vec<JoinHandle<()>>,
}

impl PromiseExecutor {
    /// Create a new promise executor
    pub fn new(bytecode: Arc<Bytecode>) -> Self {
        Self {
            bytecode,
            tasks: Vec::new(),
        }
    }

    /// Execute a promise body in a new task
    pub fn execute_promise(
        &mut self,
        promise_id: PromiseId,
        body: Value,
        sender: oneshot::Sender<VMResult<Value>>,
    ) {
        let bytecode = self.bytecode.clone();
        
        // Spawn a new task for promise execution
        let handle = tokio::spawn(async move {
            let result = Self::run_promise_body(bytecode, body).await;
            let _ = sender.send(result);
        });
        
        self.tasks.push(handle);
    }

    /// Run a promise body in its own VM instance
    async fn run_promise_body(bytecode: Arc<Bytecode>, body: Value) -> VMResult<Value> {
        // Create a fresh VM for this promise
        let mut vm = VM::with_shared_bytecode(bytecode);
        
        match body {
            Value::Function { chunk_id, env } => {
                // Get stack base before mutable borrow
                let stack_base = vm.stack().len();
                
                // Push a call frame for the promise body
                vm.call_stack_mut().push(crate::vm::CallFrame {
                    chunk_id,
                    ip: 0,
                    stack_base,
                    env,
                    start_time: None,
                });
                
                // Run the promise body using run_until_complete to avoid pushing main chunk frame
                vm.run_until_complete()
            }
            Value::Procedure(proc_arc) => {
                // For procedures, we need to extract the ID from the Arc
                // In a real implementation, we'd have a proper procedure registry
                Err(VMError::RuntimeError {
                    message: format!("Procedure execution not implemented"),
                    stack_trace: None,
                })
            }
            _ => {
                Err(VMError::TypeError {
                    operation: "promise_execution".to_string(),
                    expected: "function".to_string(),
                    got: crate::error::value_type_name(&body).to_string(),
                    location: None,
                    stack_trace: None,
                })
            }
        }
    }

    /// Wait for all promise tasks to complete
    pub async fn wait_all(&mut self) {
        let tasks = std::mem::take(&mut self.tasks);
        for task in tasks {
            let _ = task.await;
        }
    }

    /// Get the number of active tasks
    pub fn active_tasks(&self) -> usize {
        self.tasks.len()
    }

    /// Clean up completed tasks
    pub fn cleanup_completed(&mut self) {
        self.tasks.retain(|handle| !handle.is_finished());
    }
}

/// Extension trait for VM to support promise execution
pub trait PromiseExecution {
    /// Execute all pending promise bodies
    fn execute_pending_promises(&mut self, executor: &mut PromiseExecutor);
}

impl PromiseExecution for VM {
    fn execute_pending_promises(&mut self, executor: &mut PromiseExecutor) {
        let pending = std::mem::take(self.pending_promise_bodies());
        
        for (promise_id, body) in pending {
            // Create a channel for the promise result
            let (sender, receiver) = oneshot::channel();
            
            // Store the receiver in the VM
            self.promises_mut().insert(promise_id, receiver);
            
            // Execute the promise body in a separate task
            executor.execute_promise(promise_id, body, sender);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_bytecode::{BytecodeChunk, Instruction, Opcode};

    #[tokio::test]
    async fn test_promise_executor() {
        // Create a simple bytecode that returns 42
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("promise_test".to_string()));
        
        chunk.add_constant(Value::Integer(42));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::new(Opcode::Return));
        
        let chunk_id = bytecode.add_chunk(chunk);
        let bytecode = Arc::new(bytecode);
        
        // Create executor
        let mut executor = PromiseExecutor::new(bytecode.clone());
        
        // Create a promise body
        let body = Value::Function {
            chunk_id,
            env: vec![],
        };
        
        // Execute the promise
        let (sender, receiver) = oneshot::channel();
        executor.execute_promise(PromiseId(1), body, sender);
        
        // Wait for result
        let result = receiver.await.unwrap();
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Value::Integer(42));
        
        // Clean up
        executor.wait_all().await;
    }
}