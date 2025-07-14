//! Async VM execution support
//!
//! This module provides async execution capabilities for the VM,
//! allowing it to properly integrate with async runtimes and avoid
//! blocking operations.

use fluentai_bytecode::Instruction;
use crate::error::{VMError, VMResult};
use crate::promise_executor::{PromiseExecutor, PromiseExecution};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use tokio::sync::oneshot;
use tokio::time::{timeout, Duration};
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use std::task::{Context, Poll};
use crate::safety::PromiseId;

/// Async VM executor that wraps the synchronous VM
pub struct AsyncVM {
    vm: VM,
    promise_executor: PromiseExecutor,
}

/// Represents a pending async operation in the VM
pub enum AsyncOperation {
    /// Awaiting a promise/future
    AwaitPromise {
        promise_id: crate::safety::PromiseId,
        receiver: oneshot::Receiver<VMResult<Value>>,
    },
    /// No pending operation
    None,
}

impl AsyncVM {
    /// Create a new async VM from a regular VM
    pub fn new(vm: VM) -> Self {
        let bytecode = Arc::new(vm.bytecode().clone());
        Self { 
            vm,
            promise_executor: PromiseExecutor::new(bytecode),
        }
    }
    
    /// Run the VM asynchronously
    pub async fn run(&mut self) -> VMResult<Value> {
        // Get values before mutable borrow
        let main_chunk = self.vm.bytecode().main_chunk;
        let has_tracker = self.vm.usage_tracker().is_some();
        
        // Push initial call frame
        self.vm.call_stack_mut().push(crate::vm::CallFrame {
            chunk_id: main_chunk,
            ip: 0,
            stack_base: 0,
            env: Vec::new(),
            start_time: if has_tracker { Some(std::time::Instant::now()) } else { None },
        });
        
        self.run_inner().await
    }
    
    /// Inner async execution loop
    async fn run_inner(&mut self) -> VMResult<Value> {
        loop {
            // Execute instructions synchronously until we hit an async operation
            match self.execute_until_async()? {
                ExecutionResult::Completed(value) => return Ok(value),
                ExecutionResult::AsyncOperation(op) => {
                    // Handle the async operation
                    self.handle_async_operation(op).await?;
                }
            }
        }
    }
    
    /// Execute instructions until we hit an async operation or complete
    fn execute_until_async(&mut self) -> VMResult<ExecutionResult> {
        loop {
            let frame = self.vm.call_stack().last()
                .ok_or_else(|| VMError::StackUnderflow {
                    operation: "get_current_frame".to_string(),
                    stack_size: self.vm.call_stack().len(),
                    stack_trace: None,
                })?;
            
            let chunk_id = frame.chunk_id;
            let ip = frame.ip;
            
            // Check if we've reached the end of the chunk
            if ip >= self.vm.bytecode().chunks[chunk_id].instructions.len() {
                if self.vm.call_stack().len() == 1 {
                    // Main function ended without explicit return
                    return Ok(ExecutionResult::Completed(Value::Nil));
                }
                
                // Pop call frame and continue
                self.vm.call_stack_mut().pop();
                continue;
            }
            
            // Get current instruction
            let instruction = self.vm.bytecode().chunks[chunk_id].instructions[ip].clone();
            
            // Check if this is an async operation that needs special handling
            if self.is_async_operation(&instruction) {
                // Increment IP before handling async operation
                self.vm.call_stack_mut().last_mut().unwrap().ip += 1;
                
                // For await operations, we need to spawn pending promises first
                if matches!(instruction.opcode, fluentai_bytecode::Opcode::Await) {
                    self.spawn_pending_promises();
                }
                
                // Return the async operation to be handled
                return Ok(ExecutionResult::AsyncOperation(
                    self.prepare_async_operation(&instruction)?
                ));
            }
            
            // Increment IP before execution (may be modified by jumps)
            self.vm.call_stack_mut().last_mut().unwrap().ip += 1;
            
            // Execute the instruction
            match self.vm.execute_instruction(&instruction, chunk_id)? {
                VMState::Continue => {
                    // Check if any promises need to be spawned
                    self.spawn_pending_promises();
                    continue;
                }
                VMState::Return => {
                    if self.vm.call_stack().len() == 1 {
                        // Main function returning
                        let result = self.vm.stack_mut().pop()
                            .ok_or_else(|| VMError::StackUnderflow {
                                operation: "main_return".to_string(),
                                stack_size: self.vm.stack().len(),
                                stack_trace: None,
                            })?;
                        return Ok(ExecutionResult::Completed(result));
                    }
                    // Pop call frame and continue
                    self.vm.call_stack_mut().pop();
                }
                VMState::Halt => {
                    return Ok(ExecutionResult::Completed(
                        self.vm.stack_mut().pop()
                            .ok_or_else(|| VMError::StackUnderflow {
                                operation: "halt".to_string(),
                                stack_size: self.vm.stack().len(),
                                stack_trace: None,
                            })?
                    ));
                }
            }
        }
    }
    
    /// Check if an instruction is an async operation
    fn is_async_operation(&self, instruction: &Instruction) -> bool {
        use fluentai_bytecode::Opcode::*;
        matches!(instruction.opcode, Await | Receive)
    }
    
    /// Prepare an async operation from an instruction
    fn prepare_async_operation(&mut self, instruction: &Instruction) -> VMResult<AsyncOperation> {
        use fluentai_bytecode::Opcode::*;
        
        match instruction.opcode {
            Await => {
                // Pop the value to await
                let value = self.vm.pop()?;
                
                match value {
                    Value::Promise(promise_id) => {
                        let promise_id = crate::safety::PromiseId(promise_id);
                        
                        
                        // Get the receiver for this promise
                        if let Some(receiver) = self.vm.take_promise(&promise_id) {
                            Ok(AsyncOperation::AwaitPromise { promise_id, receiver })
                        } else {
                            Err(VMError::AsyncError {
                                message: format!("Promise {:?} not found", promise_id),
                                stack_trace: None,
                            })
                        }
                    }
                    v => {
                        let type_name = crate::error::value_type_name(&v);
                        Err(VMError::TypeError {
                            operation: "await".to_string(),
                            expected: "promise".to_string(),
                            got: type_name.to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            Receive => {
                // For now, just perform a synchronous receive
                // This is not ideal but avoids the ownership issues
                Ok(AsyncOperation::None)
            }
            
            _ => Ok(AsyncOperation::None),
        }
    }
    
    /// Handle an async operation
    async fn handle_async_operation(&mut self, op: AsyncOperation) -> VMResult<()> {
        match op {
            AsyncOperation::AwaitPromise { promise_id, receiver } => {
                // Wait for the promise with a timeout
                match timeout(Duration::from_secs(30), receiver).await {
                    Ok(Ok(Ok(value))) => {
                        self.vm.push(value)?;
                    }
                    Ok(Ok(Err(e))) => {
                        return Err(e);
                    }
                    Ok(Err(_)) => {
                        return Err(VMError::AsyncError {
                            message: format!("Promise {:?} channel closed", promise_id),
                            stack_trace: None,
                        });
                    }
                    Err(_) => {
                        return Err(VMError::AsyncError {
                            message: format!("Promise {:?} timed out", promise_id),
                            stack_trace: None,
                        });
                    }
                }
            }
            
            
            AsyncOperation::None => {}
        }
        Ok(())
    }
    
    /// Spawn execution for any pending promise bodies
    fn spawn_pending_promises(&mut self) {
        // Use the promise executor to run promise bodies in separate tasks
        self.vm.execute_pending_promises(&mut self.promise_executor);
        
        // Clean up any completed tasks
        self.promise_executor.cleanup_completed();
    }
}

/// Result of executing until an async operation
enum ExecutionResult {
    /// Execution completed with a value
    Completed(Value),
    /// Hit an async operation that needs to be awaited
    AsyncOperation(AsyncOperation),
}

/// Future wrapper for VM execution
pub struct VMFuture<'a> {
    async_vm: &'a mut AsyncVM,
}

impl<'a> VMFuture<'a> {
    pub fn new(async_vm: &'a mut AsyncVM) -> Self {
        Self { async_vm }
    }
}

impl<'a> Future for VMFuture<'a> {
    type Output = VMResult<Value>;
    
    fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        // This would need to be implemented to properly integrate with tokio
        // For now, we'll use the async/await approach above
        Poll::Pending
    }
}