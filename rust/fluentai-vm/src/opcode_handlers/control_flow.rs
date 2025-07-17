//! Control flow operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState, CallFrame};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ControlFlowHandler;

impl ControlFlowHandler {
    // Jump handlers
    fn handle_jump(&mut self, vm: &mut VM, target: u32) -> VMResult<()> {
        let current_ip = vm.get_ip();
        let target_ip = target as usize;
        
        // If jumping backward and we're in a loop, increment iteration count
        if target_ip < current_ip {
            vm.increment_loop_iteration();
        }
        
        vm.set_ip(target_ip);
        Ok(())
    }

    fn handle_jump_if(&mut self, vm: &mut VM, target: u32) -> VMResult<()> {
        let condition = vm.pop()?;
        if vm.is_truthy(&condition) {
            vm.set_ip(target as usize);
        }
        Ok(())
    }

    fn handle_jump_if_not(&mut self, vm: &mut VM, target: u32) -> VMResult<()> {
        let condition = vm.pop()?;
        if !vm.is_truthy(&condition) {
            vm.set_ip(target as usize);
        }
        Ok(())
    }

    // Function call handler
    fn handle_call(&mut self, vm: &mut VM, arg_count: u32) -> VMResult<()> {
        let arg_count = arg_count as usize;
        // Pop function first (it's on top of stack due to our compilation order)
        let func = vm.pop()?;
        
        // Pop arguments in reverse order
        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(vm.pop()?);
        }
        args.reverse();
        
        match &func {
            Value::Function { chunk_id, env } => {
                // Save current frame state
                let current_ip = vm.get_ip();
                let current_chunk = vm.current_chunk();
                
                // Check if learning mode should provide an optimized variant
                let actual_chunk_id = vm.get_optimized_chunk_id(*chunk_id);
                
                // The stack currently has no arguments (we popped them all)
                // We need to push args back first, then set stack_base
                
                // Push arguments back onto stack in correct order
                for arg in args {
                    vm.push(arg)?;
                }
                
                // Debug print
                if std::env::var("VM_DEBUG").is_ok() {
                    eprintln!("Call: Pushed {} args, stack_len={}, setting stack_base={}", 
                        arg_count, vm.stack_len(), vm.stack_len() - arg_count);
                    eprintln!("Stack: {:?}", vm.debug_stack());
                }
                
                // Now create the call frame with stack_base pointing to the first argument
                let frame = CallFrame {
                    chunk_id: actual_chunk_id,
                    ip: 0,
                    stack_base: vm.stack_len() - arg_count,  // Points to first argument
                    env: env.clone(),
                    start_time: None, // Will be set by push_frame if profiling enabled
                };
                
                // Update usage tracking
                // Note: Usage tracking is disabled due to Arc<RwLock> wrapper
                
                vm.push_frame(frame);
                vm.set_ip(0);
                vm.set_chunk(actual_chunk_id);
            }
            
            Value::NativeFunction { function, arity, .. } => {
                // Validate argument count
                if args.len() != *arity {
                    return Err(VMError::RuntimeError {
                        message: format!("Function expects {} arguments, got {}", arity, args.len()),
                        stack_trace: None,
                    });
                }
                
                // Call the native function
                let result = function(&args)
                    .map_err(|e| VMError::RuntimeError {
                        message: format!("Native function error: {}", e),
                        stack_trace: None,
                    })?;
                
                vm.push(result)?;
            }
            
            Value::Procedure(proc) => {
                // Call procedure through VM
                // For now, we'll treat procedures like functions
                // In a full implementation, this would handle the closure properly
                return Err(VMError::RuntimeError {
                    message: "Procedure calls not yet implemented".to_string(),
                    stack_trace: None,
                });
            }
            
            // Handle stdlib function calls
            Value::Tagged { tag, values } if tag == "__stdlib__" => {
                if let Some(Value::String(func_name)) = values.first() {
                    // Delegate to native function call handler
                    vm.call_native_function(&format!("__stdlib__{}", func_name), args)?;
                } else {
                    return Err(VMError::RuntimeError {
                        message: "Invalid stdlib function reference".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            // Handle builtin function calls
            Value::Tagged { tag, values } if tag == "__builtin__" => {
                if let Some(Value::String(func_name)) = values.first() {
                    // Delegate to native function call handler
                    vm.call_native_function(&format!("__builtin__{}", func_name), args)?;
                } else {
                    return Err(VMError::RuntimeError {
                        message: "Invalid builtin function reference".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            _ => {
                return Err(VMError::TypeError {
                    operation: "call".to_string(),
                    expected: "function".to_string(),
                    got: crate::error::value_type_name(&func).to_string(),
                    location: None,
                    stack_trace: None,
                });
            }
        }
        Ok(())
    }

    // Tail call handler
    fn handle_tail_call(&mut self, vm: &mut VM, arg_count: u32) -> VMResult<()> {
        let arg_count = arg_count as usize;
        let func = vm.pop()?;
        
        // Pop arguments
        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(vm.pop()?);
        }
        args.reverse();
        
        match &func {
            Value::Function { chunk_id, env } => {
                // For tail call, reuse current frame
                // Get the needed values before mutable borrow
                let original_chunk_id = *chunk_id;
                let actual_chunk_id = vm.get_optimized_chunk_id(original_chunk_id);
                let env = env.clone();
                let stack_base = vm.current_frame().stack_base;
                
                // Clear stack back to frame base
                vm.truncate_stack(stack_base);
                
                // Push new arguments
                for arg in args {
                    vm.push(arg)?;
                }
                
                // Update frame for new function
                let frame = vm.current_frame_mut();
                frame.chunk_id = actual_chunk_id;
                frame.ip = 0;
                frame.env = env;
                
                vm.set_chunk(actual_chunk_id);
                vm.set_ip(0);
            }
            _ => {
                // For non-Function values, fall back to regular call
                // Push func back and do regular call
                vm.push(func)?;
                for arg in args.into_iter().rev() {
                    vm.push(arg)?;
                }
                return self.handle_call(vm, arg_count as u32);
            }
        }
        Ok(())
    }

    // Return handler
    fn handle_return(&mut self, vm: &mut VM) -> VMResult<VMState> {
        // Pop return value
        let return_value = vm.pop()?;
        
        // Record usage if tracking is enabled
        // Note: Usage tracking is disabled due to Arc<RwLock> wrapper
        
        // Don't pop the frame here - the VM main loop handles it based on VMState::Return
        // Just prepare the return value
        vm.push(return_value)?;
        
        // Signal that we want to return
        Ok(VMState::Return)
    }

    // Tail return handler
    fn handle_tail_return(&mut self, vm: &mut VM) -> VMResult<VMState> {
        // For tail return, the return value is already on top of stack
        // Just handle like a regular return
        self.handle_return(vm)
    }

    // Loop handlers (for optimization hints)
    fn handle_loop_start(&mut self, vm: &mut VM) -> VMResult<()> {
        // Push a new loop tracking entry
        if let Some(_profiler) = vm.profiler() {
            // For now, we'll use the current instruction pointer as a simple loop ID
            // In a real implementation, we'd want to track the actual AST node
            let loop_id = fluentai_core::ast::NodeId(std::num::NonZeroU32::new(vm.get_ip() as u32).unwrap_or(std::num::NonZeroU32::new(1).unwrap()));
            vm.push_loop_tracking(loop_id);
        }
        Ok(())
    }

    fn handle_loop_end(&mut self, vm: &mut VM) -> VMResult<()> {
        // Pop and record loop iteration count
        let loop_data = vm.pop_loop_tracking();
        if let Some(profiler_arc) = vm.profiler() {
            if let Some((loop_id, iterations)) = loop_data {
                profiler_arc.record_loop_iteration(loop_id, iterations);
            }
        }
        Ok(())
    }
}

impl OpcodeHandler for ControlFlowHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Unconditional jump
            Jump => {
                self.handle_jump(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            // Conditional jumps
            JumpIf => {
                self.handle_jump_if(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            JumpIfNot => {
                self.handle_jump_if_not(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            // Function calls
            Call => {
                self.handle_call(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            TailCall => {
                self.handle_tail_call(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            // Returns
            Return => self.handle_return(vm),
            TailReturn => self.handle_tail_return(vm),
            
            // Loops
            LoopStart => {
                self.handle_loop_start(vm)?;
                Ok(VMState::Continue)
            }
            
            LoopEnd => {
                self.handle_loop_end(vm)?;
                Ok(VMState::Continue)
            }
            
            // Halt
            Halt => Ok(VMState::Halt),
            
            _ => unreachable!("ControlFlowHandler received non-control-flow opcode"),
        }
    }
}