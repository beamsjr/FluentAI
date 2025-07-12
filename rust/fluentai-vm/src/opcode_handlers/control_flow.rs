//! Control flow operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState, CallFrame};
use fluentai_core::value::Value;
use std::time::Instant;
use super::OpcodeHandler;

pub struct ControlFlowHandler;

impl OpcodeHandler for ControlFlowHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Unconditional jump
            Jump => {
                vm.set_ip(instruction.arg as usize);
            }
            
            // Conditional jumps
            JumpIf => {
                let condition = vm.pop()?;
                if vm.is_truthy(&condition) {
                    vm.set_ip(instruction.arg as usize);
                }
            }
            
            JumpIfNot => {
                let condition = vm.pop()?;
                if !vm.is_truthy(&condition) {
                    vm.set_ip(instruction.arg as usize);
                }
            }
            
            // Function calls
            Call => {
                let arg_count = instruction.arg as usize;
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
                        // Try JIT compilation if conditions are met
                        if vm.should_jit_compile(*chunk_id) && env.is_empty() {
                            // Attempt JIT execution for functions without captures
                            if let Some(result) = vm.try_jit_execute(*chunk_id)? {
                                vm.push(result)?;
                                return Ok(VMState::Continue);
                            }
                        }
                        
                        // Track function call timing if usage tracking is enabled
                        let start_time = if vm.has_usage_tracker() {
                            Some(Instant::now())
                        } else {
                            None
                        };
                        
                        // Push arguments onto stack
                        for arg in args {
                            vm.push(arg)?;
                        }
                        
                        // Create new call frame
                        vm.push_call_frame(CallFrame {
                            chunk_id: *chunk_id,
                            ip: 0,
                            stack_base: vm.stack_len() - arg_count,
                            env: env.clone(),
                            start_time,
                        })?;
                        
                        // Debug event if enabled
                        vm.emit_function_call_debug_event(&func, arg_count);
                    }
                    Value::NativeFunction { name, .. } => {
                        vm.call_native_function(&name, args)?;
                    }
                    Value::Module { .. } => {
                        return Err(VMError::TypeError {
                            operation: "call".to_string(),
                            expected: "function".to_string(),
                            got: "module".to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "call".to_string(),
                            expected: "function".to_string(),
                            got: vm.value_type_name(v).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            // Tail call optimization
            TailCall => {
                let arg_count = instruction.arg as usize;
                let func = vm.pop()?;
                
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(vm.pop()?);
                }
                args.reverse();
                
                // Reuse current call frame instead of creating new one
                vm.setup_tail_call(func, args)?;
            }
            
            // Return from function
            Return => {
                if vm.call_stack_len() <= 1 {
                    return Ok(VMState::Return);
                }
                
                // Get return value
                let return_val = vm.pop()?;
                
                // Pop call frame and handle cleanup
                vm.pop_call_frame_with_return(return_val)?;
            }
            
            // Tail-optimized return
            TailReturn => {
                let result = vm.pop()?;
                vm.handle_tail_return(result)?;
            }
            
            // Loop markers (for optimization hints)
            LoopStart => {
                // No-op: used for optimization passes
            }
            
            LoopEnd => {
                // Jump back to loop start
                vm.set_ip(instruction.arg as usize);
            }
            
            // Halt execution
            Halt => return Ok(VMState::Halt),
            
            _ => unreachable!("ControlFlowHandler received non-control-flow opcode: {:?}", instruction.opcode),
        }
        
        Ok(VMState::Continue)
    }
}