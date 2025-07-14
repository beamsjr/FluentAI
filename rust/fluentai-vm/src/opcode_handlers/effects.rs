//! Effect and handler operations

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult, value_type_name};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct EffectsHandler;

impl OpcodeHandler for EffectsHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Perform an effect
            Effect => {
                let arg_count = instruction.arg as usize;
                
                // Collect arguments
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(vm.pop()?);
                }
                args.reverse();
                
                // Pop operation and effect type
                let operation = vm.pop()?;
                let operation = operation.as_string()
                    .map_err(|_| VMError::TypeError {
                        operation: "Effect".to_string(),
                        expected: "String".to_string(),
                        got: value_type_name(&operation).to_string(),
                        location: None,
                        stack_trace: None,
                    })?;
                
                let effect_type = vm.pop()?;
                let effect_type = effect_type.as_string()
                    .map_err(|_| VMError::TypeError {
                        operation: "Effect".to_string(),
                        expected: "String".to_string(),
                        got: value_type_name(&effect_type).to_string(),
                        location: None,
                        stack_trace: None,
                    })?;
                
                // Handle the effect
                vm.perform_effect(effect_type.to_string(), operation.to_string(), args)?;
            }
            
            EffectAsync => {
                // For now, handle async effects the same as sync
                let arg_count = instruction.arg as usize;
                
                // Collect arguments
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(vm.pop()?);
                }
                args.reverse();
                
                // Pop operation and effect type
                let operation = vm.pop()?;
                let operation = operation.as_string()
                    .map_err(|_| VMError::TypeError {
                        operation: "EffectAsync".to_string(),
                        expected: "String".to_string(),
                        got: value_type_name(&operation).to_string(),
                        location: None,
                        stack_trace: None,
                    })?;
                
                let effect_type = vm.pop()?;
                let effect_type = effect_type.as_string()
                    .map_err(|_| VMError::TypeError {
                        operation: "EffectAsync".to_string(),
                        expected: "String".to_string(),
                        got: value_type_name(&effect_type).to_string(),
                        location: None,
                        stack_trace: None,
                    })?;
                
                // Handle the effect (async handling to be implemented)
                vm.perform_effect(effect_type.to_string(), operation.to_string(), args)?;
            }
            
            // Create effect handler
            MakeHandler => {
                let handler_count = instruction.arg as usize;
                let mut handlers = Vec::with_capacity(handler_count);
                
                // Pop handlers in reverse order
                for _ in 0..handler_count {
                    handlers.push(vm.pop()?);
                }
                handlers.reverse();
                
                // For now, just push a placeholder
                vm.push(Value::Nil)?;
            }
            
            // Install effect handler
            InstallHandler => {
                let handler_count = instruction.arg as usize;
                let mut handlers = Vec::with_capacity(handler_count);
                
                // Pop handlers in reverse order
                for _ in 0..handler_count {
                    handlers.push(vm.pop()?);
                }
                handlers.reverse();
                
                vm.install_effect_handlers(handlers)?;
            }
            
            // Remove effect handler
            UninstallHandler => {
                vm.uninstall_effect_handler()?;
            }
            
            // Try-catch-finally error handling
            Try => {
                let catch_ip = instruction.arg as usize;
                vm.push_error_handler(catch_ip, None)?;
            }
            
            Catch => {
                // Handle catch block - simplified
                vm.pop_error_handler()?;
            }
            
            Finally => {
                // Start finally block
                vm.start_finally_block()?;
            }
            
            EndFinally => {
                // End finally block
                vm.end_finally_block()?;
            }
            
            PushHandler => {
                let catch_ip = instruction.arg as usize;
                vm.push_error_handler(catch_ip, None)?;
            }
            
            PushFinally => {
                let finally_ip = instruction.arg as usize;
                // Get the current handler and update it with finally
                // Simplified for now
                vm.push_error_handler(0, Some(finally_ip))?;
            }
            
            PopHandler => {
                vm.pop_error_handler()?;
            }
            
            Throw => {
                let error = vm.pop()?;
                return vm.throw_error(error);
            }
            
            _ => unreachable!("EffectsHandler received non-effects opcode"),
        }
        
        Ok(VMState::Continue)
    }
}