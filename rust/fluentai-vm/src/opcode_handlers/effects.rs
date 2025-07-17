//! Effect and handler operations

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult, value_type_name};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct EffectsHandler;

impl EffectsHandler {
    // Helper for type errors
    fn type_error(&self, operation: &str, expected: &str, got: &Value) -> VMError {
        VMError::TypeError {
            operation: operation.to_string(),
            expected: expected.to_string(),
            got: value_type_name(got).to_string(),
            location: None,
            stack_trace: None,
        }
    }

    // Perform effect handler
    fn handle_effect(&mut self, vm: &mut VM, arg_count: u32) -> VMResult<()> {
        let arg_count = arg_count as usize;
        
        // Collect arguments
        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(vm.pop()?);
        }
        args.reverse();
        
        // Pop operation and effect type
        let operation = vm.pop()?;
        let operation = operation.as_string()
            .map_err(|_| self.type_error("Effect", "String", &operation))?;
        
        let effect_type = vm.pop()?;
        let effect_type = effect_type.as_string()
            .map_err(|_| self.type_error("Effect", "String", &effect_type))?;
        
        // Handle the effect
        vm.perform_effect(effect_type.to_string(), operation.to_string(), args)
    }

    // Async effect handler
    fn handle_effect_async(&mut self, vm: &mut VM, arg_count: u32) -> VMResult<()> {
        let arg_count = arg_count as usize;
        
        // Collect arguments
        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(vm.pop()?);
        }
        args.reverse();
        
        // Pop operation and effect type
        let operation = vm.pop()?;
        let operation = operation.as_string()
            .map_err(|_| self.type_error("EffectAsync", "String", &operation))?;
        
        let effect_type = vm.pop()?;
        let effect_type = effect_type.as_string()
            .map_err(|_| self.type_error("EffectAsync", "String", &effect_type))?;
        
        // Handle the async effect
        vm.perform_effect_async(effect_type.to_string(), operation.to_string(), args)
    }

    // Make handler handler
    fn handle_make_handler(&mut self, vm: &mut VM) -> VMResult<()> {
        let handler_fn = vm.pop()?;
        
        // Validate it's a function
        match handler_fn {
            Value::Function { .. } | Value::NativeFunction { .. } => {
                // Create a handler value (for now just wrap in a map)
                let handler = Value::Map({
                    let mut map = rustc_hash::FxHashMap::default();
                    map.insert("__handler__".to_string(), handler_fn);
                    map
                });
                vm.push(handler)
            }
            _ => Err(self.type_error("MakeHandler", "Function", &handler_fn))
        }
    }

    // Install handler handler
    fn handle_install_handler(&mut self, vm: &mut VM, handler_count: u32) -> VMResult<()> {
        let body = vm.pop()?;
        
        // Pop handlers
        let mut handlers = Vec::with_capacity(handler_count as usize);
        for _ in 0..handler_count {
            handlers.push(vm.pop()?);
        }
        
        // Install handlers and execute body
        // Install handlers and execute body
        // Note: This is a simplified implementation
        vm.push(body)
    }

    // Uninstall handler handler
    fn handle_uninstall_handler(&mut self, vm: &mut VM, handler_id: u32) -> VMResult<()> {
        // Uninstall handler
        vm.uninstall_effect_handler()
    }

    // Try handler
    fn handle_try(&mut self, vm: &mut VM) -> VMResult<()> {
        // TBD: Mark beginning of try block
        Ok(())
    }

    // Catch handler
    fn handle_catch(&mut self, vm: &mut VM, catch_target: u32) -> VMResult<()> {
        // Pop the value from the try block
        let value = vm.pop()?;
        
        // Check if it's an error
        match value {
            Value::Error { .. } => {
                // Push error and jump to catch block
                vm.push(value)?;
                vm.set_ip(catch_target as usize);
            }
            _ => {
                // Not an error, push back the value
                vm.push(value)?;
            }
        }
        Ok(())
    }

    // Finally handler
    fn handle_finally(&mut self, vm: &mut VM, finally_target: u32) -> VMResult<()> {
        // Jump to finally block
        vm.set_ip(finally_target as usize);
        Ok(())
    }

    // End finally handler
    fn handle_end_finally(&mut self, vm: &mut VM) -> VMResult<()> {
        // Resume normal execution after finally
        Ok(())
    }

    // Push handler handler
    fn handle_push_handler(&mut self, vm: &mut VM, catch_ip: u32, finally_ip: u32) -> VMResult<()> {
        let finally_ip = if finally_ip == 0 { None } else { Some(finally_ip as usize) };
        vm.push_error_handler(catch_ip as usize, finally_ip)
    }

    // Push finally handler
    fn handle_push_finally(&mut self, vm: &mut VM, finally_ip: u32) -> VMResult<()> {
        vm.push_error_handler(usize::MAX, Some(finally_ip as usize))
    }

    // Pop handler handler
    fn handle_pop_handler(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.pop_error_handler()
    }

    // Throw handler
    fn handle_throw(&mut self, vm: &mut VM) -> VMResult<VMState> {
        let error = vm.pop()?;
        
        // Ensure it's an error value
        let error = match error {
            Value::Error { .. } => error,
            _ => {
                // Wrap non-error values in an error
                Value::Error {
                    kind: "ThrowError".to_string(),
                    message: format!("{}", error),
                    stack_trace: None,
                }
            }
        };
        
        // Throw error by jumping to error handler
        // For now, return the error as a runtime error
        Err(match error {
            Value::Error { kind, message, .. } => VMError::RuntimeError {
                message: format!("{}: {}", kind, message),
                stack_trace: None,
            },
            _ => VMError::RuntimeError {
                message: format!("Thrown value: {}", error),
                stack_trace: None,
            }
        })
    }
}

impl OpcodeHandler for EffectsHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Perform an effect
            Effect => {
                self.handle_effect(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            EffectAsync => {
                self.handle_effect_async(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            MakeHandler => {
                self.handle_make_handler(vm)?;
                Ok(VMState::Continue)
            }
            
            InstallHandler => {
                self.handle_install_handler(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            UninstallHandler => {
                self.handle_uninstall_handler(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            Try => {
                self.handle_try(vm)?;
                Ok(VMState::Continue)
            }
            
            Catch => {
                self.handle_catch(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            Finally => {
                self.handle_finally(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            EndFinally => {
                self.handle_end_finally(vm)?;
                Ok(VMState::Continue)
            }
            
            PushHandler => {
                // Extract catch and finally IPs from instruction
                let catch_ip = instruction.arg & 0xFFFF;
                let finally_ip = instruction.arg >> 16;
                self.handle_push_handler(vm, catch_ip, finally_ip)?;
                Ok(VMState::Continue)
            }
            
            PushFinally => {
                self.handle_push_finally(vm, instruction.arg)?;
                Ok(VMState::Continue)
            }
            
            PopHandler => {
                self.handle_pop_handler(vm)?;
                Ok(VMState::Continue)
            }
            
            Throw => self.handle_throw(vm),
            
            _ => unreachable!("EffectsHandler received non-effect opcode"),
        }
    }
}