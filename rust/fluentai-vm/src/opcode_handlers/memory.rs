//! Memory and variable operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct MemoryHandler;

impl OpcodeHandler for MemoryHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Local variable operations
            Load => {
                let local_idx = instruction.arg as usize;
                let value = vm.get_local(local_idx)?.clone();
                vm.push(value)?;
            }
            
            Store => {
                let local_idx = instruction.arg as usize;
                let value = vm.peek(0)?.clone(); // Don't pop, just peek
                vm.set_local(local_idx, value)?;
            }
            
            LoadLocal => {
                let local_idx = instruction.arg as usize;
                let value = vm.get_local(local_idx)?.clone();
                vm.push(value)?;
            }
            
            StoreLocal => {
                let local_idx = instruction.arg as usize;
                let value = vm.peek(0)?.clone(); // Don't pop, just peek
                vm.set_local(local_idx, value)?;
            }
            
            // Fast local variable access
            LoadLocal0 => {
                let value = vm.get_local(0)?.clone();
                vm.push(value)?;
            }
            LoadLocal1 => {
                let value = vm.get_local(1)?.clone();
                vm.push(value)?;
            }
            LoadLocal2 => {
                let value = vm.get_local(2)?.clone();
                vm.push(value)?;
            }
            LoadLocal3 => {
                let value = vm.get_local(3)?.clone();
                vm.push(value)?;
            }
            
            StoreLocal0 => {
                let value = vm.peek(0)?.clone();
                vm.set_local(0, value)?;
            }
            StoreLocal1 => {
                let value = vm.peek(0)?.clone();
                vm.set_local(1, value)?;
            }
            StoreLocal2 => {
                let value = vm.peek(0)?.clone();
                vm.set_local(2, value)?;
            }
            StoreLocal3 => {
                let value = vm.peek(0)?.clone();
                vm.set_local(3, value)?;
            }
            
            // Global variable operations
            LoadGlobal => {
                let name_idx = instruction.arg as usize;
                let name = vm.get_constant_string_at(chunk_id, name_idx)?;
                
                // Check user-defined globals first
                let value = if let Some(global_value) = vm.get_global(&name) {
                    global_value.clone()
                } else if vm.is_stdlib_function(&name) {
                    // Standard library function
                    Value::String(format!("__stdlib__{}", name))
                } else if vm.is_builtin(&name) {
                    // For built-ins, we'll store them as a special string value
                    Value::String(format!("__builtin__{}", name))
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: name.clone(),
                        location: None,
                        stack_trace: None,
                    });
                };
                
                vm.push(value)?;
            }
            
            StoreGlobal => {
                let name_idx = instruction.arg as usize;
                let name = vm.get_constant_string_at(chunk_id, name_idx)?;
                let value = vm.pop()?;
                vm.set_global(name, value);
            }
            
            DefineGlobal => {
                let name_idx = instruction.arg as usize;
                let name = vm.get_constant_string_at(chunk_id, name_idx)?;
                let value = vm.pop()?;
                vm.define_global(name, value)?;
            }
            
            // Upvalue operations (for closures)
            LoadCaptured => {
                let captured_idx = instruction.arg as usize;
                let value = vm.get_upvalue(captured_idx)?.clone();
                vm.push(value)?;
            }
            
            LoadUpvalue => {
                let upvalue_idx = instruction.arg as usize;
                let value = vm.get_upvalue(upvalue_idx)?.clone();
                vm.push(value)?;
            }
            
            StoreUpvalue => {
                let upvalue_idx = instruction.arg as usize;
                let value = vm.peek(0)?.clone();
                vm.set_upvalue(upvalue_idx, value)?;
            }
            
            // Cell operations (for mutable captured variables)
            MakeCell => {
                let value = vm.pop()?;
                let cell_id = vm.create_cell(value);
                vm.push(Value::Cell(cell_id))?;
            }
            
            LoadCell => {
                let cell_value = vm.pop()?;
                match cell_value {
                    Value::Cell(id) => {
                        let value = vm.get_cell_value(id)?.clone();
                        vm.push(value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "load_cell".to_string(),
                            expected: "cell".to_string(),
                            got: vm.value_type_name(&cell_value).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            StoreCell => {
                let value = vm.pop()?;
                let cell_value = vm.pop()?;
                match cell_value {
                    Value::Cell(id) => {
                        vm.set_cell_value(id, value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "store_cell".to_string(),
                            expected: "cell".to_string(),
                            got: vm.value_type_name(&cell_value).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            UpdateLocal => {
                let local_idx = instruction.arg as usize;
                let value = vm.pop()?;
                vm.set_local(local_idx, value)?;
            }
            
            _ => unreachable!("MemoryHandler received non-memory opcode"),
        }
        
        Ok(VMState::Continue)
    }
}