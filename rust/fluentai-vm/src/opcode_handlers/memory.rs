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
                } else if name == "Printable" {
                    // Special handling for Printable constructor
                    // We'll treat it as a special builtin that creates Tagged values
                    Value::String("__builtin__Printable".to_string())
                } else if {
                    #[cfg(feature = "std")]
                    { vm.is_stdlib_function(&name) }
                    #[cfg(not(feature = "std"))]
                    { false }
                } {
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
            
            // Upvalue operations (for closures)
            LoadCaptured => {
                let captured_idx = instruction.arg as usize;
                let value = vm.get_upvalue(captured_idx)?.clone();
                vm.push(value)?;
            }
            
            // Cell operations (for mutable captured variables)
            MakeCell => {
                let value = vm.pop()?;
                let cell_id = vm.create_cell(value);
                vm.push(Value::Cell(cell_id))?;
            }
            
            CellGet => {
                let cell_value = vm.pop()?;
                match cell_value {
                    Value::Cell(id) => {
                        let value = vm.get_cell_value(id)?.clone();
                        vm.push(value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "cell_get".to_string(),
                            expected: "cell".to_string(),
                            got: vm.value_type_name(&cell_value).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            CellSet => {
                let value = vm.pop()?;
                let cell_value = vm.pop()?;
                match cell_value {
                    Value::Cell(id) => {
                        vm.set_cell_value(id, value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "cell_set".to_string(),
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
            
            // Function creation operations
            MakeFunc => {
                let chunk_id = instruction.arg as usize;
                let func_value = Value::Function { chunk_id, env: vec![] };
                vm.push(func_value)?;
            }
            
            MakeClosure => {
                let chunk_id = vm.pop()?;
                let env_size = instruction.arg as usize;
                let mut env = Vec::with_capacity(env_size);
                for _ in 0..env_size {
                    env.push(vm.pop()?);
                }
                env.reverse();
                
                match chunk_id {
                    Value::Integer(id) => {
                        let func_value = Value::Function { 
                            chunk_id: id as usize, 
                            env 
                        };
                        vm.push(func_value)?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "make_closure".to_string(),
                            expected: "chunk id".to_string(),
                            got: vm.value_type_name(&chunk_id).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            MakeFuture => {
                let chunk_id = instruction.arg as usize;
                let future_value = Value::Future { chunk_id, env: vec![] };
                vm.push(future_value)?;
            }
            
            MakeEnv => {
                // Create environment - implementation depends on VM design
                // For now, just continue
            }
            
            PopEnv => {
                // Pop environment - implementation depends on VM design
                // For now, just continue
            }
            
            // Tagged value operations
            MakeTagged => {
                let arity = instruction.arg as usize;
                let tag_idx = vm.pop()?;
                
                let mut values = Vec::with_capacity(arity);
                for _ in 0..arity {
                    values.push(vm.pop()?);
                }
                values.reverse();
                
                match tag_idx {
                    Value::String(tag) => {
                        vm.push(Value::Tagged { tag, values })?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "make_tagged".to_string(),
                            expected: "string tag".to_string(),
                            got: vm.value_type_name(&tag_idx).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            GetTag => {
                let value = vm.pop()?;
                match value {
                    Value::Tagged { tag, .. } => {
                        vm.push(Value::String(tag))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "get_tag".to_string(),
                            expected: "tagged value".to_string(),
                            got: vm.value_type_name(&value).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            GetTaggedField => {
                let index = instruction.arg as usize;
                let value = vm.pop()?;
                match value {
                    Value::Tagged { values, .. } => {
                        if index < values.len() {
                            vm.push(values[index].clone())?;
                        } else {
                            return Err(VMError::RuntimeError {
                                message: format!("Tagged field index {} out of bounds", index),
                                stack_trace: None,
                            });
                        }
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "get_tagged_field".to_string(),
                            expected: "tagged value".to_string(),
                            got: vm.value_type_name(&value).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            IsTagged => {
                let expected_tag = vm.pop()?;
                let value = vm.pop()?;
                
                let is_match = match (&value, &expected_tag) {
                    (Value::Tagged { tag, .. }, Value::String(expected)) => tag == expected,
                    _ => false,
                };
                
                vm.push(Value::Boolean(is_match))?;
            }
            
            // GC operations (simplified for now)
            GcAlloc => {
                let value = vm.pop()?;
                // For now, just push the value back
                vm.push(value)?;
            }
            
            GcDeref => {
                // For now, just a no-op
            }
            
            GcSet => {
                let _value = vm.pop()?;
                let _handle = vm.pop()?;
                // For now, just a no-op
            }
            
            GcCollect => {
                // Manual GC collection - no-op for now
            }
            
            _ => unreachable!("MemoryHandler received non-memory opcode: {:?}", instruction.opcode),
        }
        
        Ok(VMState::Continue)
    }
}