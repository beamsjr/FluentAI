//! Memory and variable operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct MemoryHandler;

impl MemoryHandler {
    // Load local variable
    fn handle_load(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let local_idx = arg as usize;
        let value = vm.get_local(local_idx)?.clone();
        vm.push(value)
    }

    // Store to local variable
    fn handle_store(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let local_idx = arg as usize;
        let value = vm.peek(0)?.clone(); // Don't pop, just peek
        vm.set_local(local_idx, value)
    }

    // Fast local variable access handlers
    fn handle_load_local(&mut self, vm: &mut VM, idx: usize) -> VMResult<()> {
        let value = vm.get_local(idx)?.clone();
        // Debug print to understand what's happening
        if std::env::var("VM_DEBUG").is_ok() {
            eprintln!("LoadLocal{}: Loading value {:?} from stack position {}", idx, value, vm.current_frame().stack_base + idx);
        }
        vm.push(value)
    }

    fn handle_store_local(&mut self, vm: &mut VM, idx: usize) -> VMResult<()> {
        let value = vm.peek(0)?.clone();
        vm.set_local(idx, value)
    }

    // Global variable operations
    fn handle_load_global(&mut self, vm: &mut VM, arg: u16, chunk_id: usize) -> VMResult<()> {
        let name_idx = arg as usize;
        let name = vm.bytecode()
            .chunks
            .get(chunk_id)
            .and_then(|chunk| chunk.constants.get(name_idx))
            .and_then(|v| match v {
                Value::String(s) => Some(s.as_str()),
                _ => None,
            })
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid global name at index {}", name_idx),
                stack_trace: None,
            })?;
        
        // For performance-critical built-ins, use direct references
        match name {
            // Handle Printable constructor
            "Printable" => {
                // Create a placeholder that will be recognized during Call
                vm.push(Value::Tagged {
                    tag: "__builtin__".to_string(),
                    values: vec![Value::String("Printable".to_string())],
                })?;
                return Ok(());
            }
            // TODO: Add fast path for other common globals
            _ => {
                // General lookup through registry
                if let Some(value) = vm.get_global(name) {
                    vm.push(value.clone())?;
                } else {
                    #[cfg(feature = "std")]
                    if vm.get_stdlib_registry().contains(name) {
                        // Create a placeholder that will be recognized during Call
                        // We use a special tagged value to represent stdlib functions
                        vm.push(Value::Tagged {
                            tag: "__stdlib__".to_string(),
                            values: vec![Value::String(name.to_string())],
                        })?;
                        return Ok(());
                    }
                    
                    return Err(VMError::RuntimeError {
                        message: format!("Undefined global variable: {}", name),
                        stack_trace: None,
                    });
                }
            }
        }
        Ok(())
    }

    fn handle_store_global(&mut self, vm: &mut VM, arg: u16, chunk_id: usize) -> VMResult<()> {
        let name_idx = arg as usize;
        let name = vm.bytecode()
            .chunks
            .get(chunk_id)
            .and_then(|chunk| chunk.constants.get(name_idx))
            .and_then(|v| match v {
                Value::String(s) => Some(s.clone()),
                _ => None,
            })
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid global name at index {}", name_idx),
                stack_trace: None,
            })?;
        
        let value = vm.pop()?;
        
        // Check if trying to overwrite a stdlib function
        #[cfg(feature = "std")]
        if vm.get_stdlib_registry().contains(&name) {
            return Err(VMError::RuntimeError {
                message: format!("Cannot reassign built-in function '{}'", name),
                stack_trace: None,
            });
        }
        
        vm.set_global(name, value);
        Ok(())
    }

    // Captured variable operations
    fn handle_load_captured(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let capture_idx = arg as usize;
        let frame = vm.current_frame();
        if let Some(value) = frame.env.get(capture_idx) {
            vm.push(value.clone())
        } else {
            Err(VMError::RuntimeError {
                message: format!("Captured variable index {} out of bounds", capture_idx),
                stack_trace: None,
            })
        }
    }

    // Cell operations for mutable captured variables
    fn handle_make_cell(&mut self, vm: &mut VM) -> VMResult<()> {
        let value = vm.pop()?;
        let cell_idx = vm.cells().len();
        vm.cells_mut().push(value);
        vm.push(Value::Cell(cell_idx))
    }

    fn handle_cell_get(&mut self, vm: &mut VM) -> VMResult<()> {
        let cell = vm.pop()?;
        match cell {
            Value::Cell(idx) => {
                if let Some(value) = vm.cells().get(idx) {
                    vm.push(value.clone())
                } else {
                    Err(VMError::RuntimeError {
                        message: format!("Invalid cell index {}", idx),
                        stack_trace: None,
                    })
                }
            }
            _ => Err(VMError::TypeError {
                operation: "cell_get".to_string(),
                expected: "cell".to_string(),
                got: crate::error::value_type_name(&cell).to_string(),
                location: None,
                stack_trace: None,
            })
        }
    }

    fn handle_cell_set(&mut self, vm: &mut VM) -> VMResult<()> {
        let value = vm.pop()?;
        let cell = vm.pop()?;
        match cell {
            Value::Cell(idx) => {
                if let Some(cell_val) = vm.cells_mut().get_mut(idx) {
                    *cell_val = value;
                    vm.push(Value::Nil)
                } else {
                    Err(VMError::RuntimeError {
                        message: format!("Invalid cell index {}. Total cells: {}", idx, vm.cells().len()),
                        stack_trace: None,
                    })
                }
            }
            Value::Integer(i) => {
                // Provide helpful error for common mistake
                Err(VMError::TypeError {
                    operation: "cell_set".to_string(),
                    expected: "cell".to_string(),
                    got: format!("int ({}). Did you forget to call MakeCell?", i),
                    location: None,
                    stack_trace: None,
                })
            }
            _ => Err(VMError::TypeError {
                operation: "cell_set".to_string(),
                expected: "cell".to_string(),
                got: crate::error::value_type_name(&cell).to_string(),
                location: None,
                stack_trace: None,
            })
        }
    }

    fn handle_update_local(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let local_idx = arg as usize;
        let new_value = vm.pop()?;
        
        // Check if the local is a cell (for mutable captured variables)
        let current = vm.get_local(local_idx)?.clone();
        match current {
            Value::Cell(idx) => {
                // Update the cell's value
                if let Some(cell_val) = vm.cells_mut().get_mut(idx) {
                    *cell_val = new_value;
                } else {
                    return Err(VMError::RuntimeError {
                        message: format!("Invalid cell index {}", idx),
                        stack_trace: None,
                    });
                }
            }
            _ => {
                // Direct update
                vm.set_local(local_idx, new_value)?;
            }
        }
        vm.push(Value::Nil)
    }

    // Function creation operations
    fn handle_make_func(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let func_chunk_id = arg as usize;
        let func = Value::Function {
            chunk_id: func_chunk_id,
            env: vec![],
        };
        vm.push(func)
    }

    fn handle_make_closure(&mut self, vm: &mut VM, arg: u32) -> VMResult<()> {
        // The compiler packs both chunk_id and capture count into the argument
        // Upper 16 bits: chunk_id, Lower 16 bits: capture_count
        let func_chunk_id = (arg >> 16) as usize;
        let n = (arg & 0xFFFF) as usize;
        
        let mut captured = Vec::with_capacity(n);
        
        // Pop captured values in reverse order
        for _ in 0..n {
            captured.push(vm.pop()?);
        }
        captured.reverse();
        
        let closure = Value::Function {
            chunk_id: func_chunk_id,
            env: captured,
        };
        vm.push(closure)
    }

    fn handle_make_future(&mut self, vm: &mut VM) -> VMResult<()> {
        let value = vm.pop()?;
        #[cfg(feature = "std")]
        {
            use tokio::sync::oneshot;
            
            // Create a new promise ID and channel
            let promise_id = vm.id_generator().next_promise_id();
            let (tx, rx) = oneshot::channel();
            
            // Store the receiver
            vm.promises_mut().insert(promise_id, rx);
            
            // Immediately resolve the promise with the value
            let _ = tx.send(Ok(value)); // Ignore send error if receiver is dropped
            
            vm.push(Value::Promise(promise_id.0))
        }
        #[cfg(not(feature = "std"))]
        {
            // Without std, just return the value directly
            vm.push(value)
        }
    }

    // Environment operations
    fn handle_make_env(&mut self, _vm: &mut VM) -> VMResult<()> {
        // TODO: Implement environment operations if needed
        Ok(())
    }

    fn handle_pop_env(&mut self, _vm: &mut VM) -> VMResult<()> {
        // TODO: Implement environment operations if needed
        Ok(())
    }

    // Tagged value operations
    fn handle_make_tagged(&mut self, vm: &mut VM) -> VMResult<()> {
        let fields = vm.pop()?;
        let tag = vm.pop()?;
        
        match (tag, fields) {
            (Value::String(tag_str), Value::List(field_list)) => {
                vm.push(Value::Tagged {
                    tag: tag_str,
                    values: field_list,
                })
            }
            (tag, fields) => {
                let got = format!("{} and {}", 
                    crate::error::value_type_name(&tag),
                    crate::error::value_type_name(&fields)
                );
                Err(VMError::TypeError {
                    operation: "make_tagged".to_string(),
                    expected: "string and list".to_string(),
                    got,
                    location: None,
                    stack_trace: None,
                })
            }
        }
    }

    fn handle_get_tag(&mut self, vm: &mut VM) -> VMResult<()> {
        let value = vm.pop()?;
        match value {
            Value::Tagged { tag, .. } => vm.push(Value::String(tag)),
            _ => Err(VMError::TypeError {
                operation: "get_tag".to_string(),
                expected: "tagged value".to_string(),
                got: crate::error::value_type_name(&value).to_string(),
                location: None,
                stack_trace: None,
            })
        }
    }

    fn handle_get_tagged_field(&mut self, vm: &mut VM, arg: u16) -> VMResult<()> {
        let field_idx = arg as usize;
        let value = vm.pop()?;
        
        match value {
            Value::Tagged { values: fields, .. } => {
                if field_idx < fields.len() {
                    vm.push(fields[field_idx].clone())
                } else {
                    Err(VMError::RuntimeError {
                        message: format!("Tagged field index {} out of bounds", field_idx),
                        stack_trace: None,
                    })
                }
            }
            _ => Err(VMError::TypeError {
                operation: "get_tagged_field".to_string(),
                expected: "tagged value".to_string(),
                got: crate::error::value_type_name(&value).to_string(),
                location: None,
                stack_trace: None,
            })
        }
    }

    fn handle_is_tagged(&mut self, vm: &mut VM, arg: u16, chunk_id: usize) -> VMResult<()> {
        let tag_idx = arg as usize;
        let expected_tag = vm.bytecode()
            .chunks
            .get(chunk_id)
            .and_then(|chunk| chunk.constants.get(tag_idx))
            .and_then(|v| match v {
                Value::String(s) => Some(s.clone()),
                _ => None,
            })
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid tag name at index {}", tag_idx),
                stack_trace: None,
            })?;
        
        let value = vm.pop()?;
        let result = match value {
            Value::Tagged { tag, .. } => tag == expected_tag,
            _ => false,
        };
        
        vm.push(Value::Boolean(result))
    }

    // GC operations (placeholders)
    fn handle_gc_alloc(&mut self, vm: &mut VM) -> VMResult<()> {
        // Placeholder for future GC implementation
        let value = vm.pop()?;
        vm.push(value)
    }

    fn handle_gc_deref(&mut self, vm: &mut VM) -> VMResult<()> {
        // Placeholder for future GC implementation
        Ok(())
    }

    fn handle_gc_set(&mut self, vm: &mut VM) -> VMResult<()> {
        // Placeholder for future GC implementation
        let _value = vm.pop()?;
        let _target = vm.pop()?;
        vm.push(Value::Nil)
    }

    fn handle_gc_collect(&mut self, vm: &mut VM) -> VMResult<()> {
        // Placeholder for future GC implementation
        vm.push(Value::Nil)
    }
}

impl OpcodeHandler for MemoryHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Local variable operations
            Load => self.handle_load(vm, instruction.arg as u16)?,
            Store => self.handle_store(vm, instruction.arg as u16)?,
            
            // Fast local variable access
            LoadLocal0 => self.handle_load_local(vm, 0)?,
            LoadLocal1 => self.handle_load_local(vm, 1)?,
            LoadLocal2 => self.handle_load_local(vm, 2)?,
            LoadLocal3 => self.handle_load_local(vm, 3)?,
            
            StoreLocal0 => self.handle_store_local(vm, 0)?,
            StoreLocal1 => self.handle_store_local(vm, 1)?,
            StoreLocal2 => self.handle_store_local(vm, 2)?,
            StoreLocal3 => self.handle_store_local(vm, 3)?,
            
            // Global variable operations
            LoadGlobal => self.handle_load_global(vm, instruction.arg as u16, chunk_id)?,
            StoreGlobal => self.handle_store_global(vm, instruction.arg as u16, chunk_id)?,
            
            // Captured variable operations
            LoadCaptured => self.handle_load_captured(vm, instruction.arg as u16)?,
            
            // Cell operations
            MakeCell => self.handle_make_cell(vm)?,
            CellGet => self.handle_cell_get(vm)?,
            CellSet => self.handle_cell_set(vm)?,
            UpdateLocal => self.handle_update_local(vm, instruction.arg as u16)?,
            
            // Function creation
            MakeFunc => self.handle_make_func(vm, instruction.arg as u16)?,
            MakeClosure => self.handle_make_closure(vm, instruction.arg)?,
            MakeFuture => self.handle_make_future(vm)?,
            
            // Environment operations
            MakeEnv => self.handle_make_env(vm)?,
            PopEnv => self.handle_pop_env(vm)?,
            
            // Tagged value operations
            MakeTagged => self.handle_make_tagged(vm)?,
            GetTag => self.handle_get_tag(vm)?,
            GetTaggedField => self.handle_get_tagged_field(vm, instruction.arg as u16)?,
            IsTagged => self.handle_is_tagged(vm, instruction.arg as u16, chunk_id)?,
            
            // GC operations
            GcAlloc => self.handle_gc_alloc(vm)?,
            GcDeref => self.handle_gc_deref(vm)?,
            GcSet => self.handle_gc_set(vm)?,
            GcCollect => self.handle_gc_collect(vm)?,
            
            _ => unreachable!("MemoryHandler received non-memory opcode"),
        }
        
        Ok(VMState::Continue)
    }
}