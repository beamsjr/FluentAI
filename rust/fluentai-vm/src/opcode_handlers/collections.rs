//! Collection operations handler (Lists and Maps)

use crate::bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct CollectionsHandler;

impl OpcodeHandler for CollectionsHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // List operations
            MakeList => {
                let count = instruction.arg as usize;
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(vm.pop()?);
                }
                items.reverse();
                vm.push(Value::List(items))?;
            }
            
            ListGet => {
                let index = vm.pop()?;
                let list = vm.pop()?;
                
                match (&list, &index) {
                    (Value::List(items), Value::Integer(idx)) => {
                        let idx = *idx as usize;
                        if idx < items.len() {
                            vm.push(items[idx].clone())?;
                        } else {
                            return Err(VMError::RuntimeError {
                                message: format!("List index out of bounds: {} (length: {})", idx, items.len()),
                                stack_trace: None,
                            });
                        }
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_get".to_string(),
                            expected: "list and integer index".to_string(),
                            got: format!("{} and {}", vm.value_type_name(&list), vm.value_type_name(&index)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListSet => {
                let value = vm.pop()?;
                let index = vm.pop()?;
                let list = vm.pop()?;
                
                match (list, &index) {
                    (Value::List(mut items), Value::Integer(idx)) => {
                        let idx = *idx as usize;
                        if idx < items.len() {
                            items[idx] = value;
                            vm.push(Value::List(items))?;
                        } else {
                            return Err(VMError::RuntimeError {
                                message: format!("List index out of bounds: {} (length: {})", idx, items.len()),
                                stack_trace: None,
                            });
                        }
                    }
                    (list, _) => {
                        return Err(VMError::TypeError {
                            operation: "list_set".to_string(),
                            expected: "list and integer index".to_string(),
                            got: format!("{} and {}", vm.value_type_name(&list), vm.value_type_name(&index)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListHead => {
                let list = vm.pop()?;
                match list {
                    Value::List(items) => {
                        if !items.is_empty() {
                            vm.push(items[0].clone())?;
                        } else {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take head of empty list".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_head".to_string(),
                            expected: "list".to_string(),
                            got: vm.value_type_name(&list).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListTail => {
                let list = vm.pop()?;
                match list {
                    Value::List(items) => {
                        if !items.is_empty() {
                            let tail = items[1..].to_vec();
                            vm.push(Value::List(tail))?;
                        } else {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take tail of empty list".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_tail".to_string(),
                            expected: "list".to_string(),
                            got: vm.value_type_name(&list).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListCons => {
                let list = vm.pop()?;
                let head = vm.pop()?;
                match list {
                    Value::List(mut items) => {
                        items.insert(0, head);
                        vm.push(Value::List(items))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_cons".to_string(),
                            expected: "list".to_string(),
                            got: vm.value_type_name(&list).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListLen => {
                let list = vm.pop()?;
                match list {
                    Value::List(items) => {
                        vm.push(Value::Integer(items.len() as i64))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_len".to_string(),
                            expected: "list".to_string(),
                            got: vm.value_type_name(&list).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            ListEmpty => {
                let list = vm.pop()?;
                match list {
                    Value::List(items) => {
                        vm.push(Value::Boolean(items.is_empty()))?;
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "list_empty".to_string(),
                            expected: "list".to_string(),
                            got: vm.value_type_name(&list).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            // Map operations
            MakeMap => {
                let count = instruction.arg as usize;
                let mut map = rustc_hash::FxHashMap::default();
                for _ in 0..count {
                    let value = vm.pop()?;
                    let key = vm.pop()?;
                    if let Value::String(k) = key {
                        map.insert(k, value);
                    } else {
                        return Err(VMError::TypeError {
                            operation: "make_map".to_string(),
                            expected: "string key".to_string(),
                            got: vm.value_type_name(&key).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
                vm.push(Value::Map(map))?;
            }
            
            MapGet => {
                let key = vm.pop()?;
                let map = vm.pop()?;
                
                match (&map, &key) {
                    (Value::Map(m), Value::String(k)) => {
                        if let Some(value) = m.get(k) {
                            vm.push(value.clone())?;
                        } else {
                            vm.push(Value::Nil)?;
                        }
                    }
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "map_get".to_string(),
                            expected: "map and string key".to_string(),
                            got: format!("{} and {}", vm.value_type_name(&map), vm.value_type_name(&key)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            MapSet => {
                let value = vm.pop()?;
                let key = vm.pop()?;
                let map = vm.pop()?;
                
                match (map, &key) {
                    (Value::Map(mut m), Value::String(k)) => {
                        m.insert(k.clone(), value);
                        vm.push(Value::Map(m))?;
                    }
                    (map, _) => {
                        return Err(VMError::TypeError {
                            operation: "map_set".to_string(),
                            expected: "map and string key".to_string(),
                            got: format!("{} and {}", vm.value_type_name(&map), vm.value_type_name(&key)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            _ => unreachable!("CollectionsHandler received non-collection opcode"),
        }
        
        Ok(VMState::Continue)
    }
}