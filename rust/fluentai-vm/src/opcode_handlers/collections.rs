//! Collection operations handler (Lists and Maps)

use fluentai_bytecode::{Instruction, Opcode};
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
            
            _ => unreachable!("CollectionsHandler received non-collection opcode"),
        }
        
        Ok(VMState::Continue)
    }
}