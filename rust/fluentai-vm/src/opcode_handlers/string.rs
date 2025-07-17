//! String operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{value_type_name, VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct StringHandler;

impl OpcodeHandler for StringHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            StrLen => {
                let string = vm.pop()?;
                match string {
                    Value::String(s) => vm.push(Value::Integer(s.len() as i64))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_len".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            StrConcat => vm.binary_op(|a, b| {
                // Convert both values to strings for concatenation
                let str_a = match a {
                    Value::String(s) => s,
                    Value::Integer(i) => i.to_string(),
                    Value::Float(f) => f.to_string(),
                    Value::Boolean(b) => b.to_string(),
                    Value::Nil => "nil".to_string(),
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "str_concat".to_string(),
                            expected: "string or convertible value".to_string(),
                            got: value_type_name(&a).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                };
                
                let str_b = match b {
                    Value::String(s) => s,
                    Value::Integer(i) => i.to_string(),
                    Value::Float(f) => f.to_string(),
                    Value::Boolean(b) => b.to_string(),
                    Value::Nil => "nil".to_string(),
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "str_concat".to_string(),
                            expected: "string or convertible value".to_string(),
                            got: value_type_name(&b).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                };
                
                Ok(Value::String(str_a + &str_b))
            })?,
            
            StrUpper => {
                let string = vm.pop()?;
                match string {
                    Value::String(s) => vm.push(Value::String(s.to_uppercase()))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_upper".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            StrLower => {
                let string = vm.pop()?;
                match string {
                    Value::String(s) => vm.push(Value::String(s.to_lowercase()))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_lower".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            _ => unreachable!("StringHandler received non-string opcode"),
        }
        
        Ok(VMState::Continue)
    }
}