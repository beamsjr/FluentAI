//! Comparison operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{value_type_name, VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ComparisonHandler;

impl OpcodeHandler for ComparisonHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            Eq => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                let equal = match (&a, &b) {
                    (Value::Integer(x), Value::Integer(y)) => x == y,
                    (Value::Float(x), Value::Float(y)) => x == y,
                    (Value::String(x), Value::String(y)) => x == y,
                    (Value::Boolean(x), Value::Boolean(y)) => x == y,
                    (Value::Nil, Value::Nil) => true,
                    (Value::Symbol(x), Value::Symbol(y)) => x == y,
                    _ => false,
                };
                vm.push(Value::Boolean(equal))?;
            }
            
            Ne => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                let equal = match (&a, &b) {
                    (Value::Integer(x), Value::Integer(y)) => x == y,
                    (Value::Float(x), Value::Float(y)) => x == y,
                    (Value::String(x), Value::String(y)) => x == y,
                    (Value::Boolean(x), Value::Boolean(y)) => x == y,
                    (Value::Nil, Value::Nil) => true,
                    (Value::Symbol(x), Value::Symbol(y)) => x == y,
                    _ => false,
                };
                vm.push(Value::Boolean(!equal))?;
            }
            
            Lt => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x < y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x < y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x < y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "lt".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Le => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x <= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x <= y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x <= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "le".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Gt => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x > y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x > y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x > y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "gt".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Ge => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x >= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x >= y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x >= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "ge".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            // Type-specialized comparison
            LtInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x < y))?;
                    }
                    (a, b) => {
                        return Err(VMError::TypeError {
                            operation: "lt_int".to_string(),
                            expected: "integer".to_string(),
                            got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            LeInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x <= y))?;
                    }
                    (a, b) => {
                        return Err(VMError::TypeError {
                            operation: "le_int".to_string(),
                            expected: "integer".to_string(),
                            got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            GtInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x > y))?;
                    }
                    (a, b) => {
                        return Err(VMError::TypeError {
                            operation: "gt_int".to_string(),
                            expected: "integer".to_string(),
                            got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            GeInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x >= y))?;
                    }
                    (a, b) => {
                        return Err(VMError::TypeError {
                            operation: "ge_int".to_string(),
                            expected: "integer".to_string(),
                            got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                            location: None,
                            stack_trace: None,
                        });
                    }
                }
            }
            
            _ => unreachable!("ComparisonHandler received non-comparison opcode"),
        }
        
        Ok(VMState::Continue)
    }
}