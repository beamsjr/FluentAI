//! Logical operations handler (comparison and boolean)

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{value_type_name, VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct LogicalHandler;

impl OpcodeHandler for LogicalHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Comparison operations
            Eq => vm.binary_op(|a, b| Ok(Value::Boolean(a == b)))?,
            Ne => vm.binary_op(|a, b| Ok(Value::Boolean(a != b)))?,
            
            Lt => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x < y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x < y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "lt".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Le => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x <= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x <= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "le".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Gt => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x > y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x > y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "gt".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Ge => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x >= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x >= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "ge".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            // Integer-specialized comparison
            LtInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x < y))?
                    }
                    (a, b) => return Err(VMError::TypeError {
                        operation: "lt_int".to_string(),
                        expected: "int".to_string(),
                        got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            LeInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x <= y))?
                    }
                    (a, b) => return Err(VMError::TypeError {
                        operation: "le_int".to_string(),
                        expected: "int".to_string(),
                        got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            GtInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x > y))?
                    }
                    (a, b) => return Err(VMError::TypeError {
                        operation: "gt_int".to_string(),
                        expected: "int".to_string(),
                        got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            GeInt => {
                let b = vm.pop()?;
                let a = vm.pop()?;
                match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => {
                        vm.push(Value::Boolean(x >= y))?
                    }
                    (a, b) => return Err(VMError::TypeError {
                        operation: "ge_int".to_string(),
                        expected: "int".to_string(),
                        got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                        location: None,
                        stack_trace: None,
                    }),
                }
            },
            
            // Boolean operations
            And => vm.binary_op(|a, b| match (a, b) {
                (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(x && y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "and".to_string(),
                    expected: "bool".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Or => vm.binary_op(|a, b| match (a, b) {
                (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(x || y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "or".to_string(),
                    expected: "bool".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Not => {
                let value = vm.pop()?;
                match value {
                    Value::Boolean(b) => vm.push(Value::Boolean(!b))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "not".to_string(),
                            expected: "bool".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            _ => unreachable!("LogicalHandler received non-logical opcode"),
        }
        
        Ok(VMState::Continue)
    }
}