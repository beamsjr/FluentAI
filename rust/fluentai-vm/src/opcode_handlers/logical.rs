//! Logical operations handler

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
                    Value::Boolean(x) => vm.push(Value::Boolean(!x))?,
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