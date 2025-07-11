//! Stack manipulation operations handler

use crate::bytecode::{Instruction, Opcode};
use crate::error::{VMError, VMResult};
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct StackHandler;

impl OpcodeHandler for StackHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Basic stack manipulation
            Push => {
                let value = vm.get_constant(chunk_id, instruction.arg as usize)?
                    .clone();
                vm.push(value)?;
            }
            
            Pop => {
                vm.pop()?;
            }
            
            PopN => {
                // Pop N values but preserve the top value
                let n = instruction.arg as usize;
                if n > 0 {
                    let top = vm.pop()?;
                    for _ in 0..n {
                        vm.pop()?;
                    }
                    vm.push(top)?;
                }
            }
            
            Dup => {
                let value = vm.peek(0)?.clone();
                vm.push(value)?;
            }
            
            Swap => {
                let len = vm.stack_len();
                if len < 2 {
                    return Err(VMError::StackUnderflow {
                        operation: "swap".to_string(),
                        stack_size: len,
                        stack_trace: None,
                    });
                }
                vm.stack_swap(len - 1, len - 2);
            }
            
            // Specialized constants
            PushInt0 => vm.push(Value::Integer(0))?,
            PushInt1 => vm.push(Value::Integer(1))?,
            PushInt2 => vm.push(Value::Integer(2))?,
            PushIntSmall => vm.push(Value::Integer(instruction.arg as i64))?,
            PushTrue => vm.push(Value::Boolean(true))?,
            PushFalse => vm.push(Value::Boolean(false))?,
            PushNil => vm.push(Value::Nil)?,
            
            PushConst => {
                let const_idx = instruction.arg as usize;
                let value = vm.get_constant(chunk_id, const_idx)?
                    .clone();
                vm.push(value)?;
            }
            
            _ => unreachable!("StackHandler received non-stack opcode"),
        }
        
        Ok(VMState::Continue)
    }
}