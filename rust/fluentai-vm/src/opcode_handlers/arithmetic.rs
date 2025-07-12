//! Arithmetic operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{value_type_name, VMError, VMResult};
use crate::safety::checked_ops;
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ArithmeticHandler;

impl OpcodeHandler for ArithmeticHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Basic arithmetic
            Add => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => checked_ops::add_i64(x, y)
                    .map(Value::Integer)
                    .map_err(|_| VMError::IntegerOverflow {
                        operation: "add".to_string(),
                        operands: (x, y),
                        stack_trace: None,
                    }),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "add".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Sub => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => checked_ops::sub_i64(x, y)
                    .map(Value::Integer)
                    .map_err(|_| VMError::IntegerOverflow {
                        operation: "sub".to_string(),
                        operands: (x, y),
                        stack_trace: None,
                    }),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "sub".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Mul => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => checked_ops::mul_i64(x, y)
                    .map(Value::Integer)
                    .map_err(|_| VMError::IntegerOverflow {
                        operation: "mul".to_string(),
                        operands: (x, y),
                        stack_trace: None,
                    }),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "mul".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Div => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => {
                    checked_ops::div_i64(x, y).map(Value::Integer).map_err(|_| {
                        if y == 0 {
                            VMError::DivisionByZero {
                                location: None,
                                stack_trace: None,
                            }
                        } else {
                            VMError::IntegerOverflow {
                                operation: "div".to_string(),
                                operands: (x, y),
                                stack_trace: None,
                            }
                        }
                    })
                }
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "div".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Mod => vm.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => {
                    checked_ops::mod_i64(x, y).map(Value::Integer).map_err(|_| {
                        if y == 0 {
                            VMError::DivisionByZero {
                                location: None,
                                stack_trace: None,
                            }
                        } else {
                            VMError::IntegerOverflow {
                                operation: "mod".to_string(),
                                operands: (x, y),
                                stack_trace: None,
                            }
                        }
                    })
                }
                (a, b) => Err(VMError::TypeError {
                    operation: "mod".to_string(),
                    expected: "int".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Neg => {
                let value = vm.pop()?;
                match value {
                    Value::Integer(x) => {
                        let negated =
                            checked_ops::neg_i64(x).map_err(|_| VMError::IntegerOverflow {
                                operation: "neg".to_string(),
                                operands: (x, 0),
                                stack_trace: None,
                            })?;
                        vm.push(Value::Integer(negated))?
                    }
                    Value::Float(x) => vm.push(Value::Float(-x))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "neg".to_string(),
                            expected: "int/float".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }
            
            // Type-specialized arithmetic
            AddInt => vm.binary_int_op(|x, y| {
                checked_ops::add_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "add_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            
            SubInt => vm.binary_int_op(|x, y| {
                checked_ops::sub_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "sub_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            
            MulInt => vm.binary_int_op(|x, y| {
                checked_ops::mul_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "mul_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            
            DivInt => vm.binary_int_op(|x, y| {
                checked_ops::div_i64(x, y).map_err(|_| {
                    if y == 0 {
                        VMError::DivisionByZero {
                            location: None,
                            stack_trace: None,
                        }
                    } else {
                        VMError::IntegerOverflow {
                            operation: "div_int".to_string(),
                            operands: (x, y),
                            stack_trace: None,
                        }
                    }
                })
            })?,
            
            // Float-specialized arithmetic
            AddFloat => vm.binary_float_op(|x, y| x + y)?,
            SubFloat => vm.binary_float_op(|x, y| x - y)?,
            MulFloat => vm.binary_float_op(|x, y| x * y)?,
            DivFloat => vm.binary_float_op(|x, y| x / y)?,
            
            _ => unreachable!("ArithmeticHandler received non-arithmetic opcode"),
        }
        
        Ok(VMState::Continue)
    }
}