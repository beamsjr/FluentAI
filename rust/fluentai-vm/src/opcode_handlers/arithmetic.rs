//! Arithmetic operations handler

use fluentai_bytecode::{Instruction, Opcode};
use crate::error::{value_type_name, VMError, VMResult};
use crate::safety::checked_ops;
use crate::vm::{VM, VMState};
use fluentai_core::value::Value;
use super::OpcodeHandler;

pub struct ArithmeticHandler;

impl ArithmeticHandler {
    // Add operation handler
    fn handle_add(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_op(|a, b| match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => checked_ops::add_i64(x, y)
                .map(Value::Integer)
                .map_err(|_| VMError::IntegerOverflow {
                    operation: "add".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                }),
            (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
            (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
            // Allow string concatenation with automatic conversion
            (Value::String(s), Value::Integer(n)) => Ok(Value::String(format!("{}{}", s, n))),
            (Value::Integer(n), Value::String(s)) => Ok(Value::String(format!("{}{}", n, s))),
            (Value::String(s), Value::Float(f)) => Ok(Value::String(format!("{}{}", s, f))),
            (Value::Float(f), Value::String(s)) => Ok(Value::String(format!("{}{}", f, s))),
            (Value::String(s), Value::Boolean(b)) => Ok(Value::String(format!("{}{}", s, b))),
            (Value::Boolean(b), Value::String(s)) => Ok(Value::String(format!("{}{}", b, s))),
            (Value::String(s), Value::Nil) => Ok(Value::String(format!("{}nil", s))),
            (Value::Nil, Value::String(s)) => Ok(Value::String(format!("nil{}", s))),
            (a, b) => Err(VMError::TypeError {
                operation: "add".to_string(),
                expected: "numbers or strings".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
        })
    }

    // Subtract operation handler
    fn handle_sub(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_op(|a, b| match (a, b) {
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
        })
    }

    // Multiply operation handler
    fn handle_mul(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_op(|a, b| match (a, b) {
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
        })
    }

    // Divide operation handler
    fn handle_div(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_op(|a, b| match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    Err(VMError::DivisionByZero {
                        location: None,
                        stack_trace: None,
                    })
                } else {
                    // Integer division in FLC returns a float
                    Ok(Value::Float(x as f64 / y as f64))
                }
            }
            (Value::Float(x), Value::Float(y)) => {
                if y == 0.0 {
                    Err(VMError::DivisionByZero {
                        location: None,
                        stack_trace: None,
                    })
                } else {
                    Ok(Value::Float(x / y))
                }
            }
            (a, b) => Err(VMError::TypeError {
                operation: "div".to_string(),
                expected: "int/float".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
        })
    }

    // Modulo operation handler
    fn handle_mod(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_op(|a, b| match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                if y == 0 {
                    Err(VMError::DivisionByZero {
                        location: None,
                        stack_trace: None,
                    })
                } else {
                    Ok(Value::Integer(x % y))
                }
            }
            (Value::Float(x), Value::Float(y)) => {
                if y == 0.0 {
                    Err(VMError::DivisionByZero {
                        location: None,
                        stack_trace: None,
                    })
                } else {
                    Ok(Value::Float(x % y))
                }
            }
            (a, b) => Err(VMError::TypeError {
                operation: "mod".to_string(),
                expected: "int/float".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
        })
    }

    // Negate operation handler
    fn handle_neg(&mut self, vm: &mut VM) -> VMResult<()> {
        let val = vm.pop()?;
        match val {
            Value::Integer(x) => {
                match checked_ops::neg_i64(x) {
                    Ok(result) => vm.push(Value::Integer(result)),
                    Err(_) => Err(VMError::IntegerOverflow {
                        operation: "neg".to_string(),
                        operands: (x, 0),
                        stack_trace: None,
                    }),
                }
            }
            Value::Float(x) => vm.push(Value::Float(-x)),
            _ => Err(VMError::TypeError {
                operation: "neg".to_string(),
                expected: "int/float".to_string(),
                got: value_type_name(&val).to_string(),
                location: None,
                stack_trace: None,
            }),
        }
    }

    // Integer add operation handler
    fn handle_add_int(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_int_op(|x, y| {
            checked_ops::add_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "add_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            })
        })
    }

    // Integer subtract operation handler
    fn handle_sub_int(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_int_op(|x, y| {
            checked_ops::sub_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "sub_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            })
        })
    }

    // Integer multiply operation handler
    fn handle_mul_int(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_int_op(|x, y| {
            checked_ops::mul_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "mul_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            })
        })
    }

    // Integer divide operation handler
    fn handle_div_int(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_int_op(|x, y| {
            checked_ops::div_i64(x, y).map_err(|_| VMError::DivisionByZero {
                location: None,
                stack_trace: None,
            })
        })
    }

    // Float add operation handler
    fn handle_add_float(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_float_op(|x, y| x + y)
    }

    // Float subtract operation handler
    fn handle_sub_float(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_float_op(|x, y| x - y)
    }

    // Float multiply operation handler
    fn handle_mul_float(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_float_op(|x, y| x * y)
    }

    // Float divide operation handler
    fn handle_div_float(&mut self, vm: &mut VM) -> VMResult<()> {
        vm.binary_float_op(|x, y| x / y)
    }
}

impl OpcodeHandler for ArithmeticHandler {
    fn execute(&mut self, vm: &mut VM, instruction: &Instruction, _chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Basic arithmetic
            Add => self.handle_add(vm)?,
            Sub => self.handle_sub(vm)?,
            Mul => self.handle_mul(vm)?,
            Div => self.handle_div(vm)?,
            Mod => self.handle_mod(vm)?,
            Neg => self.handle_neg(vm)?,
            
            // Type-specific operations
            AddInt => self.handle_add_int(vm)?,
            SubInt => self.handle_sub_int(vm)?,
            MulInt => self.handle_mul_int(vm)?,
            DivInt => self.handle_div_int(vm)?,
            AddFloat => self.handle_add_float(vm)?,
            SubFloat => self.handle_sub_float(vm)?,
            MulFloat => self.handle_mul_float(vm)?,
            DivFloat => self.handle_div_float(vm)?,
            
            _ => unreachable!("ArithmeticHandler received non-arithmetic opcode"),
        }
        
        Ok(VMState::Continue)
    }
}