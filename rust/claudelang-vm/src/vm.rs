//! Virtual machine implementation

use crate::bytecode::{Bytecode, Opcode, Instruction};
use claudelang_core::{Result, Error, value::Value};

pub struct VM {
    stack: Vec<Value>,
    ip: usize,
    globals: rustc_hash::FxHashMap<String, Value>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            stack: Vec::with_capacity(1024),
            ip: 0,
            globals: rustc_hash::FxHashMap::default(),
        }
    }
    
    pub fn execute(&mut self, bytecode: &Bytecode) -> Result<Value> {
        self.ip = 0;
        self.stack.clear();
        
        while self.ip < bytecode.instructions.len() {
            let instruction = &bytecode.instructions[self.ip];
            self.execute_instruction(instruction, bytecode)?;
            self.ip += 1;
        }
        
        // Return top of stack or Nil
        Ok(self.stack.pop().unwrap_or(Value::Nil))
    }
    
    fn execute_instruction(&mut self, instruction: &Instruction, bytecode: &Bytecode) -> Result<()> {
        match instruction.opcode {
            Opcode::Push => {
                if let Some(idx) = instruction.operand {
                    let constant = &bytecode.constants[idx as usize];
                    let value = match constant {
                        crate::bytecode::Constant::Integer(n) => Value::Integer(*n),
                        crate::bytecode::Constant::Float(f) => Value::Float(*f),
                        crate::bytecode::Constant::String(s) => Value::String(s.clone()),
                        crate::bytecode::Constant::Boolean(b) => Value::Boolean(*b),
                        crate::bytecode::Constant::Nil => Value::Nil,
                    };
                    self.stack.push(value);
                }
            }
            
            Opcode::Pop => {
                self.stack.pop();
            }
            
            Opcode::Add => {
                let b = self.stack.pop().ok_or(Error::Runtime("Stack underflow".to_string()))?;
                let a = self.stack.pop().ok_or(Error::Runtime("Stack underflow".to_string()))?;
                
                let result = match (a, b) {
                    (Value::Integer(x), Value::Integer(y)) => Value::Integer(x + y),
                    (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                    (Value::Integer(x), Value::Float(y)) => Value::Float(x as f64 + y),
                    (Value::Float(x), Value::Integer(y)) => Value::Float(x + y as f64),
                    _ => return Err(Error::Type("Cannot add non-numeric values".to_string())),
                };
                
                self.stack.push(result);
            }
            
            // TODO: Implement remaining opcodes
            _ => return Err(Error::Runtime(format!("Unimplemented opcode: {:?}", instruction.opcode))),
        }
        
        Ok(())
    }
}