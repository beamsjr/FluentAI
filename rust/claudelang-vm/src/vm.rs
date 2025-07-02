//! High-performance stack-based virtual machine

use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode, Value};
use anyhow::{anyhow, Result};
use std::collections::HashMap;

const STACK_SIZE: usize = 10_000;

pub struct CallFrame {
    chunk_id: usize,
    ip: usize,
    stack_base: usize,
}

pub struct VM {
    bytecode: Bytecode,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    trace: bool,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            bytecode,
            stack: Vec::with_capacity(STACK_SIZE),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            trace: false,
        }
    }
    
    pub fn enable_trace(&mut self) {
        self.trace = true;
    }
    
    pub fn run(&mut self) -> Result<Value> {
        self.call_stack.push(CallFrame {
            chunk_id: self.bytecode.main_chunk,
            ip: 0,
            stack_base: 0,
        });
        
        loop {
            let frame = self.call_stack.last().ok_or_else(|| anyhow!("Call stack underflow"))?;
            let chunk_id = frame.chunk_id;
            let ip = frame.ip;
            
            if ip >= self.bytecode.chunks[chunk_id].instructions.len() {
                return Err(anyhow!("Instruction pointer out of bounds"));
            }
            
            let instruction = self.bytecode.chunks[chunk_id].instructions[ip].clone();
            
            if self.trace {
                eprintln!("Stack: {:?}", self.stack);
                eprintln!("Executing: {:?} at {}", instruction.opcode, ip);
            }
            
            // Increment IP before execution (may be modified by jumps)
            self.call_stack.last_mut().unwrap().ip += 1;
            
            match self.execute_instruction(&instruction, chunk_id)? {
                VMState::Continue => {}
                VMState::Return => {
                    if self.call_stack.len() == 1 {
                        // Main function returning
                        return self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"));
                    }
                    // Pop call frame and continue
                    self.call_stack.pop();
                }
                VMState::Halt => {
                    return self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"));
                }
            }
        }
    }
    
    fn execute_instruction(&mut self, instruction: &Instruction, chunk_id: usize) -> Result<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Stack manipulation
            Push => {
                let value = self.bytecode.chunks[chunk_id].constants.get(instruction.arg as usize)
                    .ok_or_else(|| anyhow!("Invalid constant index"))?
                    .clone();
                self.push(value)?;
            }
            Pop => {
                self.pop()?;
            }
            Dup => {
                let value = self.peek(0)?.clone();
                self.push(value)?;
            }
            Swap => {
                let len = self.stack.len();
                if len < 2 {
                    return Err(anyhow!("Stack underflow"));
                }
                self.stack.swap(len - 1, len - 2);
            }
            
            // Specialized constants
            PushInt0 => self.push(Value::Int(0))?,
            PushInt1 => self.push(Value::Int(1))?,
            PushInt2 => self.push(Value::Int(2))?,
            PushIntSmall => self.push(Value::Int(instruction.arg as i64))?,
            PushTrue => self.push(Value::Bool(true))?,
            PushFalse => self.push(Value::Bool(false))?,
            PushNil => self.push(Value::Nil)?,
            
            // Arithmetic
            Add => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x + y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                _ => Err(anyhow!("Type error in add")),
            })?,
            
            Sub => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x - y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
                _ => Err(anyhow!("Type error in sub")),
            })?,
            
            Mul => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Int(x * y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
                _ => Err(anyhow!("Type error in mul")),
            })?,
            
            Div => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    if y == 0 {
                        Err(anyhow!("Division by zero"))
                    } else {
                        Ok(Value::Int(x / y))
                    }
                }
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
                _ => Err(anyhow!("Type error in div")),
            })?,
            
            Mod => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => {
                    if y == 0 {
                        Err(anyhow!("Modulo by zero"))
                    } else {
                        Ok(Value::Int(x % y))
                    }
                }
                _ => Err(anyhow!("Type error in mod")),
            })?,
            
            Neg => {
                let value = self.pop()?;
                match value {
                    Value::Int(x) => self.push(Value::Int(-x))?,
                    Value::Float(x) => self.push(Value::Float(-x))?,
                    _ => return Err(anyhow!("Type error in neg")),
                }
            }
            
            // Type-specialized arithmetic
            AddInt => self.binary_int_op(|x, y| Ok(x + y))?,
            SubInt => self.binary_int_op(|x, y| Ok(x - y))?,
            MulInt => self.binary_int_op(|x, y| Ok(x * y))?,
            DivInt => self.binary_int_op(|x, y| {
                if y == 0 {
                    Err(anyhow!("Division by zero"))
                } else {
                    Ok(x / y)
                }
            })?,
            
            // Comparison
            Eq => {
                let b = self.pop()?;
                let a = self.pop()?;
                let equal = self.values_equal(&a, &b);
                self.push(Value::Bool(equal))?;
            }
            Ne => {
                let b = self.pop()?;
                let a = self.pop()?;
                let equal = self.values_equal(&a, &b);
                self.push(Value::Bool(!equal))?;
            }
            
            Lt => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x < y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x < y)),
                _ => Err(anyhow!("Type error in lt")),
            })?,
            
            Le => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x <= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x <= y)),
                _ => Err(anyhow!("Type error in le")),
            })?,
            
            Gt => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x > y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x > y)),
                _ => Err(anyhow!("Type error in gt")),
            })?,
            
            Ge => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x >= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x >= y)),
                _ => Err(anyhow!("Type error in ge")),
            })?,
            
            // Type-specialized comparison
            LtInt => self.binary_int_cmp(|x, y| x < y)?,
            LeInt => self.binary_int_cmp(|x, y| x <= y)?,
            GtInt => self.binary_int_cmp(|x, y| x > y)?,
            GeInt => self.binary_int_cmp(|x, y| x >= y)?,
            
            // Boolean
            And => self.binary_op(|a, b| match (a, b) {
                (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x && y)),
                _ => Err(anyhow!("Type error in and")),
            })?,
            
            Or => self.binary_op(|a, b| match (a, b) {
                (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x || y)),
                _ => Err(anyhow!("Type error in or")),
            })?,
            
            Not => {
                let value = self.pop()?;
                match value {
                    Value::Bool(x) => self.push(Value::Bool(!x))?,
                    _ => return Err(anyhow!("Type error in not")),
                }
            }
            
            // Control flow
            Jump => {
                self.call_stack.last_mut().unwrap().ip = instruction.arg as usize;
            }
            
            JumpIf => {
                let condition = self.pop()?;
                if self.is_truthy(&condition) {
                    self.call_stack.last_mut().unwrap().ip = instruction.arg as usize;
                }
            }
            
            JumpIfNot => {
                let condition = self.pop()?;
                if !self.is_truthy(&condition) {
                    self.call_stack.last_mut().unwrap().ip = instruction.arg as usize;
                }
            }
            
            Return => return Ok(VMState::Return),
            
            // Lists
            MakeList => {
                let count = instruction.arg as usize;
                let mut items = Vec::with_capacity(count);
                for _ in 0..count {
                    items.push(self.pop()?);
                }
                items.reverse();
                self.push(Value::List(items))?;
            }
            
            ListLen => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => self.push(Value::Int(items.len() as i64))?,
                    _ => return Err(anyhow!("Type error in list_len")),
                }
            }
            
            ListEmpty => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => self.push(Value::Bool(items.is_empty()))?,
                    _ => return Err(anyhow!("Type error in list_empty")),
                }
            }
            
            // Strings
            StrLen => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::Int(s.len() as i64))?,
                    _ => return Err(anyhow!("Type error in str_len")),
                }
            }
            
            StrConcat => self.binary_op(|a, b| match (a, b) {
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                _ => Err(anyhow!("Type error in str_concat")),
            })?,
            
            StrUpper => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::String(s.to_uppercase()))?,
                    _ => return Err(anyhow!("Type error in str_upper")),
                }
            }
            
            StrLower => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::String(s.to_lowercase()))?,
                    _ => return Err(anyhow!("Type error in str_lower")),
                }
            }
            
            // Special
            Halt => return Ok(VMState::Halt),
            Nop => {}
            
            // TODO: Implement remaining opcodes
            _ => return Err(anyhow!("Unimplemented opcode: {:?}", instruction.opcode)),
        }
        
        Ok(VMState::Continue)
    }
    
    fn push(&mut self, value: Value) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            return Err(anyhow!("Stack overflow"));
        }
        self.stack.push(value);
        Ok(())
    }
    
    fn pop(&mut self) -> Result<Value> {
        self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"))
    }
    
    fn peek(&self, offset: usize) -> Result<&Value> {
        let len = self.stack.len();
        if offset >= len {
            return Err(anyhow!("Stack underflow"));
        }
        Ok(&self.stack[len - 1 - offset])
    }
    
    fn binary_op<F>(&mut self, op: F) -> Result<()>
    where
        F: FnOnce(Value, Value) -> Result<Value>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        let result = op(a, b)?;
        self.push(result)
    }
    
    fn binary_int_op<F>(&mut self, op: F) -> Result<()>
    where
        F: FnOnce(i64, i64) -> Result<i64>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => {
                let result = op(x, y)?;
                self.push(Value::Int(result))
            }
            _ => Err(anyhow!("Type error in binary int op")),
        }
    }
    
    fn binary_int_cmp<F>(&mut self, op: F) -> Result<()>
    where
        F: FnOnce(i64, i64) -> bool,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => {
                let result = op(x, y);
                self.push(Value::Bool(result))
            }
            _ => Err(anyhow!("Type error in binary int comparison")),
        }
    }
    
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::List(x), Value::List(y)) => {
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| self.values_equal(a, b))
            }
            _ => false,
        }
    }
    
    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Bool(b) => *b,
            Value::Int(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::Function { .. } => true,
        }
    }
}

enum VMState {
    Continue,
    Return,
    Halt,
}