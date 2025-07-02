//! High-performance stack-based virtual machine

use crate::bytecode::{Bytecode, Instruction, Opcode, Value};
use anyhow::{anyhow, Result};
use std::collections::HashMap;
use claudelang_effects::{EffectContext, runtime::EffectRuntime};
use std::sync::Arc;
use tokio::sync::{mpsc, oneshot};

const STACK_SIZE: usize = 10_000;

pub struct CallFrame {
    chunk_id: usize,
    ip: usize,
    stack_base: usize,
    env: Vec<Value>, // Captured environment for closures
}

pub struct VM {
    bytecode: Bytecode,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: HashMap<String, Value>,
    trace: bool,
    effect_context: Arc<EffectContext>,
    effect_runtime: Arc<EffectRuntime>,
    // Async support
    promises: HashMap<String, oneshot::Receiver<Result<Value>>>,
    channels: HashMap<String, (mpsc::UnboundedSender<Value>, mpsc::UnboundedReceiver<Value>)>,
    // Mutable cells
    cells: Vec<Value>,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            bytecode,
            stack: Vec::with_capacity(STACK_SIZE),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            trace: false,
            effect_context: Arc::new(EffectContext::default()),
            effect_runtime: Arc::new(EffectRuntime::default()),
            promises: HashMap::new(),
            channels: HashMap::new(),
            cells: Vec::new(),
        }
    }
    
    pub fn enable_trace(&mut self) {
        self.trace = true;
    }
    
    pub fn set_effect_runtime(&mut self, runtime: Arc<EffectRuntime>) {
        self.effect_runtime = runtime;
    }
    
    pub fn set_effect_context(&mut self, context: Arc<EffectContext>) {
        self.effect_context = context;
    }
    
    pub fn run(&mut self) -> Result<Value> {
        self.call_stack.push(CallFrame {
            chunk_id: self.bytecode.main_chunk,
            ip: 0,
            stack_base: 0,
            env: Vec::new(),
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
            PopN => {
                // Pop N values but preserve the top value
                let n = instruction.arg as usize;
                if n > 0 {
                    let top = self.pop()?;
                    for _ in 0..n {
                        self.pop()?;
                    }
                    self.push(top)?;
                }
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
            
            PushConst => {
                let const_idx = instruction.arg as usize;
                let value = self.bytecode.chunks[chunk_id].constants
                    .get(const_idx)
                    .ok_or_else(|| anyhow!("Invalid constant index"))?
                    .clone();
                self.push(value)?;
            }
            
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
            
            
            Return => {
                if self.call_stack.len() <= 1 {
                    return Ok(VMState::Return);
                }
                
                // Get return value
                let return_val = self.pop()?;
                
                // Pop call frame
                let frame = self.call_stack.pop().unwrap();
                
                // Clean up stack (remove arguments)
                self.stack.truncate(frame.stack_base);
                
                // Push return value
                self.push(return_val)?;
            }
            
            // Variables
            Load => {
                let local_idx = instruction.arg as usize;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + local_idx;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            Store => {
                let local_idx = instruction.arg as usize;
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + local_idx;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                self.stack[value_idx] = value;
            }
            
            LoadGlobal => {
                let name_idx = instruction.arg as usize;
                let name = match &self.bytecode.chunks[chunk_id].constants.get(name_idx) {
                    Some(Value::String(s)) => s,
                    _ => return Err(anyhow!("Invalid global name constant")),
                };
                
                // Check if it's a built-in function name
                let value = if let Some(_) = self.builtin_to_opcode(name) {
                    // For built-ins, we'll store them as a special string value
                    Value::String(format!("__builtin__{}", name))
                } else if name == "cons" {
                    // Special built-in for list construction
                    Value::String("__builtin__cons".to_string())
                } else {
                    // Look up in globals
                    self.globals.get(name)
                        .cloned()
                        .unwrap_or(Value::Nil)
                };
                
                self.push(value)?;
            }
            
            StoreGlobal => {
                let name_idx = instruction.arg as usize;
                let name = match &self.bytecode.chunks[chunk_id].constants.get(name_idx) {
                    Some(Value::String(s)) => s.clone(),
                    _ => return Err(anyhow!("Invalid global name constant")),
                };
                
                let value = self.pop()?;
                self.globals.insert(name, value);
            }
            
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
            
            ListHead => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err(anyhow!("Cannot take head of empty list"));
                        }
                        self.push(items[0].clone())?;
                    }
                    _ => return Err(anyhow!("Type error in list_head: expected list")),
                }
            }
            
            ListTail => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err(anyhow!("Cannot take tail of empty list"));
                        }
                        let tail = items[1..].to_vec();
                        self.push(Value::List(tail))?;
                    }
                    _ => return Err(anyhow!("Type error in list_tail: expected list")),
                }
            }
            
            ListCons => {
                let list = self.pop()?;
                let elem = self.pop()?;
                match list {
                    Value::List(mut items) => {
                        items.insert(0, elem);
                        self.push(Value::List(items))?;
                    }
                    _ => return Err(anyhow!("Type error in list_cons: second argument must be a list")),
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
            
            // Effects
            Effect => {
                // Pop arguments, operation name, and effect type
                let arg_count = instruction.arg as usize;
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(self.pop()?);
                }
                args.reverse();
                
                let operation = match self.pop()? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("Effect operation must be a string")),
                };
                
                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("Effect type must be a string")),
                };
                
                // Convert string to EffectType enum
                let effect_type = match effect_type_str.as_str() {
                    "IO" => claudelang_core::ast::EffectType::IO,
                    "State" => claudelang_core::ast::EffectType::State,
                    "Error" => claudelang_core::ast::EffectType::Error,
                    "Time" => claudelang_core::ast::EffectType::Time,
                    "Network" => claudelang_core::ast::EffectType::Network,
                    "Random" => claudelang_core::ast::EffectType::Random,
                    "Dom" => claudelang_core::ast::EffectType::Dom,
                    "Async" => claudelang_core::ast::EffectType::Async,
                    "Concurrent" => claudelang_core::ast::EffectType::Concurrent,
                    "Pure" => claudelang_core::ast::EffectType::Pure,
                    _ => return Err(anyhow!("Unknown effect type: {}", effect_type_str)),
                };
                
                // Convert VM values to core values for effect handlers
                let core_args: Vec<claudelang_core::value::Value> = args.iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect();
                
                // Execute the effect synchronously
                let result = self.effect_context.perform_sync(effect_type, &operation, &core_args)
                    .map_err(|e| anyhow!("Effect error: {}", e))?;
                
                // Convert result back to VM value
                let vm_result = self.core_value_to_vm_value(&result);
                self.push(vm_result)?;
            }
            
            EffectAsync => {
                // Pop arguments, operation name, and effect type
                let arg_count = instruction.arg as usize;
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(self.pop()?);
                }
                args.reverse();
                
                let operation = match self.pop()? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("Effect operation must be a string")),
                };
                
                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("Effect type must be a string")),
                };
                
                // Convert string to EffectType enum
                let effect_type = match effect_type_str.as_str() {
                    "IO" => claudelang_core::ast::EffectType::IO,
                    "State" => claudelang_core::ast::EffectType::State,
                    "Error" => claudelang_core::ast::EffectType::Error,
                    "Time" => claudelang_core::ast::EffectType::Time,
                    "Network" => claudelang_core::ast::EffectType::Network,
                    "Random" => claudelang_core::ast::EffectType::Random,
                    "Dom" => claudelang_core::ast::EffectType::Dom,
                    "Async" => claudelang_core::ast::EffectType::Async,
                    "Concurrent" => claudelang_core::ast::EffectType::Concurrent,
                    "Pure" => claudelang_core::ast::EffectType::Pure,
                    _ => return Err(anyhow!("Unknown effect type: {}", effect_type_str)),
                };
                
                // Generate a promise ID
                let promise_id = uuid::Uuid::new_v4().to_string();
                
                // Convert VM values to core values
                let core_args: Vec<claudelang_core::value::Value> = args.iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect();
                
                // Create a oneshot channel for the result
                let (tx, rx) = oneshot::channel();
                
                // Store the receiver
                self.promises.insert(promise_id.clone(), rx);
                
                // Create the async task
                let effect_context = self.effect_context.clone();
                let operation = operation.clone();
                let runtime = self.effect_runtime.clone();
                
                let future = async move {
                    let result = effect_context.perform_async(effect_type, &operation, &core_args).await
                        .map_err(|e| anyhow!("Async effect error: {}", e));
                    
                    // Convert result back to VM value
                    let vm_result = match result {
                        Ok(_core_value) => {
                            // We need to convert core value to VM value
                            // This is tricky without access to self
                            // For now, just send a placeholder
                            Ok(Value::String("async result".to_string()))
                        }
                        Err(e) => Err(e),
                    };
                    
                    let _ = tx.send(vm_result);
                };
                
                // Spawn the task on the runtime
                runtime.spawn(future);
                
                // Return the promise ID
                self.push(Value::Promise(promise_id))?;
            }
            
            Await => {
                // Pop promise
                let promise_id = match self.pop()? {
                    Value::Promise(id) => id,
                    _ => return Err(anyhow!("Await requires a promise")),
                };
                
                // Check if we have this promise
                if let Some(mut rx) = self.promises.remove(&promise_id) {
                    // Try to receive the result non-blocking
                    match rx.try_recv() {
                        Ok(Ok(value)) => self.push(value)?,
                        Ok(Err(e)) => return Err(e),
                        Err(oneshot::error::TryRecvError::Empty) => {
                            // Not ready yet, put it back and return the promise
                            self.promises.insert(promise_id.clone(), rx);
                            self.push(Value::Promise(promise_id))?;
                        }
                        Err(oneshot::error::TryRecvError::Closed) => {
                            return Err(anyhow!("Promise channel closed"));
                        }
                    }
                } else {
                    return Err(anyhow!("Unknown promise"));
                }
            }
            
            Spawn => {
                // Pop function value
                let _function = self.pop()?;
                
                // For now, we'll create a placeholder goroutine
                // In a real implementation, this would spawn a new VM instance
                let goroutine_id = uuid::Uuid::new_v4().to_string();
                
                // Return a promise that represents the goroutine
                self.push(Value::Promise(goroutine_id))?;
            }
            
            Channel => {
                // Create a new channel
                let channel_id = uuid::Uuid::new_v4().to_string();
                let (tx, rx) = mpsc::unbounded_channel();
                self.channels.insert(channel_id.clone(), (tx, rx));
                self.push(Value::Channel(channel_id))?;
            }
            
            Send => {
                // Pop value and channel
                let value = self.pop()?;
                let channel = match self.pop()? {
                    Value::Channel(id) => id,
                    _ => return Err(anyhow!("Send requires a channel")),
                };
                
                // Get the channel sender
                if let Some((tx, _)) = self.channels.get(&channel) {
                    tx.send(value)
                        .map_err(|_| anyhow!("Channel closed"))?;
                    self.push(Value::Nil)?;
                } else {
                    return Err(anyhow!("Unknown channel"));
                }
            }
            
            Receive => {
                // Pop channel
                let channel = match self.pop()? {
                    Value::Channel(id) => id,
                    _ => return Err(anyhow!("Receive requires a channel")),
                };
                
                // Try to receive non-blocking
                if let Some((_, rx)) = self.channels.get_mut(&channel) {
                    match rx.try_recv() {
                        Ok(value) => self.push(value)?,
                        Err(mpsc::error::TryRecvError::Empty) => {
                            // No value available, push nil
                            self.push(Value::Nil)?;
                        }
                        Err(mpsc::error::TryRecvError::Disconnected) => {
                            return Err(anyhow!("Channel disconnected"));
                        }
                    }
                } else {
                    return Err(anyhow!("Unknown channel"));
                }
            }
            
            // Functions
            MakeFunc => {
                let chunk_id = instruction.arg as usize;
                // Capture current environment (for closures)
                let env = Vec::new(); // TODO: Capture free variables
                let func = Value::Function { chunk_id, env };
                self.push(func)?;
            }
            
            MakeClosure => {
                // Unpack chunk_id and capture count
                let packed = instruction.arg;
                let chunk_id = (packed >> 16) as usize;
                let capture_count = (packed & 0xFFFF) as usize;
                
                // Pop captured values from stack
                let mut env = Vec::with_capacity(capture_count);
                for _ in 0..capture_count {
                    env.push(self.pop()?);
                }
                env.reverse(); // Restore original order
                
                let func = Value::Function { chunk_id, env };
                self.push(func)?;
            }
            
            LoadCaptured => {
                let capture_idx = instruction.arg as usize;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                
                // Load value from captured environment
                if capture_idx >= frame.env.len() {
                    return Err(anyhow!("Invalid capture index: {}", capture_idx));
                }
                
                let value = frame.env[capture_idx].clone();
                self.push(value)?;
            }
            
            Call => {
                let arg_count = instruction.arg as usize;
                
                // Pop function first (it's on top of stack due to our compilation order)
                let func = self.pop()?;
                
                // Pop arguments in reverse order
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(self.pop()?);
                }
                args.reverse();
                
                
                match func {
                    Value::Function { chunk_id, env } => {
                        // Current IP was already incremented by main loop,
                        // so it's already pointing to the next instruction after Call
                        
                        // Push arguments back onto stack in correct order
                        for arg in args {
                            self.push(arg)?;
                        }
                        
                        // Push new frame
                        self.call_stack.push(CallFrame {
                            chunk_id,
                            ip: 0,
                            stack_base: self.stack.len() - arg_count,
                            env,
                        });
                    }
                    _ => return Err(anyhow!("Cannot call non-function value")),
                }
            }
            
            // Mutable cells
            MakeCell => {
                let initial_value = self.pop()?;
                let cell_idx = self.cells.len();
                self.cells.push(initial_value);
                self.push(Value::Cell(cell_idx))?;
            }
            
            CellGet => {
                let cell = self.pop()?;
                match cell {
                    Value::Cell(idx) => {
                        if idx < self.cells.len() {
                            let value = self.cells[idx].clone();
                            self.push(value)?;
                        } else {
                            return Err(anyhow!("Invalid cell index"));
                        }
                    }
                    _ => return Err(anyhow!("CellGet requires a cell")),
                }
            }
            
            CellSet => {
                let value = self.pop()?;
                let cell = self.pop()?;
                match cell {
                    Value::Cell(idx) => {
                        if idx < self.cells.len() {
                            self.cells[idx] = value;
                            self.push(Value::Nil)?; // CellSet returns nil
                        } else {
                            return Err(anyhow!("Invalid cell index"));
                        }
                    }
                    _ => return Err(anyhow!("CellSet requires a cell")),
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
            Value::Map(m) => !m.is_empty(),
            Value::Function { .. } => true,
            Value::Promise(_) => true,
            Value::Channel(_) => true,
            Value::Cell(_) => true,
        }
    }
    
    fn builtin_to_opcode(&self, name: &str) -> Option<Opcode> {
        match name {
            "+" => Some(Opcode::Add),
            "-" => Some(Opcode::Sub),
            "*" => Some(Opcode::Mul),
            "/" => Some(Opcode::Div),
            "%" => Some(Opcode::Mod),
            "=" | "==" => Some(Opcode::Eq),
            "!=" | "<>" => Some(Opcode::Ne),
            "<" => Some(Opcode::Lt),
            "<=" => Some(Opcode::Le),
            ">" => Some(Opcode::Gt),
            ">=" => Some(Opcode::Ge),
            "and" => Some(Opcode::And),
            "or" => Some(Opcode::Or),
            "not" => Some(Opcode::Not),
            "list-len" | "length" => Some(Opcode::ListLen),
            "list-empty?" | "empty?" => Some(Opcode::ListEmpty),
            "str-len" | "string-length" => Some(Opcode::StrLen),
            "str-concat" | "string-append" => Some(Opcode::StrConcat),
            "str-upper" | "string-upcase" => Some(Opcode::StrUpper),
            "str-lower" | "string-downcase" => Some(Opcode::StrLower),
            _ => None,
        }
    }
    
    fn vm_value_to_core_value(&self, value: &Value) -> claudelang_core::value::Value {
        match value {
            Value::Nil => claudelang_core::value::Value::Nil,
            Value::Bool(b) => claudelang_core::value::Value::Boolean(*b),
            Value::Int(i) => claudelang_core::value::Value::Integer(*i),
            Value::Float(f) => claudelang_core::value::Value::Float(*f),
            Value::String(s) => claudelang_core::value::Value::String(s.clone()),
            Value::List(items) => {
                claudelang_core::value::Value::List(
                    items.iter().map(|v| self.vm_value_to_core_value(v)).collect()
                )
            }
            Value::Map(map) => {
                let mut core_map = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    core_map.insert(k.clone(), self.vm_value_to_core_value(v));
                }
                claudelang_core::value::Value::Map(core_map)
            }
            Value::Function { .. } => {
                // Functions can't be directly converted, return a placeholder
                claudelang_core::value::Value::String("<function>".to_string())
            }
            Value::Promise(id) => {
                claudelang_core::value::Value::String(format!("<promise:{}>", id))
            }
            Value::Channel(id) => {
                claudelang_core::value::Value::String(format!("<channel:{}>", id))
            }
            Value::Cell(idx) => {
                claudelang_core::value::Value::String(format!("<cell:{}>", idx))
            }
        }
    }
    
    fn core_value_to_vm_value(&self, value: &claudelang_core::value::Value) -> Value {
        match value {
            claudelang_core::value::Value::Nil => Value::Nil,
            claudelang_core::value::Value::Boolean(b) => Value::Bool(*b),
            claudelang_core::value::Value::Integer(i) => Value::Int(*i),
            claudelang_core::value::Value::Float(f) => Value::Float(*f),
            claudelang_core::value::Value::String(s) => Value::String(s.clone()),
            claudelang_core::value::Value::List(items) => {
                Value::List(
                    items.iter().map(|v| self.core_value_to_vm_value(v)).collect()
                )
            }
            claudelang_core::value::Value::Map(map) => {
                let mut vm_map = std::collections::HashMap::new();
                for (k, v) in map.iter() {
                    vm_map.insert(k.clone(), self.core_value_to_vm_value(v));
                }
                Value::Map(vm_map)
            }
            claudelang_core::value::Value::Function(_) => {
                // Functions can't be directly converted, return a placeholder
                Value::String("<function>".to_string())
            }
            claudelang_core::value::Value::Error(msg) => {
                // Convert errors to strings
                Value::String(format!("Error: {}", msg))
            }
        }
    }
}

enum VMState {
    Continue,
    Return,
    Halt,
}