//! High-performance stack-based virtual machine

use crate::bytecode::{Bytecode, Instruction, Opcode, Value};
use crate::debug::{VMDebugEvent, DebugConfig, StepMode};
use crate::safety::{IdGenerator, PromiseId, ChannelId, ResourceLimits, checked_ops};
use crate::error::{StackTrace, StackFrame};
use anyhow::{anyhow, Result};
use rustc_hash::FxHashMap;
use claudelang_effects::{EffectContext, runtime::EffectRuntime};
use claudelang_stdlib::{StdlibRegistry, init_stdlib};
use claudelang_stdlib::value::Value as StdlibValue;
use claudelang_modules::{ModuleLoader, ModuleResolver};
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
    globals: FxHashMap<String, Value>,
    trace: bool,
    effect_context: Arc<EffectContext>,
    effect_runtime: Arc<EffectRuntime>,
    // Async support with typed IDs
    id_generator: IdGenerator,
    promises: FxHashMap<PromiseId, oneshot::Receiver<Result<Value>>>,
    channels: FxHashMap<ChannelId, (mpsc::Sender<Value>, mpsc::Receiver<Value>)>,
    // Mutable cells
    cells: Vec<Value>,
    // Standard library
    stdlib: StdlibRegistry,
    // Module system
    module_loader: ModuleLoader,
    #[allow(dead_code)]
    module_resolver: ModuleResolver,
    loaded_modules: FxHashMap<String, Value>, // Cache of loaded modules
    current_module: Option<String>, // Name of currently executing module
    module_stack: Vec<String>, // Stack of module names for nested module execution
    // Debug support
    debug_config: DebugConfig,
    instruction_count: u64,
    // Resource limits
    resource_limits: ResourceLimits,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self {
            bytecode,
            stack: Vec::with_capacity(STACK_SIZE),
            call_stack: Vec::new(),
            globals: FxHashMap::default(),
            trace: false,
            effect_context: Arc::new(EffectContext::default()),
            effect_runtime: Arc::new(EffectRuntime::default()),
            id_generator: IdGenerator::new(),
            promises: FxHashMap::default(),
            channels: FxHashMap::default(),
            cells: Vec::new(),
            stdlib: init_stdlib(),
            module_loader: ModuleLoader::new(claudelang_modules::ModuleConfig::default()),
            module_resolver: ModuleResolver::new(ModuleLoader::new(claudelang_modules::ModuleConfig::default())),
            loaded_modules: FxHashMap::default(),
            current_module: None,
            module_stack: Vec::new(),
            debug_config: DebugConfig::default(),
            instruction_count: 0,
            resource_limits: ResourceLimits::default(),
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
    
    pub fn set_stdlib_registry(&mut self, registry: StdlibRegistry) {
        self.stdlib = registry;
    }
    
    pub fn set_module_loader(&mut self, loader: ModuleLoader) {
        // Create a new resolver with a fresh loader instance
        let config = claudelang_modules::ModuleConfig::default();
        self.module_resolver = ModuleResolver::new(ModuleLoader::new(config));
        self.module_loader = loader;
    }
    
    pub fn set_debug_config(&mut self, config: DebugConfig) {
        self.debug_config = config;
    }
    
    pub fn get_debug_config(&self) -> &DebugConfig {
        &self.debug_config
    }
    
    pub fn get_debug_config_mut(&mut self) -> &mut DebugConfig {
        &mut self.debug_config
    }
    
    /// Get current VM state for debugging
    pub fn get_stack(&self) -> &[Value] {
        &self.stack
    }
    
    pub fn get_globals(&self) -> &FxHashMap<String, Value> {
        &self.globals
    }
    
    pub fn get_call_stack_depth(&self) -> usize {
        self.call_stack.len()
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
            
            // Check for breakpoints
            if self.debug_config.enabled && self.debug_config.should_break(ip) {
                self.debug_config.send_event(VMDebugEvent::Breakpoint { pc: ip });
                self.debug_config.step_mode = StepMode::Step;
            }
            
            // Send pre-instruction debug event
            if self.debug_config.enabled {
                self.debug_config.send_event(VMDebugEvent::PreInstruction {
                    pc: ip,
                    instruction: instruction.clone(),
                    stack_size: self.stack.len(),
                });
            }
            
            if self.trace {
                eprintln!("Stack: {:?}", self.stack);
                eprintln!("Executing: {:?} at {}", instruction.opcode, ip);
            }
            
            // Increment IP before execution (may be modified by jumps)
            self.call_stack.last_mut().unwrap().ip += 1;
            self.instruction_count += 1;
            
            match self.execute_instruction(&instruction, chunk_id)? {
                VMState::Continue => {
                    // Send post-instruction debug event
                    if self.debug_config.enabled {
                        let stack_top = self.stack.last().cloned();
                        self.debug_config.send_event(VMDebugEvent::PostInstruction {
                            pc: ip,
                            stack_size: self.stack.len(),
                            stack_top,
                        });
                    }
                }
                VMState::Return => {
                    if self.call_stack.len() == 1 {
                        // Main function returning
                        let result = self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                        if self.debug_config.enabled {
                            self.debug_config.send_event(VMDebugEvent::FunctionReturn {
                                value: result.clone(),
                                call_depth: self.call_stack.len(),
                            });
                        }
                        return Ok(result);
                    }
                    // Pop call frame and continue
                    self.call_stack.pop();
                    if self.debug_config.enabled {
                        let return_value = self.stack.last().cloned().unwrap_or(Value::Nil);
                        self.debug_config.send_event(VMDebugEvent::FunctionReturn {
                            value: return_value,
                            call_depth: self.call_stack.len(),
                        });
                    }
                }
                VMState::Halt => {
                    return self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"));
                }
            }
            
            // Handle step mode
            if self.debug_config.enabled {
                match self.debug_config.step_mode {
                    StepMode::Step => {
                        // Pause after each instruction
                        self.debug_config.step_mode = StepMode::Run;
                        // In a real implementation, we'd wait for a continue signal here
                    }
                    StepMode::StepOver => {
                        // Continue until we're back at the same call depth
                        // Implementation would track the initial call depth
                    }
                    StepMode::StepOut => {
                        // Continue until we return from current function
                        // Implementation would track when we exit current frame
                    }
                    StepMode::Run => {
                        // Continue normally
                    }
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
                (Value::Int(x), Value::Int(y)) => checked_ops::add_i64(x, y).map(Value::Int),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x + y)),
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                _ => Err(anyhow!("Type error in add")),
            })?,
            
            Sub => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::sub_i64(x, y).map(Value::Int),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x - y)),
                _ => Err(anyhow!("Type error in sub")),
            })?,
            
            Mul => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::mul_i64(x, y).map(Value::Int),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x * y)),
                _ => Err(anyhow!("Type error in mul")),
            })?,
            
            Div => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::div_i64(x, y).map(Value::Int),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
                _ => Err(anyhow!("Type error in div")),
            })?,
            
            Mod => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::mod_i64(x, y).map(Value::Int),
                _ => Err(anyhow!("Type error in mod")),
            })?,
            
            Neg => {
                let value = self.pop()?;
                match value {
                    Value::Int(x) => {
                        let negated = checked_ops::neg_i64(x)?;
                        self.push(Value::Int(negated))?
                    }
                    Value::Float(x) => self.push(Value::Float(-x))?,
                    _ => return Err(anyhow!("Type error in neg")),
                }
            }
            
            // Type-specialized arithmetic
            AddInt => self.binary_int_op(|x, y| checked_ops::add_i64(x, y))?,
            SubInt => self.binary_int_op(|x, y| checked_ops::sub_i64(x, y))?,
            MulInt => self.binary_int_op(|x, y| checked_ops::mul_i64(x, y))?,
            DivInt => self.binary_int_op(|x, y| checked_ops::div_i64(x, y))?,
            
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
            
            // Fast local variable access opcodes
            LoadLocal0 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal1 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 1;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal2 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 2;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal3 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 3;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            StoreLocal0 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal1 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 1;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal2 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 2;
                
                if value_idx >= self.stack.len() {
                    return Err(anyhow!("Invalid local variable index"));
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal3 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| anyhow!("No active call frame"))?;
                let value_idx = frame.stack_base + 3;
                
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
                } else if self.stdlib.contains(name) {
                    // Standard library function
                    Value::String(format!("__stdlib__{}", name))
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
                let promise_id = self.id_generator.next_promise_id();
                
                // Convert VM values to core values
                let core_args: Vec<claudelang_core::value::Value> = args.iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect();
                
                // Create a oneshot channel for the result
                let (tx, rx) = oneshot::channel();
                
                // Store the receiver
                self.promises.insert(promise_id, rx);
                
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
                
                // Return the promise ID as string for compatibility
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
                            self.promises.insert(promise_id, rx);
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
                let promise_id = self.id_generator.next_promise_id();
                
                // Return a promise that represents the goroutine
                self.push(Value::Promise(promise_id))?;
            }
            
            Channel => {
                // Check resource limit
                if self.channels.len() >= self.resource_limits.max_channels {
                    return Err(anyhow!("Channel limit exceeded: {}", self.resource_limits.max_channels));
                }
                
                // Create a new channel with bounded capacity
                let channel_id = self.id_generator.next_channel_id();
                let (tx, rx) = mpsc::channel(self.resource_limits.channel_buffer_size);
                self.channels.insert(channel_id, (tx, rx));
                self.push(Value::Channel(channel_id))?;
            }
            
            Send => {
                // Pop value and channel
                let value = self.pop()?;
                let channel_id = match self.pop()? {
                    Value::Channel(id) => id,
                    _ => return Err(anyhow!("Send requires a channel")),
                };
                
                // Get the channel sender
                if let Some((tx, _)) = self.channels.get(&channel_id) {
                    tx.try_send(value)
                        .map_err(|e| match e {
                            mpsc::error::TrySendError::Full(_) => anyhow!("Channel buffer full"),
                            mpsc::error::TrySendError::Closed(_) => anyhow!("Channel closed"),
                        })?;
                    self.push(Value::Nil)?;
                } else {
                    return Err(anyhow!("Unknown channel: {}", channel_id));
                }
            }
            
            Receive => {
                // Pop channel
                let channel_id = match self.pop()? {
                    Value::Channel(id) => id,
                    _ => return Err(anyhow!("Receive requires a channel")),
                };
                
                // Try to receive non-blocking
                if let Some((_, rx)) = self.channels.get_mut(&channel_id) {
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
                    return Err(anyhow!("Unknown channel: {}", channel_id));
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
                
                
                match &func {
                    Value::Function { chunk_id, env } => {
                        // Current IP was already incremented by main loop,
                        // so it's already pointing to the next instruction after Call
                        
                        // Push arguments back onto stack in correct order
                        for arg in args {
                            self.push(arg)?;
                        }
                        
                        // Push new frame
                        self.call_stack.push(CallFrame {
                            chunk_id: *chunk_id,
                            ip: 0,
                            stack_base: self.stack.len() - arg_count,
                            env: env.clone(),
                        });
                    }
                    Value::String(s) if s.starts_with("__builtin__") => {
                        // Handle built-in function calls
                        let builtin_name = &s[11..]; // Remove "__builtin__" prefix
                        
                        // Special handling for cons which needs different argument order
                        if builtin_name == "cons" {
                            if args.len() != 2 {
                                return Err(anyhow!("cons requires exactly 2 arguments"));
                            }
                            match &args[1] {
                                Value::List(items) => {
                                    let mut new_list = vec![args[0].clone()];
                                    new_list.extend(items.iter().cloned());
                                    self.push(Value::List(new_list))?;
                                }
                                _ => return Err(anyhow!("cons: second argument must be a list")),
                            }
                        } else {
                            // For other built-ins, generate appropriate instructions
                            // This is a simplified approach - in a full implementation,
                            // we might want to directly execute the operations
                            return Err(anyhow!("Built-in function {} should be optimized by compiler", builtin_name));
                        }
                    }
                    Value::String(s) if s.starts_with("__stdlib__") => {
                        // Handle stdlib function calls
                        let func_name = &s[10..]; // Remove "__stdlib__" prefix
                        
                        // Check if it's a higher-order function that needs special handling
                        match func_name {
                            "map" | "filter" | "fold" => {
                                // Use the stdlib bridge for higher-order functions
                                use crate::stdlib_bridge::VMStdlibExt;
                                let stdlib_args: Vec<StdlibValue> = args.iter()
                                    .map(|v| self.vm_value_to_stdlib_value(v))
                                    .collect();
                                let result = self.call_higher_order_stdlib(func_name, &stdlib_args)?;
                                let vm_result = self.stdlib_value_to_vm_value(&result);
                                self.push(vm_result)?;
                            }
                            _ => {
                                // Regular stdlib function
                                if let Some(stdlib_func) = self.stdlib.get(func_name) {
                                    // Convert VM values to stdlib values
                                    let stdlib_args: Vec<StdlibValue> = args.iter()
                                        .map(|v| self.vm_value_to_stdlib_value(v))
                                        .collect();
                                    
                                    // Call the stdlib function
                                    let stdlib_result = stdlib_func.call(&stdlib_args)?;
                                    
                                    // Convert result back to VM value
                                    let vm_result = self.stdlib_value_to_vm_value(&stdlib_result);
                                    self.push(vm_result)?;
                                } else {
                                    return Err(anyhow!("Unknown stdlib function: {}", func_name));
                                }
                            }
                        }
                    }
                    _ => return Err(anyhow!("Cannot call non-function value: {:?}", func)),
                }
            }
            
            // Mutable cells
            MakeCell => {
                // Check resource limit
                if self.cells.len() >= self.resource_limits.max_cells {
                    return Err(anyhow!("Cell limit exceeded: {}", self.resource_limits.max_cells));
                }
                
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
            
            // Tagged values
            MakeTagged => {
                let arity = instruction.arg as usize;
                
                // Pop all values first
                let mut values = Vec::with_capacity(arity);
                for _ in 0..arity {
                    values.push(self.pop()?);
                }
                values.reverse();
                
                // Then pop the tag
                let tag_val = self.pop()?;
                let tag = match tag_val {
                    Value::String(s) => s,
                    _ => return Err(anyhow!("MakeTagged requires string tag, got {:?}", tag_val)),
                };
                
                self.push(Value::Tagged { tag, values })?;
            }
            
            GetTag => {
                let value = self.pop()?;
                match value {
                    Value::Tagged { tag, .. } => {
                        self.push(Value::String(tag))?;
                    }
                    _ => return Err(anyhow!("GetTag requires tagged value")),
                }
            }
            
            GetTaggedField => {
                let field_idx = instruction.arg as usize;
                let value = self.pop()?;
                match value {
                    Value::Tagged { values, .. } => {
                        if field_idx < values.len() {
                            self.push(values[field_idx].clone())?;
                        } else {
                            return Err(anyhow!("Tagged field index out of bounds"));
                        }
                    }
                    _ => return Err(anyhow!("GetTaggedField requires tagged value")),
                }
            }
            
            IsTagged => {
                let expected_tag_idx = instruction.arg as usize;
                let expected_tag = match self.bytecode.chunks[chunk_id].constants.get(expected_tag_idx) {
                    Some(Value::String(s)) => s,
                    _ => return Err(anyhow!("Invalid tag constant")),
                };
                
                let value = self.peek(0)?;
                let is_match = match value {
                    Value::Tagged { tag, .. } => tag == expected_tag,
                    _ => false,
                };
                
                self.push(Value::Bool(is_match))?;
            }
            
            // Environment management (reserved for future use)
            MakeEnv => {
                // MakeEnv could be used for creating dynamic environments
                // or implementing with-environment constructs
                // Currently environments are managed through closures
                // This is a no-op for now
            }
            
            PopEnv => {
                // PopEnv would restore a previous environment
                // Currently not used as environments are handled via closures
                // This is a no-op for now
            }
            
            // Module operations
            LoadModule => {
                let module_name = self.get_constant_string(instruction.arg)?;
                self.load_module(&module_name)?;
            }
            
            ImportBinding => {
                // arg encodes module_idx (high 16 bits) and binding_idx (low 16 bits)
                let module_idx = (instruction.arg >> 16) as usize;
                let binding_idx = (instruction.arg & 0xFFFF) as usize;
                
                let module_name = self.get_constant_string(module_idx as u32)?;
                let binding_name = self.get_constant_string(binding_idx as u32)?;
                
                if let Some(Value::Module { exports, .. }) = self.loaded_modules.get(&module_name) {
                    if let Some(value) = exports.get(&binding_name) {
                        self.push(value.clone())?;
                    } else {
                        return Err(anyhow!("Module '{}' does not export '{}'", module_name, binding_name));
                    }
                } else {
                    return Err(anyhow!("Module '{}' not loaded", module_name));
                }
            }
            
            LoadQualified => {
                // arg encodes module_idx (high 16 bits) and var_idx (low 16 bits)
                let module_idx = (instruction.arg >> 16) as usize;
                let var_idx = (instruction.arg & 0xFFFF) as usize;
                
                let module_name = self.get_constant_string(module_idx as u32)?;
                let var_name = self.get_constant_string(var_idx as u32)?;
                
                if let Some(Value::Module { exports, .. }) = self.loaded_modules.get(&module_name) {
                    if let Some(value) = exports.get(&var_name) {
                        self.push(value.clone())?;
                    } else {
                        return Err(anyhow!("Module '{}' does not export '{}'", module_name, var_name));
                    }
                } else {
                    return Err(anyhow!("Module '{}' not found", module_name));
                }
            }
            
            BeginModule => {
                let module_name = self.get_constant_string(instruction.arg)?;
                self.module_stack.push(self.current_module.clone().unwrap_or_default());
                self.current_module = Some(module_name);
            }
            
            EndModule => {
                if let Some(prev_module) = self.module_stack.pop() {
                    self.current_module = if prev_module.is_empty() { None } else { Some(prev_module) };
                }
            }
            
            ExportBinding => {
                let binding_name = self.get_constant_string(instruction.arg)?;
                if let Some(current_module_name) = &self.current_module {
                    let value = self.peek(0)?.clone();
                    
                    // Get or create the module in loaded_modules
                    let module = self.loaded_modules.entry(current_module_name.clone())
                        .or_insert_with(|| Value::Module {
                            name: current_module_name.clone(),
                            exports: FxHashMap::default(),
                        });
                    
                    // Add the export
                    if let Value::Module { exports, .. } = module {
                        exports.insert(binding_name, value);
                    }
                } else {
                    return Err(anyhow!("Cannot export outside of module context"));
                }
            }
            
            // Special
            Halt => return Ok(VMState::Halt),
            Nop => {}
            
            // All opcodes are handled exhaustively
        }
        
        Ok(VMState::Continue)
    }
    
    pub fn push(&mut self, value: Value) -> Result<()> {
        if self.stack.len() >= STACK_SIZE {
            return Err(anyhow!("Stack overflow"));
        }
        
        // Send debug event
        if self.debug_config.enabled {
            self.debug_config.send_event(VMDebugEvent::StackPush {
                value: value.clone(),
            });
        }
        
        self.stack.push(value);
        Ok(())
    }
    
    pub fn pop(&mut self) -> Result<Value> {
        let value = self.stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
        
        // Send debug event
        if self.debug_config.enabled {
            self.debug_config.send_event(VMDebugEvent::StackPop {
                value: value.clone(),
            });
        }
        
        Ok(value)
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
            (Value::Tagged { tag: tag1, values: vals1 }, Value::Tagged { tag: tag2, values: vals2 }) => {
                tag1 == tag2 && vals1.len() == vals2.len() && 
                vals1.iter().zip(vals2).all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Module { name: n1, .. }, Value::Module { name: n2, .. }) => n1 == n2,
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
            Value::Tagged { .. } => true,
            Value::Module { .. } => true,
        }
    }
    
    pub fn vm_value_to_stdlib_value(&self, value: &Value) -> StdlibValue {
        match value {
            Value::Nil => StdlibValue::Nil,
            Value::Bool(b) => StdlibValue::Bool(*b),
            Value::Int(i) => StdlibValue::Int(*i),
            Value::Float(f) => StdlibValue::Float(*f),
            Value::String(s) => StdlibValue::String(s.clone()),
            Value::List(items) => {
                StdlibValue::List(
                    items.iter().map(|v| self.vm_value_to_stdlib_value(v)).collect()
                )
            }
            Value::Map(map) => {
                let mut stdlib_map = FxHashMap::default();
                for (k, v) in map {
                    stdlib_map.insert(k.clone(), self.vm_value_to_stdlib_value(v));
                }
                StdlibValue::Map(stdlib_map)
            }
            Value::Function { chunk_id, env } => {
                StdlibValue::Function {
                    chunk_id: *chunk_id,
                    env: env.iter().map(|v| self.vm_value_to_stdlib_value(v)).collect(),
                }
            }
            Value::Promise(id) => StdlibValue::Promise(id.0),
            Value::Channel(id) => StdlibValue::Channel(id.0),
            Value::Cell(idx) => StdlibValue::Cell(*idx),
            Value::Tagged { tag, values } => {
                StdlibValue::Tagged {
                    tag: tag.clone(),
                    values: values.iter().map(|v| self.vm_value_to_stdlib_value(v)).collect(),
                }
            }
            Value::Module { name, .. } => {
                // No direct stdlib equivalent, use string representation
                StdlibValue::String(format!("<module {}>", name))
            }
        }
    }
    
    pub fn stdlib_value_to_vm_value(&self, value: &StdlibValue) -> Value {
        match value {
            StdlibValue::Nil => Value::Nil,
            StdlibValue::Bool(b) => Value::Bool(*b),
            StdlibValue::Int(i) => Value::Int(*i),
            StdlibValue::Float(f) => Value::Float(*f),
            StdlibValue::String(s) => Value::String(s.clone()),
            StdlibValue::List(items) => {
                Value::List(
                    items.iter().map(|v| self.stdlib_value_to_vm_value(v)).collect()
                )
            }
            StdlibValue::Map(map) => {
                let mut vm_map = FxHashMap::default();
                for (k, v) in map {
                    vm_map.insert(k.clone(), self.stdlib_value_to_vm_value(v));
                }
                Value::Map(vm_map)
            }
            StdlibValue::Function { chunk_id, env } => {
                Value::Function {
                    chunk_id: *chunk_id,
                    env: env.iter().map(|v| self.stdlib_value_to_vm_value(v)).collect(),
                }
            }
            StdlibValue::Promise(id) => Value::Promise(PromiseId(*id)),
            StdlibValue::Channel(id) => Value::Channel(ChannelId(*id)),
            StdlibValue::Cell(idx) => Value::Cell(*idx),
            StdlibValue::Tagged { tag, values } => {
                Value::Tagged {
                    tag: tag.clone(),
                    values: values.iter().map(|v| self.stdlib_value_to_vm_value(v)).collect(),
                }
            }
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
    
    /// Call a value as a function (used by stdlib bridge)
    pub fn call_value(&mut self, arg_count: usize) -> Result<()> {
        // Pop arguments
        let mut args = Vec::with_capacity(arg_count);
        for _ in 0..arg_count {
            args.push(self.pop()?);
        }
        args.reverse();
        
        // Pop function
        let func = self.pop()?;
        
        match func {
            Value::Function { chunk_id, env } => {
                // Save current env to stack if needed
                let stack_base = self.stack.len();
                
                // Push arguments back
                for arg in args {
                    self.push(arg)?;
                }
                
                // Create new call frame
                self.call_stack.push(CallFrame {
                    chunk_id,
                    ip: 0,
                    stack_base,
                    env,
                });
                
                // Continue execution until this call returns
                let initial_call_depth = self.call_stack.len();
                while self.call_stack.len() >= initial_call_depth {
                    let frame = self.call_stack.last().ok_or_else(|| anyhow!("Call stack underflow"))?;
                    let chunk_id = frame.chunk_id;
                    let ip = frame.ip;
                    
                    if ip >= self.bytecode.chunks[chunk_id].instructions.len() {
                        return Err(anyhow!("Instruction pointer out of bounds"));
                    }
                    
                    let instruction = self.bytecode.chunks[chunk_id].instructions[ip].clone();
                    self.call_stack.last_mut().unwrap().ip += 1;
                    
                    match self.execute_instruction(&instruction, chunk_id)? {
                        VMState::Continue => {}
                        VMState::Return => {
                            if self.call_stack.len() == initial_call_depth {
                                // This is our call returning
                                self.call_stack.pop();
                                break;
                            } else {
                                // Inner call returning
                                self.call_stack.pop();
                            }
                        }
                        VMState::Halt => {
                            return Err(anyhow!("Unexpected halt in function call"));
                        }
                    }
                }
                
                Ok(())
            }
            _ => Err(anyhow!("Cannot call non-function value: {:?}", func)),
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
                let mut core_map = FxHashMap::default();
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
            Value::Tagged { tag, values } => {
                // Convert to a map representation for now
                let mut map = FxHashMap::default();
                map.insert("__tag__".to_string(), claudelang_core::value::Value::String(tag.clone()));
                map.insert("__values__".to_string(), claudelang_core::value::Value::List(
                    values.iter().map(|v| self.vm_value_to_core_value(v)).collect()
                ));
                claudelang_core::value::Value::Map(map)
            }
            Value::Module { name, exports } => {
                // Convert to a map representation  
                let mut map = FxHashMap::default();
                map.insert("__module__".to_string(), claudelang_core::value::Value::String(name.clone()));
                let mut export_map = FxHashMap::default();
                for (key, val) in exports {
                    export_map.insert(key.clone(), self.vm_value_to_core_value(val));
                }
                map.insert("__exports__".to_string(), claudelang_core::value::Value::Map(export_map));
                claudelang_core::value::Value::Map(map)
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
                let mut vm_map = FxHashMap::default();
                for (k, v) in map.iter() {
                    vm_map.insert(k.clone(), self.core_value_to_vm_value(v));
                }
                Value::Map(vm_map)
            }
            claudelang_core::value::Value::Procedure(_) => {
                // Functions can't be directly converted, return a placeholder
                Value::String("<function>".to_string())
            }
            claudelang_core::value::Value::NativeFunction { name, .. } => {
                // Native functions can't be directly converted, return a placeholder
                Value::String(format!("<native-function: {}>", name))
            }
            claudelang_core::value::Value::Tagged { tag, values } => {
                Value::Tagged {
                    tag: tag.clone(),
                    values: values.iter().map(|v| self.core_value_to_vm_value(v)).collect(),
                }
            }
            claudelang_core::value::Value::Symbol(s) => {
                // Convert symbols to strings in VM representation
                Value::String(s.clone())
            }
            claudelang_core::value::Value::Vector(items) => {
                // Convert vectors to lists in VM representation
                Value::List(
                    items.iter().map(|v| self.core_value_to_vm_value(v)).collect()
                )
            }
        }
    }
    
    // Module system helper methods
    fn get_constant_string(&self, idx: u32) -> Result<String> {
        let value = self.bytecode.chunks[self.current_chunk()].constants
            .get(idx as usize)
            .ok_or_else(|| anyhow!("Invalid constant index: {}", idx))?;
            
        match value {
            Value::String(s) => Ok(s.clone()),
            _ => Err(anyhow!("Expected string constant at index {}", idx)),
        }
    }
    
    fn load_module(&mut self, module_name: &str) -> Result<()> {
        // Check if already loaded
        if self.loaded_modules.contains_key(module_name) {
            return Ok(());
        }
        
        // Load the module file
        let _module_info = self.module_loader.load_module(module_name)?;
        
        // Create a module value with empty exports initially
        let module_value = Value::Module {
            name: module_name.to_string(),
            exports: FxHashMap::default(),
        };
        
        self.loaded_modules.insert(module_name.to_string(), module_value);
        
        // TODO: Actually compile and execute the module to populate exports
        // This would involve:
        // 1. Parsing the module file
        // 2. Compiling it to bytecode
        // 3. Executing it in a module context
        // 4. Collecting the exports
        
        Ok(())
    }
    
    fn current_chunk(&self) -> usize {
        self.call_stack.last()
            .map(|frame| frame.chunk_id)
            .unwrap_or(self.bytecode.main_chunk)
    }
    
    /// Set a global variable
    pub fn set_global(&mut self, name: String, value: Value) {
        self.globals.insert(name, value);
    }
    
    pub fn get_global(&self, name: &str) -> Option<&Value> {
        self.globals.get(name)
    }
    
    /// Build a stack trace from current call stack
    pub fn build_stack_trace(&self) -> StackTrace {
        let mut trace = StackTrace::new();
        
        for frame in &self.call_stack {
            let function_name = self.bytecode.chunks.get(frame.chunk_id)
                .and_then(|chunk| chunk.name.clone())
                .unwrap_or_else(|| format!("<anonymous:{}>", frame.chunk_id));
            
            trace.push_frame(StackFrame {
                function_name,
                chunk_id: frame.chunk_id,
                ip: frame.ip,
                location: None, // TODO: Add source mapping
            });
        }
        
        trace
    }
    
    /// Set resource limits
    pub fn set_resource_limits(&mut self, limits: ResourceLimits) {
        self.resource_limits = limits;
    }
}

enum VMState {
    Continue,
    Return,
    Halt,
}