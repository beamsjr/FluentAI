//! High-performance stack-based virtual machine

use crate::bytecode::{Bytecode, Instruction, Opcode, Value};
use crate::debug::{VMDebugEvent, DebugConfig, StepMode};
use crate::safety::{IdGenerator, PromiseId, ChannelId, ResourceLimits, checked_ops};
use crate::security::{SecurityManager, SecurityPolicy};
use crate::error::{VMError, VMResult, StackTrace, StackFrame, value_type_name};
use crate::gc::{GarbageCollector, GcScope, GcConfig};
use rustc_hash::FxHashMap;
use fluentai_effects::{EffectContext, runtime::EffectRuntime};
use fluentai_stdlib::{StdlibRegistry, init_stdlib};
use fluentai_stdlib::value::Value as StdlibValue;
use fluentai_modules::{ModuleLoader, ModuleResolver};
use std::sync::{Arc, RwLock};
use std::time::Instant;
use tokio::sync::{mpsc, oneshot};
use fluentai_core::ast::{NodeId, UsageStatistics};

const STACK_SIZE: usize = 10_000;

pub struct CallFrame {
    chunk_id: usize,
    ip: usize,
    stack_base: usize,
    env: Vec<Value>, // Captured environment for closures
    #[allow(dead_code)]
    start_time: Option<Instant>, // Track when this frame started executing
}

/// Tracks usage statistics for nodes during execution
pub struct UsageTracker {
    /// Map from chunk_id to NodeId for tracking
    chunk_to_node: FxHashMap<usize, NodeId>,
    /// Accumulated statistics per node
    stats: FxHashMap<NodeId, UsageStatistics>,
    /// Execution time tracking
    execution_times: FxHashMap<NodeId, Vec<u64>>,
}

impl UsageTracker {
    pub fn new() -> Self {
        Self {
            chunk_to_node: FxHashMap::default(),
            stats: FxHashMap::default(),
            execution_times: FxHashMap::default(),
        }
    }
    
    /// Register a chunk ID to node ID mapping
    pub fn register_chunk(&mut self, chunk_id: usize, node_id: NodeId) {
        self.chunk_to_node.insert(chunk_id, node_id);
    }
    
    /// Record execution of a chunk
    pub fn record_execution(&mut self, chunk_id: usize, execution_time_ns: u64) {
        if let Some(&node_id) = self.chunk_to_node.get(&chunk_id) {
            let stats = self.stats.entry(node_id).or_default();
            stats.execution_count += 1;
            
            // Update average execution time
            let times = self.execution_times.entry(node_id).or_default();
            times.push(execution_time_ns);
            
            // Keep last 100 samples for moving average
            if times.len() > 100 {
                times.remove(0);
            }
            
            stats.avg_execution_time_ns = times.iter().sum::<u64>() / times.len() as u64;
            
            // Mark as hot path if executed frequently
            if stats.execution_count > 1000 {
                stats.is_hot_path = true;
            }
        }
    }
    
    /// Record an error for a chunk
    pub fn record_error(&mut self, chunk_id: usize) {
        if let Some(&node_id) = self.chunk_to_node.get(&chunk_id) {
            let stats = self.stats.entry(node_id).or_default();
            stats.error_count += 1;
        }
    }
    
    /// Get statistics for a node
    pub fn get_stats(&self, node_id: NodeId) -> Option<&UsageStatistics> {
        self.stats.get(&node_id)
    }
    
    /// Get all statistics
    pub fn get_all_stats(&self) -> &FxHashMap<NodeId, UsageStatistics> {
        &self.stats
    }
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
    promises: FxHashMap<PromiseId, oneshot::Receiver<VMResult<Value>>>,
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
    // Security manager
    security_manager: Option<Arc<SecurityManager>>,
    // Garbage collector
    gc: Option<Arc<GarbageCollector>>,
    // Usage tracking for context memory
    usage_tracker: Option<Arc<RwLock<UsageTracker>>>,
    // Effect handler stack
    handler_stack: Vec<HandlerFrame>,
}

/// Handler frame for tracking active effect handlers
#[derive(Clone)]
pub struct HandlerFrame {
    /// Map from effect type + operation to handler function value
    handlers: FxHashMap<(String, Option<String>), Value>,
    /// Continuation point - where to return after handler execution
    return_ip: usize,
    /// Stack depth when handler was installed
    stack_depth: usize,
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
            module_loader: ModuleLoader::new(fluentai_modules::ModuleConfig::default()),
            module_resolver: ModuleResolver::new(ModuleLoader::new(fluentai_modules::ModuleConfig::default())),
            loaded_modules: FxHashMap::default(),
            current_module: None,
            module_stack: Vec::new(),
            debug_config: DebugConfig::default(),
            instruction_count: 0,
            resource_limits: ResourceLimits::default(),
            security_manager: None,
            gc: None,
            usage_tracker: None,
            handler_stack: Vec::new(),
        }
    }
    
    pub fn enable_trace(&mut self) {
        self.trace = true;
    }
    
    /// Reset VM state for reuse while keeping expensive initializations
    pub fn reset(&mut self) {
        // Clear runtime state
        self.stack.clear();
        self.call_stack.clear();
        self.globals.clear();
        self.promises.clear();
        self.channels.clear();
        self.cells.clear();
        self.instruction_count = 0;
        self.handler_stack.clear();
        
        // Keep these expensive initializations:
        // - self.stdlib (258 functions)
        // - self.module_loader
        // - self.module_resolver
        // - self.effect_context
        // - self.effect_runtime
        // - self.id_generator
        // - self.resource_limits
        // - self.security_manager
        // - self.gc
        // - self.usage_tracker
        
        // Clear module state but keep the loader
        self.loaded_modules.clear();
        self.current_module = None;
        self.module_stack.clear();
    }
    
    /// Enable usage tracking
    pub fn enable_usage_tracking(&mut self) {
        self.usage_tracker = Some(Arc::new(RwLock::new(UsageTracker::new())));
    }
    
    /// Get usage tracker
    pub fn usage_tracker(&self) -> Option<Arc<RwLock<UsageTracker>>> {
        self.usage_tracker.clone()
    }
    
    /// Register a chunk to node mapping for usage tracking
    pub fn register_chunk_mapping(&mut self, chunk_id: usize, node_id: NodeId) {
        if let Some(tracker) = &self.usage_tracker {
            if let Ok(mut tracker) = tracker.write() {
                tracker.register_chunk(chunk_id, node_id);
            }
        }
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
        self.module_loader = loader;
    }
    
    pub fn set_security_manager(&mut self, manager: Arc<SecurityManager>) {
        self.security_manager = Some(manager);
    }
    
    pub fn with_sandbox_security(&mut self) -> &mut Self {
        self.security_manager = Some(Arc::new(SecurityManager::sandbox()));
        self
    }
    
    pub fn with_security_policy(&mut self, policy: SecurityPolicy) -> &mut Self {
        self.security_manager = Some(Arc::new(SecurityManager::new(policy)));
        self
    }
    
    /// Enable garbage collection with default configuration
    pub fn with_gc(&mut self) -> &mut Self {
        self.gc = Some(Arc::new(GarbageCollector::new(GcConfig::default())));
        self
    }
    
    /// Enable garbage collection with custom configuration
    pub fn with_gc_config(&mut self, config: GcConfig) -> &mut Self {
        self.gc = Some(Arc::new(GarbageCollector::new(config)));
        self
    }
    
    /// Get the garbage collector if enabled
    pub fn gc(&self) -> Option<&Arc<GarbageCollector>> {
        self.gc.as_ref()
    }
    
    /// Allocate a value using GC if enabled
    pub fn gc_alloc(&self, value: Value) -> VMResult<Value> {
        if let Some(ref gc) = self.gc {
            let handle = gc.allocate(value)?;
            // Store handle ID in a special GC value variant
            Ok(Value::GcHandle(Box::new(handle)))
        } else {
            // If GC is not enabled, return the value as-is
            Ok(value)
        }
    }
    
    /// Create a GC scope for temporary allocations
    pub fn gc_scope<F, R>(&self, f: F) -> VMResult<R>
    where
        F: FnOnce(&mut GcScope) -> VMResult<R>,
    {
        if let Some(ref gc) = self.gc {
            let mut scope = GcScope::new(gc);
            f(&mut scope)
        } else {
            Err(VMError::RuntimeError {
                message: "GC not enabled".to_string(),
                stack_trace: None,
            })
        }
    }
    
    /// Manually trigger garbage collection
    pub fn gc_collect(&self) -> VMResult<()> {
        if let Some(ref gc) = self.gc {
            gc.collect().map_err(|e| VMError::RuntimeError {
                message: format!("GC error: {}", e),
                stack_trace: None,
            })
        } else {
            Ok(()) // No-op if GC not enabled
        }
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
    
    /// Get usage statistics for a specific node
    pub fn get_usage_stats(&self, node_id: NodeId) -> Option<UsageStatistics> {
        self.usage_tracker.as_ref()?.read().ok()?.get_stats(node_id).cloned()
    }
    
    /// Get all usage statistics
    pub fn get_all_usage_stats(&self) -> Option<FxHashMap<NodeId, UsageStatistics>> {
        self.usage_tracker.as_ref()?.read().ok().map(|tracker| tracker.get_all_stats().clone())
    }
    
    pub fn run(&mut self) -> VMResult<Value> {
        self.call_stack.push(CallFrame {
            chunk_id: self.bytecode.main_chunk,
            ip: 0,
            stack_base: 0,
            env: Vec::new(),
            start_time: self.usage_tracker.as_ref().map(|_| Instant::now()),
        });
        
        let result = self.run_inner();
        
        // Track error if one occurred
        if result.is_err() {
            if let Some(tracker) = &self.usage_tracker {
                if let Some(frame) = self.call_stack.last() {
                    if let Ok(mut tracker_guard) = tracker.write() {
                        tracker_guard.record_error(frame.chunk_id);
                    }
                }
            }
        }
        
        result
    }
    
    fn run_inner(&mut self) -> VMResult<Value> {
        loop {
            let frame = self.call_stack.last().ok_or_else(|| {
                VMError::StackUnderflow {
                    operation: "get_current_frame".to_string(),
                    stack_size: self.call_stack.len(),
                    stack_trace: None,
                }
            })?;
            let chunk_id = frame.chunk_id;
            let ip = frame.ip;
            
            if ip >= self.bytecode.chunks[chunk_id].instructions.len() {
                return Err(VMError::InvalidJumpTarget {
                    target: ip,
                    chunk_size: self.bytecode.chunks[chunk_id].instructions.len(),
                    stack_trace: None,
                });
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
            
            // Security checks
            if let Some(ref security) = self.security_manager {
                security.context.track_instruction()?;
                
                // Check specific instruction security requirements
                match &instruction.opcode {
                    Opcode::Call => {
                        // Check call depth
                        if self.call_stack.len() >= self.resource_limits.max_call_depth {
                            return Err(VMError::CallStackOverflow {
                                current_depth: self.call_stack.len(),
                                max_depth: self.resource_limits.max_call_depth,
                                stack_trace: None,
                            });
                        }
                    }
                    _ => {}
                }
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
                        let result = self.stack.pop().ok_or_else(|| VMError::StackUnderflow {
                            operation: "main_return".to_string(),
                            stack_size: self.stack.len(),
                            stack_trace: None,
                        })?;
                        
                        // Track main function execution time
                        if let Some(tracker) = &self.usage_tracker {
                            if let Some(frame) = self.call_stack.last() {
                                if let Some(start_time) = frame.start_time {
                                    let elapsed = start_time.elapsed().as_nanos() as u64;
                                    if let Ok(mut tracker_guard) = tracker.write() {
                                        tracker_guard.record_execution(frame.chunk_id, elapsed);
                                    }
                                }
                            }
                        }
                        
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
                    return self.stack.pop().ok_or_else(|| VMError::StackUnderflow {
                        operation: "halt".to_string(),
                        stack_size: self.stack.len(),
                        stack_trace: None,
                    });
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
    
    fn execute_instruction(&mut self, instruction: &Instruction, chunk_id: usize) -> VMResult<VMState> {
        use Opcode::*;
        
        match instruction.opcode {
            // Stack manipulation
            Push => {
                let value = self.bytecode.chunks[chunk_id].constants.get(instruction.arg as usize)
                    .ok_or_else(|| VMError::InvalidConstantIndex {
                        index: instruction.arg,
                        max_index: self.bytecode.chunks[chunk_id].constants.len(),
                        stack_trace: None,
                    })?
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
                    return Err(VMError::StackUnderflow {
                        operation: "swap".to_string(),
                        stack_size: len,
                        stack_trace: None,
                    });
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
                    .ok_or_else(|| VMError::InvalidConstantIndex {
                        index: instruction.arg,
                        max_index: self.bytecode.chunks[chunk_id].constants.len(),
                        stack_trace: None,
                    })?
                    .clone();
                self.push(value)?;
            }
            
            // Arithmetic
            Add => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::add_i64(x, y).map(Value::Int).map_err(|_| VMError::IntegerOverflow {
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
            
            Sub => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::sub_i64(x, y).map(Value::Int).map_err(|_| VMError::IntegerOverflow {
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
            
            Mul => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::mul_i64(x, y).map(Value::Int).map_err(|_| VMError::IntegerOverflow {
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
            
            Div => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::div_i64(x, y).map(Value::Int).map_err(|_| {
                    if y == 0 {
                        VMError::DivisionByZero { location: None, stack_trace: None }
                    } else {
                        VMError::IntegerOverflow {
                            operation: "div".to_string(),
                            operands: (x, y),
                            stack_trace: None,
                        }
                    }
                }),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Float(x / y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "div".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Mod => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => checked_ops::mod_i64(x, y).map(Value::Int).map_err(|_| {
                    if y == 0 {
                        VMError::DivisionByZero { location: None, stack_trace: None }
                    } else {
                        VMError::IntegerOverflow {
                            operation: "mod".to_string(),
                            operands: (x, y),
                            stack_trace: None,
                        }
                    }
                }),
                (a, b) => Err(VMError::TypeError {
                    operation: "mod".to_string(),
                    expected: "int".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Neg => {
                let value = self.pop()?;
                match value {
                    Value::Int(x) => {
                        let negated = checked_ops::neg_i64(x).map_err(|_| VMError::IntegerOverflow {
                            operation: "neg".to_string(),
                            operands: (x, 0),
                            stack_trace: None,
                        })?;
                        self.push(Value::Int(negated))?
                    }
                    Value::Float(x) => self.push(Value::Float(-x))?,
                    v => return Err(VMError::TypeError {
                        operation: "neg".to_string(),
                        expected: "int/float".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            // Type-specialized arithmetic
            AddInt => self.binary_int_op(|x, y| checked_ops::add_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "add_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            }))?,
            SubInt => self.binary_int_op(|x, y| checked_ops::sub_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "sub_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            }))?,
            MulInt => self.binary_int_op(|x, y| checked_ops::mul_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                operation: "mul_int".to_string(),
                operands: (x, y),
                stack_trace: None,
            }))?,
            DivInt => self.binary_int_op(|x, y| checked_ops::div_i64(x, y).map_err(|_| {
                if y == 0 {
                    VMError::DivisionByZero { location: None, stack_trace: None }
                } else {
                    VMError::IntegerOverflow {
                        operation: "div_int".to_string(),
                        operands: (x, y),
                        stack_trace: None,
                    }
                }
            }))?,
            
            // Float-specialized arithmetic
            AddFloat => self.binary_float_op(|x, y| x + y)?,
            SubFloat => self.binary_float_op(|x, y| x - y)?,
            MulFloat => self.binary_float_op(|x, y| x * y)?,
            DivFloat => self.binary_float_op(|x, y| x / y)?,
            
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
                (a, b) => Err(VMError::TypeError {
                    operation: "lt".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Le => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x <= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x <= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "le".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Gt => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x > y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x > y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "gt".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Ge => self.binary_op(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => Ok(Value::Bool(x >= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Bool(x >= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "ge".to_string(),
                    expected: "int/float".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            // Type-specialized comparison
            LtInt => self.binary_int_cmp(|x, y| x < y)?,
            LeInt => self.binary_int_cmp(|x, y| x <= y)?,
            GtInt => self.binary_int_cmp(|x, y| x > y)?,
            GeInt => self.binary_int_cmp(|x, y| x >= y)?,
            
            // Boolean
            And => self.binary_op(|a, b| match (a, b) {
                (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x && y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "and".to_string(),
                    expected: "bool".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Or => self.binary_op(|a, b| match (a, b) {
                (Value::Bool(x), Value::Bool(y)) => Ok(Value::Bool(x || y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "or".to_string(),
                    expected: "bool".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            Not => {
                let value = self.pop()?;
                match value {
                    Value::Bool(x) => self.push(Value::Bool(!x))?,
                    v => return Err(VMError::TypeError {
                        operation: "not".to_string(),
                        expected: "bool".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                
                // Track execution time if usage tracking is enabled
                if let Some(tracker) = &self.usage_tracker {
                    if let Some(start_time) = frame.start_time {
                        let elapsed = start_time.elapsed().as_nanos() as u64;
                        if let Ok(mut tracker_guard) = tracker.write() {
                            tracker_guard.record_execution(frame.chunk_id, elapsed);
                        }
                    }
                }
                
                // Clean up stack (remove arguments)
                self.stack.truncate(frame.stack_base);
                
                // Push return value
                self.push(return_val)?;
            }
            
            // Variables
            Load => {
                let local_idx = instruction.arg as usize;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + local_idx;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: local_idx,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            Store => {
                let local_idx = instruction.arg as usize;
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + local_idx;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: local_idx,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                self.stack[value_idx] = value;
            }
            
            // Fast local variable access opcodes
            LoadLocal0 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 0,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal1 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 1;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 1,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal2 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 2;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 2,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            LoadLocal3 => {
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 3;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 3,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                let value = self.stack[value_idx].clone();
                self.push(value)?;
            }
            
            StoreLocal0 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 0,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal1 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 1;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 1,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal2 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 2;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 2,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                self.stack[value_idx] = value;
            }
            
            StoreLocal3 => {
                let value = self.pop()?;
                let frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                let value_idx = frame.stack_base + 3;
                
                if value_idx >= self.stack.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: 3,
                        frame_size: self.stack.len() - frame.stack_base,
                        stack_trace: None,
                    });
                }
                
                self.stack[value_idx] = value;
            }
            
            LoadGlobal => {
                let name_idx = instruction.arg as usize;
                let name = match &self.bytecode.chunks[chunk_id].constants.get(name_idx) {
                    Some(Value::String(s)) => s,
                    _ => return Err(VMError::InvalidConstantIndex {
                        index: name_idx as u32,
                        max_index: self.bytecode.chunks[chunk_id].constants.len(),
                        stack_trace: None,
                    }),
                };
                
                // Check stdlib first to allow operators to be used as first-class functions
                let value = if self.stdlib.contains(name) {
                    // Standard library function
                    Value::String(format!("__stdlib__{}", name))
                } else if let Some(_) = self.builtin_to_opcode(name) {
                    // For built-ins, we'll store them as a special string value
                    // This is only used as a fallback for operators not in stdlib
                    Value::String(format!("__builtin__{}", name))
                } else if name == "cons" {
                    // Special built-in for list construction
                    // Check if it's in stdlib first
                    if self.stdlib.contains("cons") {
                        Value::String("__stdlib__cons".to_string())
                    } else {
                        Value::String("__builtin__cons".to_string())
                    }
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
                    _ => return Err(VMError::InvalidConstantIndex {
                        index: name_idx as u32,
                        max_index: self.bytecode.chunks[chunk_id].constants.len(),
                        stack_trace: None,
                    }),
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
                    v => return Err(VMError::TypeError {
                        operation: "list_len".to_string(),
                        expected: "list".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            ListEmpty => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => self.push(Value::Bool(items.is_empty()))?,
                    v => return Err(VMError::TypeError {
                        operation: "list_empty".to_string(),
                        expected: "list".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            ListHead => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take head of empty list".to_string(),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                        self.push(items[0].clone())?;
                    }
                    v => return Err(VMError::TypeError {
                        operation: "list_head".to_string(),
                        expected: "list".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            ListTail => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => {
                        if items.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take tail of empty list".to_string(),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                        let tail = items[1..].to_vec();
                        self.push(Value::List(tail))?;
                    }
                    v => return Err(VMError::TypeError {
                        operation: "list_tail".to_string(),
                        expected: "list".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                    v => return Err(VMError::TypeError {
                        operation: "list_cons".to_string(),
                        expected: "list as second argument".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            // Strings
            StrLen => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::Int(s.len() as i64))?,
                    v => return Err(VMError::TypeError {
                        operation: "str_len".to_string(),
                        expected: "string".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            StrConcat => self.binary_op(|a, b| match (a, b) {
                (Value::String(x), Value::String(y)) => Ok(Value::String(x + &y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "str_concat".to_string(),
                    expected: "string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,
            
            StrUpper => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::String(s.to_uppercase()))?,
                    v => return Err(VMError::TypeError {
                        operation: "str_upper".to_string(),
                        expected: "string".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            StrLower => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::String(s.to_lowercase()))?,
                    v => return Err(VMError::TypeError {
                        operation: "str_lower".to_string(),
                        expected: "string".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                    v => return Err(VMError::TypeError {
                        operation: "effect".to_string(),
                        expected: "string for operation name".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    v => return Err(VMError::TypeError {
                        operation: "effect".to_string(),
                        expected: "string for effect type".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                // Convert string to EffectType enum
                let effect_type = match effect_type_str.as_str() {
                    "IO" => fluentai_core::ast::EffectType::IO,
                    "State" => fluentai_core::ast::EffectType::State,
                    "Error" => fluentai_core::ast::EffectType::Error,
                    "Time" => fluentai_core::ast::EffectType::Time,
                    "Network" => fluentai_core::ast::EffectType::Network,
                    "Random" => fluentai_core::ast::EffectType::Random,
                    "Dom" => fluentai_core::ast::EffectType::Dom,
                    "Async" => fluentai_core::ast::EffectType::Async,
                    "Concurrent" => fluentai_core::ast::EffectType::Concurrent,
                    "Pure" => fluentai_core::ast::EffectType::Pure,
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown effect type: {}", effect_type_str),
                        stack_trace: Some(self.build_stack_trace()),
                    }),
                };
                
                // Check handler stack for a matching handler
                let mut handler_result = None;
                
                
                // Search from most recent to oldest handler
                for handler_frame in self.handler_stack.iter().rev() {
                    // Check for exact match first (effect_type + operation)
                    if let Some(handler_fn) = handler_frame.handlers.get(&(effect_type_str.clone(), Some(operation.clone()))) {
                        // Found a specific handler for this effect + operation
                        // Call the handler function with args
                        handler_result = Some(self.call_handler_function(handler_fn.clone(), args.clone())?);
                        break;
                    }
                    
                    // Check for general handler (effect_type only)
                    if let Some(handler_fn) = handler_frame.handlers.get(&(effect_type_str.clone(), None)) {
                        // Found a general handler for this effect type
                        // Create a list with operation as first argument, followed by other args
                        let mut handler_args = vec![Value::String(operation.clone())];
                        handler_args.extend(args.clone());
                        handler_result = Some(self.call_handler_function(handler_fn.clone(), handler_args)?);
                        break;
                    }
                }
                
                if let Some(result) = handler_result {
                    // Handler was called, push its result
                    self.push(result)?;
                } else {
                    // No handler found, execute the default effect
                    // Convert VM values to core values for effect handlers
                    let core_args: Vec<fluentai_core::value::Value> = args.iter()
                        .map(|v| self.vm_value_to_core_value(v))
                        .collect();
                    
                    // Execute the effect synchronously
                    let result = self.effect_context.perform_sync(effect_type, &operation, &core_args)
                        .map_err(|e| VMError::RuntimeError {
                            message: format!("Effect error: {}", e),
                            stack_trace: Some(self.build_stack_trace()),
                        })?;
                    
                    // Convert result back to VM value
                    let vm_result = self.core_value_to_vm_value(&result);
                    self.push(vm_result)?;
                }
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
                    v => return Err(VMError::TypeError {
                        operation: "effect_async".to_string(),
                        expected: "string for operation name".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    v => return Err(VMError::TypeError {
                        operation: "effect_async".to_string(),
                        expected: "string for effect type".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                // Convert string to EffectType enum
                let effect_type = match effect_type_str.as_str() {
                    "IO" => fluentai_core::ast::EffectType::IO,
                    "State" => fluentai_core::ast::EffectType::State,
                    "Error" => fluentai_core::ast::EffectType::Error,
                    "Time" => fluentai_core::ast::EffectType::Time,
                    "Network" => fluentai_core::ast::EffectType::Network,
                    "Random" => fluentai_core::ast::EffectType::Random,
                    "Dom" => fluentai_core::ast::EffectType::Dom,
                    "Async" => fluentai_core::ast::EffectType::Async,
                    "Concurrent" => fluentai_core::ast::EffectType::Concurrent,
                    "Pure" => fluentai_core::ast::EffectType::Pure,
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown effect type: {}", effect_type_str),
                        stack_trace: Some(self.build_stack_trace()),
                    }),
                };
                
                // Generate a promise ID
                let promise_id = self.id_generator.next_promise_id();
                
                // Convert VM values to core values
                let core_args: Vec<fluentai_core::value::Value> = args.iter()
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
                        .map_err(|e| VMError::AsyncError {
                            message: format!("Async effect error: {}", e),
                            stack_trace: None,
                        });
                    
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
                    v => return Err(VMError::TypeError {
                        operation: "await".to_string(),
                        expected: "promise".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                            return Err(VMError::AsyncError {
                                message: "Promise channel closed".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: format!("promise:{}", promise_id),
                        location: None,
                        stack_trace: None,
                    });
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
                    return Err(VMError::ResourceLimitExceeded {
                        resource: "channels".to_string(),
                        limit: self.resource_limits.max_channels,
                        requested: self.channels.len() + 1,
                        stack_trace: None,
                    });
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
                    v => return Err(VMError::TypeError {
                        operation: "send".to_string(),
                        expected: "channel".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                // Get the channel sender
                if let Some((tx, _)) = self.channels.get(&channel_id) {
                    tx.try_send(value)
                        .map_err(|e| match e {
                            mpsc::error::TrySendError::Full(_) => VMError::AsyncError {
                                message: "Channel buffer full".to_string(),
                                stack_trace: None,
                            },
                            mpsc::error::TrySendError::Closed(_) => VMError::AsyncError {
                                message: "Channel closed".to_string(),
                                stack_trace: None,
                            },
                        })?;
                    self.push(Value::Nil)?;
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: format!("channel:{}", channel_id),
                        location: None,
                        stack_trace: None,
                    });
                }
            }
            
            Receive => {
                // Pop channel
                let channel_id = match self.pop()? {
                    Value::Channel(id) => id,
                    v => return Err(VMError::TypeError {
                        operation: "receive".to_string(),
                        expected: "channel".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                            return Err(VMError::AsyncError {
                                message: "Channel disconnected".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: format!("channel:{}", channel_id),
                        location: None,
                        stack_trace: None,
                    });
                }
            }
            
            // Functions
            MakeFunc => {
                let chunk_id = instruction.arg as usize;
                // MakeFunc is used for functions with no free variables
                // Functions with free variables use MakeClosure instead
                let env = Vec::new();
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
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    })?;
                
                // Load value from captured environment
                if capture_idx >= frame.env.len() {
                    return Err(VMError::InvalidLocalIndex {
                        index: capture_idx,
                        frame_size: frame.env.len(),
                        stack_trace: None,
                    });
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
                        // Track function call timing if usage tracking is enabled
                        let start_time = if self.usage_tracker.is_some() {
                            Some(Instant::now())
                        } else {
                            None
                        };
                        
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
                            start_time,
                        });
                    }
                    Value::String(s) if s.starts_with("__builtin__") => {
                        // Handle built-in function calls
                        let builtin_name = &s[11..]; // Remove "__builtin__" prefix
                        
                        // Special handling for cons which needs different argument order
                        if builtin_name == "cons" {
                            if args.len() != 2 {
                                return Err(VMError::RuntimeError {
                                    message: "cons requires exactly 2 arguments".to_string(),
                                    stack_trace: Some(self.build_stack_trace()),
                                });
                            }
                            match &args[1] {
                                Value::List(items) => {
                                    let mut new_list = vec![args[0].clone()];
                                    new_list.extend(items.iter().cloned());
                                    self.push(Value::List(new_list))?;
                                }
                                _ => return Err(VMError::TypeError {
                                    operation: "cons".to_string(),
                                    expected: "list as second argument".to_string(),
                                    got: value_type_name(&args[1]).to_string(),
                                    location: None,
                                    stack_trace: None,
                                }),
                            }
                        } else {
                            // For other built-ins, generate appropriate instructions
                            // This is a simplified approach - in a full implementation,
                            // we might want to directly execute the operations
                            return Err(VMError::RuntimeError {
                                message: format!("Built-in function {} should be optimized by compiler", builtin_name),
                                stack_trace: Some(self.build_stack_trace()),
                            });
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
                                    return Err(VMError::UnknownIdentifier {
                                        name: func_name.to_string(),
                                        location: None,
                                        stack_trace: None,
                                    });
                                }
                            }
                        }
                    }
                    v => return Err(VMError::TypeError {
                        operation: "call".to_string(),
                        expected: "function".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            // Mutable cells
            MakeCell => {
                // Check resource limit
                if self.cells.len() >= self.resource_limits.max_cells {
                    return Err(VMError::ResourceLimitExceeded {
                        resource: "cells".to_string(),
                        limit: self.resource_limits.max_cells,
                        requested: self.cells.len() + 1,
                        stack_trace: None,
                    });
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
                            return Err(VMError::CellError {
                                index: idx,
                                message: "Invalid cell index".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                    v => return Err(VMError::TypeError {
                        operation: "cell_get".to_string(),
                        expected: "cell".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                            return Err(VMError::CellError {
                                index: idx,
                                message: "Invalid cell index".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                    v => return Err(VMError::TypeError {
                        operation: "cell_set".to_string(),
                        expected: "cell".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                    v => return Err(VMError::TypeError {
                        operation: "make_tagged".to_string(),
                        expected: "string for tag".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                self.push(Value::Tagged { tag, values })?;
            }
            
            GetTag => {
                let value = self.pop()?;
                match value {
                    Value::Tagged { tag, .. } => {
                        self.push(Value::String(tag))?;
                    }
                    v => return Err(VMError::TypeError {
                        operation: "get_tag".to_string(),
                        expected: "tagged value".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
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
                            return Err(VMError::RuntimeError {
                                message: format!("Tagged field index {} out of bounds (size: {})", field_idx, values.len()),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                    }
                    v => return Err(VMError::TypeError {
                        operation: "get_tagged_field".to_string(),
                        expected: "tagged value".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            IsTagged => {
                let expected_tag_idx = instruction.arg as usize;
                let expected_tag = match self.bytecode.chunks[chunk_id].constants.get(expected_tag_idx) {
                    Some(Value::String(s)) => s,
                    _ => return Err(VMError::InvalidConstantIndex {
                        index: expected_tag_idx as u32,
                        max_index: self.bytecode.chunks[chunk_id].constants.len(),
                        stack_trace: None,
                    }),
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
                        return Err(VMError::ModuleError {
                            module_name: module_name.clone(),
                            message: format!("Module does not export '{}'", binding_name),
                            stack_trace: None,
                        });
                    }
                } else {
                    return Err(VMError::ModuleError {
                        module_name: module_name.clone(),
                        message: "Module not loaded".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            ImportAll => {
                let module_name = self.get_constant_string(instruction.arg)?;
                
                if let Some(Value::Module { exports, .. }) = self.loaded_modules.get(&module_name) {
                    // Import all exports into the current scope
                    for (export_name, value) in exports {
                        // Store each export as a global variable
                        self.globals.insert(export_name.clone(), value.clone());
                    }
                } else {
                    return Err(VMError::ModuleError {
                        module_name: module_name.clone(),
                        message: "Module not found".to_string(),
                        stack_trace: None,
                    });
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
                        return Err(VMError::ModuleError {
                            module_name: module_name.clone(),
                            message: format!("Module does not export '{}'", var_name),
                            stack_trace: None,
                        });
                    }
                } else {
                    return Err(VMError::ModuleError {
                        module_name: module_name.clone(),
                        message: "Module not found".to_string(),
                        stack_trace: None,
                    });
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
                    return Err(VMError::RuntimeError {
                        message: "Cannot export outside of module context".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    });
                }
            }
            
            // GC operations
            GcAlloc => {
                let value = self.pop()?;
                let gc_value = self.gc_alloc(value)?;
                self.push(gc_value)?;
            }
            
            GcDeref => {
                let handle = self.pop()?;
                match handle {
                    Value::GcHandle(gc_handle) => {
                        let value = gc_handle.get();
                        self.push(value)?;
                    }
                    v => return Err(VMError::TypeError {
                        operation: "gc_deref".to_string(),
                        expected: "GC handle".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            GcSet => {
                let value = self.pop()?;
                let handle = self.pop()?;
                match handle {
                    Value::GcHandle(gc_handle) => {
                        gc_handle.set(value);
                        self.push(Value::Nil)?;
                    }
                    v => return Err(VMError::TypeError {
                        operation: "gc_set".to_string(),
                        expected: "GC handle".to_string(),
                        got: value_type_name(&v).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                }
            }
            
            GcCollect => {
                self.gc_collect()?;
                self.push(Value::Nil)?;
            }
            
            // Tail call optimization
            TailCall => {
                
                // Pop arguments for the tail call
                let arg_count = instruction.arg as usize;
                
                // Get the function to call (it's on top)
                let func = self.pop()?;
                
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(self.pop()?);
                }
                args.reverse();
                
                // Reuse current call frame instead of creating new one
                if let Some(frame) = self.call_stack.last_mut() {
                    // Update locals with new arguments
                    let base = frame.stack_base;
                    for (i, arg) in args.into_iter().enumerate() {
                        if base + i < self.stack.len() {
                            self.stack[base + i] = arg;
                        }
                    }
                    
                    // Jump to function start
                    match func {
                        Value::Function { chunk_id, env } => {
                            frame.ip = 0;
                            frame.chunk_id = chunk_id;
                            frame.env = env;
                        }
                        v => return Err(VMError::TypeError {
                            operation: "tail_call".to_string(),
                            expected: "function".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        }),
                    }
                } else {
                    return Err(VMError::RuntimeError {
                        message: "TailCall with no active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    });
                }
            }
            
            TailReturn => {
                // Like Return but optimized for tail recursion
                let result = self.pop()?;
                if let Some(frame) = self.call_stack.pop() {
                    // Restore stack
                    self.stack.truncate(frame.stack_base);
                    self.push(result)?;
                    
                    // The previous frame's IP will be used automatically
                    // when we continue execution in run_inner()
                } else {
                    return Err(VMError::RuntimeError {
                        message: "TailReturn with empty call stack".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    });
                }
            }
            
            LoopStart => {
                // Mark loop start for potential jump-back
                // Store current IP in instruction arg for later reference
                // This is a no-op at runtime but helps with debugging
            }
            
            LoopEnd => {
                // Check if we should continue the loop
                // The compiler should have emitted appropriate jump instructions
            }
            
            UpdateLocal => {
                // Update a local variable (for loop parameter updates)
                let local_idx = instruction.arg as usize;
                let value = self.pop()?;
                
                if let Some(frame) = self.call_stack.last() {
                    let stack_idx = frame.stack_base + local_idx;
                    if stack_idx < self.stack.len() {
                        self.stack[stack_idx] = value;
                    } else {
                        return Err(VMError::InvalidLocalIndex {
                            index: local_idx,
                            frame_size: self.stack.len() - frame.stack_base,
                            stack_trace: None,
                        });
                    }
                } else {
                    return Err(VMError::RuntimeError {
                        message: "UpdateLocal with no active call frame".to_string(),
                        stack_trace: Some(self.build_stack_trace()),
                    });
                }
            }
            
            // Effect handlers
            MakeHandler => {
                // Create handler table from stack values
                let handler_count = instruction.arg as usize;
                let mut handler_data = FxHashMap::default();
                
                for _ in 0..handler_count {
                    // Pop handler function
                    let handler_fn = self.pop()?;
                    
                    // Pop operation filter
                    let op_filter = match self.pop()? {
                        Value::Nil => None,
                        Value::String(s) => Some(s),
                        v => return Err(VMError::TypeError {
                            operation: "make_handler".to_string(),
                            expected: "string or nil for operation filter".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        }),
                    };
                    
                    // Pop effect type
                    let effect_type = match self.pop()? {
                        Value::String(s) => s,
                        v => return Err(VMError::TypeError {
                            operation: "make_handler".to_string(),
                            expected: "string for effect type".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        }),
                    };
                    
                    // Create key in the format that InstallHandler expects
                    let key = if let Some(ref op) = op_filter {
                        format!("handler:{}:{}", effect_type, op)
                    } else {
                        format!("handler:{}:nil", effect_type)
                    };
                    handler_data.insert(key, handler_fn);
                }
                
                // Add metadata
                handler_data.insert("_type".to_string(), Value::String("handler".to_string()));
                handler_data.insert("_count".to_string(), Value::Int(handler_count as i64));
                
                self.push(Value::Map(handler_data))?;
            }
            InstallHandler => {
                // Pop handler object (created by MakeHandler)
                let handler = self.pop()?;
                
                // Extract handler map from the Map value
                let handlers = match handler {
                    Value::Map(map) => {
                        let mut handler_funcs = FxHashMap::default();
                        
                        // Convert Map entries to handler entries
                        for (key, value) in map {
                            if key.starts_with("handler:") {
                                // Extract effect type and optional operation
                                let parts: Vec<&str> = key[8..].split(':').collect();
                                if !parts.is_empty() {
                                    let effect_type = parts[0].to_string();
                                    let operation = if parts.len() > 1 && parts[1] != "nil" {
                                        Some(parts[1].to_string())
                                    } else {
                                        None
                                    };
                                    handler_funcs.insert((effect_type, operation), value);
                                }
                            }
                        }
                        handler_funcs
                    }
                    _ => return Err(VMError::TypeError {
                        operation: "install_handler".to_string(),
                        expected: "handler map".to_string(),
                        got: value_type_name(&handler).to_string(),
                        location: None,
                        stack_trace: None,
                    }),
                };
                
                // Create handler frame with current execution context
                let current_frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "Cannot install handler without active call frame".to_string(),
                        stack_trace: None,
                    })?;
                
                let handler_frame = HandlerFrame {
                    handlers,
                    return_ip: current_frame.ip,
                    stack_depth: self.stack.len(),
                };
                
                // Push handler frame onto handler stack
                self.handler_stack.push(handler_frame);
            }
            UninstallHandler => {
                // Pop the most recent handler frame
                if self.handler_stack.pop().is_none() {
                    return Err(VMError::RuntimeError {
                        message: "No handler to uninstall".to_string(),
                        stack_trace: None,
                    });
                }
                // Result value stays on stack
            }
            
            // Special
            Halt => return Ok(VMState::Halt),
            Nop => {}
            
            // All opcodes are handled exhaustively
        }
        
        Ok(VMState::Continue)
    }
    
    pub fn push(&mut self, value: Value) -> VMResult<()> {
        if self.stack.len() >= STACK_SIZE {
            return Err(VMError::StackOverflow {
                current_depth: self.stack.len(),
                max_depth: STACK_SIZE,
                stack_trace: None,
            });
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
    
    pub fn pop(&mut self) -> VMResult<Value> {
        let value = self.stack.pop().ok_or_else(|| VMError::StackUnderflow {
            operation: "pop".to_string(),
            stack_size: self.stack.len(),
            stack_trace: None,
        })?;
        
        // Send debug event
        if self.debug_config.enabled {
            self.debug_config.send_event(VMDebugEvent::StackPop {
                value: value.clone(),
            });
        }
        
        Ok(value)
    }
    
    fn peek(&self, offset: usize) -> VMResult<&Value> {
        let len = self.stack.len();
        if offset >= len {
            return Err(VMError::StackUnderflow {
                operation: "peek".to_string(),
                stack_size: len,
                stack_trace: None,
            });
        }
        Ok(&self.stack[len - 1 - offset])
    }
    
    fn binary_op<F>(&mut self, op: F) -> VMResult<()>
    where
        F: FnOnce(Value, Value) -> VMResult<Value>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        let result = op(a, b)?;
        self.push(result)
    }
    
    fn binary_int_op<F>(&mut self, op: F) -> VMResult<()>
    where
        F: FnOnce(i64, i64) -> VMResult<i64>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => {
                let result = op(x, y)?;
                self.push(Value::Int(result))
            }
            (a, b) => Err(VMError::TypeError {
                operation: "binary int operation".to_string(),
                expected: "int".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    fn binary_int_cmp<F>(&mut self, op: F) -> VMResult<()>
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
            (a, b) => Err(VMError::TypeError {
                operation: "binary int comparison".to_string(),
                expected: "int".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    fn binary_float_op<F>(&mut self, op: F) -> VMResult<()>
    where
        F: FnOnce(f64, f64) -> f64,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        match (a, b) {
            (Value::Float(x), Value::Float(y)) => {
                let result = op(x, y);
                self.push(Value::Float(result))
            }
            (a, b) => Err(VMError::TypeError {
                operation: "binary float operation".to_string(),
                expected: "float".to_string(),
                got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                location: None,
                stack_trace: None,
            }),
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
            Value::GcHandle(_) => true,
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
            Value::GcHandle(_) => {
                // No direct stdlib equivalent, use string representation
                StdlibValue::String("<gc-handle>".to_string())
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
    pub fn call_value(&mut self, arg_count: usize) -> VMResult<()> {
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
                    start_time: if self.usage_tracker.is_some() {
                        Some(Instant::now())
                    } else {
                        None
                    },
                });
                
                // Continue execution until this call returns
                let initial_call_depth = self.call_stack.len();
                while self.call_stack.len() >= initial_call_depth {
                    let frame = self.call_stack.last().ok_or_else(|| VMError::StackUnderflow {
                operation: "get_current_frame".to_string(),
                stack_size: self.call_stack.len(),
                stack_trace: None,
            })?;
                    let chunk_id = frame.chunk_id;
                    let ip = frame.ip;
                    
                    if ip >= self.bytecode.chunks[chunk_id].instructions.len() {
                        return Err(VMError::InvalidJumpTarget {
                    target: ip,
                    chunk_size: self.bytecode.chunks[chunk_id].instructions.len(),
                    stack_trace: None,
                });
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
                            return Err(VMError::RuntimeError {
                                message: "Unexpected halt in function call".to_string(),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                    }
                }
                
                Ok(())
            }
            v => Err(VMError::TypeError {
                operation: "call_value".to_string(),
                expected: "function".to_string(),
                got: value_type_name(&v).to_string(),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    fn call_handler_function(&mut self, handler: Value, args: Vec<Value>) -> VMResult<Value> {
        // Call a handler function with the given arguments
        match handler {
            Value::Function { chunk_id, env } => {
                // Save current call frame state
                let call_frame = CallFrame {
                    chunk_id,
                    ip: 0,
                    stack_base: self.stack.len(),
                    env,
                    start_time: None,
                };
                
                // Push arguments onto stack
                for arg in args {
                    self.push(arg)?;
                }
                
                self.call_stack.push(call_frame);
                
                // Execute the handler function
                let result;
                let handler_chunk_id = chunk_id; // Save the handler's chunk ID
                loop {
                    // Get current frame
                    let frame = self.call_stack.last().ok_or_else(|| VMError::RuntimeError {
                        message: "No call frame in handler execution".to_string(),
                        stack_trace: None,
                    })?;
                    
                    // Check if we're still in the handler function
                    if frame.chunk_id != handler_chunk_id {
                        // We've returned from the handler
                        result = self.pop()?;
                        break;
                    }
                    
                    let chunk_id = frame.chunk_id;
                    let ip = frame.ip;
                    
                    // Check bounds
                    let chunk = &self.bytecode.chunks[chunk_id];
                    if ip >= chunk.instructions.len() {
                        return Err(VMError::RuntimeError {
                            message: "Instruction pointer out of bounds in handler".to_string(),
                            stack_trace: Some(self.build_stack_trace()),
                        });
                    }
                    
                    // Get instruction and advance IP
                    let instruction = chunk.instructions[ip].clone();
                    self.call_stack.last_mut().unwrap().ip += 1;
                    
                    
                    match self.execute_instruction(&instruction, chunk_id)? {
                        VMState::Continue => {}
                        VMState::Return => {
                            // The Return opcode already handled everything including
                            // popping the call frame and pushing the return value
                            // We need to pop the return value that was pushed
                            result = self.pop()?;
                            break;
                        }
                        VMState::Halt => {
                            // Lambdas end with Halt, but we treat it like Return
                            // if there's a value on the stack
                            let frame = self.call_stack.last().unwrap();
                            let stack_base = frame.stack_base;
                            
                            if self.stack.len() > stack_base {
                                result = self.pop()?;
                                
                                // Clean up stack to stack_base
                                self.stack.truncate(stack_base);
                                
                                self.call_stack.pop();
                                break;
                            } else {
                                return Err(VMError::RuntimeError {
                                    message: "Handler function halted without return value".to_string(),
                                    stack_trace: Some(self.build_stack_trace()),
                                });
                            }
                        }
                    }
                }
                
                Ok(result)
            }
            v => Err(VMError::TypeError {
                operation: "call_handler_function".to_string(),
                expected: "function".to_string(),
                got: value_type_name(&v).to_string(),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    fn vm_value_to_core_value(&self, value: &Value) -> fluentai_core::value::Value {
        match value {
            Value::Nil => fluentai_core::value::Value::Nil,
            Value::Bool(b) => fluentai_core::value::Value::Boolean(*b),
            Value::Int(i) => fluentai_core::value::Value::Integer(*i),
            Value::Float(f) => fluentai_core::value::Value::Float(*f),
            Value::String(s) => fluentai_core::value::Value::String(s.clone()),
            Value::List(items) => {
                fluentai_core::value::Value::List(
                    items.iter().map(|v| self.vm_value_to_core_value(v)).collect()
                )
            }
            Value::Map(map) => {
                let mut core_map = FxHashMap::default();
                for (k, v) in map.iter() {
                    core_map.insert(k.clone(), self.vm_value_to_core_value(v));
                }
                fluentai_core::value::Value::Map(core_map)
            }
            Value::Function { .. } => {
                // Functions can't be directly converted, return a placeholder
                fluentai_core::value::Value::String("<function>".to_string())
            }
            Value::Promise(id) => {
                fluentai_core::value::Value::String(format!("<promise:{}>", id))
            }
            Value::Channel(id) => {
                fluentai_core::value::Value::String(format!("<channel:{}>", id))
            }
            Value::Cell(idx) => {
                fluentai_core::value::Value::String(format!("<cell:{}>", idx))
            }
            Value::Tagged { tag, values } => {
                // Convert to a map representation for now
                let mut map = FxHashMap::default();
                map.insert("__tag__".to_string(), fluentai_core::value::Value::String(tag.clone()));
                map.insert("__values__".to_string(), fluentai_core::value::Value::List(
                    values.iter().map(|v| self.vm_value_to_core_value(v)).collect()
                ));
                fluentai_core::value::Value::Map(map)
            }
            Value::Module { name, exports } => {
                // Convert to a map representation  
                let mut map = FxHashMap::default();
                map.insert("__module__".to_string(), fluentai_core::value::Value::String(name.clone()));
                let mut export_map = FxHashMap::default();
                for (key, val) in exports {
                    export_map.insert(key.clone(), self.vm_value_to_core_value(val));
                }
                map.insert("__exports__".to_string(), fluentai_core::value::Value::Map(export_map));
                fluentai_core::value::Value::Map(map)
            }
            Value::GcHandle(_) => {
                fluentai_core::value::Value::String("<gc-handle>".to_string())
            }
        }
    }
    
    fn core_value_to_vm_value(&self, value: &fluentai_core::value::Value) -> Value {
        match value {
            fluentai_core::value::Value::Nil => Value::Nil,
            fluentai_core::value::Value::Boolean(b) => Value::Bool(*b),
            fluentai_core::value::Value::Integer(i) => Value::Int(*i),
            fluentai_core::value::Value::Float(f) => Value::Float(*f),
            fluentai_core::value::Value::String(s) => Value::String(s.clone()),
            fluentai_core::value::Value::List(items) => {
                Value::List(
                    items.iter().map(|v| self.core_value_to_vm_value(v)).collect()
                )
            }
            fluentai_core::value::Value::Map(map) => {
                let mut vm_map = FxHashMap::default();
                for (k, v) in map.iter() {
                    vm_map.insert(k.clone(), self.core_value_to_vm_value(v));
                }
                Value::Map(vm_map)
            }
            fluentai_core::value::Value::Procedure(_) => {
                // Functions can't be directly converted, return a placeholder
                Value::String("<function>".to_string())
            }
            fluentai_core::value::Value::NativeFunction { name, .. } => {
                // Native functions can't be directly converted, return a placeholder
                Value::String(format!("<native-function: {}>", name))
            }
            fluentai_core::value::Value::Tagged { tag, values } => {
                Value::Tagged {
                    tag: tag.clone(),
                    values: values.iter().map(|v| self.core_value_to_vm_value(v)).collect(),
                }
            }
            fluentai_core::value::Value::Symbol(s) => {
                // Convert symbols to strings in VM representation
                Value::String(s.clone())
            }
            fluentai_core::value::Value::Vector(items) => {
                // Convert vectors to lists in VM representation
                Value::List(
                    items.iter().map(|v| self.core_value_to_vm_value(v)).collect()
                )
            }
        }
    }
    
    // Module system helper methods
    fn get_constant_string(&self, idx: u32) -> VMResult<String> {
        let value = self.bytecode.chunks[self.current_chunk()].constants
            .get(idx as usize)
            .ok_or_else(|| VMError::InvalidConstantIndex {
                index: idx,
                max_index: self.bytecode.chunks[self.current_chunk()].constants.len(),
                stack_trace: None,
            })?;
            
        match value {
            Value::String(s) => Ok(s.clone()),
            _ => Err(VMError::TypeError {
                operation: "get_constant_string".to_string(),
                expected: "string constant".to_string(),
                got: value_type_name(value).to_string(),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    fn load_module(&mut self, module_name: &str) -> VMResult<()> {
        // Check if already loaded
        if self.loaded_modules.contains_key(module_name) {
            return Ok(());
        }
        
        // Load the module file
        let module_info = self.module_loader.load_module(module_name)
            .map_err(|e| VMError::ModuleError {
                module_name: module_name.to_string(),
                message: e.to_string(),
                stack_trace: None,
            })?;
        
        // Compile the module
        let options = crate::compiler::CompilerOptions {
            optimization_level: fluentai_optimizer::OptimizationLevel::None,
            debug_info: false,
        };
        let compiler = crate::compiler::Compiler::with_options(options);
        let module_bytecode = compiler.compile(&module_info.graph)
            .map_err(|e| VMError::ModuleError {
                module_name: module_name.to_string(),
                message: format!("Failed to compile module: {}", e),
                stack_trace: None,
            })?;
        
        // Save current VM state
        let saved_module = self.current_module.clone();
        let saved_stack_len = self.stack.len();
        let saved_globals = self.globals.clone();
        
        // Set current module context
        self.current_module = Some(module_name.to_string());
        
        // Create a new VM instance for module execution
        let mut module_vm = VM::new(module_bytecode);
        module_vm.stdlib = self.stdlib.clone();
        module_vm.globals = self.globals.clone();
        module_vm.current_module = Some(module_name.to_string());
        
        // Execute the module
        let result = module_vm.run();
        
        // Restore VM state
        self.current_module = saved_module;
        self.stack.truncate(saved_stack_len);
        
        // Handle execution result
        if let Err(e) = result {
            self.globals = saved_globals;
            return Err(VMError::ModuleError {
                module_name: module_name.to_string(),
                message: format!("Module execution failed: {:?}", e),
                stack_trace: None,
            });
        }
        
        // Collect exports from module globals
        let mut exports = FxHashMap::default();
        for export_name in &module_info.exports {
            if let Some(value) = module_vm.globals.get(export_name) {
                exports.insert(export_name.clone(), value.clone());
            }
        }
        
        // Create module value with actual exports
        let module_value = Value::Module {
            name: module_name.to_string(),
            exports,
        };
        
        self.loaded_modules.insert(module_name.to_string(), module_value);
        
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

#[derive(Debug)]
enum VMState {
    Continue,
    Return,
    Halt,
}

#[cfg(test)]
mod inline_tests {
    use super::*;
    use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    
    #[test]
    fn test_vm_creation_inline() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(chunk);
        
        let vm = VM::new(bytecode);
        assert_eq!(vm.stack.len(), 0);
        assert_eq!(vm.globals.len(), 0);
        assert_eq!(vm.call_stack.len(), 0);
    }
    
    #[test]
    fn test_usage_tracker_inline() {
        let mut tracker = UsageTracker::new();
        let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
        
        tracker.register_chunk(0, node_id);
        tracker.record_execution(0, 1000);
        
        let stats = tracker.get_stats(node_id);
        assert!(stats.is_some());
        assert_eq!(stats.unwrap().execution_count, 1);
    }
    
    #[test]
    fn test_call_frame_creation() {
        let frame = CallFrame {
            chunk_id: 0,
            ip: 0,
            stack_base: 0,
            env: vec![],
            start_time: Some(Instant::now()),
        };
        
        assert_eq!(frame.chunk_id, 0);
        assert_eq!(frame.ip, 0);
        assert_eq!(frame.stack_base, 0);
        assert!(frame.env.is_empty());
        assert!(frame.start_time.is_some());
    }
    
    #[test]
    fn test_usage_tracker_error_recording() {
        let mut tracker = UsageTracker::new();
        let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
        
        tracker.register_chunk(0, node_id);
        tracker.record_error(0);
        
        let stats = tracker.get_stats(node_id);
        assert!(stats.is_some());
        assert_eq!(stats.unwrap().error_count, 1);
    }
    
    #[test]
    fn test_usage_tracker_hot_path() {
        let mut tracker = UsageTracker::new();
        let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());
        
        tracker.register_chunk(0, node_id);
        
        // Execute many times to trigger hot path detection
        for _ in 0..1001 {
            tracker.record_execution(0, 100);
        }
        
        let stats = tracker.get_stats(node_id);
        assert!(stats.is_some());
        assert!(stats.unwrap().is_hot_path);
    }
    
    #[test]
    fn test_usage_tracker_all_stats() {
        let mut tracker = UsageTracker::new();
        let node_id1 = NodeId(std::num::NonZeroU32::new(1).unwrap());
        let node_id2 = NodeId(std::num::NonZeroU32::new(2).unwrap());
        
        tracker.register_chunk(0, node_id1);
        tracker.register_chunk(1, node_id2);
        
        tracker.record_execution(0, 100);
        tracker.record_execution(1, 200);
        
        let all_stats = tracker.get_all_stats();
        assert_eq!(all_stats.len(), 2);
        assert!(all_stats.contains_key(&node_id1));
        assert!(all_stats.contains_key(&node_id2));
    }
    
    #[test]
    fn test_value_conversions() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(chunk);
        
        let vm = VM::new(bytecode);
        
        // Test VM value to stdlib value conversion
        let vm_val = Value::Int(42);
        let stdlib_val = vm.vm_value_to_stdlib_value(&vm_val);
        match stdlib_val {
            StdlibValue::Int(i) => assert_eq!(i, 42),
            _ => panic!("Expected integer"),
        }
        
        // Test list conversion
        let vm_list = Value::List(vec![Value::Int(1), Value::Int(2)]);
        let stdlib_list = vm.vm_value_to_stdlib_value(&vm_list);
        match stdlib_list {
            StdlibValue::List(_) => {},
            _ => panic!("Expected list"),
        }
    }
}