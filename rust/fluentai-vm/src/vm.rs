//! High-performance stack-based virtual machine

use fluentai_bytecode::{Bytecode, Instruction, Opcode};
use crate::cow_globals::CowGlobals;
use crate::debug::{DebugConfig, StepMode, VMDebugEvent};
use crate::error::{value_type_name, StackFrame, StackTrace, VMError, VMResult};
use crate::gc::{GarbageCollector, GcConfig, GcScope};
#[cfg(feature = "jit")]
use crate::jit_integration::{JitConfig, JitManager};
use crate::safety::{checked_ops, ActorId, ChannelId, IdGenerator, PromiseId, ResourceLimits};
use crate::security::{SecurityManager, SecurityPolicy};
use fluentai_core::ast::{NodeId, UsageStatistics};
use fluentai_core::value::Value;
use fluentai_effects::{runtime::EffectRuntime, EffectContext};
use fluentai_modules::{ModuleLoader, ModuleResolver};
use fluentai_stdlib::value::Value as StdlibValue;
use fluentai_stdlib::{init_stdlib, StdlibRegistry};
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
use std::time::Instant;
use tokio::sync::{mpsc, oneshot};

const STACK_SIZE: usize = 10_000;
const MAX_PRESERVED_LOCALS: usize = 1000;
const FINALLY_NORMAL_MARKER: &str = "__finally_normal__";
const FINALLY_EXCEPTION_MARKER: &str = "__finally_exception__";

/// Bit masks for MakeClosure instruction unpacking
const MAKECLOSURE_CHUNK_ID_SHIFT: u32 = 16;
const MAKECLOSURE_CAPTURE_COUNT_MASK: u32 = 0xFFFF;

/// Actor state and message handling
pub struct Actor {
    /// Current state of the actor
    state: Value,
    /// Handler function that processes messages
    handler: Value,
    /// Mailbox for incoming messages
    mailbox: mpsc::Receiver<Value>,
    /// Sender for the mailbox
    sender: mpsc::Sender<Value>,
}

pub struct CallFrame {
    pub chunk_id: usize,
    pub ip: usize,
    pub stack_base: usize,
    pub env: Vec<Value>, // Captured environment for closures
    #[allow(dead_code)]
    pub start_time: Option<Instant>, // Track when this frame started executing
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
    
    /// Get usage statistics for a specific chunk
    pub fn get_stats_for_chunk(&self, chunk_id: usize) -> Option<UsageStatistics> {
        let node_id = self.chunk_to_node.get(&chunk_id)?;
        self.stats.get(node_id).cloned()
    }
}

pub struct VM {
    bytecode: Arc<Bytecode>,
    stack: Vec<Value>,
    call_stack: Vec<CallFrame>,
    globals: CowGlobals,
    trace: bool,
    effect_context: Arc<EffectContext>,
    effect_runtime: Arc<EffectRuntime>,
    // Async support with typed IDs
    id_generator: IdGenerator,
    promises: FxHashMap<PromiseId, oneshot::Receiver<VMResult<Value>>>,
    channels: FxHashMap<ChannelId, (mpsc::Sender<Value>, mpsc::Receiver<Value>)>,
    // Actor support
    actors: FxHashMap<ActorId, Actor>,
    // Mutable cells
    cells: Vec<Value>,
    // Standard library
    stdlib: StdlibRegistry,
    // Module system
    module_loader: ModuleLoader,
    #[allow(dead_code)]
    module_resolver: ModuleResolver,
    loaded_modules: FxHashMap<String, Value>, // Cache of loaded modules
    current_module: Option<String>,           // Name of currently executing module
    module_stack: Vec<String>,                // Stack of module names for nested module execution
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
    // Error handler stack for try-catch-finally
    error_handler_stack: Vec<ErrorHandler>,
    // Finally block state storage
    finally_states: Vec<FinallyState>,
    // JIT compilation manager
    #[cfg(feature = "jit")]
    jit_manager: JitManager,
}

/// Handler frame for tracking active effect handlers
#[derive(Clone)]
pub struct HandlerFrame {
    /// Map from effect type + operation to handler function value
    handlers: FxHashMap<(String, Option<String>), Value>,
    /// Continuation point - where to return after handler execution
    _return_ip: usize,
    /// Stack depth when handler was installed
    _stack_depth: usize,
}

/// Error handler for try-catch-finally blocks
#[derive(Clone)]
struct ErrorHandler {
    /// Catch handler IP (where to jump on error)
    catch_ip: usize,
    /// Finally handler IP (optional)
    finally_ip: Option<usize>,
    /// Stack depth when handler was installed
    stack_depth: usize,
    /// Call frame index
    call_frame: usize,
    /// Number of local variables at the handler's scope
    locals_count: usize,
}

/// State saved during finally block execution
#[derive(Clone)]
struct FinallyState {
    /// The value to restore after finally (result or error)
    value: Value,
    /// Marker indicating normal or exception path
    marker: Value,
}

impl VM {
    pub fn new(bytecode: Bytecode) -> Self {
        Self::with_shared_bytecode(Arc::new(bytecode))
    }
    
    pub fn with_shared_bytecode(bytecode: Arc<Bytecode>) -> Self {
        Self {
            bytecode,
            stack: Vec::with_capacity(STACK_SIZE),
            call_stack: Vec::new(),
            globals: CowGlobals::new(),
            trace: false,
            effect_context: Arc::new(EffectContext::default()),
            effect_runtime: Arc::new(EffectRuntime::default()),
            id_generator: IdGenerator::new(),
            promises: FxHashMap::default(),
            channels: FxHashMap::default(),
            actors: FxHashMap::default(),
            cells: Vec::new(),
            stdlib: init_stdlib(),
            module_loader: ModuleLoader::new(fluentai_modules::ModuleConfig::default()),
            module_resolver: ModuleResolver::new(ModuleLoader::new(
                fluentai_modules::ModuleConfig::default(),
            )),
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
            error_handler_stack: Vec::new(),
            finally_states: Vec::new(),
            #[cfg(feature = "jit")]
            jit_manager: JitManager::new(JitConfig::default()),
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
        self.error_handler_stack.clear();
        self.finally_states.clear();

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

    pub fn get_effect_context(&self) -> Arc<EffectContext> {
        self.effect_context.clone()
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
            Ok(Value::GcHandle(Arc::new(handle)))
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

    pub fn get_globals(&self) -> FxHashMap<String, Value> {
        self.globals.as_map()
    }

    pub fn get_call_stack_depth(&self) -> usize {
        self.call_stack.len()
    }

    /// Get usage statistics for a specific node
    pub fn get_usage_stats(&self, node_id: NodeId) -> Option<UsageStatistics> {
        self.usage_tracker
            .as_ref()?
            .read()
            .ok()?
            .get_stats(node_id)
            .cloned()
    }

    /// Get all usage statistics
    pub fn get_all_usage_stats(&self) -> Option<FxHashMap<NodeId, UsageStatistics>> {
        self.usage_tracker
            .as_ref()?
            .read()
            .ok()
            .map(|tracker| tracker.get_all_stats().clone())
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

    /// Run until the VM completes and return the final value
    pub fn run_until_complete(&mut self) -> VMResult<Value> {
        // This is similar to run() but doesn't set up the initial call frame
        // as it's expected to be already set up by the caller
        self.run_inner()
    }
    
    fn run_inner(&mut self) -> VMResult<Value> {
        loop {
            let frame = self
                .call_stack
                .last()
                .ok_or_else(|| VMError::StackUnderflow {
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

            // Check for breakpoints
            if self.debug_config.enabled && self.debug_config.should_break(ip) {
                self.debug_config
                    .send_event(VMDebugEvent::Breakpoint { pc: ip });
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

    pub fn execute_instruction(
        &mut self,
        instruction: &Instruction,
        chunk_id: usize,
    ) -> VMResult<VMState> {
        use crate::opcode_handlers::{
            ArithmeticHandler, StackHandler, ControlFlowHandler,
            MemoryHandler, CollectionsHandler, ConcurrentHandler,
            EffectsHandler, OpcodeHandler
        };
        use Opcode::*;
        
        // Create handler instances
        let mut arithmetic_handler = ArithmeticHandler;
        let mut stack_handler = StackHandler;
        let mut control_flow_handler = ControlFlowHandler;
        let mut memory_handler = MemoryHandler;
        let mut collections_handler = CollectionsHandler;
        let mut concurrent_handler = ConcurrentHandler;
        let mut effects_handler = EffectsHandler;
        
        // Dispatch to appropriate handler based on opcode category
        match instruction.opcode {
            // Stack operations
            Push | Pop | PopN | Dup | Swap |
            PushInt0 | PushInt1 | PushInt2 | PushIntSmall |
            PushTrue | PushFalse | PushNil | PushConst => {
                return stack_handler.execute(self, instruction, chunk_id);
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
            PushInt0 => self.push(Value::Integer(0))?,
            PushInt1 => self.push(Value::Integer(1))?,
            PushInt2 => self.push(Value::Integer(2))?,
            PushIntSmall => self.push(Value::Integer(instruction.arg as i64))?,
            PushTrue => self.push(Value::Boolean(true))?,
            PushFalse => self.push(Value::Boolean(false))?,
            PushNil => self.push(Value::Nil)?,

            PushConst => {
                let const_idx = instruction.arg as usize;
                let value = self.bytecode.chunks[chunk_id]
                    .constants
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

            Sub => self.binary_op(|a, b| match (a, b) {
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

            Mul => self.binary_op(|a, b| match (a, b) {
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

            Div => self.binary_op(|a, b| match (a, b) {
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

            Mod => self.binary_op(|a, b| match (a, b) {
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
                let value = self.pop()?;
                match value {
                    Value::Integer(x) => {
                        let negated =
                            checked_ops::neg_i64(x).map_err(|_| VMError::IntegerOverflow {
                                operation: "neg".to_string(),
                                operands: (x, 0),
                                stack_trace: None,
                            })?;
                        self.push(Value::Integer(negated))?
                    }
                    Value::Float(x) => self.push(Value::Float(-x))?,
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
            AddInt => self.binary_int_op(|x, y| {
                checked_ops::add_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "add_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            SubInt => self.binary_int_op(|x, y| {
                checked_ops::sub_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "sub_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            MulInt => self.binary_int_op(|x, y| {
                checked_ops::mul_i64(x, y).map_err(|_| VMError::IntegerOverflow {
                    operation: "mul_int".to_string(),
                    operands: (x, y),
                    stack_trace: None,
                })
            })?,
            DivInt => self.binary_int_op(|x, y| {
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
            AddFloat => self.binary_float_op(|x, y| x + y)?,
            SubFloat => self.binary_float_op(|x, y| x - y)?,
            MulFloat => self.binary_float_op(|x, y| x * y)?,
            DivFloat => self.binary_float_op(|x, y| x / y)?,

            // Comparison
            Eq => {
                let b = self.pop()?;
                let a = self.pop()?;
                let equal = self.values_equal(&a, &b);
                self.push(Value::Boolean(equal))?;
            }
            Ne => {
                let b = self.pop()?;
                let a = self.pop()?;
                let equal = self.values_equal(&a, &b);
                self.push(Value::Boolean(!equal))?;
            }

            Lt => self.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x < y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x < y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x < y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "lt".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,

            Le => self.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x <= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x <= y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x <= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "le".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,

            Gt => self.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x > y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x > y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x > y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "gt".to_string(),
                    expected: "int/float/string".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,

            Ge => self.binary_op(|a, b| match (a, b) {
                (Value::Integer(x), Value::Integer(y)) => Ok(Value::Boolean(x >= y)),
                (Value::Float(x), Value::Float(y)) => Ok(Value::Boolean(x >= y)),
                (Value::String(x), Value::String(y)) => Ok(Value::Boolean(x >= y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "ge".to_string(),
                    expected: "int/float/string".to_string(),
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
                (Value::Boolean(x), Value::Boolean(y)) => Ok(Value::Boolean(x && y)),
                (a, b) => Err(VMError::TypeError {
                    operation: "and".to_string(),
                    expected: "bool".to_string(),
                    got: format!("{} and {}", value_type_name(&a), value_type_name(&b)),
                    location: None,
                    stack_trace: None,
                }),
            })?,

            Or => self.binary_op(|a, b| match (a, b) {
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
                let value = self.pop()?;
                match value {
                    Value::Boolean(x) => self.push(Value::Boolean(!x))?,
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                let frame = self
                    .call_stack
                    .last()
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
                    _ => {
                        return Err(VMError::InvalidConstantIndex {
                            index: name_idx as u32,
                            max_index: self.bytecode.chunks[chunk_id].constants.len(),
                            stack_trace: None,
                        })
                    }
                };

                // Check user-defined globals first
                let value = if let Some(global_value) = self.globals.get(name) {
                    global_value.clone()
                } else if self.stdlib.contains(name) {
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
                    // Not found anywhere, return nil
                    Value::Nil
                };

                self.push(value)?;
            }

            StoreGlobal => {
                let name_idx = instruction.arg as usize;
                let name = match &self.bytecode.chunks[chunk_id].constants.get(name_idx) {
                    Some(Value::String(s)) => s.clone(),
                    _ => {
                        return Err(VMError::InvalidConstantIndex {
                            index: name_idx as u32,
                            max_index: self.bytecode.chunks[chunk_id].constants.len(),
                            stack_trace: None,
                        })
                    }
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
                    Value::List(items) => self.push(Value::Integer(items.len() as i64))?,
                    Value::Vector(items) => self.push(Value::Integer(items.len() as i64))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "list_len".to_string(),
                            expected: "list/vector".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            ListEmpty => {
                let list = self.pop()?;
                match list {
                    Value::List(items) => self.push(Value::Boolean(items.is_empty()))?,
                    Value::Vector(items) => self.push(Value::Boolean(items.is_empty()))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "list_empty".to_string(),
                            expected: "list/vector".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    Value::Vector(items) => {
                        if items.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take head of empty vector".to_string(),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                        self.push(items[0].clone())?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "list_head".to_string(),
                            expected: "list/vector".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    Value::Vector(items) => {
                        if items.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "Cannot take tail of empty vector".to_string(),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                        let tail = items[1..].to_vec();
                        self.push(Value::Vector(tail))?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "list_tail".to_string(),
                            expected: "list/vector".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    Value::Vector(mut items) => {
                        items.insert(0, elem);
                        self.push(Value::Vector(items))?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "list_cons".to_string(),
                            expected: "list/vector as second argument".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            // Strings
            StrLen => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::Integer(s.len() as i64))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_len".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_upper".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            StrLower => {
                let string = self.pop()?;
                match string {
                    Value::String(s) => self.push(Value::String(s.to_lowercase()))?,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "str_lower".to_string(),
                            expected: "string".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "effect".to_string(),
                            expected: "string for operation name".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "effect".to_string(),
                            expected: "string for effect type".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    _ => {
                        return Err(VMError::RuntimeError {
                            message: format!("Unknown effect type: {}", effect_type_str),
                            stack_trace: Some(self.build_stack_trace()),
                        })
                    }
                };

                // Check handler stack for a matching handler
                let mut handler_result = None;

                // Search from most recent to oldest handler
                for handler_frame in self.handler_stack.iter().rev() {
                    // Check for exact match first (effect_type + operation)
                    if let Some(handler_fn) = handler_frame
                        .handlers
                        .get(&(effect_type_str.clone(), Some(operation.clone())))
                    {
                        // Found a specific handler for this effect + operation
                        // Call the handler function with args
                        handler_result =
                            Some(self.call_handler_function(handler_fn.clone(), args.clone())?);
                        break;
                    }

                    // Check for general handler (effect_type only)
                    if let Some(handler_fn) =
                        handler_frame.handlers.get(&(effect_type_str.clone(), None))
                    {
                        // Found a general handler for this effect type
                        // Create a list with operation as first argument, followed by other args
                        let mut handler_args = vec![Value::String(operation.clone())];
                        handler_args.extend(args.clone());
                        handler_result =
                            Some(self.call_handler_function(handler_fn.clone(), handler_args)?);
                        break;
                    }
                }

                if let Some(result) = handler_result {
                    // Handler was called, push its result
                    self.push(result)?;
                } else {
                    // No handler found, execute the default effect
                    // Convert VM values to core values for effect handlers
                    let core_args: Vec<fluentai_core::value::Value> = args
                        .iter()
                        .map(|v| self.vm_value_to_core_value(v))
                        .collect();

                    // Execute the effect synchronously
                    let result = self
                        .effect_context
                        .perform_sync(effect_type, &operation, &core_args)
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "effect_async".to_string(),
                            expected: "string for operation name".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                let effect_type_str = match self.pop()? {
                    Value::String(s) => s,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "effect_async".to_string(),
                            expected: "string for effect type".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    _ => {
                        return Err(VMError::RuntimeError {
                            message: format!("Unknown effect type: {}", effect_type_str),
                            stack_trace: Some(self.build_stack_trace()),
                        })
                    }
                };

                // Generate a promise ID
                let promise_id = self.id_generator.next_promise_id();

                // Convert VM values to core values
                let core_args: Vec<fluentai_core::value::Value> = args
                    .iter()
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
                    let result = effect_context
                        .perform_async(effect_type, &operation, &core_args)
                        .await
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
                self.push(Value::Promise(promise_id.0))?;
            }

            Await => {
                // Pop the value to await
                let value = self.pop()?;
                
                match value {
                    // Handle Future - execute it and return the result
                    Value::Future { chunk_id, env } => {
                        
                        // Execute the future by spawning it and waiting for result
                        let promise_id = self.id_generator.next_promise_id();
                        let (sender, receiver) = oneshot::channel();
                        
                        
                        // Store the promise receiver
                        self.promises.insert(promise_id, receiver);
                        
                        // Create a new VM instance for the future execution
                        let mut new_vm = VM::with_shared_bytecode(Arc::clone(&self.bytecode));
                        new_vm.set_effect_context(self.effect_context.clone());
                        new_vm.set_effect_runtime(self.effect_runtime.clone());
                        new_vm.resource_limits = self.resource_limits.clone();
                        new_vm.debug_config = self.debug_config.clone();
                        new_vm.security_manager = self.security_manager.clone();
                        new_vm.gc = self.gc.clone();
                        new_vm.usage_tracker = self.usage_tracker.clone();
                        
                        // Copy global state to new VM
                        new_vm.globals = self.globals.clone();
                        
                        // Create the function value from future
                        let function = Value::Function { chunk_id, env };
                        
                        // Spawn the task using the effect runtime
                        self.effect_runtime.spawn(async move {
                            let result = {
                                // Push function onto stack
                                if let Err(e) = new_vm.push(function) {
                                    let _ = sender.send(Err(e));
                                    return;
                                }
                                
                                // Call the function with 0 arguments
                                // call_value runs the function to completion
                                if let Err(e) = new_vm.call_value(0) {
                                    let _ = sender.send(Err(e));
                                    return;
                                }
                                
                                
                                // call_value already ran the function, now get the result
                                if new_vm.stack.is_empty() {
                                    Err(VMError::StackUnderflow {
                                        operation: "get_future_result".to_string(),
                                        stack_size: 0,
                                        stack_trace: None,
                                    })
                                } else {
                                    Ok(new_vm.pop().unwrap())
                                }
                            };
                            
                            // Send the result
                            let _ = sender.send(result);
                        });
                        
                        // Use a blocking channel to wait for the result
                        let (tx, rx) = std::sync::mpsc::channel();
                        
                        // Get the receiver from promises
                        if let Some(promise_rx) = self.promises.remove(&promise_id) {
                            // Spawn a task to wait for the promise and send result via channel
                            self.effect_runtime.spawn(async move {
                                match promise_rx.await {
                                    Ok(result) => {
                                        let _ = tx.send(result);
                                    }
                                    Err(_) => {
                                        let _ = tx.send(Err(VMError::AsyncError {
                                            message: "Promise channel closed".to_string(),
                                            stack_trace: None,
                                        }));
                                    }
                                }
                            });
                            
                            // Block waiting for the result
                            match rx.recv() {
                                Ok(Ok(value)) => self.push(value)?,
                                Ok(Err(e)) => return Err(e),
                                Err(_) => {
                                    return Err(VMError::AsyncError {
                                        message: "Failed to receive future result".to_string(),
                                        stack_trace: None,
                                    });
                                }
                            }
                        } else {
                            return Err(VMError::AsyncError {
                                message: "Future promise not found".to_string(),
                                stack_trace: None,
                            });
                        }
                    }
                    
                    // Handle Promise - existing implementation
                    Value::Promise(id) => {
                        let promise_id = PromiseId(id);

                // Check if we have this promise
                if let Some(mut rx) = self.promises.remove(&promise_id) {
                    // First check if promise is already resolved (non-blocking)
                    match rx.try_recv() {
                        Ok(result) => {
                            // Promise was already resolved
                            match result {
                                Ok(value) => self.push(value)?,
                                Err(e) => return Err(e),
                            }
                        }
                        Err(oneshot::error::TryRecvError::Empty) => {
                            // Promise not ready yet - try to use runtime to wait
                            if let Some(recv_result) = self.effect_runtime.try_block_on(Box::pin(async move {
                                rx.await
                            })) {
                                // Successfully waited for promise with runtime
                                match recv_result {
                                    Ok(Ok(value)) => self.push(value)?,
                                    Ok(Err(e)) => return Err(e),
                                    Err(_) => {
                                        return Err(VMError::AsyncError {
                                            message: "Promise channel closed".to_string(),
                                            stack_trace: None,
                                        });
                                    }
                                }
                            } else {
                                // No runtime available and promise not ready
                                return Err(VMError::AsyncError {
                                    message: "Cannot await promise - no async runtime available and promise not ready".to_string(),
                                    stack_trace: None,
                                });
                            }
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
                        name: promise_id.to_string(),
                        location: None,
                        stack_trace: None,
                    });
                }
            }
            
            // Handle invalid types for await
            v => {
                return Err(VMError::TypeError {
                    operation: "await".to_string(),
                    expected: "future or promise".to_string(),
                    got: value_type_name(&v).to_string(),
                    location: None,
                    stack_trace: None,
                })
            }
        }
    }

            Spawn => {
                // Pop function value
                let function = self.pop()?;

                // Create promise for the spawned task
                let promise_id = self.id_generator.next_promise_id();
                let (sender, receiver) = oneshot::channel();

                // Store the promise receiver
                self.promises.insert(promise_id, receiver);

                // Create a new VM instance for the spawned task
                let mut new_vm = VM::with_shared_bytecode(Arc::clone(&self.bytecode));
                new_vm.set_effect_context(self.effect_context.clone());
                new_vm.set_effect_runtime(self.effect_runtime.clone());
                new_vm.resource_limits = self.resource_limits.clone();
                new_vm.debug_config = self.debug_config.clone();
                new_vm.security_manager = self.security_manager.clone();
                new_vm.gc = self.gc.clone();
                new_vm.usage_tracker = self.usage_tracker.clone();

                // Copy global state to new VM
                new_vm.globals = self.globals.clone();
                // Fix for spawn integration test: Copy channel senders so spawned tasks can send to them
                // We can't copy receivers, so we only copy the senders
                for (channel_id, (tx, _)) in &self.channels {
                    // Clone only the sender - the receiver stays with the original VM
                    new_vm.channels.insert(*channel_id, (tx.clone(), 
                        // Create a dummy receiver that will error if used
                        mpsc::channel(1).1));
                }

                // Spawn the task using the effect runtime
                self.effect_runtime.spawn(async move {
                    let result = {
                        // Push function onto stack
                        if let Err(e) = new_vm.push(function) {
                            let _ = sender.send(Err(e));
                            return;
                        }

                        // Call the function with no arguments
                        if let Err(e) = new_vm.call_value(0) {
                            let _ = sender.send(Err(e));
                            return;
                        }
                        
                        // call_value executes the function to completion, so the result should be on the stack
                        match new_vm.pop() {
                            Ok(value) => Ok(value),
                            Err(e) => Err(e),
                        }
                    };

                    let _ = sender.send(result);
                });

                // Return a promise that represents the spawned task
                self.push(Value::Promise(promise_id.0))?;
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

                // Create a new channel with default capacity
                let channel_id = self.id_generator.next_channel_id();
                let (tx, rx) = mpsc::channel(1); // Default capacity of 1
                self.channels.insert(channel_id, (tx, rx));
                self.push(Value::Channel(channel_id.0))?;
            }
            
            ChannelWithCapacity => {
                // Pop capacity from stack
                let capacity = match self.pop()? {
                    Value::Integer(n) if n > 0 => n as usize,
                    Value::Integer(n) => {
                        return Err(VMError::RuntimeError {
                            message: format!("Channel capacity must be positive, got {}", n),
                            stack_trace: None,
                        });
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "channel".to_string(),
                            expected: "positive integer".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        });
                    }
                };
                
                // Check resource limit
                if self.channels.len() >= self.resource_limits.max_channels {
                    return Err(VMError::ResourceLimitExceeded {
                        resource: "channels".to_string(),
                        limit: self.resource_limits.max_channels,
                        requested: self.channels.len() + 1,
                        stack_trace: None,
                    });
                }
                
                // Check capacity doesn't exceed limit
                if capacity > self.resource_limits.channel_buffer_size {
                    return Err(VMError::ResourceLimitExceeded {
                        resource: "channel buffer".to_string(),
                        limit: self.resource_limits.channel_buffer_size,
                        requested: capacity,
                        stack_trace: None,
                    });
                }
                
                // Create a new channel with specified capacity
                let channel_id = self.id_generator.next_channel_id();
                let (tx, rx) = mpsc::channel(capacity);
                self.channels.insert(channel_id, (tx, rx));
                self.push(Value::Channel(channel_id.0))?;
            }

            Send => {
                // Pop value and channel
                let value = self.pop()?;
                let channel_id = match self.pop()? {
                    Value::Channel(id) => ChannelId(id),
                    v => {
                        return Err(VMError::TypeError {
                            operation: "send".to_string(),
                            expected: "channel".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                // Get the channel sender
                if let Some((tx, _)) = self.channels.get(&channel_id) {
                    tx.try_send(value).map_err(|e| match e {
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
                    Value::Channel(id) => ChannelId(id),
                    v => {
                        return Err(VMError::TypeError {
                            operation: "receive".to_string(),
                            expected: "channel".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                // Blocking receive - wait until a value is available
                // Fix for spawn integration test: Receive should block, not return nil
                
                // First check if runtime is available
                let has_runtime = self.effect_runtime.try_handle().is_some();
                
                if let Some((_, rx)) = self.channels.get_mut(&channel_id) {
                    if has_runtime {
                        // Try to receive with a timeout first to avoid blocking forever
                        match rx.try_recv() {
                            Ok(value) => self.push(value)?,
                            Err(mpsc::error::TryRecvError::Empty) => {
                                // Channel is empty, we need to wait
                                // Since we can't move rx, we'll use a different approach
                                // We'll loop with small sleeps until a value is available
                                let mut attempts = 0;
                                const MAX_ATTEMPTS: u32 = 1000; // 10 seconds with 10ms sleeps
                                
                                loop {
                                    match rx.try_recv() {
                                        Ok(value) => {
                                            self.push(value)?;
                                            break;
                                        }
                                        Err(mpsc::error::TryRecvError::Empty) => {
                                            attempts += 1;
                                            if attempts >= MAX_ATTEMPTS {
                                                // Timeout - push nil
                                                self.push(Value::Nil)?;
                                                break;
                                            }
                                            // Sleep briefly
                                            std::thread::sleep(std::time::Duration::from_millis(10));
                                        }
                                        Err(mpsc::error::TryRecvError::Disconnected) => {
                                            return Err(VMError::AsyncError {
                                                message: "Channel disconnected".to_string(),
                                                stack_trace: None,
                                            });
                                        }
                                    }
                                }
                            }
                            Err(mpsc::error::TryRecvError::Disconnected) => {
                                return Err(VMError::AsyncError {
                                    message: "Channel disconnected".to_string(),
                                    stack_trace: None,
                                });
                            }
                        }
                    } else {
                        // No runtime available, fall back to try_recv
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
                    }
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: format!("channel:{}", channel_id),
                        location: None,
                        stack_trace: None,
                    });
                }
            }
            
            TrySend => {
                // Pop value and channel
                let value = self.pop()?;
                let channel_id = match self.pop()? {
                    Value::Channel(id) => ChannelId(id),
                    v => {
                        return Err(VMError::TypeError {
                            operation: "try-send".to_string(),
                            expected: "channel".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };
                // Get the channel sender
                if let Some((tx, _)) = self.channels.get(&channel_id) {
                    match tx.try_send(value) {
                        Ok(_) => {
                            // Success - push a success result (true)
                            self.push(Value::Boolean(true))?;
                        }
                        Err(mpsc::error::TrySendError::Full(_)) => {
                            // Channel full - push a failure result (false)
                            self.push(Value::Boolean(false))?;
                        }
                        Err(mpsc::error::TrySendError::Closed(_)) => {
                            // Channel closed - could return false or error
                            // For now, return false to indicate failure
                            self.push(Value::Boolean(false))?;
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
            
            TryReceive => {
                // Pop channel
                let channel_id = match self.pop()? {
                    Value::Channel(id) => ChannelId(id),
                    v => {
                        return Err(VMError::TypeError {
                            operation: "try-receive".to_string(),
                            expected: "channel".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };
                // Try to receive non-blocking
                if let Some((_, rx)) = self.channels.get_mut(&channel_id) {
                    match rx.try_recv() {
                        Ok(value) => {
                            // Success - push a list with [true, value]
                            self.push(Value::List(vec![Value::Boolean(true), value]))?;
                        }
                        Err(mpsc::error::TryRecvError::Empty) => {
                            // No value available - push [false, nil]
                            self.push(Value::List(vec![Value::Boolean(false), Value::Nil]))?;
                        }
                        Err(mpsc::error::TryRecvError::Disconnected) => {
                            // Channel disconnected - push [false, nil]
                            self.push(Value::List(vec![Value::Boolean(false), Value::Nil]))?;
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
            
            Select => {
                // Select is now compiled to use TryReceive and jumps
                // This opcode should not be reached
                return Err(VMError::InvalidOpcode {
                    opcode: Opcode::Select as u8,
                    location: None,
                });
            }
            
            // Actor model opcodes
            CreateActor => {
                // Pop handler and initial state
                let handler = self.pop()?;
                let initial_state = self.pop()?;
                
                // Validate handler is a function
                match &handler {
                    Value::Function { .. } | Value::Procedure(_) => {},
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "actor".to_string(),
                            expected: "function".to_string(),
                            got: value_type_name(&handler).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
                
                // Create actor ID and mailbox
                let actor_id = self.id_generator.next_actor_id();
                let (tx, rx) = mpsc::channel(100); // Buffer size of 100
                
                // Create actor
                let actor = Actor {
                    state: initial_state,
                    handler,
                    mailbox: rx,
                    sender: tx,
                };
                
                // Store actor
                self.actors.insert(actor_id, actor);
                
                // Push actor ID
                self.push(Value::Actor(actor_id.0))?;
            }
            
            ActorSend => {
                // Pop message and actor
                let message = self.pop()?;
                let actor_id = match self.pop()? {
                    Value::Actor(id) => ActorId(id),
                    v => {
                        return Err(VMError::TypeError {
                            operation: "!".to_string(),
                            expected: "actor".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };
                
                // Get actor's sender
                if let Some(actor) = self.actors.get(&actor_id) {
                    // Send message (non-blocking)
                    match actor.sender.try_send(message) {
                        Ok(_) => {
                            // Success - push nil
                            self.push(Value::Nil)?;
                        }
                        Err(_) => {
                            // Mailbox full or closed - for now just push nil
                            // In the future we might want to handle this differently
                            self.push(Value::Nil)?;
                        }
                    }
                } else {
                    return Err(VMError::UnknownIdentifier {
                        name: format!("actor:{}", actor_id),
                        location: None,
                        stack_trace: None,
                    });
                }
            }
            
            ActorReceive => {
                // Actor receive is complex and needs proper implementation
                // For now, just push nil
                self.push(Value::Nil)?;
            }
            
            Become => {
                // Become is only valid within actor context
                // For now, just pop the new state and push nil
                let _new_state = self.pop()?;
                self.push(Value::Nil)?;
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
            
            MakeFuture => {
                // Pop the function and convert it to a future
                let func = self.pop()?;
                match func {
                    Value::Function { chunk_id, env } => {
                        let future = Value::Future { chunk_id, env };
                        self.push(future)?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "make_future".to_string(),
                            expected: "function".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            MakeClosure => {
                // MakeClosure bit unpacking:
                // The packed argument contains both the chunk ID and capture count
                // Upper 16 bits: chunk_id - which bytecode chunk contains the function
                // Lower 16 bits: capture_count - how many values to capture from stack
                let packed = instruction.arg;
                let chunk_id = (packed >> MAKECLOSURE_CHUNK_ID_SHIFT) as usize;
                let capture_count = (packed & MAKECLOSURE_CAPTURE_COUNT_MASK) as usize;

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
                let frame = self
                    .call_stack
                    .last()
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
                                _ => {
                                    return Err(VMError::TypeError {
                                        operation: "cons".to_string(),
                                        expected: "list as second argument".to_string(),
                                        got: value_type_name(&args[1]).to_string(),
                                        location: None,
                                        stack_trace: None,
                                    })
                                }
                            }
                        } else {
                            // For other built-ins, generate appropriate instructions
                            // This is a simplified approach - in a full implementation,
                            // we might want to directly execute the operations
                            return Err(VMError::RuntimeError {
                                message: format!(
                                    "Built-in function {} should be optimized by compiler",
                                    builtin_name
                                ),
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
                                let stdlib_args: Vec<StdlibValue> = args
                                    .iter()
                                    .map(|v| self.vm_value_to_stdlib_value(v))
                                    .collect();
                                let result =
                                    self.call_higher_order_stdlib(func_name, &stdlib_args)?;
                                let vm_result = self.stdlib_value_to_vm_value(&result);
                                self.push(vm_result)?;
                            }
                            _ => {
                                // Regular stdlib function
                                if let Some(stdlib_func) = self.stdlib.get(func_name) {
                                    // Convert VM values to stdlib values
                                    let stdlib_args: Vec<StdlibValue> = args
                                        .iter()
                                        .map(|v| self.vm_value_to_stdlib_value(v))
                                        .collect();

                                    // Create StdlibContext with effect context
                                    // Note: Higher-order functions (map, filter, fold) are handled
                                    // specially in the VM via stdlib_bridge to avoid callback complexity
                                    let mut context =
                                        fluentai_stdlib::vm_bridge::StdlibContext::default();
                                    context.effect_context_override =
                                        Some(self.effect_context.clone());

                                    // Call the stdlib function with context
                                    let stdlib_result = stdlib_func
                                        .call_with_context(&mut context, &stdlib_args)?;

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
                    Value::NativeFunction {
                        function, arity, ..
                    } => {
                        // Check arity
                        if args.len() != *arity {
                            return Err(VMError::RuntimeError {
                                message: format!(
                                    "Native function expects {} arguments, got {}",
                                    arity,
                                    args.len()
                                ),
                                stack_trace: None,
                            });
                        }

                        // Call the native function
                        let result = function(&args).map_err(|e| VMError::RuntimeError {
                            message: format!("Native function error: {}", e),
                            stack_trace: None,
                        })?;

                        // Push result
                        self.push(result)?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "call".to_string(),
                            expected: "function".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "cell_get".to_string(),
                            expected: "cell".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "cell_set".to_string(),
                            expected: "cell".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                    v => {
                        return Err(VMError::TypeError {
                            operation: "make_tagged".to_string(),
                            expected: "string for tag".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                self.push(Value::Tagged { tag, values })?;
            }

            GetTag => {
                let value = self.pop()?;
                match value {
                    Value::Tagged { tag, .. } => {
                        self.push(Value::String(tag))?;
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "get_tag".to_string(),
                            expected: "tagged value".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                                message: format!(
                                    "Tagged field index {} out of bounds (size: {})",
                                    field_idx,
                                    values.len()
                                ),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "get_tagged_field".to_string(),
                            expected: "tagged value".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            IsTagged => {
                let expected_tag_idx = instruction.arg as usize;
                let expected_tag = match self.bytecode.chunks[chunk_id]
                    .constants
                    .get(expected_tag_idx)
                {
                    Some(Value::String(s)) => s,
                    _ => {
                        return Err(VMError::InvalidConstantIndex {
                            index: expected_tag_idx as u32,
                            max_index: self.bytecode.chunks[chunk_id].constants.len(),
                            stack_trace: None,
                        })
                    }
                };

                let value = self.peek(0)?;
                let is_match = match value {
                    Value::Tagged { tag, .. } => tag == expected_tag,
                    _ => false,
                };

                self.push(Value::Boolean(is_match))?;
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
                self.module_stack
                    .push(self.current_module.clone().unwrap_or_default());
                self.current_module = Some(module_name);
            }

            EndModule => {
                if let Some(prev_module) = self.module_stack.pop() {
                    self.current_module = if prev_module.is_empty() {
                        None
                    } else {
                        Some(prev_module)
                    };
                }
            }

            ExportBinding => {
                let binding_name = self.get_constant_string(instruction.arg)?;
                if let Some(current_module_name) = &self.current_module {
                    let value = self.peek(0)?.clone();

                    // Get or create the module in loaded_modules
                    let module = self
                        .loaded_modules
                        .entry(current_module_name.clone())
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
                    Value::GcHandle(any_handle) => {
                        if let Some(gc_handle) = any_handle.downcast_ref::<crate::gc::GcHandle>() {
                            let value = gc_handle.get();
                            self.push(value)?;
                        } else {
                            return Err(VMError::TypeError {
                                operation: "gc_deref".to_string(),
                                expected: "GC handle".to_string(),
                                got: "invalid GC handle type".to_string(),
                                location: None,
                                stack_trace: None,
                            });
                        }
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "gc_deref".to_string(),
                            expected: "GC handle".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                }
            }

            GcSet => {
                let value = self.pop()?;
                let handle = self.pop()?;
                match handle {
                    Value::GcHandle(any_handle) => {
                        if let Some(gc_handle) = any_handle.downcast_ref::<crate::gc::GcHandle>() {
                            gc_handle.set(value);
                            self.push(Value::Nil)?;
                        } else {
                            return Err(VMError::TypeError {
                                operation: "gc_set".to_string(),
                                expected: "GC handle".to_string(),
                                got: "invalid GC handle type".to_string(),
                                location: None,
                                stack_trace: None,
                            });
                        }
                    }
                    v => {
                        return Err(VMError::TypeError {
                            operation: "gc_set".to_string(),
                            expected: "GC handle".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
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
                        Value::NativeFunction { .. } => {
                            // Native functions don't support tail call optimization
                            // Fall back to regular call semantics
                            return Err(VMError::RuntimeError {
                                message: "Native functions do not support tail call optimization"
                                    .to_string(),
                                stack_trace: None,
                            });
                        }
                        v => {
                            return Err(VMError::TypeError {
                                operation: "tail_call".to_string(),
                                expected: "function".to_string(),
                                got: value_type_name(&v).to_string(),
                                location: None,
                                stack_trace: None,
                            })
                        }
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
                        v => {
                            return Err(VMError::TypeError {
                                operation: "make_handler".to_string(),
                                expected: "string or nil for operation filter".to_string(),
                                got: value_type_name(&v).to_string(),
                                location: None,
                                stack_trace: None,
                            })
                        }
                    };

                    // Pop effect type
                    let effect_type = match self.pop()? {
                        Value::String(s) => s,
                        v => {
                            return Err(VMError::TypeError {
                                operation: "make_handler".to_string(),
                                expected: "string for effect type".to_string(),
                                got: value_type_name(&v).to_string(),
                                location: None,
                                stack_trace: None,
                            })
                        }
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
                handler_data.insert("_count".to_string(), Value::Integer(handler_count as i64));

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
                    _ => {
                        return Err(VMError::TypeError {
                            operation: "install_handler".to_string(),
                            expected: "handler map".to_string(),
                            got: value_type_name(&handler).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };

                // Create handler frame with current execution context
                let current_frame =
                    self.call_stack
                        .last()
                        .ok_or_else(|| VMError::RuntimeError {
                            message: "Cannot install handler without active call frame".to_string(),
                            stack_trace: None,
                        })?;

                let handler_frame = HandlerFrame {
                    handlers,
                    _return_ip: current_frame.ip,
                    _stack_depth: self.stack.len(),
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

            // Error handling opcodes
            Try => {
                // Begin try block - push error handler
                // For now, just continue execution
                // Proper implementation would set up error handling context
            }
            
            Catch => {
                // Catch errors matching pattern
                // For now, just continue
                // Proper implementation would check error type and jump if no match
            }
            
            Finally => {
                // Start of finally block
                // Stack has [value, marker] that we need to preserve
                // We save them and let the finally block run with clean stack
                
                if self.stack.len() < 2 {
                    return Err(VMError::RuntimeError {
                        message: "Finally expects at least 2 values on stack".to_string(),
                        stack_trace: None,
                    });
                }
                
                // Pop and save the marker and value
                let marker = self.pop()?;
                let value = self.pop()?;
                
                // Check for resource limits to prevent unbounded growth
                const MAX_FINALLY_DEPTH: usize = 1000;
                if self.finally_states.len() >= MAX_FINALLY_DEPTH {
                    return Err(VMError::RuntimeError {
                        message: format!("Finally block nesting limit exceeded: {}", MAX_FINALLY_DEPTH),
                        stack_trace: None,
                    });
                }
                
                // Save state to dedicated storage
                self.finally_states.push(FinallyState { value, marker });
                
                // Continue with finally block execution with clean stack
            }
            
            EndFinally => {
                // End of finally block
                // Retrieve saved values and handle appropriately
                
                // Pop saved state from dedicated storage
                let state = self.finally_states.pop()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "EndFinally: no saved finally state".to_string(),
                        stack_trace: None,
                    })?;
                
                match state.marker {
                    Value::Symbol(s) if s == FINALLY_NORMAL_MARKER => {
                        // Normal path - push value back and continue
                        self.push(state.value)?;
                    }
                    Value::Symbol(s) if s == FINALLY_EXCEPTION_MARKER => {
                        // Exception path - re-throw the error
                        // Look for the next error handler (catch block)
                        if let Some(handler) = self.error_handler_stack.pop() {
                            // Unwind stack to handler's depth with bounds checking
                            let target_depth = handler.stack_depth.min(self.stack.len());
                            while self.stack.len() > target_depth {
                                self.stack.pop();
                            }
                            
                            // Push error value for catch handler
                            self.push(state.value)?;
                            
                            // Jump to catch handler
                            if let Some(frame) = self.call_stack.last_mut() {
                                frame.ip = handler.catch_ip;
                            }
                            
                            // Continue execution at catch handler
                            return Ok(VMState::Continue);
                        } else {
                            // No handler found, convert to runtime error
                            return Err(VMError::RuntimeError {
                                message: format!("Uncaught error: {:?}", state.value),
                                stack_trace: Some(self.build_stack_trace()),
                            });
                        }
                    }
                    _ => {
                        return Err(VMError::RuntimeError {
                            message: format!("Invalid finally marker: {:?}", state.marker),
                            stack_trace: None,
                        });
                    }
                }
            }
            
            Throw => {
                // Throw an error
                let error = self.pop()?;
                
                // Look for an error handler
                if let Some(handler) = self.error_handler_stack.pop() {
                    // Check if there's a finally block to execute first
                    if let Some(finally_ip) = handler.finally_ip {
                        // Unwind stack to handler's depth with bounds checking
                        let target_depth = handler.stack_depth.min(self.stack.len());
                        while self.stack.len() > target_depth {
                            self.stack.pop();
                        }
                        
                        // Push error value to preserve it across finally execution
                        self.push(error.clone())?;
                        
                        // Push a special marker to indicate we're in exception finally path
                        self.push(Value::Symbol(FINALLY_EXCEPTION_MARKER.to_string()))?;
                        
                        // Jump to finally block
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.ip = finally_ip;
                        }
                        
                        // Push handler back for catch execution after finally
                        let mut catch_handler = handler.clone();
                        catch_handler.finally_ip = None; // Prevent infinite loop
                        
                        // Check resource limit
                        if self.error_handler_stack.len() >= self.resource_limits.max_error_handlers {
                            return Err(VMError::ResourceLimitExceeded {
                                resource: "error handlers".to_string(),
                                limit: self.resource_limits.max_error_handlers,
                                requested: self.error_handler_stack.len() + 1,
                                stack_trace: None,
                            });
                        }
                        
                        self.error_handler_stack.push(catch_handler);
                        
                        return Ok(VMState::Continue);
                    } else {
                        // No finally block, execute catch directly
                        // Calculate the base of local variables
                        let current_frame = self.call_stack.get(handler.call_frame)
                            .ok_or_else(|| VMError::RuntimeError {
                                message: "Invalid call frame in error handler".to_string(),
                                stack_trace: None,
                            })?;
                        let locals_base = current_frame.stack_base;
                        
                        // Check resource limits to prevent DoS attacks
                        if handler.locals_count > MAX_PRESERVED_LOCALS {
                            return Err(VMError::RuntimeError {
                                message: format!("Too many locals to preserve: {} (max: {})", 
                                               handler.locals_count, MAX_PRESERVED_LOCALS),
                                stack_trace: None,
                            });
                        }
                        
                        // Preserve local variables while unwinding with proper bounds checking
                        let mut preserved_locals = Vec::new();
                        if handler.locals_count > 0 {
                            // Validate locals_base before any stack access
                            if locals_base < self.stack.len() {
                                let locals_end = locals_base + handler.locals_count;
                                if locals_end <= self.stack.len() {
                                    // Safe to preserve locals
                                    preserved_locals = self.stack[locals_base..locals_end].to_vec();
                                }
                            }
                        }
                        
                        // Unwind stack to handler's depth
                        let target_depth = handler.stack_depth.min(self.stack.len());
                        self.stack.truncate(target_depth);
                        
                        // Restore preserved locals if we have any
                        if !preserved_locals.is_empty() && locals_base < self.stack.len() {
                            // Ensure we have enough space on the stack
                            let needed_space = locals_base + preserved_locals.len();
                            if needed_space > self.stack.len() {
                                // Expand stack to accommodate locals
                                self.stack.resize(needed_space, Value::Nil);
                            }
                            
                            // Restore locals safely
                            for (i, local) in preserved_locals.into_iter().enumerate() {
                                if locals_base + i < self.stack.len() {
                                    self.stack[locals_base + i] = local;
                                }
                            }
                        }
                        
                        // Push error value for catch handler
                        self.push(error)?;
                        
                        // Jump to catch handler
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.ip = handler.catch_ip;
                        }
                        
                        // Continue execution at catch handler
                        return Ok(VMState::Continue);
                    }
                }
                
                // No handler found, convert to runtime error
                return Err(VMError::RuntimeError {
                    message: format!("Thrown error: {:?}", error),
                    stack_trace: Some(self.build_stack_trace()),
                });
            }
            
            PushHandler => {
                // Push error handler onto stack
                let catch_ip = instruction.arg as usize;
                let current_frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame for error handler".to_string(),
                        stack_trace: None,
                    })?;
                
                // Validate that catch_ip is within bounds
                let chunk = &self.bytecode.chunks[current_frame.chunk_id];
                if catch_ip >= chunk.instructions.len() {
                    return Err(VMError::RuntimeError {
                        message: format!("Invalid catch target: {} (max: {})", 
                                       catch_ip, chunk.instructions.len() - 1),
                        stack_trace: None,
                    });
                }
                
                // Calculate number of locals at this scope
                // Locals are values on the stack from the frame base to current stack top
                let locals_count = if current_frame.stack_base <= self.stack.len() {
                    self.stack.len() - current_frame.stack_base
                } else {
                    // Invalid stack base - this shouldn't happen but handle it gracefully
                    return Err(VMError::RuntimeError {
                        message: format!("Invalid stack base: {} (stack size: {})", 
                                       current_frame.stack_base, self.stack.len()),
                        stack_trace: None,
                    });
                };
                
                // Check resource limits during handler creation
                if locals_count > MAX_PRESERVED_LOCALS {
                    return Err(VMError::RuntimeError {
                        message: format!("Too many locals for error handler: {} (max: {})", 
                                       locals_count, MAX_PRESERVED_LOCALS),
                        stack_trace: None,
                    });
                }
                
                // Check error handler stack limit
                if self.error_handler_stack.len() >= self.resource_limits.max_error_handlers {
                    return Err(VMError::ResourceLimitExceeded {
                        resource: "error handlers".to_string(),
                        limit: self.resource_limits.max_error_handlers,
                        requested: self.error_handler_stack.len() + 1,
                        stack_trace: None,
                    });
                }
                
                self.error_handler_stack.push(ErrorHandler {
                    catch_ip,
                    finally_ip: None, // Will be set by PushFinally if present
                    stack_depth: self.stack.len(),
                    call_frame: self.call_stack.len() - 1,
                    locals_count,
                });
            }
            
            PushFinally => {
                // Set finally IP on the most recent error handler
                let finally_ip = instruction.arg as usize;
                
                // Validate that finally_ip is within bounds
                let current_frame = self.call_stack.last()
                    .ok_or_else(|| VMError::RuntimeError {
                        message: "No active call frame for finally handler".to_string(),
                        stack_trace: None,
                    })?;
                let chunk = &self.bytecode.chunks[current_frame.chunk_id];
                if finally_ip >= chunk.instructions.len() {
                    return Err(VMError::RuntimeError {
                        message: format!("Invalid finally target: {} (max: {})", 
                                       finally_ip, chunk.instructions.len() - 1),
                        stack_trace: None,
                    });
                }
                
                if let Some(handler) = self.error_handler_stack.last_mut() {
                    handler.finally_ip = Some(finally_ip);
                } else {
                    return Err(VMError::RuntimeError {
                        message: "PushFinally without PushHandler".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            PopHandler => {
                // Pop error handler from stack
                if let Some(handler) = self.error_handler_stack.pop() {
                    // If there's a finally block, execute it
                    if let Some(finally_ip) = handler.finally_ip {
                        // The top of stack has the result value from try block
                        // We need to set up stack as [result, marker] for finally
                        
                        // Result is already on top of stack
                        // Push marker after it
                        self.push(Value::Symbol(FINALLY_NORMAL_MARKER.to_string()))?;
                        
                        // Jump to finally block
                        if let Some(frame) = self.call_stack.last_mut() {
                            frame.ip = finally_ip;
                        }
                        
                        // Continue execution at finally block
                        return Ok(VMState::Continue);
                    }
                } else {
                    return Err(VMError::RuntimeError {
                        message: "No error handler to pop".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            // Promise operations
            PromiseNew => {
                // Create a new promise
                // For now, create a promise ID
                let promise_id = self.id_generator.next_promise_id();
                self.push(Value::Promise(promise_id.0))?;
            }
            
            PromiseAll => {
                // Wait for all promises
                let count = instruction.arg as usize;
                let mut promises = Vec::with_capacity(count);
                
                for _ in 0..count {
                    match self.pop()? {
                        Value::Promise(id) => promises.push(id),
                        v => {
                            return Err(VMError::TypeError {
                                operation: "promise_all".to_string(),
                                expected: "promise".to_string(),
                                got: value_type_name(&v).to_string(),
                                location: None,
                                stack_trace: None,
                            })
                        }
                    }
                }
                
                // For now, just create a new promise
                let result_id = self.id_generator.next_promise_id();
                self.push(Value::Promise(result_id.0))?;
            }
            
            PromiseRace => {
                // Race multiple promises
                let count = instruction.arg as usize;
                let mut promises = Vec::with_capacity(count);
                
                for _ in 0..count {
                    match self.pop()? {
                        Value::Promise(id) => promises.push(id),
                        v => {
                            return Err(VMError::TypeError {
                                operation: "promise_race".to_string(),
                                expected: "promise".to_string(),
                                got: value_type_name(&v).to_string(),
                                location: None,
                                stack_trace: None,
                            })
                        }
                    }
                }
                
                // For now, just create a new promise
                let result_id = self.id_generator.next_promise_id();
                self.push(Value::Promise(result_id.0))?;
            }
            
            WithTimeout => {
                // Add timeout to promise
                let _default = if instruction.arg > 0 {
                    Some(self.pop()?)
                } else {
                    None
                };
                
                let promise = match self.pop()? {
                    Value::Promise(id) => id,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "with_timeout".to_string(),
                            expected: "promise".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };
                
                let timeout_ms = match self.pop()? {
                    Value::Integer(ms) => ms,
                    v => {
                        return Err(VMError::TypeError {
                            operation: "with_timeout".to_string(),
                            expected: "integer (milliseconds)".to_string(),
                            got: value_type_name(&v).to_string(),
                            location: None,
                            stack_trace: None,
                        })
                    }
                };
                
                // For now, just return the same promise
                self.push(Value::Promise(promise))?;
            }

            // Special
            Halt => return Ok(VMState::Halt),
            Nop => {} // All opcodes are handled exhaustively
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

    pub fn peek(&self, offset: usize) -> VMResult<&Value> {
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

    pub fn binary_op<F>(&mut self, op: F) -> VMResult<()>
    where
        F: FnOnce(Value, Value) -> VMResult<Value>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        let result = op(a, b)?;
        self.push(result)
    }

    pub fn binary_int_op<F>(&mut self, op: F) -> VMResult<()>
    where
        F: FnOnce(i64, i64) -> VMResult<i64>,
    {
        let b = self.pop()?;
        let a = self.pop()?;
        match (a, b) {
            (Value::Integer(x), Value::Integer(y)) => {
                let result = op(x, y)?;
                self.push(Value::Integer(result))
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
            (Value::Integer(x), Value::Integer(y)) => {
                let result = op(x, y);
                self.push(Value::Boolean(result))
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

    pub fn binary_float_op<F>(&mut self, op: F) -> VMResult<()>
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
            (Value::Boolean(x), Value::Boolean(y)) => x == y,
            (Value::Integer(x), Value::Integer(y)) => x == y,
            (Value::Float(x), Value::Float(y)) => x == y,
            (Value::String(x), Value::String(y)) => x == y,
            (Value::Symbol(x), Value::Symbol(y)) => x == y,
            (Value::List(x), Value::List(y)) => {
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Vector(x), Value::Vector(y)) => {
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Map(x), Value::Map(y)) => {
                x.len() == y.len()
                    && x.iter()
                        .all(|(k, v)| y.get(k).map_or(false, |v2| self.values_equal(v, v2)))
            }
            (
                Value::Tagged {
                    tag: tag1,
                    values: vals1,
                },
                Value::Tagged {
                    tag: tag2,
                    values: vals2,
                },
            ) => {
                tag1 == tag2
                    && vals1.len() == vals2.len()
                    && vals1
                        .iter()
                        .zip(vals2)
                        .all(|(a, b)| self.values_equal(a, b))
            }
            (Value::Module { name: n1, .. }, Value::Module { name: n2, .. }) => n1 == n2,
            (Value::Promise(x), Value::Promise(y)) => x == y,
            (Value::Channel(x), Value::Channel(y)) => x == y,
            (Value::Cell(x), Value::Cell(y)) => x == y,
            _ => false,
        }
    }

    pub fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            Value::Integer(i) => *i != 0,
            Value::Float(f) => *f != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Symbol(_) => true,
            Value::List(l) => !l.is_empty(),
            Value::Procedure(_) => true,
            Value::Vector(v) => !v.is_empty(),
            Value::Map(m) => !m.is_empty(),
            Value::NativeFunction { .. } => true,
            Value::Function { .. } => true,
            Value::Promise(_) => true,
            Value::Channel(_) => true,
            Value::Cell(_) => true,
            Value::Tagged { .. } => true,
            Value::Module { .. } => true,
            Value::GcHandle(_) => true,
            Value::Actor(_) => true,
            Value::Error { .. } => false, // Errors are falsy
            Value::Future { .. } => true,
        }
    }

    pub fn vm_value_to_stdlib_value(&self, value: &Value) -> StdlibValue {
        // Since StdlibValue is just a re-export of Value, we can clone directly
        value.clone()
    }

    pub fn stdlib_value_to_vm_value(&self, value: &StdlibValue) -> Value {
        // Since StdlibValue is just a re-export of Value, we can clone directly
        value.clone()
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
                    let frame = self
                        .call_stack
                        .last()
                        .ok_or_else(|| VMError::StackUnderflow {
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
            Value::NativeFunction {
                function, arity, ..
            } => {
                // Check arity
                if args.len() != arity {
                    return Err(VMError::RuntimeError {
                        message: format!(
                            "Native function expects {} arguments, got {}",
                            arity,
                            args.len()
                        ),
                        stack_trace: None,
                    });
                }

                // Call the native function
                let result = function(&args).map_err(|e| VMError::RuntimeError {
                    message: format!("Native function error: {}", e),
                    stack_trace: None,
                })?;

                // Push result
                self.push(result)?;
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
                    let frame = self
                        .call_stack
                        .last()
                        .ok_or_else(|| VMError::RuntimeError {
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
                                    message: "Handler function halted without return value"
                                        .to_string(),
                                    stack_trace: Some(self.build_stack_trace()),
                                });
                            }
                        }
                    }
                }

                Ok(result)
            }
            Value::NativeFunction {
                function, arity, ..
            } => {
                // Check arity
                if args.len() != arity {
                    return Err(VMError::RuntimeError {
                        message: format!(
                            "Native handler expects {} arguments, got {}",
                            arity,
                            args.len()
                        ),
                        stack_trace: None,
                    });
                }

                // Call the native function directly
                function(&args).map_err(|e| VMError::RuntimeError {
                    message: format!("Native handler error: {}", e),
                    stack_trace: None,
                })
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
            Value::Boolean(b) => fluentai_core::value::Value::Boolean(*b),
            Value::Integer(i) => fluentai_core::value::Value::Integer(*i),
            Value::Float(f) => fluentai_core::value::Value::Float(*f),
            Value::String(s) => fluentai_core::value::Value::String(s.clone()),
            Value::Symbol(s) => fluentai_core::value::Value::Symbol(s.clone()),
            Value::List(items) => fluentai_core::value::Value::List(
                items
                    .iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect(),
            ),
            Value::Procedure(proc) => fluentai_core::value::Value::Procedure(proc.clone()),
            Value::Vector(items) => fluentai_core::value::Value::Vector(
                items
                    .iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect(),
            ),
            Value::Map(map) => {
                let mut core_map = FxHashMap::default();
                for (k, v) in map.iter() {
                    core_map.insert(k.clone(), self.vm_value_to_core_value(v));
                }
                fluentai_core::value::Value::Map(core_map)
            }
            Value::NativeFunction {
                name,
                arity,
                function,
            } => fluentai_core::value::Value::NativeFunction {
                name: name.clone(),
                arity: *arity,
                function: function.clone(),
            },
            Value::Function { .. } => {
                // Functions can't be directly converted, return a placeholder
                fluentai_core::value::Value::String("<function>".to_string())
            }
            Value::Promise(id) => fluentai_core::value::Value::String(format!("<promise:{}>", id)),
            Value::Channel(id) => fluentai_core::value::Value::String(format!("<channel:{}>", id)),
            Value::Cell(idx) => fluentai_core::value::Value::String(format!("<cell:{}>", idx)),
            Value::Tagged { tag, values } => fluentai_core::value::Value::Tagged {
                tag: tag.clone(),
                values: values
                    .iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect(),
            },
            Value::Module { name, exports } => {
                // Convert to a map representation
                let mut map = FxHashMap::default();
                map.insert(
                    "__module__".to_string(),
                    fluentai_core::value::Value::String(name.clone()),
                );
                let mut export_map = FxHashMap::default();
                for (key, val) in exports {
                    export_map.insert(key.clone(), self.vm_value_to_core_value(val));
                }
                map.insert(
                    "__exports__".to_string(),
                    fluentai_core::value::Value::Map(export_map),
                );
                fluentai_core::value::Value::Map(map)
            }
            Value::GcHandle(_) => fluentai_core::value::Value::String("<gc-handle>".to_string()),
            Value::Actor(id) => fluentai_core::value::Value::String(format!("<actor:{}>", id)),
            Value::Future { .. } => fluentai_core::value::Value::String("<future>".to_string()),
            Value::Error { kind, message, .. } => {
                fluentai_core::value::Value::String(format!("<error:{}:{}>", kind, message))
            }
        }
    }

    fn core_value_to_vm_value(&self, value: &fluentai_core::value::Value) -> Value {
        match value {
            fluentai_core::value::Value::Nil => Value::Nil,
            fluentai_core::value::Value::Boolean(b) => Value::Boolean(*b),
            fluentai_core::value::Value::Integer(i) => Value::Integer(*i),
            fluentai_core::value::Value::Float(f) => Value::Float(*f),
            fluentai_core::value::Value::String(s) => Value::String(s.clone()),
            fluentai_core::value::Value::List(items) => Value::List(
                items
                    .iter()
                    .map(|v| self.core_value_to_vm_value(v))
                    .collect(),
            ),
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
            fluentai_core::value::Value::Tagged { tag, values } => Value::Tagged {
                tag: tag.clone(),
                values: values
                    .iter()
                    .map(|v| self.core_value_to_vm_value(v))
                    .collect(),
            },
            fluentai_core::value::Value::Symbol(s) => {
                // Preserve symbols as symbols
                Value::Symbol(s.clone())
            }
            fluentai_core::value::Value::Vector(items) => {
                // Convert vectors to lists in VM representation
                Value::List(
                    items
                        .iter()
                        .map(|v| self.core_value_to_vm_value(v))
                        .collect(),
                )
            }
            fluentai_core::value::Value::Function { chunk_id, env } => Value::Function {
                chunk_id: *chunk_id,
                env: env.iter().map(|v| self.core_value_to_vm_value(v)).collect(),
            },
            fluentai_core::value::Value::Promise(id) => Value::Promise(*id),
            fluentai_core::value::Value::Channel(id) => Value::Channel(*id),
            fluentai_core::value::Value::Cell(idx) => Value::Cell(*idx),
            fluentai_core::value::Value::Module { name, exports } => {
                let mut vm_exports = FxHashMap::default();
                for (k, v) in exports {
                    vm_exports.insert(k.clone(), self.core_value_to_vm_value(v));
                }
                Value::Module {
                    name: name.clone(),
                    exports: vm_exports,
                }
            }
            fluentai_core::value::Value::GcHandle(handle) => Value::GcHandle(handle.clone()),
            fluentai_core::value::Value::Actor(id) => Value::Actor(*id),
            fluentai_core::value::Value::Error { kind, message, stack_trace } => Value::Error {
                kind: kind.clone(),
                message: message.clone(),
                stack_trace: stack_trace.clone(),
            },
            fluentai_core::value::Value::Future { chunk_id, env } => Value::Future {
                chunk_id: *chunk_id,
                env: env.iter().map(|v| self.core_value_to_vm_value(v)).collect(),
            },
        }
    }

    // Module system helper methods
    fn get_constant_string(&self, idx: u32) -> VMResult<String> {
        let value = self.bytecode.chunks[self.current_chunk()]
            .constants
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
        let module_info =
            self.module_loader
                .load_module(module_name)
                .map_err(|e| VMError::ModuleError {
                    module_name: module_name.to_string(),
                    message: e.to_string(),
                    stack_trace: None,
                })?;

        // Compile the module
        // Disable optimization to work around optimizer node ID remapping bug
        // TODO: Fix optimizer to properly remap node IDs in Module nodes
        let options = crate::compiler::CompilerOptions {
            optimization_level: fluentai_optimizer::OptimizationLevel::None,
            debug_info: false,
        };
        let compiler = crate::compiler::Compiler::with_options(options);
        let module_bytecode =
            compiler
                .compile(&module_info.graph)
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

        // Collect exports from module
        let mut exports = FxHashMap::default();
        
        // First check if the module registered its exports via ExportBinding
        if let Some(Value::Module { exports: module_exports, .. }) = module_vm.loaded_modules.get(module_name) {
            // Use the exports from ExportBinding
            exports = module_exports.clone();
        } else {
            // Fall back to collecting from globals (for modules using define)
            for export_name in &module_info.exports {
                if let Some(value) = module_vm.globals.get(export_name) {
                    exports.insert(export_name.clone(), value.clone());
                }
            }
        }

        // Create module value with actual exports
        let module_value = Value::Module {
            name: module_name.to_string(),
            exports,
        };

        self.loaded_modules
            .insert(module_name.to_string(), module_value);

        Ok(())
    }

    fn current_chunk(&self) -> usize {
        self.call_stack
            .last()
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
            let function_name = self
                .bytecode
                .chunks
                .get(frame.chunk_id)
                .and_then(|chunk| chunk.name.clone())
                .unwrap_or_else(|| format!("<anonymous:{}>", frame.chunk_id));

            // Get source location from source map if available
            let location = self
                .bytecode
                .chunks
                .get(frame.chunk_id)
                .and_then(|chunk| chunk.source_map.as_ref())
                .and_then(|source_map| source_map.get_location(frame.ip))
                .map(|src_loc| crate::error::SourceLocation {
                    file: self.bytecode.chunks.get(frame.chunk_id)
                        .and_then(|chunk| chunk.source_map.as_ref())
                        .and_then(|sm| sm.filename.clone()),
                    line: src_loc.line.unwrap_or(0) as usize,
                    column: src_loc.column.unwrap_or(0) as usize,
                    function: Some(function_name.clone()),
                });

            trace.push_frame(StackFrame {
                function_name,
                chunk_id: frame.chunk_id,
                ip: frame.ip,
                location,
            });
        }

        trace
    }
    
    /// Create an error with source location from current instruction
    pub fn create_error_with_location(&self, mut error: VMError) -> VMError {
        // Add stack trace if not already present
        match &mut error {
            VMError::StackOverflow { stack_trace, .. } |
            VMError::StackUnderflow { stack_trace, .. } |
            VMError::CallStackOverflow { stack_trace, .. } |
            VMError::TypeError { stack_trace, .. } |
            VMError::DivisionByZero { stack_trace, .. } |
            VMError::IntegerOverflow { stack_trace, .. } |
            VMError::InvalidConstantIndex { stack_trace, .. } |
            VMError::InvalidLocalIndex { stack_trace, .. } |
            VMError::InvalidJumpTarget { stack_trace, .. } |
            VMError::ResourceLimitExceeded { stack_trace, .. } |
            VMError::ModuleError { stack_trace, .. } |
            VMError::AsyncError { stack_trace, .. } |
            VMError::CellError { stack_trace, .. } |
            VMError::UnknownIdentifier { stack_trace, .. } |
            VMError::RuntimeError { stack_trace, .. } => {
                if stack_trace.is_none() {
                    *stack_trace = Some(self.build_stack_trace());
                }
            }
            _ => {}
        }
        
        // Add source location if not already present and applicable
        if let Some(frame) = self.call_stack.last() {
            match &mut error {
                VMError::TypeError { location, .. } |
                VMError::DivisionByZero { location, .. } |
                VMError::InvalidOpcode { location, .. } |
                VMError::UnknownIdentifier { location, .. } => {
                    if location.is_none() {
                        *location = self
                            .bytecode
                            .chunks
                            .get(frame.chunk_id)
                            .and_then(|chunk| chunk.source_map.as_ref())
                            .and_then(|source_map| source_map.get_location(frame.ip))
                            .map(|src_loc| crate::error::SourceLocation {
                                file: self.bytecode.chunks.get(frame.chunk_id)
                                    .and_then(|chunk| chunk.source_map.as_ref())
                                    .and_then(|sm| sm.filename.clone()),
                                line: src_loc.line.unwrap_or(0) as usize,
                                column: src_loc.column.unwrap_or(0) as usize,
                                function: self.bytecode.chunks.get(frame.chunk_id)
                                    .and_then(|chunk| chunk.name.clone()),
                            });
                    }
                }
                _ => {}
            }
        }
        
        error
    }

    /// Set resource limits
    pub fn set_resource_limits(&mut self, limits: ResourceLimits) {
        self.resource_limits = limits;
    }
    
    // ===== Public accessor methods for opcode handlers =====
    
    // Stack operations
    pub fn stack_len(&self) -> usize {
        self.stack.len()
    }
    
    pub fn stack_swap(&mut self, a: usize, b: usize) {
        self.stack.swap(a, b);
    }
    
    // Constant access
    pub fn get_constant(&self, chunk_id: usize, index: usize) -> VMResult<&Value> {
        self.bytecode.chunks[chunk_id]
            .constants
            .get(index)
            .ok_or_else(|| VMError::InvalidConstantIndex {
                index: index as u32,
                max_index: self.bytecode.chunks[chunk_id].constants.len(),
                stack_trace: None,
            })
    }
    
    pub fn get_constant_string_at(&self, chunk_id: usize, index: usize) -> VMResult<String> {
        match self.get_constant(chunk_id, index)? {
            Value::String(s) => Ok(s.clone()),
            _ => Err(VMError::InvalidConstantIndex {
                index: index as u32,
                max_index: self.bytecode.chunks[chunk_id].constants.len(),
                stack_trace: None,
            }),
        }
    }
    
    // Control flow
    pub fn set_ip(&mut self, ip: usize) {
        if let Some(frame) = self.call_stack.last_mut() {
            frame.ip = ip;
        }
    }
    
    
    pub fn call_stack_len(&self) -> usize {
        self.call_stack.len()
    }
    
    pub fn push_call_frame(&mut self, frame: CallFrame) -> VMResult<()> {
        if self.call_stack.len() >= self.resource_limits.max_call_depth {
            return Err(VMError::CallStackOverflow {
                current_depth: self.call_stack.len(),
                max_depth: self.resource_limits.max_call_depth,
                stack_trace: None,
            });
        }
        self.call_stack.push(frame);
        Ok(())
    }
    
    pub fn pop_call_frame_with_return(&mut self, return_val: Value) -> VMResult<()> {
        if let Some(frame) = self.call_stack.pop() {
            // Track execution time if usage tracking is enabled
            if let Some(tracker) = &self.usage_tracker {
                if let Some(start_time) = frame.start_time {
                    let elapsed = start_time.elapsed().as_nanos() as u64;
                    if let Ok(mut tracker_guard) = tracker.write() {
                        tracker_guard.record_execution(frame.chunk_id, elapsed);
                    }
                }
            }
            
            // Restore stack to frame base and push return value
            self.stack.truncate(frame.stack_base);
            self.push(return_val)?;
        }
        Ok(())
    }
    
    pub fn has_usage_tracker(&self) -> bool {
        self.usage_tracker.is_some()
    }
    
    /// Check if a chunk should be JIT compiled
    #[cfg(feature = "jit")]
    pub fn should_jit_compile(&self, chunk_id: usize) -> bool {
        if let Some(tracker) = &self.usage_tracker {
            if let Ok(tracker_guard) = tracker.read() {
                if let Some(stats) = tracker_guard.get_stats_for_chunk(chunk_id) {
                    return self.jit_manager.should_compile(&stats);
                }
            }
        }
        false
    }
    
    #[cfg(not(feature = "jit"))]
    pub fn should_jit_compile(&self, _chunk_id: usize) -> bool {
        false
    }
    
    /// Try to execute a function using JIT compilation
    #[cfg(feature = "jit")]
    pub fn try_jit_execute(&mut self, chunk_id: usize) -> VMResult<Option<Value>> {
        // First attempt compilation if needed
        if let Some(tracker) = &self.usage_tracker {
            if let Ok(tracker_guard) = tracker.read() {
                if let Some(stats) = tracker_guard.get_stats_for_chunk(chunk_id) {
                    if self.jit_manager.should_compile(&stats) {
                        drop(tracker_guard);
                        self.jit_manager.compile_chunk(chunk_id, &self.bytecode)?;
                    }
                }
            }
        }
        
        // Try to execute the JIT-compiled version
        match self.jit_manager.execute_if_compiled(chunk_id, &self.bytecode) {
            Some(Ok(value)) => Ok(Some(value)),
            Some(Err(_)) => Ok(None), // Fall back to interpreter
            None => Ok(None),
        }
    }
    
    #[cfg(not(feature = "jit"))]
    pub fn try_jit_execute(&mut self, _chunk_id: usize) -> VMResult<Option<Value>> {
        Ok(None)
    }
    
    /// Get JIT compilation statistics
    #[cfg(feature = "jit")]
    pub fn jit_stats(&self) -> &crate::jit_integration::JitStats {
        self.jit_manager.stats()
    }
    
    pub fn emit_function_call_debug_event(&self, func: &Value, arg_count: usize) {
        if self.debug_config.enabled {
            self.debug_config.send_event(VMDebugEvent::FunctionCall {
                name: match func {
                    Value::Function { .. } => Some("lambda".to_string()),
                    Value::NativeFunction { name, .. } => Some(name.clone()),
                    _ => None,
                },
                arg_count,
                call_depth: self.call_stack.len(),
            });
        }
    }
    
    pub fn value_type_name(&self, value: &Value) -> &str {
        value_type_name(value)
    }
    
    // ===== Async support methods =====
    
    pub fn create_channel(&mut self) -> ChannelId {
        let channel_id = self.id_generator.next_channel_id();
        let (tx, rx) = mpsc::channel(100); // Default capacity
        self.channels.insert(channel_id, (tx, rx));
        channel_id
    }
    
    pub fn send_to_channel(&mut self, channel_id: ChannelId, value: Value) -> VMResult<()> {
        if let Some((tx, _)) = self.channels.get(&channel_id) {
            tx.try_send(value).map_err(|e| match e {
                mpsc::error::TrySendError::Full(_) => VMError::AsyncError {
                    message: "Channel buffer full".to_string(),
                    stack_trace: None,
                },
                mpsc::error::TrySendError::Closed(_) => VMError::AsyncError {
                    message: "Channel closed".to_string(),
                    stack_trace: None,
                },
            })
        } else {
            Err(VMError::UnknownIdentifier {
                name: format!("channel:{}", channel_id.0),
                location: None,
                stack_trace: None,
            })
        }
    }
    
    pub fn receive_from_channel(&mut self, channel_id: ChannelId) -> VMResult<Value> {
        if let Some((_, rx)) = self.channels.get_mut(&channel_id) {
            match rx.try_recv() {
                Ok(value) => Ok(value),
                Err(mpsc::error::TryRecvError::Empty) => {
                    // For synchronous execution, return Nil on empty channel
                    Ok(Value::Nil)
                },
                Err(mpsc::error::TryRecvError::Disconnected) => {
                    Err(VMError::AsyncError {
                        message: "Channel disconnected".to_string(),
                        stack_trace: None,
                    })
                }
            }
        } else {
            Err(VMError::UnknownIdentifier {
                name: format!("channel:{}", channel_id.0),
                location: None,
                stack_trace: None,
            })
        }
    }
    
    pub fn get_channel_receiver_mut(&mut self, channel_id: &ChannelId) -> Option<&mut mpsc::Receiver<Value>> {
        self.channels.get_mut(channel_id).map(|(_, rx)| rx)
    }
    
    pub fn await_promise(&mut self, promise_id: PromiseId) -> VMResult<Value> {
        if let Some(mut receiver) = self.promises.remove(&promise_id) {
            // Blocking receive for synchronous execution
            match receiver.try_recv() {
                Ok(Ok(value)) => Ok(value),
                Ok(Err(e)) => Err(e),
                Err(_) => {
                    // Channel not ready, return a placeholder
                    Ok(Value::Nil)
                }
            }
        } else {
            Err(VMError::AsyncError {
                message: format!("Promise {:?} not found", promise_id),
                stack_trace: None,
            })
        }
    }
    
    pub fn spawn_task(&mut self, func: Value) -> VMResult<()> {
        match func {
            Value::Function { chunk_id, env } => {
                // Create a promise for the result
                let promise_id = self.id_generator.next_promise_id();
                let (tx, rx) = oneshot::channel();
                
                // Store the receiver
                self.promises.insert(promise_id, rx);
                
                // Clone shared resources
                let bytecode = Arc::clone(&self.bytecode);
                let stdlib = self.stdlib.clone();
                let effect_runtime = Arc::clone(&self.effect_runtime);
                let effect_context = Arc::clone(&self.effect_context);
                let globals = self.globals.clone(); // COW clone
                
                // Spawn the task
                tokio::spawn(async move {
                    // Create a new VM with shared bytecode
                    let mut task_vm = VM::with_shared_bytecode(bytecode);
                    task_vm.stdlib = stdlib;
                    task_vm.effect_runtime = effect_runtime;
                    task_vm.effect_context = effect_context;
                    task_vm.globals = globals;
                    
                    // Set up the call frame for the function
                    task_vm.call_stack.push(CallFrame {
                        chunk_id,
                        ip: 0,
                        stack_base: 0,
                        env,
                        start_time: None,
                    });
                    
                    // Run the function
                    let result = task_vm.run_inner();
                    
                    // Send the result through the promise channel
                    let _ = tx.send(result);
                });
                
                // Push the promise ID
                self.push(Value::Promise(promise_id.0))?;
                Ok(())
            }
            _ => Err(VMError::TypeError {
                operation: "spawn".to_string(),
                expected: "function".to_string(),
                got: value_type_name(&func).to_string(),
                location: None,
                stack_trace: None,
            }),
        }
    }
    
    pub fn create_actor(&mut self, initial_state: Value, handler: Value) -> VMResult<ActorId> {
        // Validate handler is a function
        match &handler {
            Value::Function { .. } | Value::Procedure(_) => {},
            _ => {
                return Err(VMError::TypeError {
                    operation: "create_actor".to_string(),
                    expected: "function".to_string(),
                    got: value_type_name(&handler).to_string(),
                    location: None,
                    stack_trace: None,
                })
            }
        }
        
        // Create actor ID and mailbox
        let actor_id = self.id_generator.next_actor_id();
        let (tx, rx) = mpsc::channel(100); // Buffer size of 100
        
        // Create actor
        let actor = Actor {
            state: initial_state,
            handler,
            mailbox: rx,
            sender: tx,
        };
        
        // Store actor
        self.actors.insert(actor_id, actor);
        
        Ok(actor_id)
    }
    
    pub fn send_to_actor(&mut self, actor_id: ActorId, message: Value) -> VMResult<()> {
        if let Some(actor) = self.actors.get(&actor_id) {
            // Send message (non-blocking)
            actor.sender.try_send(message).map_err(|_| VMError::AsyncError {
                message: "Actor mailbox full or closed".to_string(),
                stack_trace: None,
            })
        } else {
            Err(VMError::UnknownIdentifier {
                name: format!("actor:{}", actor_id.0),
                location: None,
                stack_trace: None,
            })
        }
    }
    
    pub fn take_promise(&mut self, promise_id: &PromiseId) -> Option<oneshot::Receiver<VMResult<Value>>> {
        self.promises.remove(promise_id)
    }
    
    // Memory operations
    pub fn get_local(&self, index: usize) -> VMResult<&Value> {
        let frame = self.call_stack.last()
            .ok_or_else(|| VMError::RuntimeError {
                message: "No active call frame".to_string(),
                stack_trace: None,
            })?;
        
        let stack_idx = frame.stack_base + index;
        self.stack.get(stack_idx)
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid local variable index: {}", index),
                stack_trace: None,
            })
    }
    
    pub fn set_local(&mut self, index: usize, value: Value) -> VMResult<()> {
        let frame = self.call_stack.last()
            .ok_or_else(|| VMError::RuntimeError {
                message: "No active call frame".to_string(),
                stack_trace: None,
            })?;
        
        let stack_idx = frame.stack_base + index;
        if stack_idx < self.stack.len() {
            self.stack[stack_idx] = value;
            Ok(())
        } else {
            Err(VMError::RuntimeError {
                message: format!("Invalid local variable index: {}", index),
                stack_trace: None,
            })
        }
    }
    
    
    pub fn define_global(&mut self, name: String, value: Value) -> VMResult<()> {
        // In this implementation, define is the same as set
        self.globals.insert(name, value);
        Ok(())
    }
    
    pub fn is_stdlib_function(&self, name: &str) -> bool {
        self.stdlib.contains(name)
    }
    
    pub fn is_builtin(&self, name: &str) -> bool {
        self.builtin_to_opcode(name).is_some()
    }
    
    // Upvalue operations
    pub fn get_upvalue(&self, index: usize) -> VMResult<&Value> {
        let frame = self.call_stack.last()
            .ok_or_else(|| VMError::RuntimeError {
                message: "No active call frame".to_string(),
                stack_trace: None,
            })?;
        
        frame.env.get(index)
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid upvalue index: {}", index),
                stack_trace: None,
            })
    }
    
    pub fn set_upvalue(&mut self, index: usize, value: Value) -> VMResult<()> {
        let frame = self.call_stack.last_mut()
            .ok_or_else(|| VMError::RuntimeError {
                message: "No active call frame".to_string(),
                stack_trace: None,
            })?;
        
        if index < frame.env.len() {
            frame.env[index] = value;
            Ok(())
        } else {
            Err(VMError::RuntimeError {
                message: format!("Invalid upvalue index: {}", index),
                stack_trace: None,
            })
        }
    }
    
    // Cell operations
    pub fn create_cell(&mut self, value: Value) -> usize {
        let id = self.cells.len();
        self.cells.push(value);
        id
    }
    
    pub fn get_cell_value(&self, id: usize) -> VMResult<&Value> {
        self.cells.get(id)
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Invalid cell id: {}", id),
                stack_trace: None,
            })
    }
    
    pub fn set_cell_value(&mut self, id: usize, value: Value) -> VMResult<()> {
        if id < self.cells.len() {
            self.cells[id] = value;
            Ok(())
        } else {
            Err(VMError::RuntimeError {
                message: format!("Invalid cell id: {}", id),
                stack_trace: None,
            })
        }
    }
    
    // Native function calls
    pub fn call_native_function(&mut self, native_func: &str, args: Vec<Value>) -> VMResult<()> {
        // Implementation simplified for now - would need to handle native functions properly
        self.push(Value::Nil)?;
        Ok(())
    }
    
    // Tail call support
    pub fn setup_tail_call(&mut self, func: Value, args: Vec<Value>) -> VMResult<()> {
        // Implementation would reuse current call frame
        Ok(())
    }
    
    pub fn handle_tail_return(&mut self, result: Value) -> VMResult<()> {
        if let Some(frame) = self.call_stack.pop() {
            self.stack.truncate(frame.stack_base);
            self.push(result)?;
        }
        Ok(())
    }
    
    
    // Effect operations
    pub fn perform_effect(&mut self, operation: String) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn install_effect_handlers(&mut self, handlers: Vec<Value>) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn uninstall_effect_handler(&mut self) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn resume_from_handler(&mut self, value: Value) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    // Error handling
    pub fn push_error_handler(&mut self, catch_ip: usize, finally_ip: Option<usize>) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn pop_error_handler(&mut self) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn throw_error(&mut self, error: Value) -> VMResult<VMState> {
        // Simplified implementation
        Ok(VMState::Continue)
    }
    
    pub fn start_finally_block(&mut self) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    pub fn end_finally_block(&mut self) -> VMResult<()> {
        // Simplified implementation
        Ok(())
    }
    
    // ===== Async support accessor methods =====
    
    /// Get a reference to the bytecode
    pub fn bytecode(&self) -> &Bytecode {
        &self.bytecode
    }
    
    /// Get a reference to the stack
    pub fn stack(&self) -> &[Value] {
        &self.stack
    }
    
    /// Get a mutable reference to the stack
    pub fn stack_mut(&mut self) -> &mut Vec<Value> {
        &mut self.stack
    }
    
    /// Get a reference to the call stack
    pub fn call_stack(&self) -> &[CallFrame] {
        &self.call_stack
    }
    
    /// Get a mutable reference to the call stack
    pub fn call_stack_mut(&mut self) -> &mut Vec<CallFrame> {
        &mut self.call_stack
    }
    
    
    /// Get a reference to a channel
    pub fn get_channel(&self, channel_id: &crate::safety::ChannelId) -> Option<&(mpsc::Sender<Value>, mpsc::Receiver<Value>)> {
        self.channels.get(channel_id)
    }
    
    /// Get a mutable reference to a channel
    pub fn get_channel_mut(&mut self, channel_id: &crate::safety::ChannelId) -> Option<&mut (mpsc::Sender<Value>, mpsc::Receiver<Value>)> {
        self.channels.get_mut(channel_id)
    }
    
}

#[derive(Debug)]
pub enum VMState {
    Continue,
    Return,
    Halt,
}

#[cfg(test)]
mod inline_tests {
    use super::*;
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

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
        let vm_val = Value::Integer(42);
        let stdlib_val = vm.vm_value_to_stdlib_value(&vm_val);
        match stdlib_val {
            StdlibValue::Integer(i) => assert_eq!(i, 42),
            _ => panic!("Expected integer"),
        }

        // Test list conversion
        let vm_list = Value::List(vec![Value::Integer(1), Value::Integer(2)]);
        let stdlib_list = vm.vm_value_to_stdlib_value(&vm_list);
        match stdlib_list {
            StdlibValue::List(_) => {}
            _ => panic!("Expected list"),
        }
    }
}
