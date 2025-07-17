//! High-performance stack-based virtual machine

use fluentai_bytecode::{Bytecode, Instruction, Opcode};
use crate::continuation::{ContinuationId, ContinuationManager, ContinuationSupport, ErrorHandlerFrame};
use crate::cow_globals::CowGlobals;
use crate::debug::{DebugConfig, StepMode, VMDebugEvent};
use crate::error::{value_type_name, StackFrame, StackTrace, VMError, VMResult};
use crate::gc::{GarbageCollector, GcConfig, GcScope};
#[cfg(feature = "jit")]
use crate::jit_integration::{JitConfig, JitManager};
use crate::module_registry::ModuleRegistry;
use crate::profiler::Profiler;
use crate::safety::{ActorId, IdGenerator, ResourceLimits};
#[cfg(feature = "std")]
use crate::safety::{ChannelId, PromiseId};
use crate::security::{SecurityManager, SecurityPolicy};
use fluentai_core::ast::{Graph, NodeId, UsageStatistics};
use fluentai_core::value::Value;
#[cfg(feature = "std")]
use fluentai_effects::{runtime::EffectRuntime, EffectContext};
use fluentai_modules::{ModuleLoader, ModuleResolver};
#[cfg(feature = "std")]
use fluentai_stdlib::value::Value as StdlibValue;
#[cfg(feature = "std")]
use fluentai_stdlib::{init_stdlib, StdlibRegistry};
use rustc_hash::FxHashMap;
use std::sync::{Arc, RwLock};
#[cfg(feature = "std")]
use tokio::sync::{mpsc, oneshot};

const STACK_SIZE: usize = 10_000;
const MAX_PRESERVED_LOCALS: usize = 1000;
const FINALLY_NORMAL_MARKER: &str = "__finally_normal__";
const FINALLY_EXCEPTION_MARKER: &str = "__finally_exception__";

/// Bit masks for MakeClosure instruction unpacking
const MAKECLOSURE_CHUNK_ID_SHIFT: u32 = 16;
const MAKECLOSURE_CAPTURE_COUNT_MASK: u32 = 0xFFFF;

/// Actor state and message handling
#[cfg(feature = "std")]
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

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub chunk_id: usize,
    pub ip: usize,
    pub stack_base: usize,
    pub env: Vec<Value>, // Captured environment for closures
    #[allow(dead_code)]
    pub start_time: Option<web_time::Instant>, // Track when this frame started executing
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
    #[cfg(feature = "std")]
    effect_context: Arc<EffectContext>,
    #[cfg(feature = "std")]
    effect_runtime: Arc<EffectRuntime>,
    // Async support with typed IDs
    id_generator: IdGenerator,
    #[cfg(feature = "std")]
    promises: FxHashMap<PromiseId, oneshot::Receiver<VMResult<Value>>>,
    #[cfg(feature = "std")]
    pending_promise_bodies: FxHashMap<PromiseId, Value>,
    #[cfg(feature = "std")]
    channels: FxHashMap<ChannelId, (mpsc::Sender<Value>, mpsc::Receiver<Value>)>,
    // Actor support
    #[cfg(feature = "std")]
    actors: FxHashMap<ActorId, Actor>,
    // Mutable cells
    cells: Vec<Value>,
    // Standard library
    #[cfg(feature = "std")]
    stdlib: StdlibRegistry,
    // Module system
    module_loader: ModuleLoader,
    #[allow(dead_code)]
    module_resolver: ModuleResolver,
    loaded_modules: FxHashMap<String, Value>, // Cache of loaded modules
    current_module: Option<String>,           // Name of currently executing module
    module_stack: Vec<String>,                // Stack of module names for nested module execution
    module_registry: ModuleRegistry,          // Registry of loaded modules with exports
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
    // Profiler for runtime optimization
    profiler: Option<Arc<Profiler>>,
    // Learning mode manager for runtime optimization exploration
    learning_mode: Option<Box<crate::learning_mode::LearningModeManager>>,
    // Original AST graph for variant compilation
    ast_graph: Option<Arc<Graph>>,
    // Effect handler stack
    handler_stack: Vec<HandlerFrame>,
    // Error handler stack for try-catch-finally
    error_handler_stack: Vec<ErrorHandler>,
    // Finally block state storage
    finally_states: Vec<FinallyState>,
    // JIT compilation manager
    #[cfg(feature = "jit")]
    jit_manager: JitManager,
    // Renderer bridge for Dom effects
    // #[cfg(feature = "renderer")]
    // renderer_bridge: Option<fluentai_renderer::RendererBridge>,
    // Continuation support for async/await
    continuation_manager: ContinuationManager,
    // Local variables storage
    locals: Vec<Value>,
    // Current actor context (if executing within an actor)
    current_actor_id: Option<ActorId>,
    // Current actor message (if processing actor message)
    current_actor_message: Option<Value>,
    // Loop tracking for profiling
    loop_stack: Vec<(NodeId, usize, web_time::Instant)>, // (loop_id, iteration_count, start_time)
    // Variant execution support
    pending_variant_execution: Option<(Arc<Bytecode>, usize)>, // (variant_bytecode, variant_chunk_id)
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
            #[cfg(feature = "std")]
            effect_context: Arc::new(EffectContext::default()),
            #[cfg(feature = "std")]
            effect_runtime: Arc::new(EffectRuntime::default()),
            id_generator: IdGenerator::new(),
            #[cfg(feature = "std")]
            promises: FxHashMap::default(),
            #[cfg(feature = "std")]
            pending_promise_bodies: FxHashMap::default(),
            #[cfg(feature = "std")]
            channels: FxHashMap::default(),
            #[cfg(feature = "std")]
            actors: FxHashMap::default(),
            cells: Vec::new(),
            #[cfg(feature = "std")]
            stdlib: init_stdlib(),
            module_loader: ModuleLoader::new(fluentai_modules::ModuleConfig::default()),
            module_resolver: ModuleResolver::new(ModuleLoader::new(
                fluentai_modules::ModuleConfig::default(),
            )),
            loaded_modules: FxHashMap::default(),
            current_module: None,
            module_stack: Vec::new(),
            module_registry: ModuleRegistry::new(),
            debug_config: DebugConfig::default(),
            instruction_count: 0,
            resource_limits: ResourceLimits::default(),
            security_manager: None,
            gc: None,
            usage_tracker: None,
            profiler: None,
            learning_mode: None,
            ast_graph: None,
            handler_stack: Vec::new(),
            error_handler_stack: Vec::new(),
            finally_states: Vec::new(),
            #[cfg(feature = "jit")]
            jit_manager: JitManager::new(JitConfig::default()),
            // #[cfg(feature = "renderer")]
            // renderer_bridge: None,
            continuation_manager: ContinuationManager::new(),
            locals: Vec::with_capacity(256),
            current_actor_id: None,
            current_actor_message: None,
            loop_stack: Vec::new(),
            pending_variant_execution: None,
        }
    }

    pub fn enable_trace(&mut self) {
        self.trace = true;
    }
    
    // /// Set the renderer bridge for Dom effects
    // #[cfg(feature = "renderer")]
    // pub fn set_renderer_bridge(&mut self, bridge: fluentai_renderer::RendererBridge) {
    //     self.renderer_bridge = Some(bridge);
    // }

    /// Reset VM state for reuse while keeping expensive initializations
    pub fn reset(&mut self) {
        // Clear runtime state
        self.stack.clear();
        self.call_stack.clear();
        self.globals.clear();
        #[cfg(feature = "std")]
        self.promises.clear();
        #[cfg(feature = "std")]
        self.channels.clear();
        self.cells.clear();
        self.instruction_count = 0;
        self.handler_stack.clear();
        self.error_handler_stack.clear();
        self.finally_states.clear();
        self.continuation_manager.clear();
        self.locals.clear();
        self.current_actor_id = None;
        self.current_actor_message = None;
        self.pending_variant_execution = None;

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

    #[cfg(feature = "std")]
    pub fn set_effect_runtime(&mut self, runtime: Arc<EffectRuntime>) {
        self.effect_runtime = runtime;
    }

    #[cfg(feature = "std")]
    pub fn get_effect_runtime(&self) -> Arc<EffectRuntime> {
        self.effect_runtime.clone()
    }

    #[cfg(feature = "std")]
    pub fn get_effect_context(&self) -> Arc<EffectContext> {
        self.effect_context.clone()
    }

    #[cfg(feature = "std")]
    pub fn set_effect_context(&mut self, context: Arc<EffectContext>) {
        self.effect_context = context;
    }

    #[cfg(feature = "std")]
    pub fn set_stdlib_registry(&mut self, registry: StdlibRegistry) {
        self.stdlib = registry;
    }

    #[cfg(feature = "std")]
    pub fn get_stdlib_registry(&self) -> &StdlibRegistry {
        &self.stdlib
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
        // Create initial frame with profiling if enabled
        let start_time = if let Some(profiler) = &self.profiler {
            profiler.record_function_entry(self.bytecode.main_chunk, Some("main".to_string()))
        } else {
            None
        };
        
        self.call_stack.push(CallFrame {
            chunk_id: self.bytecode.main_chunk,
            ip: 0,
            stack_base: 0,
            env: Vec::new(),
            start_time,
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

            // Validate chunk_id first
            if chunk_id >= self.bytecode.chunks.len() {
                return Err(VMError::RuntimeError {
                    message: format!("Invalid chunk_id: {}. Total chunks: {}", chunk_id, self.bytecode.chunks.len()),
                    stack_trace: None,
                });
            }

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
            
            // Check if we have a pending variant execution before executing the next instruction
            if self.pending_variant_execution.is_some() {
                // Take the pending variant to avoid borrow issues
                if let Some((variant_bytecode, variant_chunk_id)) = self.pending_variant_execution.take() {
                    // Get the original function ID for performance tracking
                    let original_function_id = if let Some(frame) = self.call_stack.last() {
                        self.usage_tracker.as_ref()
                            .and_then(|tracker| tracker.read().ok())
                            .and_then(|t| t.chunk_to_node.get(&frame.chunk_id).cloned())
                    } else {
                        None
                    };
                    
                    // Execute the variant with performance monitoring
                    let start_time = web_time::Instant::now();
                    let initial_instruction_count = self.instruction_count;
                    let initial_allocations = self.gc.as_ref().map(|gc| gc.stats().allocations).unwrap_or(0);
                    
                    let execution_result = self.execute_with_variant(variant_bytecode.clone(), variant_chunk_id);
                    
                    // Collect performance metrics
                    let elapsed = start_time.elapsed();
                    let instructions_executed = self.instruction_count - initial_instruction_count;
                    let new_allocations = self.gc.as_ref().map(|gc| gc.stats().allocations).unwrap_or(0) - initial_allocations;
                    
                    // Update learning mode with performance data
                    if let (Some(lm), Some(function_id)) = (&mut self.learning_mode, original_function_id) {
                        use crate::learning_mode::ExecutionMetrics;
                        
                        let metrics = ExecutionMetrics {
                            duration: elapsed,
                            instruction_count: instructions_executed as u64,
                            allocations: new_allocations as u64,
                            cache_misses: 0, // Simulated for now
                            branch_mispredictions: 0, // Simulated for now
                        };
                        
                        // Find the strategy used for this variant
                        if let Some(variant) = lm.get_best_variant(function_id) {
                            lm.update_performance(function_id, variant.strategy, metrics);
                        }
                    }
                    
                    // Re-raise error if execution failed
                    execution_result?;
                    
                    // After variant execution completes, continue with normal execution
                    continue;
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
                        #[cfg(not(target_arch = "wasm32"))]
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
                    } else {
                        // Non-main function returning
                        // The return value is on top of stack (pushed by Return handler)
                        let return_value = self.pop()?;
                        
                        // Get the frame we're returning from
                        let returning_frame = self.call_stack.pop().ok_or_else(|| VMError::RuntimeError {
                            message: "No frame to return from".to_string(),
                            stack_trace: None,
                        })?;
                        
                        // Clean up stack to where it was before the call
                        // (the caller's frame base is where we should restore to)
                        if let Some(caller_frame) = self.call_stack.last() {
                            // For non-main frames, we should restore to the caller's stack position
                            self.stack.truncate(returning_frame.stack_base);
                        }
                        
                        // Push return value for caller
                        self.push(return_value.clone())?;
                        
                        if self.debug_config.enabled {
                            self.debug_config.send_event(VMDebugEvent::FunctionReturn {
                                value: return_value,
                                call_depth: self.call_stack.len(),
                            });
                        }
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
        // Record instruction execution start time if profiling
        let start_time = if let Some(profiler) = &self.profiler {
            if profiler.is_enabled() {
                Some(web_time::Instant::now())
            } else {
                None
            }
        } else {
            None
        };
        
        let pc = self.call_stack.last().map(|f| f.ip.saturating_sub(1)).unwrap_or(0);
        
        use crate::opcode_handlers::{
            ArithmeticHandler, StackHandler, ControlFlowHandler,
            MemoryHandler, CollectionsHandler, ConcurrentHandler,
            EffectsHandler, LogicalHandler, ComparisonHandler, 
            StringHandler, ModuleHandler, OpcodeHandler
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
        let mut logical_handler = LogicalHandler;
        let mut comparison_handler = ComparisonHandler;
        let mut string_handler = StringHandler;
        let mut module_handler = ModuleHandler::new();
        
        // Dispatch to appropriate handler based on opcode category
        let result = match instruction.opcode {
            // Stack operations
            Push | Pop | PopN | Dup | Swap |
            PushInt0 | PushInt1 | PushInt2 | PushIntSmall |
            PushTrue | PushFalse | PushNil | PushConst => {
                stack_handler.execute(self, instruction, chunk_id)
            }
            
            // Arithmetic operations
            Add | Sub | Mul | Div | Mod | Neg |
            AddInt | SubInt | MulInt | DivInt |
            AddFloat | SubFloat | MulFloat | DivFloat => {
                arithmetic_handler.execute(self, instruction, chunk_id)
            }
            
            // Comparison operations
            Eq | Ne | Lt | Le | Gt | Ge |
            LtInt | LeInt | GtInt | GeInt => {
                comparison_handler.execute(self, instruction, chunk_id)
            }
            
            // Logical operations
            And | Or | Not => {
                logical_handler.execute(self, instruction, chunk_id)
            }
            
            // Control flow operations
            Jump | JumpIf | JumpIfNot | Call |
            Return | TailCall => {
                control_flow_handler.execute(self, instruction, chunk_id)
            }
            
            // Memory operations
            Load | Store | LoadLocal | StoreLocal | DefineGlobal |
            LoadLocal0 | LoadLocal1 | LoadLocal2 | LoadLocal3 |
            StoreLocal0 | StoreLocal1 | StoreLocal2 | StoreLocal3 |
            LoadGlobal | StoreGlobal | LoadCaptured | UpdateLocal |
            LoadUpvalue | StoreUpvalue => {
                memory_handler.execute(self, instruction, chunk_id)
            }
            
            // Collection operations
            MakeList | ListGet | ListSet | ListLen | ListEmpty | ListHead | ListTail | ListCons |
            MakeMap | MapGet | MapSet => {
                collections_handler.execute(self, instruction, chunk_id)
            }
            
            // String operations
            StrLen | StrConcat | StrUpper | StrLower => {
                string_handler.execute(self, instruction, chunk_id)
            }
            
            // Concurrent operations
            Await | Spawn | Channel | ChannelWithCapacity | MakeChannel | Send | Receive |
            TrySend | TryReceive | Select | CreateActor | MakeActor | ActorSend |
            ActorReceive | Become | PromiseNew | PromiseAll | PromiseRace |
            WithTimeout => {
                concurrent_handler.execute(self, instruction, chunk_id)
            }
            
            // Effect operations
            Effect | EffectAsync | Perform | Resume |
            Try | TryStart | TryStartWithFinally | TryEnd |
            Catch | Finally | FinallyStart | FinallyEnd | EndFinally |
            Throw | PushHandler | PushFinally | PopHandler |
            MakeHandler | InstallHandler | UninstallHandler => {
                effects_handler.execute(self, instruction, chunk_id)
            }
            
            // Cell operations
            MakeCell | LoadCell | StoreCell | CellGet | CellSet => {
                memory_handler.execute(self, instruction, chunk_id)
            }
            
            // Tagged value operations
            MakeTagged | GetTag | GetTaggedField | IsTagged => {
                memory_handler.execute(self, instruction, chunk_id)
            }
            
            // Module operations
            LoadModule | ImportBinding | ImportAll | LoadQualified |
            BeginModule | EndModule | ExportBinding => {
                module_handler.execute(self, instruction, chunk_id)
            }
            
            // GC operations
            GcAlloc | GcDeref | GcSet | GcCollect => {
                memory_handler.execute(self, instruction, chunk_id)
            }
            
            // Other control flow
            TailReturn | LoopStart | LoopEnd => {
                control_flow_handler.execute(self, instruction, chunk_id)
            }
            
            // Function operations
            MakeFunc | MakeClosure | MakeFuture | MakeEnv | PopEnv => {
                memory_handler.execute(self, instruction, chunk_id)
            }
            
            // Special
            Halt | Nop => {
                match instruction.opcode {
                    Halt => Ok(VMState::Halt),
                    Nop => Ok(VMState::Continue),
                    _ => unreachable!(),
                }
            }
        };
        
        // Record profiling data
        if let (Some(profiler), Some(start_time)) = (&self.profiler, start_time) {
            profiler.record_instruction(chunk_id, pc, instruction.opcode, start_time);
            
            // Record branch information for conditional jumps
            match instruction.opcode {
                JumpIf | JumpIfNot => {
                    if let Ok(VMState::Continue) = &result {
                        // Check if we jumped by comparing current IP with expected next IP
                        let current_ip = self.call_stack.last().map(|f| f.ip).unwrap_or(0);
                        let expected_ip = pc + 1;
                        let jumped = current_ip != expected_ip;
                        profiler.record_branch(chunk_id, pc, jumped);
                    }
                }
                _ => {}
            }
        }
        
        result
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
            Value::Set(s) => !s.is_empty(),
        }
    }

    #[cfg(feature = "std")]
    pub fn vm_value_to_stdlib_value(&self, value: &Value) -> StdlibValue {
        // Since StdlibValue is just a re-export of Value, we can clone directly
        value.clone()
    }

    #[cfg(feature = "std")]
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
                self.push_frame(CallFrame {
                    chunk_id,
                    ip: 0,
                    stack_base,
                    env,
                    start_time: None, // Will be set by push_frame if profiling enabled
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

                    // Validate chunk_id first
                    if chunk_id >= self.bytecode.chunks.len() {
                        return Err(VMError::RuntimeError {
                            message: format!("Invalid chunk_id: {}. Total chunks: {}", chunk_id, self.bytecode.chunks.len()),
                            stack_trace: None,
                        });
                    }

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

                    // Validate chunk_id first
                    if chunk_id >= self.bytecode.chunks.len() {
                        return Err(VMError::RuntimeError {
                            message: format!("Invalid chunk_id: {}. Total chunks: {}", chunk_id, self.bytecode.chunks.len()),
                            stack_trace: None,
                        });
                    }

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
            Value::Set(items) => fluentai_core::value::Value::Set(
                items
                    .iter()
                    .map(|v| self.vm_value_to_core_value(v))
                    .collect(),
            ),
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
            fluentai_core::value::Value::Set(items) => {
                // Convert sets to lists in VM representation
                Value::List(
                    items
                        .iter()
                        .map(|v| self.core_value_to_vm_value(v))
                        .collect(),
                )
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
        let options = crate::compiler::CompilerOptions {
            optimization_level: crate::compiler::OptimizationLevel::Standard,
            debug_info: false,
            #[cfg(feature = "ai-analysis")]
            ai_optimization: false,
            #[cfg(feature = "ai-analysis")]
            hybrid_optimization: false,
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
        #[cfg(feature = "std")]
        {
            module_vm.stdlib = self.stdlib.clone();
        }
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

    pub fn current_chunk(&self) -> usize {
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
        // Validate chunk_id first
        if chunk_id >= self.bytecode.chunks.len() {
            return Err(VMError::RuntimeError {
                message: format!("Invalid chunk_id: {}. Total chunks: {}", chunk_id, self.bytecode.chunks.len()),
                stack_trace: None,
            });
        }
        
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
    
    pub fn get_ip(&self) -> usize {
        self.call_stack.last().map(|f| f.ip).unwrap_or(0)
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
            #[cfg(not(target_arch = "wasm32"))]
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
    
    #[cfg(feature = "std")]
    pub fn create_channel(&mut self) -> ChannelId {
        let channel_id = self.id_generator.next_channel_id();
        let (tx, rx) = mpsc::channel(100); // Default capacity
        self.channels.insert(channel_id, (tx, rx));
        channel_id
    }
    
    #[cfg(feature = "std")]
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
    
    #[cfg(feature = "std")]
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
    
    #[cfg(feature = "std")]
    pub fn get_channel_receiver_mut(&mut self, channel_id: &ChannelId) -> Option<&mut mpsc::Receiver<Value>> {
        self.channels.get_mut(channel_id).map(|(_, rx)| rx)
    }
    
    #[cfg(feature = "std")]
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
    
    #[cfg(feature = "std")]
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
                    task_vm.push_frame(CallFrame {
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
    
    #[cfg(feature = "std")]
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
    
    #[cfg(feature = "std")]
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
    
    /// Get the current actor message being processed
    /// This is used by the ActorReceive opcode
    pub fn get_current_actor_message(&self) -> Option<Value> {
        self.current_actor_message.clone()
    }
    
    /// Get the current actor context (actor ID being executed)
    /// This is used by the Become opcode
    pub fn current_actor_context(&self) -> Option<ActorId> {
        self.current_actor_id
    }
    
    /// Update the state of an actor
    /// This is used by the Become opcode
    pub fn update_actor_state(&mut self, _actor_id: ActorId, _new_state: Value) -> VMResult<()> {
        // TODO: Implement actor state update
        // For now, this is a no-op as actor state management is not fully implemented
        Ok(())
    }
    
    /// Process messages for a specific actor
    #[cfg(feature = "std")]
    pub fn process_actor_messages(&mut self, actor_id: ActorId) -> VMResult<()> {
        // Get the actor (we need to temporarily remove it to avoid borrow issues)
        let mut actor = self.actors.remove(&actor_id).ok_or_else(|| VMError::UnknownIdentifier {
            name: format!("actor:{}", actor_id.0),
            location: None,
            stack_trace: None,
        })?;
        
        // Set current actor context
        self.current_actor_id = Some(actor_id);
        
        // Try to receive a message from the mailbox
        while let Ok(message) = actor.mailbox.try_recv() {
            // Set the current message for ActorReceive opcode
            self.current_actor_message = Some(message.clone());
            
            // For now, we'll set the state as a global variable
            // This is a workaround until we fix the actor compilation
            self.set_global("__state".to_string(), actor.state.clone());
            
            // Call the handler function with state and message
            let handler = actor.handler.clone();
            let state = actor.state.clone();
            
            // Save the current VM state to avoid interfering with it
            let saved_call_stack = self.call_stack.clone();
            let saved_stack_len = self.stack.len();
            
            // Clear the call stack for actor execution
            self.call_stack.clear();
            
            // Push handler, state, and message onto stack
            self.push(handler)?;
            self.push(state)?;
            self.push(message)?;
            
            // Call the handler with 2 arguments (state and message)
            // Track the call stack depth for this execution
            let initial_stack_depth = self.stack.len();
            
            // Create a simple call - push function and args, then Call opcode
            let msg = self.pop()?;   // message (second arg)
            let state = self.pop()?;  // state (first arg)
            let func = self.pop()?;   // handler function
            
            // Push them back in the right order for Call
            self.push(state)?;     // first arg
            self.push(msg)?;       // second arg
            self.push(func)?;      // function on top
            
            // Create a temporary main frame to execute from
            self.push_frame(CallFrame {
                chunk_id: 0,  // Use main chunk (doesn't matter, we won't execute from it)
                ip: 0,
                stack_base: 0,
                env: Vec::new(),
                start_time: None,
            });
            
            // Execute call with 2 arguments
            match self.execute_instruction(
                &Instruction::with_arg(Opcode::Call, 2), 
                0  // chunk_id doesn't matter for Call instruction
            )? {
                VMState::Continue => {
                    // Continue executing until we return to the initial level
                    while self.call_stack.len() > 1 {
                        let result = self.run_inner();
                        if let Err(e) = result {
                            // Restore state before propagating error
                            self.call_stack = saved_call_stack;
                            self.stack.truncate(saved_stack_len);
                            return Err(e);
                        }
                    }
                    // Pop the temporary main frame
                    self.call_stack.pop();
                }
                VMState::Return => {
                    // Should not happen for Call instruction
                    self.call_stack.pop();
                }
                VMState::Halt => {
                    self.call_stack.pop();
                    return Err(VMError::RuntimeError {
                        message: "Actor handler halted unexpectedly".to_string(),
                        stack_trace: None,
                    });
                }
            }
            
            // The result is the new state (handler should return new state)
            // In the simple test, the handler just returns the current state
            let result = self.pop()?; // Get the return value from handler
            actor.state = result;
            
            // Restore the VM state
            self.call_stack = saved_call_stack;
            self.stack.truncate(saved_stack_len);
            
            // Clear the current message and state
            self.current_actor_message = None;
            self.globals.remove("__state");
        }
        
        // Clear actor context
        self.current_actor_id = None;
        
        // Put the actor back
        self.actors.insert(actor_id, actor);
        Ok(())
    }
    
    /// Process all pending actor messages
    /// This is used in tests to drive actor message processing
    #[cfg(feature = "std")]
    pub fn process_all_actor_messages(&mut self) -> VMResult<()> {
        let actor_ids: Vec<ActorId> = self.actors.keys().cloned().collect();
        for actor_id in actor_ids {
            self.process_actor_messages(actor_id)?;
        }
        Ok(())
    }
    
    /// Process all pending actor messages (no-op for no_std)
    #[cfg(not(feature = "std"))]
    pub fn process_all_actor_messages(&mut self) -> VMResult<()> {
        Ok(())
    }
    
    #[cfg(feature = "std")]
    pub fn take_promise(&mut self, promise_id: &PromiseId) -> Option<oneshot::Receiver<VMResult<Value>>> {
        self.promises.remove(promise_id)
    }
    
    /// Get the ID generator for creating new IDs
    pub fn id_generator(&mut self) -> &mut IdGenerator {
        &mut self.id_generator
    }
    
    /// Get the pending promise bodies map
    #[cfg(feature = "std")]
    pub fn pending_promise_bodies(&mut self) -> &mut FxHashMap<PromiseId, Value> {
        &mut self.pending_promise_bodies
    }
    
    /// Get mutable access to promises map
    #[cfg(feature = "std")]
    pub fn promises_mut(&mut self) -> &mut FxHashMap<PromiseId, oneshot::Receiver<VMResult<Value>>> {
        &mut self.promises
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
    
    #[cfg(feature = "std")]
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
        // Check if it's a stdlib function with __stdlib__ prefix
        #[cfg(feature = "std")]
        if let Some(func_name) = native_func.strip_prefix("__stdlib__") {
            // Handle stdlib function calls
            if let Some(stdlib_func) = self.stdlib.get(func_name) {
                // Convert VM values to stdlib values
                let stdlib_args: Vec<StdlibValue> = args
                    .iter()
                    .map(|v| self.vm_value_to_stdlib_value(v))
                    .collect();

                // Check if it's a higher-order function that needs special handling
                match func_name {
                    "map" | "filter" | "fold" | "reduce" => {
                        // For method calls, we need to reorder arguments
                        // Method syntax: list.map(fn) -> args are [list, fn]
                        // Stdlib expects: map(fn, list)
                        let reordered_args = if func_name == "fold" || func_name == "reduce" {
                            // fold/reduce: list.reduce(init, fn) -> fold(fn, init, list)
                            if args.len() == 3 {
                                vec![args[2].clone(), args[1].clone(), args[0].clone()]
                            } else {
                                args
                            }
                        } else {
                            // map/filter: list.map(fn) -> map(fn, list)
                            if args.len() == 2 {
                                vec![args[1].clone(), args[0].clone()]
                            } else {
                                args
                            }
                        };
                        
                        // Use the VM's higher-order stdlib implementation
                        use crate::stdlib_bridge::VMStdlibExt;
                        let result = self.call_higher_order_stdlib(func_name, &reordered_args)?;
                        self.push(result)?;
                    }
                    _ => {
                        // For non-higher-order functions, we can't create VM callback
                        // so we'll use NoOpVMCallback 
                        let mut context = fluentai_stdlib::vm_bridge::StdlibContext::default();
                        context.effect_context_override = Some(self.effect_context.clone());

                        // Call the stdlib function with context
                        let stdlib_result = stdlib_func
                            .call_with_context(&mut context, &stdlib_args)
                            .map_err(|e| VMError::RuntimeError {
                                message: format!("Stdlib function '{}' failed: {}", func_name, e),
                                stack_trace: Some(self.build_stack_trace()),
                            })?;

                        // Convert result back to VM value
                        let vm_result = self.stdlib_value_to_vm_value(&stdlib_result);
                        self.push(vm_result)?;
                    }
                }
            } else {
                return Err(VMError::UnknownIdentifier {
                    name: func_name.to_string(),
                    location: None,
                    stack_trace: None,
                });
            }
        } else if let Some(func_name) = native_func.strip_prefix("__builtin__") {
            // Handle builtin function calls
            if func_name == "Printable" {
                // Special handling for Printable constructor
                // It takes one argument (the content) and creates a Tagged value
                if args.len() != 1 {
                    return Err(VMError::RuntimeError {
                        message: format!("Printable expects 1 argument, got {}", args.len()),
                        stack_trace: Some(self.build_stack_trace()),
                    });
                }
                
                // Extract the string content
                let content = match &args[0] {
                    Value::String(s) => s.clone(),
                    other => {
                        // Convert other values to string representation
                        format!("{}", other)
                    }
                };
                
                // Create a Tagged value with the content as tag and "Printable" as value
                let tagged = Value::Tagged {
                    tag: content,
                    values: vec![Value::String("Printable".to_string())],
                };
                
                self.push(tagged)?;
            } else {
                return Err(VMError::RuntimeError {
                    message: format!("Builtin function '{}' should be compiled to opcode", func_name),
                    stack_trace: Some(self.build_stack_trace()),
                });
            }
        } else {
            // Regular native function - not implemented yet
            return Err(VMError::RuntimeError {
                message: format!("Native function '{}' not implemented", native_func),
                stack_trace: Some(self.build_stack_trace()),
            });
        }
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
    pub fn perform_effect(&mut self, effect_type: String, operation: String, args: Vec<Value>) -> VMResult<()> {
        let effect_type = effect_type.as_str();
        let effect_op = operation.as_str();
        
        // The effect type might come as "IO" or "Dom" from debug format
        let effect_type = effect_type.trim_matches('"');
        
        match effect_type {
            "IO" => {
                // Handle IO effects through stdlib
                match effect_op {
                    "print" => {
                        if args.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "IO.print requires an argument".to_string(),
                                stack_trace: None,
                            });
                        }
                        self.perform_io_print(args[0].clone())?;
                        self.push(Value::Nil)?;
                    }
                    "println" => {
                        if args.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "IO.println requires an argument".to_string(),
                                stack_trace: None,
                            });
                        }
                        self.perform_io_println(args[0].clone())?;
                        self.push(Value::Nil)?;
                    }
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown IO operation: {}", effect_op),
                        stack_trace: None,
                    }),
                }
            }
            "Dom" => {
                // Handle Dom effects
                // Note: Renderer integration is now done externally via WASM bindings
                // The VM provides a simple mock implementation for testing
                match effect_op {
                        "create_element" => {
                            // Dom.create_element(type, props) -> element_id
                            if args.len() < 2 {
                                return Err(VMError::RuntimeError {
                                    message: "Dom.create_element requires type and props".to_string(),
                                    stack_trace: None,
                                });
                            }
                            // For now, just print what would be created
                            println!("DOM: Creating {} element", args[0]);
                            if let Value::List(props) = &args[1] {
                                println!("     Properties: {:?}", props);
                            }
                            // Return a mock element ID
                            self.push(Value::String(format!("element_{}", self.stack.len())))?;
                        }
                        "create_surface" => {
                            // Dom.create_surface(name, props) -> surface_id
                            if args.len() < 2 {
                                return Err(VMError::RuntimeError {
                                    message: "Dom.create_surface requires name and props".to_string(),
                                    stack_trace: None,
                                });
                            }
                            println!("DOM: Creating surface '{}'", args[0]);
                            self.push(Value::String(format!("surface_{}", args[0])))?;
                        }
                        "update_element" => {
                            // Dom.update_element(id, props) -> unit
                            if args.len() < 2 {
                                return Err(VMError::RuntimeError {
                                    message: "Dom.update_element requires id and props".to_string(),
                                    stack_trace: None,
                                });
                            }
                            println!("DOM: Updating element {}", args[0]);
                            self.push(Value::Nil)?;
                        }
                        _ => {
                            return Err(VMError::RuntimeError {
                                message: format!("Unknown Dom operation: {}", effect_op),
                                stack_trace: None,
                            });
                        }
                    }
            }
            "Time" => {
                match effect_op {
                    "now" => {
                        // Return current time in milliseconds since Unix epoch
                        use std::time::{SystemTime, UNIX_EPOCH};
                        let duration = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .map_err(|_| VMError::RuntimeError {
                                message: "System time before Unix epoch".to_string(),
                                stack_trace: None,
                            })?;
                        let millis = duration.as_millis() as i64;
                        self.push(Value::Integer(millis))?;
                    }
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown Time operation: {}", effect_op),
                        stack_trace: None,
                    }),
                }
            }
            "Random" => {
                match effect_op {
                    "int" => {
                        if args.len() < 2 {
                            return Err(VMError::RuntimeError {
                                message: "Random.int requires min and max arguments".to_string(),
                                stack_trace: None,
                            });
                        }
                        let min = match &args[0] {
                            Value::Integer(i) => *i,
                            _ => return Err(VMError::RuntimeError {
                                message: "Random.int min must be an integer".to_string(),
                                stack_trace: None,
                            }),
                        };
                        let max = match &args[1] {
                            Value::Integer(i) => *i,
                            _ => return Err(VMError::RuntimeError {
                                message: "Random.int max must be an integer".to_string(),
                                stack_trace: None,
                            }),
                        };
                        // Simple deterministic "random" for now
                        let random_val = (min + max) / 2; // Just return midpoint for determinism
                        self.push(Value::Integer(random_val))?;
                    }
                    "float" => {
                        // Return a deterministic "random" float between 0 and 1
                        self.push(Value::Float(0.5))?;
                    }
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown Random operation: {}", effect_op),
                        stack_trace: None,
                    }),
                }
            }
            "State" => {
                match effect_op {
                    "get" => {
                        if args.is_empty() {
                            return Err(VMError::RuntimeError {
                                message: "State.get requires a key argument".to_string(),
                                stack_trace: None,
                            });
                        }
                        let key = match &args[0] {
                            Value::String(s) => s,
                            _ => return Err(VMError::RuntimeError {
                                message: "State.get key must be a string".to_string(),
                                stack_trace: None,
                            }),
                        };
                        // Use globals for state storage
                        let state_key = format!("__state_{}", key);
                        if std::env::var("VM_DEBUG").is_ok() {
                            eprintln!("State.get: Looking for key {}", state_key);
                        }
                        if let Some(value) = self.get_global(&state_key) {
                            if std::env::var("VM_DEBUG").is_ok() {
                                eprintln!("State.get: Found value {:?}", value);
                            }
                            self.push(value.clone())?;
                        } else {
                            if std::env::var("VM_DEBUG").is_ok() {
                                eprintln!("State.get: Key not found, returning nil");
                            }
                            self.push(Value::Nil)?;
                        }
                    }
                    "set" => {
                        if args.len() < 2 {
                            return Err(VMError::RuntimeError {
                                message: "State.set requires key and value arguments".to_string(),
                                stack_trace: None,
                            });
                        }
                        let key = match &args[0] {
                            Value::String(s) => s,
                            _ => return Err(VMError::RuntimeError {
                                message: "State.set key must be a string".to_string(),
                                stack_trace: None,
                            }),
                        };
                        let value = args[1].clone();
                        let state_key = format!("__state_{}", key);
                        if std::env::var("VM_DEBUG").is_ok() {
                            eprintln!("State.set: Setting {} = {:?}", state_key, value);
                        }
                        self.set_global(state_key, value);
                        self.push(Value::Nil)?;
                    }
                    _ => return Err(VMError::RuntimeError {
                        message: format!("Unknown State operation: {}", effect_op),
                        stack_trace: None,
                    }),
                }
            }
            _ => {
                // For other effects, try the effect context
                // This is a simplified version - in reality we'd need to handle async effects properly
                return Err(VMError::RuntimeError {
                    message: format!("Unhandled effect type: {}", effect_type),
                    stack_trace: None,
                });
            }
        }
        
        Ok(())
    }
    
    pub fn perform_effect_async(&mut self, effect_type: String, operation: String, args: Vec<Value>) -> VMResult<()> {
        // For now, just handle it synchronously
        self.perform_effect(effect_type, operation, args)
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
    
    pub fn throw_error(&mut self, _error: Value) -> VMResult<VMState> {
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
    
    // Helper methods for effects
    fn perform_io_print(&mut self, value: Value) -> VMResult<()> {
        // Convert to string and print
        let s = match value {
            Value::String(s) => s,
            Value::Tagged { tag, values } if tag == "Printable" => {
                if let Some(inner) = values.first() {
                    match inner {
                        Value::String(s) => s.clone(),
                        _ => inner.to_string(),
                    }
                } else {
                    String::new()
                }
            }
            _ => value.to_string(),
        };
        print!("{}", s);
        Ok(())
    }
    
    fn perform_io_println(&mut self, value: Value) -> VMResult<()> {
        // Convert to string and print with newline
        let s = match value {
            Value::String(s) => s,
            Value::Tagged { tag, values } if tag == "Printable" => {
                if let Some(inner) = values.first() {
                    match inner {
                        Value::String(s) => s.clone(),
                        _ => inner.to_string(),
                    }
                } else {
                    String::new()
                }
            }
            _ => value.to_string(),
        };
        println!("{}", s);
        Ok(())
    }
    
    #[cfg(feature = "renderer")]
    fn value_to_json(&self, value: Value) -> VMResult<serde_json::Value> {
        match value {
            Value::Nil => Ok(serde_json::Value::Null),
            Value::Boolean(b) => Ok(serde_json::Value::Bool(b)),
            Value::Integer(i) => Ok(serde_json::Value::Number(i.into())),
            Value::Float(f) => {
                serde_json::Number::from_f64(f)
                    .map(serde_json::Value::Number)
                    .ok_or_else(|| VMError::RuntimeError("Invalid float value".to_string()))
            }
            Value::String(s) => Ok(serde_json::Value::String(s)),
            Value::List(list) => {
                let json_list: Result<Vec<_>, _> = list.into_iter()
                    .map(|v| self.value_to_json(v))
                    .collect();
                Ok(serde_json::Value::Array(json_list?))
            }
            Value::Map(map) => {
                let mut json_map = serde_json::Map::new();
                for (k, v) in map {
                    let key = match k {
                        Value::String(s) => s,
                        _ => k.to_string(),
                    };
                    json_map.insert(key, self.value_to_json(v)?);
                }
                Ok(serde_json::Value::Object(json_map))
            }
            _ => Err(VMError::RuntimeError(format!("Cannot convert {} to JSON", value.type_name()))),
        }
    }
    
    #[cfg(feature = "renderer")]
    fn json_to_value(&self, json: serde_json::Value) -> VMResult<Value> {
        match json {
            serde_json::Value::Null => Ok(Value::Nil),
            serde_json::Value::Bool(b) => Ok(Value::Boolean(b)),
            serde_json::Value::Number(n) => {
                if let Some(i) = n.as_i64() {
                    Ok(Value::Integer(i))
                } else if let Some(f) = n.as_f64() {
                    Ok(Value::Float(f))
                } else {
                    Err(VMError::RuntimeError("Invalid number value".to_string()))
                }
            }
            serde_json::Value::String(s) => Ok(Value::String(s)),
            serde_json::Value::Array(arr) => {
                let values: Result<Vec<_>, _> = arr.into_iter()
                    .map(|v| self.json_to_value(v))
                    .collect();
                Ok(Value::List(values?))
            }
            serde_json::Value::Object(obj) => {
                let mut map = Vec::new();
                for (k, v) in obj {
                    map.push((Value::String(k), self.json_to_value(v)?));
                }
                Ok(Value::Map(map))
            }
        }
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
    #[cfg(feature = "std")]
    pub fn get_channel(&self, channel_id: &crate::safety::ChannelId) -> Option<&(mpsc::Sender<Value>, mpsc::Receiver<Value>)> {
        self.channels.get(channel_id)
    }
    
    /// Get a mutable reference to a channel
    #[cfg(feature = "std")]
    pub fn get_channel_mut(&mut self, channel_id: &crate::safety::ChannelId) -> Option<&mut (mpsc::Sender<Value>, mpsc::Receiver<Value>)> {
        self.channels.get_mut(channel_id)
    }
    
    // ===== Module system methods =====
    
    /// Register a compiled module with the VM
    pub fn register_module(
        &mut self,
        graph: &fluentai_core::ast::Graph,
        bytecode: Arc<Bytecode>,
        path: Option<String>,
    ) -> VMResult<String> {
        self.module_registry
            .register_module(graph, bytecode, path)
            .map_err(|e| VMError::RuntimeError {
                message: e.to_string(),
                stack_trace: None,
            })
    }
    
    /// Get an exported value from a module
    pub fn get_module_export(&mut self, module_name: &str, export_name: &str) -> VMResult<Value> {
        // First try to get the export directly
        match self.module_registry.get_export(module_name, export_name) {
            Ok(value) => Ok(value),
            Err(e) => {
                // Check if the module needs execution
                if !self.module_registry.is_module_executed(module_name) {
                    // Execute the module to populate exports
                    if let Some(module_info) = self.module_registry.get_module(module_name) {
                        let bytecode = module_info.bytecode.clone();
                        let exports = module_info.exports.clone();
                        
                        // Create a new VM instance for module execution
                        let mut module_vm = VM::with_shared_bytecode(bytecode);
                        
                        // Run the module's main chunk
                        if let Ok(result) = module_vm.run() {
                            // Extract exported values
                            for export in &exports {
                                if let Some(value) = module_vm.get_global(&export.name) {
                                    let _ = self.module_registry.set_export_value(
                                        module_name,
                                        &export.name,
                                        value.clone(),
                                    );
                                }
                            }
                            
                            // Mark module as executed
                            let _ = self.module_registry.mark_module_executed(module_name);
                            
                            // Try again to get the export
                            return self.module_registry
                                .get_export(module_name, export_name)
                                .map_err(|e| VMError::RuntimeError {
                                    message: e.to_string(),
                                    stack_trace: None,
                                });
                        }
                    }
                }
                
                // If we get here, return the original error
                Err(VMError::RuntimeError {
                    message: e.to_string(),
                    stack_trace: None,
                })
            }
        }
    }
    
    /// Set a module export value (called after module execution)
    pub fn set_module_export(
        &mut self,
        module_name: &str,
        export_name: &str,
        value: Value,
    ) -> VMResult<()> {
        self.module_registry
            .set_export_value(module_name, export_name, value)
            .map_err(|e| VMError::RuntimeError {
                message: e.to_string(),
                stack_trace: None,
            })
    }
    
    /// Check if a module is loaded
    pub fn has_module(&self, module_name: &str) -> bool {
        self.module_registry.has_module(module_name)
    }
    
    /// Get the module registry
    pub fn module_registry(&self) -> &ModuleRegistry {
        &self.module_registry
    }
    
    /// Get a mutable reference to the module registry
    pub fn module_registry_mut(&mut self) -> &mut ModuleRegistry {
        &mut self.module_registry
    }
    
    /// Call a procedure by ID
    pub fn call_procedure(&mut self, proc_id: u64, args: Vec<Value>) -> VMResult<Value> {
        // For now, this is a placeholder implementation
        // In a full implementation, we would look up the procedure and execute it
        Err(VMError::RuntimeError {
            message: format!("Procedure {} not implemented", proc_id),
            stack_trace: None,
        })
    }
    
    // Accessor methods for opcode handlers
    
    /// Get the current call frame
    pub fn current_frame(&self) -> &CallFrame {
        self.call_stack.last()
            .expect("No active call frame")
    }
    
    /// Get mutable access to cells
    pub fn cells(&self) -> &Vec<Value> {
        &self.cells
    }
    
    /// Get mutable access to cells
    pub fn cells_mut(&mut self) -> &mut Vec<Value> {
        &mut self.cells
    }
    
    /// Get reference to usage tracker
    pub fn usage_tracker_ref(&self) -> Option<&UsageTracker> {
        self.usage_tracker.as_ref().and_then(|arc| {
            arc.try_read().ok().map(|guard| {
                // This is a workaround - we can't return a reference from a guard
                // In practice, the handlers should be refactored to work with Arc<RwLock>
                // For now, we'll return None
                None as Option<&UsageTracker>
            }).flatten()
        })
    }
    
    /// Get mutable reference to usage tracker
    pub fn usage_tracker_mut(&mut self) -> Option<&mut UsageTracker> {
        // Due to the Arc<RwLock> wrapper, we can't get a direct mutable reference
        // This needs to be refactored to work with the locking mechanism
        None
    }
    
    /// Enable profiling
    pub fn enable_profiling(&mut self) {
        if self.profiler.is_none() {
            self.profiler = Some(Arc::new(Profiler::new()));
        }
        if let Some(profiler) = &self.profiler {
            profiler.enable();
        }
    }
    
    /// Disable profiling
    pub fn disable_profiling(&mut self) {
        if let Some(profiler) = &self.profiler {
            profiler.disable();
        }
    }
    
    /// Get profiler reference
    pub fn profiler(&self) -> Option<&Arc<Profiler>> {
        self.profiler.as_ref()
    }
    
    /// Get mutable reference to current frame
    pub fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut()
            .expect("No active call frame")
    }
    
    /// Push loop tracking for profiling
    pub fn push_loop_tracking(&mut self, loop_id: NodeId) {
        self.loop_stack.push((loop_id, 0, web_time::Instant::now()));
    }
    
    /// Pop loop tracking and return loop info
    pub fn pop_loop_tracking(&mut self) -> Option<(NodeId, u64)> {
        self.loop_stack.pop().map(|(id, count, _)| (id, count as u64))
    }
    
    /// Increment current loop iteration count
    pub fn increment_loop_iteration(&mut self) {
        if let Some((_, count, _)) = self.loop_stack.last_mut() {
            *count += 1;
        }
    }
    
    /// Record a value observation for profiling
    pub fn record_value_observation(&self, node_id: NodeId, value: &Value) {
        if let Some(profiler) = &self.profiler {
            // Convert value to a string representation for profiling
            let value_str = match value {
                Value::Integer(n) => format!("int:{}", n),
                Value::Float(f) => format!("float:{}", f),
                Value::Boolean(b) => format!("bool:{}", b),
                Value::String(s) => format!("str:len={}", s.len()),
                Value::Symbol(s) => format!("sym:{}", s),
                Value::Nil => "nil".to_string(),
                Value::List(l) => format!("list:len={}", l.len()),
                Value::Vector(v) => format!("vec:len={}", v.len()),
                Value::Map(m) => format!("map:len={}", m.len()),
                Value::Function { chunk_id, .. } => format!("func:chunk={}", chunk_id),
                _ => "other".to_string(),
            };
            profiler.record_value(node_id, &value_str);
        }
    }
    
    /// Get reference to current frame
    pub fn current_frame_ref(&self) -> Option<&CallFrame> {
        self.call_stack.last()
    }
    
    /// Get a debug representation of the current stack
    pub fn debug_stack(&self) -> Vec<String> {
        self.stack.iter().enumerate().map(|(i, v)| {
            format!("[{}] {:?}", i, v)
        }).collect()
    }
    
    // Learning mode methods
    
    /// Enable learning mode with default configuration
    pub fn enable_learning_mode(&mut self) {
        self.enable_learning_mode_with_config(crate::learning_mode::LearningModeConfig::default());
    }
    
    /// Enable learning mode with custom configuration
    pub fn enable_learning_mode_with_config(&mut self, config: crate::learning_mode::LearningModeConfig) {
        self.learning_mode = Some(Box::new(crate::learning_mode::LearningModeManager::new(config)));
        
        // Also enable profiling as learning mode depends on it
        self.enable_profiling();
    }
    
    /// Disable learning mode
    pub fn disable_learning_mode(&mut self) {
        self.learning_mode = None;
    }
    
    /// Check if learning mode is enabled
    pub fn is_learning_mode_enabled(&self) -> bool {
        self.learning_mode.is_some()
    }
    
    /// Get learning mode statistics
    pub fn get_learning_statistics(&self) -> Option<crate::learning_mode::LearningStatistics> {
        self.learning_mode.as_ref().map(|lm| lm.get_statistics())
    }
    
    /// Set exploration mode for learning mode
    pub fn set_exploration_mode(&mut self, mode: crate::learning_mode::ExplorationMode) {
        if let Some(manager) = &mut self.learning_mode {
            manager.config.exploration_mode = mode;
        }
    }
    
    /// Save learned optimization data
    pub fn save_learned_data(&self, path: &str) -> VMResult<()> {
        if let Some(lm) = &self.learning_mode {
            lm.save_learned_data(path)?;
        }
        Ok(())
    }
    
    /// Load previously learned optimization data
    pub fn load_learned_data(&mut self, path: &str) -> VMResult<()> {
        if let Some(lm) = &mut self.learning_mode {
            lm.load_learned_data(path)?;
        }
        Ok(())
    }
    
    /// Set RL agent for intelligent optimization selection
    #[cfg(feature = "ai-analysis")]
    pub fn set_rl_agent(&mut self, agent: Box<dyn crate::learning_mode::RLAgentInterface>) {
        if let Some(lm) = &mut self.learning_mode {
            lm.set_rl_agent(agent);
        }
    }
    
    /// Set the AST graph for variant compilation
    pub fn set_ast_graph(&mut self, graph: Arc<Graph>) {
        self.ast_graph = Some(graph);
    }
    
    /// Add a pre-compiled variant for a function
    pub fn add_compiled_variant(&mut self, function_id: NodeId, variant: crate::learning_mode::CompiledVariant) {
        if let Some(lm) = &mut self.learning_mode {
            lm.add_variant(function_id, variant);
        }
    }
    
    /// Handle function call with learning mode
    /// Returns the chunk_id to use (potentially optimized variant)
    pub fn get_optimized_chunk_id(&mut self, original_chunk_id: usize) -> usize {
        if let Some(lm) = &mut self.learning_mode {
            // Map chunk_id to function NodeId using usage tracker
            let function_id = if let Some(tracker) = &self.usage_tracker {
                if let Ok(tracker) = tracker.read() {
                    if let Some(&node_id) = tracker.chunk_to_node.get(&original_chunk_id) {
                        node_id
                    } else {
                        // Fallback: create NodeId from chunk_id
                        NodeId(std::num::NonZeroU32::new(original_chunk_id as u32 + 1).unwrap())
                    }
                } else {
                    NodeId(std::num::NonZeroU32::new(original_chunk_id as u32 + 1).unwrap())
                }
            } else {
                NodeId(std::num::NonZeroU32::new(original_chunk_id as u32 + 1).unwrap())
            };
            
            // Record execution
            if lm.record_execution(function_id) {
                // Function just became hot, start exploration
                if lm.should_explore(function_id) {
                    if let Some(graph) = self.ast_graph.clone() {
                        let strategies = lm.start_exploration(function_id, &graph);
                        
                        // Extract info we need before dropping the mutable borrow
                        let chunk_info = self.bytecode.chunks.get(original_chunk_id)
                            .map(|chunk| chunk.name.as_deref().unwrap_or("anonymous").to_string());
                        
                        // Temporarily take the learning mode to avoid borrow issues
                        let mut learning_mode = self.learning_mode.take().unwrap();
                        
                        // Compile variants for the hot function
                        if let Some(function_name) = chunk_info {
                            self.compile_variants_for_function(function_id, &function_name, &graph, &strategies);
                        }
                        
                        // Put the learning mode back
                        self.learning_mode = Some(learning_mode);
                    }
                }
            }
            
            // Re-borrow learning mode for checking variants
            let lm = self.learning_mode.as_mut().unwrap();
            
            // Check if we have an optimized variant
            if let Some(variant) = lm.get_best_variant(function_id) {
                eprintln!("Learning mode: Using optimized variant with strategy {:?} for function {:?}", 
                         variant.strategy, function_id);
                
                // Clone bytecode to avoid borrow issues
                let variant_bytecode = variant.bytecode.clone();
                
                // Drop the mutable borrow of learning mode
                drop(lm);
                
                // Find the corresponding chunk ID in the variant bytecode
                if let Some(variant_chunk_id) = self.find_variant_chunk_id(&variant_bytecode, original_chunk_id) {
                    // Schedule variant execution
                    self.pending_variant_execution = Some((variant_bytecode, variant_chunk_id));
                    
                    // The actual execution will happen in the main execution loop
                    // after we return from this method
                } else {
                    eprintln!("Learning mode: Could not find matching chunk in variant bytecode");
                }
            }
        }
        
        original_chunk_id
    }
    
    /// Truncate stack to specified size
    pub fn truncate_stack(&mut self, size: usize) {
        self.stack.truncate(size);
    }
    
    /// Check if call stack is empty
    pub fn call_stack_is_empty(&self) -> bool {
        self.call_stack.is_empty()
    }
    
    /// Set current chunk ID (updates the top frame)
    pub fn set_chunk(&mut self, chunk_id: usize) {
        if let Some(frame) = self.call_stack.last_mut() {
            frame.chunk_id = chunk_id;
        }
    }
    
    /// Push a new call frame
    pub fn push_frame(&mut self, mut frame: CallFrame) {
        // Record function entry if profiling
        if let Some(profiler) = &self.profiler {
            let start_time = profiler.record_function_entry(frame.chunk_id, None);
            if let Some(start) = start_time {
                frame.start_time = Some(start);
            }
        }
        self.call_stack.push(frame);
    }
    
    /// Pop a call frame
    pub fn pop_frame(&mut self) -> VMResult<CallFrame> {
        let frame = self.call_stack.pop()
            .ok_or_else(|| VMError::RuntimeError {
                message: "Call stack underflow".to_string(),
                stack_trace: None,
            })?;
        
        // Record function exit if profiling
        if let Some(profiler) = &self.profiler {
            if let Some(start_time) = frame.start_time {
                profiler.record_function_exit(frame.chunk_id, start_time);
            }
        }
        
        Ok(frame)
    }
    
    /// Compile variants for a function with different optimization strategies
    fn compile_variants_for_function(
        &mut self,
        function_id: NodeId,
        function_name: &str,
        graph: &Arc<Graph>,
        strategies: &[crate::learning_mode::OptimizationStrategy],
    ) {
        use crate::variant_compiler::VariantCompiler;
        use crate::learning_mode::CompiledVariant;
        
        let mut variant_compiler = VariantCompiler::new();
        
        for &strategy in strategies {
            match variant_compiler.compile_variant(graph, function_name, strategy) {
                Ok(bytecode) => {
                    let compilation_start = web_time::Instant::now();
                    let binary_size = bytecode.chunks.iter()
                        .map(|chunk| chunk.instructions.len() * std::mem::size_of::<fluentai_bytecode::Instruction>())
                        .sum();
                    
                    // Create compiled variant
                    let variant = CompiledVariant {
                        strategy,
                        bytecode: Arc::new(bytecode),
                        compilation_time: compilation_start.elapsed(),
                        binary_size,
                    };
                    
                    // Add to learning mode manager
                    if let Some(lm) = &mut self.learning_mode {
                        lm.add_variant(function_id, variant);
                    }
                    
                    eprintln!("Learning mode: Compiled variant for function {} with strategy {}", 
                             function_name, format_strategy(strategy));
                }
                Err(e) => {
                    eprintln!("Learning mode: Failed to compile variant for function {} with strategy {:?}: {}", 
                             function_name, strategy, e);
                }
            }
        }
    }
    
    /// Find the chunk ID in variant bytecode that corresponds to the original chunk ID
    fn find_variant_chunk_id(&self, variant_bytecode: &Bytecode, original_chunk_id: usize) -> Option<usize> {
        // Look for a chunk with the same name as the original
        if let Some(original_chunk) = self.bytecode.chunks.get(original_chunk_id) {
            if let Some(original_name) = &original_chunk.name {
                // Find chunk with matching name in variant bytecode
                for (idx, chunk) in variant_bytecode.chunks.iter().enumerate() {
                    if chunk.name.as_ref() == Some(original_name) {
                        return Some(idx);
                    }
                }
            }
        }
        None
    }
    
    /// Execute a function using optimized variant bytecode
    pub fn execute_with_variant(
        &mut self,
        variant_bytecode: Arc<Bytecode>,
        variant_chunk_id: usize,
    ) -> VMResult<()> {
        // Save current state for recovery
        let original_bytecode = self.bytecode.clone();
        let original_stack_len = self.stack.len();
        let original_call_stack_len = self.call_stack.len();
        let original_instruction_count = self.instruction_count;
        
        // Validate variant chunk exists
        if variant_chunk_id >= variant_bytecode.chunks.len() {
            eprintln!("Learning mode: Invalid variant chunk ID {}, skipping variant execution", variant_chunk_id);
            return Ok(());
        }
        
        // Create a new call frame for the variant execution
        let frame = CallFrame {
            chunk_id: variant_chunk_id,
            ip: 0,
            stack_base: self.stack.len(),
            env: Vec::new(),
            start_time: if let Some(profiler) = &self.profiler {
                profiler.record_function_entry(variant_chunk_id, Some("variant".to_string()))
            } else {
                None
            },
        };
        
        // Temporarily switch to variant bytecode
        self.bytecode = variant_bytecode.clone();
        
        // Push the frame and execute with error recovery
        self.push_frame(frame);
        
        // Log variant execution start
        eprintln!("Learning mode: Starting variant execution for chunk {} with {} instructions", 
                 variant_chunk_id, 
                 variant_bytecode.chunks.get(variant_chunk_id)
                     .map(|c| c.instructions.len())
                     .unwrap_or(0));
        
        // Execute until this frame completes
        let result = match self.run_until_frame_complete() {
            Ok(()) => {
                eprintln!("Learning mode: Variant execution completed successfully");
                Ok(())
            },
            Err(e) => {
                // Recovery: restore original state
                eprintln!("Learning mode: Variant execution failed: {:?}", e);
                eprintln!("Learning mode: Recovering to original execution state");
                
                // Restore VM state
                self.bytecode = original_bytecode.clone();
                self.stack.truncate(original_stack_len);
                self.call_stack.truncate(original_call_stack_len);
                self.instruction_count = original_instruction_count;
                
                // Return error to propagate up
                Err(e)
            }
        };
        
        // Always restore original bytecode
        self.bytecode = original_bytecode;
        
        result
    }
    
    /// Run VM until the current call frame completes
    fn run_until_frame_complete(&mut self) -> VMResult<()> {
        let target_depth = self.call_stack.len() - 1;
        
        loop {
            // Get current frame info
            let frame = self.call_stack.last()
                .ok_or_else(|| VMError::StackUnderflow {
                    operation: "get_current_frame".to_string(),
                    stack_size: self.call_stack.len(),
                    stack_trace: None,
                })?;
            let chunk_id = frame.chunk_id;
            let ip = frame.ip;
            
            // Validate chunk and instruction
            if chunk_id >= self.bytecode.chunks.len() {
                return Err(VMError::RuntimeError {
                    message: format!("Invalid chunk_id: {}. Total chunks: {}", chunk_id, self.bytecode.chunks.len()),
                    stack_trace: None,
                });
            }
            if ip >= self.bytecode.chunks[chunk_id].instructions.len() {
                return Err(VMError::InvalidJumpTarget {
                    target: ip,
                    chunk_size: self.bytecode.chunks[chunk_id].instructions.len(),
                    stack_trace: None,
                });
            }
            
            let instruction = self.bytecode.chunks[chunk_id].instructions[ip].clone();
            
            // Increment IP before execution
            self.call_stack.last_mut().unwrap().ip += 1;
            self.instruction_count += 1;
            
            // Execute the instruction
            match self.execute_instruction(&instruction, chunk_id)? {
                VMState::Continue => {
                    // Continue executing
                }
                VMState::Return => {
                    // Check if we've returned from the target frame
                    if self.call_stack.len() <= target_depth {
                        break;
                    }
                    // Otherwise, handle the return and continue
                    let return_value = self.pop()?;
                    let returning_frame = self.call_stack.pop().ok_or_else(|| VMError::RuntimeError {
                        message: "No frame to return from".to_string(),
                        stack_trace: None,
                    })?;
                    
                    // Clean up stack
                    if let Some(_) = self.call_stack.last() {
                        self.stack.truncate(returning_frame.stack_base);
                    }
                    
                    // Push return value for caller
                    self.push(return_value)?;
                }
                VMState::Halt => {
                    // Halt execution
                    break;
                }
            }
            
            // Check if we've returned from the target frame
            if self.call_stack.len() <= target_depth {
                break;
            }
        }
        
        Ok(())
    }
    
    /// Check if there's a pending variant execution and handle it
    pub fn check_pending_variant(&mut self) -> VMResult<()> {
        if let Some((variant_bytecode, variant_chunk_id)) = self.pending_variant_execution.take() {
            // Execute with the variant
            self.execute_with_variant(variant_bytecode, variant_chunk_id)?;
        }
        Ok(())
    }
}

/// Format optimization strategy for display
fn format_strategy(strategy: crate::learning_mode::OptimizationStrategy) -> String {
    use crate::learning_mode::OptimizationStrategy;
    
    match strategy {
        OptimizationStrategy::None => "None".to_string(),
        OptimizationStrategy::Basic => "Basic".to_string(),
        OptimizationStrategy::Standard => "Standard".to_string(),
        OptimizationStrategy::Aggressive => "Aggressive".to_string(),
        OptimizationStrategy::Custom(mask) => {
            let mut opts = Vec::new();
            if mask & 0x001 != 0 { opts.push("ConstFold"); }
            if mask & 0x002 != 0 { opts.push("DeadCode"); }
            if mask & 0x004 != 0 { opts.push("CSE"); }
            if mask & 0x008 != 0 { opts.push("Inline"); }
            if mask & 0x010 != 0 { opts.push("TailCall"); }
            if mask & 0x020 != 0 { opts.push("LoopOpt"); }
            if mask & 0x040 != 0 { opts.push("BetaRed"); }
            if mask & 0x080 != 0 { opts.push("PartialEval"); }
            if mask & 0x100 != 0 { opts.push("StrengthRed"); }
            if mask & 0x200 != 0 { opts.push("AlgebraicSimp"); }
            if mask & 0x400 != 0 { opts.push("LoopInvariant"); }
            if mask & 0x800 != 0 { opts.push("FuncSpec"); }
            if mask & 0x1000 != 0 { opts.push("Memoization"); }
            
            if opts.is_empty() {
                format!("Custom(0x{:03x})", mask)
            } else {
                format!("Custom[{}]", opts.join("+"))
            }
        }
    }
}

// Implement continuation support for the VM
impl ContinuationSupport for VM {
    fn suspend_execution(&mut self, resume_offset: isize) -> VMResult<ContinuationId> {
        // Check if we can suspend at this point
        if !self.can_suspend() {
            return Err(VMError::RuntimeError {
                message: "Cannot suspend execution at this point".to_string(),
                stack_trace: None,
            });
        }
        
        // Get current frame
        let current_frame = self.call_stack.last()
            .ok_or_else(|| VMError::RuntimeError {
                message: "No active call frame to suspend".to_string(),
                stack_trace: None,
            })?;
        
        // Calculate resume IP
        let resume_ip = (current_frame.ip as isize + resume_offset) as usize;
        let resume_chunk = current_frame.chunk_id;
        
        // Clone the current state
        let call_stack = self.call_stack.clone();
        let value_stack = self.stack.clone();
        let locals = self.locals.clone();
        
        // Convert error handlers to continuation format
        let error_handlers = self.error_handler_stack.iter()
            .map(|eh| ErrorHandlerFrame {
                catch_ip: eh.catch_ip,
                finally_ip: eh.finally_ip,
                stack_depth: eh.stack_depth,
            })
            .collect();
        
        // Create the continuation
        let continuation_id = self.continuation_manager.create_continuation(
            call_stack,
            value_stack,
            locals,
            resume_ip,
            resume_chunk,
            error_handlers,
        );
        
        Ok(continuation_id)
    }
    
    fn resume_continuation(&mut self, continuation_id: ContinuationId, resume_value: Value) -> VMResult<()> {
        // Take the continuation (removes it from the manager)
        let continuation = self.continuation_manager.take(continuation_id)
            .ok_or_else(|| VMError::RuntimeError {
                message: format!("Continuation {} not found", continuation_id),
                stack_trace: None,
            })?;
        
        // Restore the VM state
        self.call_stack = continuation.call_stack;
        self.stack = continuation.value_stack;
        self.locals = continuation.locals;
        
        // Restore error handlers
        self.error_handler_stack = continuation.error_handlers.into_iter()
            .map(|eh| ErrorHandler {
                catch_ip: eh.catch_ip,
                finally_ip: eh.finally_ip,
                stack_depth: eh.stack_depth,
                call_frame: self.call_stack.len() - 1, // Adjust based on current call stack
                locals_count: self.locals.len(),
            })
            .collect();
        
        // Update the IP to resume at
        if let Some(frame) = self.call_stack.last_mut() {
            frame.ip = continuation.resume_ip;
        }
        
        // Push the resume value onto the stack
        self.push(resume_value)?;
        
        Ok(())
    }
    
    fn can_suspend(&self) -> bool {
        // Can suspend if:
        // 1. We have an active call frame
        // 2. We're not in the middle of a finally block
        // 3. Stack is in a valid state
        !self.call_stack.is_empty() && 
        self.finally_states.is_empty() &&
        self.stack.len() < self.resource_limits.max_stack_depth
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
            start_time: Some(web_time::Instant::now()),
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
    #[cfg(feature = "std")]
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
