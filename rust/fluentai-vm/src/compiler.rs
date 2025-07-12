//! Compiler from AST to bytecode

use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use crate::compiler_builtins::BuiltinResult;
use crate::free_var_analysis::FreeVarAnalyzer;
use fluentai_bytecode::source_map::{SourceLocation, SourceMap, ModuleSourceMap};
use crate::stack_effect::stack_effect;
use anyhow::{anyhow, Result};
use fluentai_core::ast::{Graph as ASTGraph, Literal, Node, NodeId, Pattern};
use fluentai_core::value::Value;
use fluentai_optimizer::{OptimizationConfig, OptimizationLevel, OptimizationPipeline};
use std::collections::{HashMap, HashSet};

/// Bit masks for MakeClosure instruction packing
const MAKECLOSURE_CHUNK_ID_SHIFT: u32 = 16;
const MAKECLOSURE_CAPTURE_COUNT_MASK: u32 = 0xFFFF;

/// Compiler options
#[derive(Debug, Clone)]
pub struct CompilerOptions {
    /// Optimization level
    pub optimization_level: OptimizationLevel,
    /// Enable debug information
    pub debug_info: bool,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        Self {
            optimization_level: OptimizationLevel::Standard,
            debug_info: false,
        }
    }
}

pub struct Compiler {
    bytecode: Bytecode,
    current_chunk: usize,
    locals: Vec<HashMap<String, usize>>,
    captured: Vec<HashMap<String, usize>>, // Captured variables per scope
    stack_depth: usize,                    // Track current stack depth
    scope_bases: Vec<usize>,               // Base stack position for each scope
    cell_vars: Vec<HashSet<String>>,       // Variables that are cells (for letrec)
    options: CompilerOptions,
    // Tail call optimization tracking
    in_tail_position: bool, // Whether we're compiling in tail position
    current_function: Option<String>, // Name of current function being compiled
    // Source mapping
    current_node_id: Option<NodeId>, // Current AST node being compiled
    source_filename: Option<String>, // Optional source filename
}

/// Helper struct to hold error handler information during try/catch/finally compilation
struct ErrorHandlerInfo {
    push_handler_idx: usize,
    push_finally_idx: Option<usize>,
    jump_after_body: usize,
    jump_after_catch: Option<usize>,
    catch_start: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self::with_options(CompilerOptions::default())
    }

    pub fn with_options(options: CompilerOptions) -> Self {
        let mut bytecode = Bytecode::new();
        let main_chunk = bytecode.add_chunk(BytecodeChunk::new(Some("main".to_string())));
        bytecode.main_chunk = main_chunk;

        Self {
            bytecode,
            current_chunk: main_chunk,
            locals: vec![HashMap::new()],
            captured: vec![HashMap::new()],
            stack_depth: 0,
            scope_bases: vec![0],
            cell_vars: vec![HashSet::new()],
            options,
            in_tail_position: false,
            current_function: None,
            current_node_id: None,
            source_filename: None,
        }
    }

    /// Set the source filename for error reporting
    pub fn with_source_filename(mut self, filename: String) -> Self {
        self.source_filename = Some(filename);
        self
    }
    
    pub fn compile(mut self, graph: &ASTGraph) -> Result<Bytecode> {
        // Initialize module source map if debug info is enabled
        self.init_module_source_map();
        
        // Apply optimizations if enabled
        let optimized_graph = if self.options.optimization_level != OptimizationLevel::None {
            let config = OptimizationConfig::for_level(self.options.optimization_level);
            let mut pipeline = OptimizationPipeline::new(config);
            pipeline.optimize(graph)?
        } else {
            graph.clone()
        };

        let root_id = optimized_graph
            .root_id
            .ok_or_else(|| anyhow!("AST graph has no root node"))?;
        
        // Verify initial state
        self.verify_stack_invariants();
        
        self.compile_node(&optimized_graph, root_id)?;

        // Add halt instruction
        self.emit(Instruction::new(Opcode::Halt));
        
        // Verify final state
        self.verify_stack_invariants();

        Ok(self.bytecode)
    }

    /// Create a new chunk and return its ID
    fn create_chunk(&mut self) -> Result<usize> {
        let chunk = BytecodeChunk::new(None);
        Ok(self.bytecode.add_chunk(chunk))
    }
    
    /// Push a new local scope
    fn push_scope(&mut self) {
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.cell_vars.push(HashSet::new());
        self.scope_bases.push(self.stack_depth);
        
        // Verify scope tracking is consistent
        debug_assert_eq!(
            self.locals.len(),
            self.scope_bases.len(),
            "Scope tracking inconsistent: {} locals vs {} scope_bases",
            self.locals.len(),
            self.scope_bases.len()
        );
    }
    
    /// Push a new local scope for catch handlers
    /// The handler_depth is where the error value will be placed by the VM
    fn push_catch_scope(&mut self, handler_depth: usize) {
        // Verify handler depth is valid
        debug_assert!(
            handler_depth <= self.stack_depth,
            "Invalid handler depth {}: cannot exceed current stack depth {}",
            handler_depth,
            self.stack_depth
        );
        
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.cell_vars.push(HashSet::new());
        // The error will be at handler_depth after unwinding
        self.scope_bases.push(handler_depth);
        
        // Verify scope tracking is consistent
        debug_assert_eq!(
            self.locals.len(),
            self.scope_bases.len(),
            "Scope tracking inconsistent after push_catch_scope: {} locals vs {} scope_bases",
            self.locals.len(),
            self.scope_bases.len()
        );
    }
    
    /// Pop the current local scope
    fn pop_scope(&mut self) {
        // Ensure we have scopes to pop
        debug_assert!(
            self.locals.len() > 1,
            "Cannot pop global scope"
        );
        
        // Get the scope base before popping
        let scope_base = self.scope_bases.last().copied().unwrap_or(0);
        
        // Verify stack hasn't gone below scope base
        debug_assert!(
            self.stack_depth >= scope_base,
            "Stack depth {} has gone below scope base {}",
            self.stack_depth,
            scope_base
        );
        
        self.locals.pop();
        self.captured.pop();
        self.cell_vars.pop();
        self.scope_bases.pop();
        
        // Verify scope tracking remains consistent
        debug_assert_eq!(
            self.locals.len(),
            self.scope_bases.len(),
            "Scope tracking inconsistent after pop: {} locals vs {} scope_bases",
            self.locals.len(),
            self.scope_bases.len()
        );
        debug_assert!(
            !self.locals.is_empty(),
            "All scopes popped - compiler state corrupted"
        );
    }

    pub(crate) fn compile_node(&mut self, graph: &ASTGraph, node_id: NodeId) -> Result<()> {
        let node = graph
            .nodes
            .get(&node_id)
            .ok_or_else(|| anyhow!("Invalid node ID: {:?}", node_id))?;
        
        // Track current node for source mapping
        let previous_node = self.current_node_id;
        self.current_node_id = Some(node_id);
        
        // Source location is recorded in emit() method when instructions are generated

        match node {
            Node::Literal(lit) => self.compile_literal(lit)?,
            Node::Variable { name } => self.compile_variable(name)?,
            Node::Application { function, args } => {
                self.compile_application(graph, *function, args)?;
            }
            Node::Lambda { params, body } => {
                self.compile_lambda(graph, params, *body)?;
            }
            Node::Let { bindings, body } => {
                self.compile_let(graph, bindings, *body)?;
            }
            Node::Letrec { bindings, body } => {
                self.compile_letrec(graph, bindings, *body)?;
            }
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_if(graph, *condition, *then_branch, *else_branch)?;
            }
            Node::List(items) => {
                self.compile_list(graph, items)?;
            }
            Node::Effect {
                effect_type,
                operation,
                args,
            } => {
                self.compile_effect(graph, *effect_type, operation, args)?;
            }
            Node::Async { body } => {
                self.compile_async(graph, *body)?;
            }
            Node::Await { expr } => {
                self.compile_await(graph, *expr)?;
            }
            Node::Spawn { expr } => {
                self.compile_spawn(graph, *expr)?;
            }
            Node::Channel { capacity } => {
                // If capacity is provided, compile it and push onto stack
                if let Some(cap_expr) = capacity {
                    self.compile_node(graph, *cap_expr)?;
                    // Emit ChannelWithCapacity opcode
                    self.emit(Instruction::new(Opcode::ChannelWithCapacity));
                    // Stack effect: pop capacity, push channel
                } else {
                    // No capacity, use default
                    self.emit(Instruction::new(Opcode::Channel));
                    // Stack depth is now managed by emit()
                }
            }
            Node::Send { channel, value } => {
                self.compile_send(graph, *channel, *value)?;
            }
            Node::Receive { channel } => {
                self.compile_receive(graph, *channel)?;
            }
            Node::TrySend { channel, value } => {
                self.compile_try_send(graph, *channel, *value)?;
            }
            Node::TryReceive { channel } => {
                self.compile_try_receive(graph, *channel)?;
            }
            Node::Select { branches, default } => {
                self.compile_select(graph, branches, default.as_ref())?;
            }
            Node::Actor { initial_state, handler } => {
                self.compile_actor(graph, *initial_state, *handler)?;
            }
            Node::ActorSend { actor, message } => {
                self.compile_actor_send(graph, *actor, *message)?;
            }
            Node::ActorReceive { patterns, timeout } => {
                self.compile_actor_receive(graph, patterns, timeout.as_ref())?;
            }
            Node::Become { new_state } => {
                self.compile_become(graph, *new_state)?;
            }
            Node::Try { body, catch_branches, finally } => {
                self.compile_try(graph, *body, catch_branches, finally.as_ref().copied())?;
            }
            Node::Throw { error } => {
                self.compile_throw(graph, *error)?;
            }
            Node::Promise { body } => {
                self.compile_promise(graph, *body)?;
            }
            Node::PromiseAll { promises } => {
                self.compile_promise_all(graph, promises)?;
            }
            Node::PromiseRace { promises } => {
                self.compile_promise_race(graph, promises)?;
            }
            Node::Timeout { duration, promise, default } => {
                self.compile_timeout(graph, *duration, *promise, default.as_ref().copied())?;
            }
            Node::Match { expr, branches } => {
                self.compile_match(graph, *expr, branches)?;
            }
            Node::Module {
                name,
                exports,
                body,
            } => {
                self.compile_module(graph, name, exports, *body)?;
            }
            Node::Import {
                module_path,
                import_list,
                import_all,
            } => {
                self.compile_import(module_path, import_list, *import_all)?;
            }
            Node::Export { export_list } => {
                self.compile_export(export_list)?;
                // Export returns nil
                self.emit(Instruction::new(Opcode::PushNil));
            }
            Node::QualifiedVariable {
                module_name,
                variable_name,
            } => {
                self.compile_qualified_variable(module_name, variable_name)?;
            }
            Node::Contract { .. } => {
                // Contracts are metadata and don't generate bytecode directly
                // They should be attached to functions during compilation
                // For now, we'll emit a no-op
                self.emit(Instruction::new(Opcode::Nop));
            }
            Node::Handler { handlers, body } => {
                self.compile_handler(graph, handlers, *body)?;
            }
            Node::Define { name, value } => {
                // Set current function name if this is a lambda
                let saved_function = self.current_function.clone();
                if let Some(Node::Lambda { .. }) = graph.nodes.get(value) {
                    self.current_function = Some(name.clone());
                }

                // Compile the value
                self.compile_node(graph, *value)?;

                // Restore function name
                self.current_function = saved_function;

                // For now, define acts like a global assignment
                // Store in a global variable slot
                let idx = self.add_constant(Value::String(name.clone()));
                self.emit(Instruction::with_arg(Opcode::StoreGlobal, idx));

                // Define returns nil
                self.emit(Instruction::new(Opcode::PushNil));
            }
            Node::Begin { exprs } => {
                self.compile_begin(graph, exprs)?;
            }
            Node::Assignment { target, value } => {
                self.compile_assignment(graph, *target, *value)?;
            }
        }

        // Restore previous node
        self.current_node_id = previous_node;

        Ok(())
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<()> {
        // Don't increment stack_depth here - emit will handle it
        match lit {
            Literal::Integer(n) => match *n {
                0 => {
                    self.emit(Instruction::new(Opcode::PushInt0));
                }
                1 => {
                    self.emit(Instruction::new(Opcode::PushInt1));
                }
                2 => {
                    self.emit(Instruction::new(Opcode::PushInt2));
                }
                n if n >= 0 && n <= u32::MAX as i64 => {
                    self.emit(Instruction::with_arg(Opcode::PushIntSmall, n as u32));
                }
                _ => {
                    let idx = self.add_constant(Value::Integer(*n));
                    self.emit(Instruction::with_arg(Opcode::Push, idx));
                }
            },
            Literal::Float(f) => {
                let idx = self.add_constant(Value::Float(*f));
                self.emit(Instruction::with_arg(Opcode::Push, idx));
            }
            Literal::String(s) => {
                let idx = self.add_constant(Value::String(s.clone()));
                self.emit(Instruction::with_arg(Opcode::Push, idx));
            }
            Literal::Symbol(s) => {
                let idx = self.add_constant(Value::Symbol(s.clone()));
                self.emit(Instruction::with_arg(Opcode::Push, idx));
            }
            Literal::Boolean(b) => {
                if *b {
                    self.emit(Instruction::new(Opcode::PushTrue));
                } else {
                    self.emit(Instruction::new(Opcode::PushFalse));
                }
            }
            Literal::Nil => {
                self.emit(Instruction::new(Opcode::PushNil));
            }
        }

        Ok(())
    }

    fn compile_variable(&mut self, name: &str) -> Result<()> {
        // Built-in functions are valid values in FluentAi (first-class functions)
        // They will be handled specially when applied

        // Look up in locals
        for (scope_idx, scope) in self.locals.iter().enumerate().rev() {
            if let Some(&rel_pos) = scope.get(name) {
                // Debug assertions for variable resolution
                debug_assert!(
                    scope_idx < self.scope_bases.len(),
                    "Scope index {} out of bounds for scope_bases", scope_idx
                );
                
                // The position stored is relative to the scope base
                // We need to calculate the absolute position
                let abs_pos = self.get_scope_base(scope_idx)? + rel_pos;
                
                // Verify the absolute position is within current stack bounds
                debug_assert!(
                    abs_pos < self.stack_depth,
                    "Variable '{}' at absolute position {} exceeds current stack depth {}",
                    name, abs_pos, self.stack_depth
                );

                // Use fast local opcodes for indices 0-3
                match abs_pos {
                    0 => self.emit(Instruction::new(Opcode::LoadLocal0)),
                    1 => self.emit(Instruction::new(Opcode::LoadLocal1)),
                    2 => self.emit(Instruction::new(Opcode::LoadLocal2)),
                    3 => self.emit(Instruction::new(Opcode::LoadLocal3)),
                    _ => self.emit(Instruction::with_arg(Opcode::Load, abs_pos as u32)),
                };

                // If this is a cell variable (from letrec), dereference it
                if self.is_cell_var(scope_idx, name)? {
                    self.emit(Instruction::new(Opcode::CellGet));
                }

                return Ok(());
            }
        }

        // Look up in captured variables
        for (_scope_idx, scope) in self.captured.iter().enumerate().rev() {
            if let Some(&capture_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(
                    Opcode::LoadCaptured,
                    capture_idx as u32,
                ));

                // Check if this captured variable is a cell
                // This is tricky - we need to know if the captured variable was a cell
                // For now, we'll check if the name exists in any parent scope's cell_vars
                let mut is_cell = false;
                for parent_cells in &self.cell_vars {
                    if parent_cells.contains(name) {
                        is_cell = true;
                        break;
                    }
                }

                if is_cell {
                    self.emit(Instruction::new(Opcode::CellGet));
                }

                return Ok(());
            }
        }

        // Global variable
        let idx = self.add_constant(Value::String(name.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadGlobal, idx));
        Ok(())
    }

    fn compile_application(
        &mut self,
        graph: &ASTGraph,
        func: NodeId,
        args: &[NodeId],
    ) -> Result<()> {
        // Check if it's a built-in function
        if let Some(node) = graph.nodes.get(&func) {
            if let Node::Variable { name } = node {
                // Try to compile as builtin first
                match self.try_compile_builtin(graph, name, args)? {
                    BuiltinResult::Handled => return Ok(()),
                    BuiltinResult::NotBuiltin => {
                        // Continue with other checks
                    }
                }

                // Check for special forms
                if name == "gc:let" {
                    // gc:let is like regular let but allocates values with GC
                    if args.is_empty() {
                        return Err(anyhow!("gc:let requires at least one argument"));
                    }

                    // The gc:let form is (gc:let bindings body)
                    // Parse bindings and body just like regular let
                    let bindings_node = graph
                        .nodes
                        .get(&args[0])
                        .ok_or_else(|| anyhow!("Invalid bindings node in gc:let"))?;

                    // Extract bindings
                    let bindings = if let Node::List(binding_nodes) = bindings_node {
                        let mut result = Vec::new();
                        for &binding_id in binding_nodes {
                            if let Some(Node::List(pair)) = graph.nodes.get(&binding_id) {
                                if pair.len() == 2 {
                                    if let Some(Node::Variable { name }) = graph.nodes.get(&pair[0])
                                    {
                                        result.push((name.clone(), pair[1]));
                                    } else {
                                        return Err(anyhow!(
                                            "gc:let binding must have variable name"
                                        ));
                                    }
                                } else {
                                    return Err(anyhow!("gc:let binding must be a pair"));
                                }
                            } else {
                                return Err(anyhow!("gc:let binding must be a list"));
                            }
                        }
                        result
                    } else {
                        return Err(anyhow!("gc:let bindings must be a list"));
                    };

                    // Body is the rest of the arguments
                    let body_nodes = &args[1..];
                    if body_nodes.is_empty() {
                        return Err(anyhow!("gc:let requires a body"));
                    }

                    // Enter new scope
                    self.enter_scope();

                    // Store the number of bindings before consuming the vector
                    let num_bindings = bindings.len();

                    // Compile bindings with GC allocation
                    for (name, value_node) in bindings {
                        // Compile the value expression
                        self.compile_node(graph, value_node)?;

                        // Allocate with GC
                        self.emit(Instruction::new(Opcode::GcAlloc));

                        // Add to locals
                        let local_idx = self.stack_depth;
                        self.locals.last_mut().unwrap().insert(name, local_idx);
                        // Stack depth is incremented by GcAlloc emit()
                    }

                    // Compile body expressions
                    for (i, &body_node) in body_nodes.iter().enumerate() {
                        self.compile_node(graph, body_node)?;
                        // Pop intermediate results except the last one
                        if i < body_nodes.len() - 1 {
                            self.emit(Instruction::new(Opcode::Pop));
                        }
                    }

                    // Clean up bindings
                    if num_bindings > 0 {
                        self.emit(Instruction::with_arg(Opcode::PopN, num_bindings as u32));
                    }

                    self.exit_scope();
                    return Ok(());
                }

                // Check if it's a constructor (starts with uppercase)
                if name.chars().next().map_or(false, |c| c.is_uppercase()) {
                    // Constructor call - create a tagged value
                    // Push the tag as a constant
                    let tag_idx = self.add_constant(Value::String(name.clone()));
                    self.emit(Instruction::with_arg(Opcode::Push, tag_idx));

                    // Compile arguments
                    for &arg in args {
                        self.compile_node(graph, arg)?;
                    }

                    // Create tagged value
                    self.emit(Instruction::with_arg(Opcode::MakeTagged, args.len() as u32));
                    return Ok(());
                }
            }
        }

        // Check if this is a tail call
        let is_tail_call = self.in_tail_position
            && self.current_function.is_some()
            && if let Some(Node::Variable { name }) = graph.nodes.get(&func) {
                self.current_function.as_ref() == Some(name)
            } else {
                false
            };

        // Regular function call
        for &arg in args {
            self.compile_node(graph, arg)?;
        }
        self.compile_node(graph, func)?;

        if is_tail_call {
            self.emit(Instruction::with_arg(Opcode::TailCall, args.len() as u32));
        } else {
            self.emit(Instruction::with_arg(Opcode::Call, args.len() as u32));
        }

        Ok(())
    }

    fn compile_lambda(&mut self, graph: &ASTGraph, params: &[String], body: NodeId) -> Result<()> {
        // Find free variables - variables used in body but not defined as parameters
        let free_vars = self.find_free_variables(graph, body, params)?;

        // Emit code to push captured values onto stack
        for var in &free_vars {
            self.compile_captured_variable(var)?;
        }

        // Create a new chunk for the lambda
        let lambda_chunk = BytecodeChunk::new(Some("lambda".to_string()));
        let chunk_id = self.bytecode.add_chunk(lambda_chunk);

        // Save current context
        let saved_chunk = self.current_chunk;
        let saved_locals = self.locals.clone();
        let saved_captured = self.captured.clone();
        let saved_stack_depth = self.stack_depth;
        let saved_scope_bases = self.scope_bases.clone();
        let saved_cell_vars = self.cell_vars.clone();
        let saved_function = self.current_function.clone();
        let _saved_tail = self.in_tail_position;

        // Switch to lambda chunk
        self.current_chunk = chunk_id;
        self.locals = vec![HashMap::new()];
        self.captured = vec![HashMap::new()];
        // Lambda starts with parameters on stack
        self.stack_depth = params.len();
        self.scope_bases = vec![0];
        self.cell_vars = vec![HashSet::new()];
        
        // Verify lambda starts with parameters on stack
        debug_assert_eq!(
            self.stack_depth, params.len(),
            "Lambda should start with parameters on stack"
        );
        debug_assert_eq!(
            self.locals.len(), 1,
            "Lambda should start with single scope"
        );

        // Add parameters to locals
        for (i, param) in params.iter().enumerate() {
            self.locals[0].insert(param.clone(), i);
        }

        // Add captured variables to captured map
        // Also track which captured variables are cells
        for (i, var) in free_vars.iter().enumerate() {
            self.captured[0].insert(var.clone(), i);

            // Check if this variable is a cell in any parent scope
            for parent_cells in &saved_cell_vars {
                if parent_cells.contains(var) {
                    self.cell_vars[0].insert(var.clone());
                    break;
                }
            }
        }

        // Compile body in tail position
        let _saved_tail = self.in_tail_position;
        self.in_tail_position = true;
        let lambda_start_depth = self.stack_depth;
        self.compile_node(graph, body)?;
        
        // Lambda body should produce exactly one value
        // The final stack should have params + 1 (the return value)
        debug_assert!(
            self.stack_depth >= params.len() + 1,
            "Lambda body should produce at least one value: expected minimum depth {} (params={} + result=1), got {}",
            params.len() + 1,
            params.len(),
            self.stack_depth
        );
        
        self.in_tail_position = _saved_tail;
        self.emit(Instruction::new(Opcode::Return));

        // Restore context
        self.current_chunk = saved_chunk;
        self.locals = saved_locals;
        self.captured = saved_captured;
        self.stack_depth = saved_stack_depth;
        self.scope_bases = saved_scope_bases;
        
        // Verify context is properly restored
        debug_assert_eq!(
            self.stack_depth, saved_stack_depth,
            "Stack depth not restored after lambda compilation"
        );
        self.cell_vars = saved_cell_vars;
        self.current_function = saved_function;
        self.in_tail_position = _saved_tail;

        // Push function value with captures
        if free_vars.is_empty() {
            self.emit(Instruction::with_arg(Opcode::MakeFunc, chunk_id as u32));
        } else {
            // MakeClosure bit packing format:
            // The 32-bit argument is packed as follows:
            //   - Upper 16 bits (bits 31-16): chunk_id - identifies the bytecode chunk for the function
            //   - Lower 16 bits (bits 15-0): capture_count - number of values to capture from stack
            //
            // Example: chunk_id=5, capture_count=3
            //   packed = (5 << 16) | 3 = 0x00050003
            //
            // The VM will:
            //   1. Extract capture_count = packed & 0xFFFF
            //   2. Pop that many values from the stack (these are the captured values)
            //   3. Extract chunk_id = (packed >> 16) & 0xFFFF
            //   4. Create a closure with the bytecode from chunk_id and the captured values
            //
            // This encoding limits us to 65535 chunks and 65535 captures per closure,
            // which is more than sufficient for any practical program.
            let packed = ((chunk_id as u32) << MAKECLOSURE_CHUNK_ID_SHIFT) | 
                        ((free_vars.len() as u32) & MAKECLOSURE_CAPTURE_COUNT_MASK);
            self.emit(Instruction::with_arg(Opcode::MakeClosure, packed));
        }

        Ok(())
    }

    fn compile_let(
        &mut self,
        graph: &ASTGraph,
        bindings: &[(String, NodeId)],
        body: NodeId,
    ) -> Result<()> {
        // Create new scope
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.scope_bases.push(self.stack_depth);
        self.cell_vars.push(HashSet::new());
        let scope_idx = self.locals.len() - 1;

        // Compile bindings
        let initial_depth = self.stack_depth;
        for (i, (name, value)) in bindings.iter().enumerate() {
            let before_depth = self.stack_depth;

            // Set current function name if this is a lambda
            let saved_function = self.current_function.clone();
            if let Some(Node::Lambda { .. }) = graph.nodes.get(value) {
                self.current_function = Some(name.clone());
            }

            self.compile_node(graph, *value)?;

            // Restore function name
            self.current_function = saved_function;

            // After compiling the value, it should be on top of the stack
            // We expect exactly one value to be added
            debug_assert_eq!(
                self.stack_depth, before_depth + 1,
                "Let binding {} should add exactly one value to stack: expected {}, got {}",
                name, before_depth + 1, self.stack_depth
            );

            // Store relative position within this scope
            // The i-th binding is at position i relative to the scope base
            self.locals[scope_idx].insert(name.clone(), i);
            
            // The value is now on the stack, but we need to keep it there for the let scope
            // No Store instruction needed - values stay on the stack in their binding order
            // Fix for issue 26: Let bindings now properly maintain values on stack for local access
        }
        
        // Verify all bindings were added
        debug_assert_eq!(
            self.stack_depth, initial_depth + bindings.len(),
            "Let should add {} values to stack", bindings.len()
        );

        // Compile body (preserving tail position - let body is in tail position)
        let body_start_depth = self.stack_depth;
        self.compile_node(graph, body)?;
        
        // Body should produce exactly one value
        debug_assert_eq!(
            self.stack_depth, body_start_depth + 1,
            "Let body should produce exactly one value"
        );

        // Clean up bindings while preserving the result
        if !bindings.is_empty() {
            self.emit(Instruction::with_arg(Opcode::PopN, bindings.len() as u32));
            // PopN already adjusts stack_depth in emit()
            
            // After PopN, we should have initial depth + 1 (the result)
            debug_assert_eq!(
                self.stack_depth, initial_depth + 1,
                "After PopN, should have only result on stack"
            );
        }

        // Pop scope
        self.locals.pop();
        self.captured.pop();
        self.scope_bases.pop();
        self.cell_vars.pop();

        Ok(())
    }

    fn compile_begin(&mut self, graph: &ASTGraph, exprs: &[NodeId]) -> Result<()> {
        if exprs.is_empty() {
            // Empty begin returns nil
            self.emit(Instruction::new(Opcode::PushNil));
            return Ok(());
        }

        // Compile each expression
        for (i, expr) in exprs.iter().enumerate() {
            self.compile_node(graph, *expr)?;

            // Pop intermediate results (except the last one)
            if i < exprs.len() - 1 {
                self.emit(Instruction::new(Opcode::Pop));
                // Stack depth is now managed by emit()
            }
        }

        // The last expression's value remains on the stack as the result
        Ok(())
    }
    
    fn compile_assignment(&mut self, graph: &ASTGraph, target: NodeId, value: NodeId) -> Result<()> {
        // For now, we only support simple variable assignments
        // TODO: In the future, support field access (e.g., self.count)
        
        
        let target_node = graph
            .get_node(target)
            .ok_or_else(|| anyhow!("Invalid target node in assignment: {:?}", target))?;
        
        match target_node {
            Node::Variable { name } => {
                // Compile the value expression
                self.compile_node(graph, value)?;
                
                // Duplicate the value on the stack so we can return it
                self.emit(Instruction::new(Opcode::Dup));
                // Note: emit() already adjusts stack_depth for Dup
                
                // Look up in locals
                let mut found_local = false;
                for (scope_idx, scope) in self.locals.iter().enumerate().rev() {
                    if let Some(&rel_pos) = scope.get(name) {
                        // The position stored is relative to the scope base
                        let abs_pos = self.get_scope_base(scope_idx)? + rel_pos;
                        
                        // Store in local variable (consumes one copy)
                        self.emit(Instruction::with_arg(Opcode::Store, abs_pos as u32));
                        // Stack depth is now managed by emit()
                        found_local = true;
                        break;
                    }
                }
                
                if !found_local {
                    // If not local, store as global (consumes one copy)
                    let idx = self.add_constant(Value::String(name.clone()));
                    self.emit(Instruction::with_arg(Opcode::StoreGlobal, idx));
                    // Stack depth is now managed by emit()
                }
                
                // The duplicated value remains on the stack as the result
                // Assignment now returns the assigned value
                Ok(())
            }
            Node::Application { function, args } if args.len() == 2 => {
                // This might be a field access like (get obj field)
                // For now, we'll error out
                // TODO: Implement field assignment
                Err(anyhow!("Field assignment not yet implemented"))
            }
            _ => Err(anyhow!("Invalid assignment target: must be a variable"))
        }
    }

    fn compile_letrec(
        &mut self,
        graph: &ASTGraph,
        bindings: &[(String, NodeId)],
        body: NodeId,
    ) -> Result<()> {
        // Create new scope
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.scope_bases.push(self.stack_depth);
        self.cell_vars.push(HashSet::new());
        let scope_idx = self.locals.len() - 1;

        // Strategy for proper letrec using cells:
        // 1. Create cells for all bindings (initialized with nil)
        // 2. Add bindings to locals pointing to the cells
        // 3. Compile binding values
        // 4. Store values in the cells
        // 5. When accessing a letrec binding, use CellGet

        // Step 1: Create cells for all bindings
        let binding_names: Vec<String> = bindings.iter().map(|(name, _)| name.clone()).collect();
        for (i, name) in binding_names.iter().enumerate() {
            self.emit(Instruction::new(Opcode::PushNil));
            self.emit(Instruction::new(Opcode::MakeCell));
            // Cell is now on stack
            // Store relative position - the i-th binding is at position i
            self.locals[scope_idx].insert(name.clone(), i);
            self.cell_vars[scope_idx].insert(name.clone());
        }

        // Step 2: Compile binding values and store in cells
        for (_i, (name, value)) in bindings.iter().enumerate() {
            // Load the cell - need to look up actual position from locals
            let cell_rel_pos = self.locals[scope_idx]
                .get(name)
                .ok_or_else(|| anyhow!("Cell not found for binding: {}", name))?;
            // Convert relative position to absolute position for Load instruction
            let abs_pos = self.get_scope_base(scope_idx)? + cell_rel_pos;
            self.emit(Instruction::with_arg(Opcode::Load, abs_pos as u32));

            // Set current function name if this is a lambda
            let saved_function = self.current_function.clone();
            if let Some(Node::Lambda { .. }) = graph.nodes.get(value) {
                self.current_function = Some(name.clone());
            }

            // Compile the value
            self.compile_node(graph, *value)?;

            // Restore function name
            self.current_function = saved_function;

            // Store in cell: [cell, value] -> CellSet -> nil
            self.emit(Instruction::new(Opcode::CellSet));
            self.emit(Instruction::new(Opcode::Pop)); // Pop the nil
        }

        // Step 3: Compile body (preserving tail position - letrec body is in tail position)
        self.compile_node(graph, body)?;

        // Clean up bindings while preserving the result
        if !bindings.is_empty() {
            self.emit(Instruction::with_arg(Opcode::PopN, bindings.len() as u32));
        }

        // Pop scope
        self.locals.pop();
        self.captured.pop();
        self.scope_bases.pop();
        self.cell_vars.pop();

        Ok(())
    }

    fn compile_if(
        &mut self,
        graph: &ASTGraph,
        condition: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
    ) -> Result<()> {
        // Compile condition (not in tail position)
        let _saved_tail = self.in_tail_position;
        self.in_tail_position = false;
        self.compile_node(graph, condition)?;
        self.in_tail_position = _saved_tail;

        // Jump to else if false
        let jump_to_else = self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0));

        // Compile then branch (preserves tail position)
        self.compile_node(graph, then_branch)?;

        // Jump over else
        let jump_over_else = self.emit(Instruction::with_arg(Opcode::Jump, 0));

        // Patch jump to else
        let else_start = self.current_offset();
        self.patch_jump(jump_to_else, else_start);

        // Compile else branch (preserves tail position)
        self.compile_node(graph, else_branch)?;

        // Patch jump over else
        let end = self.current_offset();
        self.patch_jump(jump_over_else, end);

        Ok(())
    }

    fn compile_list(&mut self, graph: &ASTGraph, items: &[NodeId]) -> Result<()> {
        // Compile all items
        for &item in items {
            self.compile_node(graph, item)?;
        }

        // Create list
        self.emit(Instruction::with_arg(Opcode::MakeList, items.len() as u32));

        Ok(())
    }


    pub(crate) fn emit(&mut self, instruction: Instruction) -> usize {
        let initial_depth = self.stack_depth;
        
        // Get the stack effect for this instruction
        let effect = stack_effect(&instruction);
        
        // Validate we have enough values on the stack
        debug_assert!(
            self.stack_depth >= effect.pop,
            "Instruction {:?} requires {} values on stack, but stack depth is only {}",
            instruction.opcode,
            effect.pop,
            self.stack_depth
        );
        
        // Apply the stack effect
        self.stack_depth = effect.apply(self.stack_depth);
        
        // Additional validation for specific instructions
        match instruction.opcode {
            // For PopN, we need special handling because it keeps the top value
            Opcode::PopN => {
                // PopN pops N values from beneath the top value, keeping the top
                // So the actual requirement is N+1 values on the initial stack
                debug_assert!(
                    initial_depth >= instruction.arg as usize + 1,
                    "PopN({}) requires at least {} values on stack, but depth was {}",
                    instruction.arg,
                    instruction.arg as usize + 1,
                    initial_depth
                );
            }
            _ => {}
        }
        
        // Verify stack depth never goes negative (saturating operations should prevent this)
        debug_assert!(
            self.stack_depth <= 10000, // Reasonable upper bound
            "Stack depth {} seems unreasonably large - possible underflow",
            self.stack_depth
        );
        
        // For instructions that should produce values, verify stack grew
        if effect.push > 0 && effect.pop == 0 {
            debug_assert!(
                self.stack_depth > initial_depth,
                "Instruction {:?} should increase stack depth",
                instruction.opcode
            );
        }
        
        let offset = self.bytecode.chunks[self.current_chunk].add_instruction(instruction);
        
        // Record source location for this instruction if we have a current node
        if self.options.debug_info {
            if let Some(node_id) = self.current_node_id {
                self.record_source_location(offset, node_id);
            }
        }
        
        offset
    }

    fn add_constant(&mut self, value: Value) -> u32 {
        self.bytecode.chunks[self.current_chunk].add_constant(value)
    }

    fn current_offset(&self) -> usize {
        self.bytecode.chunks[self.current_chunk].instructions.len()
    }

    fn patch_jump(&mut self, offset: usize, target: usize) {
        self.bytecode.chunks[self.current_chunk].patch_jump(offset, target);
    }

    fn compile_effect(
        &mut self,
        graph: &ASTGraph,
        effect_type: fluentai_core::ast::EffectType,
        operation: &str,
        args: &[NodeId],
    ) -> Result<()> {
        // Push effect type as string
        let effect_str = format!("{:?}", effect_type);
        let idx = self.add_constant(Value::String(effect_str));
        self.emit(Instruction::with_arg(Opcode::PushConst, idx));

        // Push operation
        let idx = self.add_constant(Value::String(operation.to_string()));
        self.emit(Instruction::with_arg(Opcode::PushConst, idx));

        // Compile arguments
        for arg in args {
            self.compile_node(graph, *arg)?;
        }

        // Emit effect instruction with argument count
        self.emit(Instruction::with_arg(Opcode::Effect, args.len() as u32));

        Ok(())
    }

    fn compile_async(&mut self, graph: &ASTGraph, body: NodeId) -> Result<()> {
        // Create a new chunk for the async block body
        let chunk_id = self.create_chunk()?;
        
        // Switch to the new chunk
        let old_chunk = self.current_chunk;
        self.current_chunk = chunk_id;
        
        // Compile the async block body
        self.compile_node(graph, body)?;
        
        // Add return instruction to the async chunk
        self.emit(Instruction::new(Opcode::Return));
        
        // Switch back to the original chunk
        self.current_chunk = old_chunk;
        
        // Emit MakeFunc instruction with the chunk ID as argument
        self.emit(Instruction::with_arg(Opcode::MakeFunc, chunk_id as u32));
        
        // Convert the function to a future
        self.emit(Instruction::new(Opcode::MakeFuture));
        
        Ok(())
    }

    fn compile_await(&mut self, graph: &ASTGraph, expr: NodeId) -> Result<()> {
        // Compile the expression that should return a promise
        self.compile_node(graph, expr)?;
        // Emit await instruction
        self.emit(Instruction::new(Opcode::Await));
        Ok(())
    }

    fn compile_spawn(&mut self, graph: &ASTGraph, expr: NodeId) -> Result<()> {
        // Check if the expression is a lambda that needs special handling
        if let Some(Node::Lambda { params, body }) = graph.get_node(expr) {
            // For lambdas, we need to check if they have free variables
            // If so, they need to be compiled as closures
            self.compile_lambda(graph, params, *body)?;
        } else {
            // For other expressions, compile normally
            self.compile_node(graph, expr)?;
        }
        // Emit spawn instruction
        self.emit(Instruction::new(Opcode::Spawn));
        Ok(())
    }

    fn compile_send(&mut self, graph: &ASTGraph, channel: NodeId, value: NodeId) -> Result<()> {
        // Compile channel and value
        self.compile_node(graph, channel)?;
        self.compile_node(graph, value)?;
        // Emit send instruction
        self.emit(Instruction::new(Opcode::Send));
        Ok(())
    }

    fn compile_receive(&mut self, graph: &ASTGraph, channel: NodeId) -> Result<()> {
        // Compile channel
        self.compile_node(graph, channel)?;
        // Emit receive instruction
        self.emit(Instruction::new(Opcode::Receive));
        Ok(())
    }
    
    fn compile_try_send(&mut self, graph: &ASTGraph, channel: NodeId, value: NodeId) -> Result<()> {
        // Compile channel and value
        self.compile_node(graph, channel)?;
        self.compile_node(graph, value)?;
        // Emit try-send instruction
        self.emit(Instruction::new(Opcode::TrySend));
        Ok(())
    }
    
    fn compile_try_receive(&mut self, graph: &ASTGraph, channel: NodeId) -> Result<()> {
        // Compile channel
        self.compile_node(graph, channel)?;
        // Emit try-receive instruction
        self.emit(Instruction::new(Opcode::TryReceive));
        Ok(())
    }

    fn compile_select(
        &mut self,
        graph: &ASTGraph,
        branches: &[(NodeId, NodeId)],
        default: Option<&NodeId>,
    ) -> Result<()> {
        // For now, implement select using simpler logic
        // We'll try each channel and execute the first handler that has data
        
        let mut jump_to_end = Vec::new();
        
        for (channel_op, handler) in branches {
            // Check if this is a receive operation
            if let Some(Node::Receive { channel }) = graph.get_node(*channel_op) {
                // Compile the channel
                self.compile_node(graph, *channel)?;
                // Try to receive
                self.emit(Instruction::new(Opcode::TryReceive));
                
                // Check if we got a value (result is [bool, value])
                self.emit(Instruction::new(Opcode::Dup));
                self.emit(Instruction::new(Opcode::ListHead)); // Get success flag (first element)
                
                // If not successful, jump to next branch
                let jump_next = self.emit(Instruction::new(Opcode::JumpIfNot));
                
                // Success - get the value and evaluate handler  
                // We have the original list on stack, get the value
                self.emit(Instruction::new(Opcode::ListTail)); // Get [value]
                self.emit(Instruction::new(Opcode::ListHead)); // Extract value
                
                // For now, we'll just ignore the value and compile the handler
                self.emit(Instruction::new(Opcode::Pop)); // Pop the value
                self.compile_node(graph, *handler)?;
                
                // Jump to end
                let jump_end = self.emit(Instruction::new(Opcode::Jump));
                jump_to_end.push(jump_end);
                
                // Patch jump to next branch
                let next_pos = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.patch_jump(jump_next, next_pos);
                
                // Pop the failed result
                self.emit(Instruction::new(Opcode::Pop));
            } else {
                return Err(anyhow!("Select currently only supports receive operations"));
            }
        }
        
        // Default case
        if let Some(default_expr) = default {
            self.compile_node(graph, *default_expr)?;
        } else {
            // No default - for now return nil
            let nil_idx = self.add_constant(Value::Nil);
            self.emit(Instruction::with_arg(Opcode::PushConst, nil_idx));
        }
        
        // Patch all end jumps
        let end_pos = self.bytecode.chunks[self.current_chunk].instructions.len();
        for jump_pos in jump_to_end {
            self.patch_jump(jump_pos, end_pos);
        }
        
        Ok(())
    }
    
    fn compile_actor(
        &mut self,
        graph: &ASTGraph,
        initial_state: NodeId,
        handler: NodeId,
    ) -> Result<()> {
        // Compile initial state
        self.compile_node(graph, initial_state)?;
        
        // Compile handler function  
        self.compile_node(graph, handler)?;
        
        // Create actor
        self.emit(Instruction::new(Opcode::CreateActor));
        
        // Stack effect is now managed by emit()
        
        Ok(())
    }
    
    fn compile_actor_send(
        &mut self,
        graph: &ASTGraph,
        actor: NodeId,
        message: NodeId,
    ) -> Result<()> {
        // Compile actor
        self.compile_node(graph, actor)?;
        
        // Compile message
        self.compile_node(graph, message)?;
        
        // Send message to actor
        self.emit(Instruction::new(Opcode::ActorSend));
        
        // Stack effect is now managed by emit()
        
        Ok(())
    }
    
    fn compile_actor_receive(
        &mut self,
        graph: &ASTGraph,
        patterns: &[(Pattern, NodeId)],
        timeout: Option<&(NodeId, NodeId)>,
    ) -> Result<()> {
        // The ActorReceive opcode will put the current message on the stack
        self.emit(Instruction::new(Opcode::ActorReceive));
        
        // Now we have the message on the stack, compile it like a match expression
        // We'll compile pattern matching as a series of if-else chains
        let mut jump_to_ends = Vec::new();
        
        for (i, (pattern, handler)) in patterns.iter().enumerate() {
            let is_last = i == patterns.len() - 1;
            
            // Compile pattern test
            let (bindings, always_matches) = self.compile_pattern_test(graph, pattern)?;
            
            // Handle the test result
            let jump_to_next = if always_matches {
                // Pattern always matches - no jump needed
                None
            } else {
                // We have a boolean on the stack from the pattern test
                // Jump to next branch if false
                Some(self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)))
            };
            
            // Pattern matched - the matched value is still on the stack
            
            // Apply bindings in reverse order
            for (name, _) in bindings.iter().rev() {
                let idx = self.add_local(name.to_string())?;
                self.emit(Instruction::with_arg(Opcode::Store, idx as u32));
            }
            
            // Pop the matched value (we've extracted what we need)
            self.emit(Instruction::new(Opcode::Pop));
            
            // Compile the handler body
            self.compile_node(graph, *handler)?;
            
            // Jump to end
            let jump_to_end = self.emit(Instruction::with_arg(Opcode::Jump, 0));
            jump_to_ends.push(jump_to_end);
            
            // Patch the jump to next branch (if any)
            if let Some(jump_idx) = jump_to_next {
                let next_branch_start = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.bytecode.chunks[self.current_chunk].patch_jump(jump_idx, next_branch_start);
            }
            
            // If this is the last branch and it doesn't always match, we need a default case
            if is_last && !always_matches {
                // No pattern matched - pop the message and return nil
                self.emit(Instruction::new(Opcode::Pop));
                self.emit(Instruction::new(Opcode::PushNil));
            }
        }
        
        // Patch all jumps to end
        let end_pos = self.bytecode.chunks[self.current_chunk].instructions.len();
        for jump_idx in jump_to_ends {
            self.bytecode.chunks[self.current_chunk].patch_jump(jump_idx, end_pos);
        }
        
        // TODO: Handle timeout if provided
        if timeout.is_some() {
            // For now, ignore timeout
        }
        
        Ok(())
    }
    
    fn compile_become(
        &mut self,
        graph: &ASTGraph,
        new_state: NodeId,
    ) -> Result<()> {
        // Compile new state
        self.compile_node(graph, new_state)?;
        
        // Become new state
        self.emit(Instruction::new(Opcode::Become));
        
        // Stack effect: pop new state, push nil
        
        Ok(())
    }
    
    /// Set up error handlers for try/catch/finally block
    fn setup_error_handlers(&mut self, finally: Option<NodeId>) -> Result<(usize, Option<usize>)> {
        // Push error handler with catch target
        let push_handler_idx = self.emit(Instruction::new(Opcode::PushHandler));
        
        // If we have a finally block, push its IP too
        let push_finally_idx = if finally.is_some() {
            Some(self.emit(Instruction::new(Opcode::PushFinally)))
        } else {
            None
        };
        
        Ok((push_handler_idx, push_finally_idx))
    }
    
    /// Compile the try body and set up jump after successful completion
    fn compile_try_body(&mut self, graph: &ASTGraph, body: NodeId) -> Result<usize> {
        // Compile body
        self.compile_node(graph, body)?;
        
        // Pop handler on success
        self.emit(Instruction::new(Opcode::PopHandler));
        
        // Jump to finally (or end if no finally)
        let jump_after_body = self.emit(Instruction::new(Opcode::Jump));
        
        Ok(jump_after_body)
    }
    
    /// Set up catch handler position and patch the PushHandler instruction
    fn setup_catch_handlers(&mut self, push_handler_idx: usize) -> Result<usize> {
        // Mark catch handler start
        let catch_start = self.bytecode.chunks[self.current_chunk].instructions.len();
        
        // Patch the PushHandler instruction with catch IP
        self.bytecode.chunks[self.current_chunk].instructions[push_handler_idx].arg = catch_start as u32;
        
        Ok(catch_start)
    }
    
    /// Compile catch branches with pattern matching
    fn compile_catch_branches(&mut self, graph: &ASTGraph, catch_branches: &[(Pattern, NodeId)], handler_stack_depth: usize) -> Result<()> {
        // Compile catch handlers
        if !catch_branches.is_empty() {
            // For now, just compile the first handler with simple variable binding
            let (pattern, handler) = &catch_branches[0];
            
            // The error value is on the stack from the throw
            
            // Handle variable binding in catch pattern
            match pattern {
                Pattern::Variable(var_name) => {
                    // When we enter a catch handler, the VM has:
                    // 1. Unwound the stack to where it was when PushHandler was executed
                    // 2. Pushed the error value on top
                    // 
                    // The catch parameter is effectively the top of the stack at handler entry.
                    // We need to account for this in our variable tracking.
                    
                    // Push new scope for the catch block
                    // Use push_catch_scope with the handler stack depth
                    debug_assert!(
                        handler_stack_depth <= self.stack_depth,
                        "Handler stack depth {} should not exceed current stack depth {}",
                        handler_stack_depth,
                        self.stack_depth
                    );
                    self.push_catch_scope(handler_stack_depth);
                    
                    // The error value is at handler_stack_depth (after unwinding + push)
                    // Store relative position (0) since it's the first variable in this scope
                    self.locals.last_mut().unwrap().insert(var_name.clone(), 0);
                    
                    // Update our stack depth tracking to account for the error parameter
                    self.stack_depth = handler_stack_depth + 1;
                    debug_assert!(
                        self.stack_depth > 0,
                        "Stack depth should be positive after catch parameter"
                    );
                    
                    // Compile the handler
                    let before_handler_depth = self.stack_depth;
                    self.compile_node(graph, *handler)?;
                    
                    // After handler compilation, we should have exactly one value on stack
                    // (the handler result)
                    debug_assert!(
                        self.stack_depth == before_handler_depth,
                        "Handler should consume error parameter and produce result: expected depth {}, got {}",
                        before_handler_depth,
                        self.stack_depth
                    );
                    
                    // Pop the error value after the handler
                    // (the handler result remains on stack)
                    self.emit(Instruction::new(Opcode::Swap)); // handler_result, error
                    self.emit(Instruction::new(Opcode::Pop));  // handler_result
                    
                    // Pop scope
                    self.pop_scope();
                    
                    // Reset stack depth to handler depth (the result replaced the error)
                    self.stack_depth = handler_stack_depth;
                    debug_assert!(
                        self.stack_depth == handler_stack_depth,
                        "Stack depth should be restored to handler depth after catch"
                    );
                }
                _ => {
                    // For other patterns, just compile the handler (not fully supported yet)
                    self.compile_node(graph, *handler)?;
                }
            }
        }
        
        Ok(())
    }
    
    /// Set up jump to finally block after catch handler
    fn setup_finally_jumps(&mut self, finally: Option<NodeId>) -> Result<Option<usize>> {
        // Jump to finally after catch
        let jump_after_catch = if finally.is_some() {
            Some(self.emit(Instruction::new(Opcode::Jump)))
        } else {
            None
        };
        
        Ok(jump_after_catch)
    }
    
    /// Compile the finally block with proper jump patching
    fn compile_finally_block(
        &mut self,
        graph: &ASTGraph,
        finally: Option<NodeId>,
        push_finally_idx: Option<usize>,
        jump_after_body: usize,
        jump_after_catch: Option<usize>,
    ) -> Result<()> {
        // Compile finally block if present
        if let Some(finally_block) = finally {
            // Mark finally start
            let finally_pos = self.bytecode.chunks[self.current_chunk].instructions.len();
            
            // Patch PushFinally with the finally IP
            if let Some(idx) = push_finally_idx {
                self.bytecode.chunks[self.current_chunk].instructions[idx].arg = finally_pos as u32;
            }
            
            // Patch jumps to finally
            self.patch_jump(jump_after_body, finally_pos);
            if let Some(jump) = jump_after_catch {
                self.patch_jump(jump, finally_pos);
            }
            
            // At entry to finally block:
            // - Normal path: stack has [result, marker]
            // - Exception path: stack has [error, marker]
            
            // The Finally opcode will handle saving/restoring these values
            self.emit(Instruction::new(Opcode::Finally));
            
            // Now compile the finally block - it runs with the saved values removed
            self.compile_node(graph, finally_block)?;
            
            // Pop the finally block's result (we don't use it)
            self.emit(Instruction::new(Opcode::Pop));
            
            // EndFinally opcode restores saved values and continues appropriately
            self.emit(Instruction::new(Opcode::EndFinally));
        } else {
            // No finally block - patch jumps directly to end
            let end_pos = self.bytecode.chunks[self.current_chunk].instructions.len();
            self.patch_jump(jump_after_body, end_pos);
        }
        
        Ok(())
    }
    
    fn compile_try(
        &mut self,
        graph: &ASTGraph,
        body: NodeId,
        catch_branches: &[(Pattern, NodeId)],
        finally: Option<NodeId>,
    ) -> Result<()> {
        // Structure:
        // PushHandler catch_ip
        // PushFinally finally_ip (if finally exists)
        // <try body>
        // PopHandler
        // Jump finally (or end)
        // catch:
        //   <catch handlers>
        //   Jump finally (or end)
        // finally:
        //   <preserve stack value>
        //   <finally block>
        //   <restore stack value>
        // end:
        
        // Save the stack depth before PushHandler - this is where the VM will unwind to
        let handler_stack_depth = self.stack_depth;
        
        // Set up error handlers for try/catch/finally
        let (push_handler_idx, push_finally_idx) = self.setup_error_handlers(finally)?;
        
        // Compile try body and get jump after successful completion
        let jump_after_body = self.compile_try_body(graph, body)?;
        
        // Set up catch handler position and patch PushHandler
        self.setup_catch_handlers(push_handler_idx)?;
        
        // Compile catch branches with pattern matching
        // Pass the handler stack depth so we know where the error will be placed
        self.compile_catch_branches(graph, catch_branches, handler_stack_depth)?;
        
        // Set up jump to finally after catch
        let jump_after_catch = self.setup_finally_jumps(finally)?;
        
        // Compile finally block with proper jump patching
        self.compile_finally_block(graph, finally, push_finally_idx, jump_after_body, jump_after_catch)?;
        
        Ok(())
    }
    
    fn compile_throw(
        &mut self,
        graph: &ASTGraph,
        error: NodeId,
    ) -> Result<()> {
        // Compile error value
        self.compile_node(graph, error)?;
        
        // Throw error
        self.emit(Instruction::new(Opcode::Throw));
        
        // Stack effect is now managed by emit()
        
        Ok(())
    }
    
    fn compile_promise(
        &mut self,
        graph: &ASTGraph,
        body: NodeId,
    ) -> Result<()> {
        // For now, compile promise as immediate execution
        // In the future, this should create an actual promise
        self.compile_node(graph, body)?;
        
        // Wrap in promise
        self.emit(Instruction::new(Opcode::PromiseNew));
        
        Ok(())
    }
    
    fn compile_promise_all(
        &mut self,
        graph: &ASTGraph,
        promises: &[NodeId],
    ) -> Result<()> {
        // Compile all promises
        for promise in promises {
            self.compile_node(graph, *promise)?;
        }
        
        // Create list of promises
        self.emit(Instruction::with_arg(Opcode::MakeList, promises.len() as u32));
        
        // Wait for all
        self.emit(Instruction::new(Opcode::PromiseAll));
        
        // Stack effect: pop list, push result list
        
        Ok(())
    }
    
    fn compile_promise_race(
        &mut self,
        graph: &ASTGraph,
        promises: &[NodeId],
    ) -> Result<()> {
        // Compile all promises
        for promise in promises {
            self.compile_node(graph, *promise)?;
        }
        
        // Create list of promises
        self.emit(Instruction::with_arg(Opcode::MakeList, promises.len() as u32));
        
        // Race promises
        self.emit(Instruction::new(Opcode::PromiseRace));
        
        // Stack effect: pop list, push first result
        
        Ok(())
    }
    
    fn compile_timeout(
        &mut self,
        graph: &ASTGraph,
        duration: NodeId,
        promise: NodeId,
        default: Option<NodeId>,
    ) -> Result<()> {
        // Compile duration
        self.compile_node(graph, duration)?;
        
        // Compile promise
        self.compile_node(graph, promise)?;
        
        // Compile default if present
        if let Some(default_val) = default {
            self.compile_node(graph, default_val)?;
        } else {
            // Push nil as default
            self.emit(Instruction::new(Opcode::PushNil));
        }
        
        // Apply timeout
        self.emit(Instruction::new(Opcode::WithTimeout));
        
        // Stack effect is now managed by emit()
        
        Ok(())
    }

    fn find_free_variables(
        &self,
        graph: &ASTGraph,
        node_id: NodeId,
        params: &[String],
    ) -> Result<Vec<String>> {
        if self.options.optimization_level != OptimizationLevel::None {
            // Use the enhanced analyzer for more accurate results when optimizing
            let mut analyzer = FreeVarAnalyzer::new();
            analyzer.analyze_with_params(graph, node_id, params)
        } else {
            // Use the existing implementation for non-optimized builds
            let mut free_vars = HashSet::new();
            let mut bound_vars = HashSet::new();

            // Parameters are bound
            for param in params {
                bound_vars.insert(param.clone());
            }

            self.collect_free_variables(graph, node_id, &mut free_vars, &mut bound_vars)?;

            // Return in deterministic order
            let mut result: Vec<_> = free_vars.into_iter().collect();
            result.sort();
            Ok(result)
        }
    }

    fn collect_free_variables(
        &self,
        graph: &ASTGraph,
        node_id: NodeId,
        free_vars: &mut HashSet<String>,
        bound_vars: &mut HashSet<String>,
    ) -> Result<()> {
        let node = graph
            .nodes
            .get(&node_id)
            .ok_or_else(|| anyhow!("Invalid node ID: {:?}", node_id))?;

        match node {
            Node::Variable { name } => {
                // If not bound in lambda, check if it's in outer scope
                if !bound_vars.contains(name) {
                    // Check if it's in our locals (outer scope)
                    for scope in self.locals.iter().rev() {
                        if scope.contains_key(name) {
                            free_vars.insert(name.clone());
                            break;
                        }
                    }
                    // Also check if it's in captured variables (for nested lambdas)
                    for scope in self.captured.iter().rev() {
                        if scope.contains_key(name) {
                            free_vars.insert(name.clone());
                            break;
                        }
                    }
                }
            }
            Node::Lambda { params, body } => {
                // Create new bound set with lambda params
                let mut new_bound = bound_vars.clone();
                for param in params {
                    new_bound.insert(param.clone());
                }
                self.collect_free_variables(graph, *body, free_vars, &mut new_bound)?;
            }
            Node::Let { bindings, body } => {
                // Let bindings are evaluated in sequence
                let mut new_bound = bound_vars.clone();
                for (name, value) in bindings {
                    // Value can reference previous bindings
                    self.collect_free_variables(graph, *value, free_vars, &mut new_bound)?;
                    new_bound.insert(name.clone());
                }
                self.collect_free_variables(graph, *body, free_vars, &mut new_bound)?;
            }
            Node::Application { function, args } => {
                self.collect_free_variables(graph, *function, free_vars, bound_vars)?;
                for arg in args {
                    self.collect_free_variables(graph, *arg, free_vars, bound_vars)?;
                }
            }
            Node::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.collect_free_variables(graph, *condition, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *then_branch, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *else_branch, free_vars, bound_vars)?;
            }
            Node::List(items) => {
                for item in items {
                    self.collect_free_variables(graph, *item, free_vars, bound_vars)?;
                }
            }
            Node::Effect { args, .. } => {
                for arg in args {
                    self.collect_free_variables(graph, *arg, free_vars, bound_vars)?;
                }
            }
            Node::Async { body } => {
                self.collect_free_variables(graph, *body, free_vars, bound_vars)?;
            }
            Node::Await { expr } => {
                self.collect_free_variables(graph, *expr, free_vars, bound_vars)?;
            }
            Node::Spawn { expr } => {
                self.collect_free_variables(graph, *expr, free_vars, bound_vars)?;
            }
            Node::Send { channel, value } => {
                self.collect_free_variables(graph, *channel, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *value, free_vars, bound_vars)?;
            }
            Node::Receive { channel } => {
                self.collect_free_variables(graph, *channel, free_vars, bound_vars)?;
            }
            Node::TrySend { channel, value } => {
                self.collect_free_variables(graph, *channel, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *value, free_vars, bound_vars)?;
            }
            Node::TryReceive { channel } => {
                self.collect_free_variables(graph, *channel, free_vars, bound_vars)?;
            }
            Node::Select { branches, default } => {
                for (channel_op, handler) in branches {
                    self.collect_free_variables(graph, *channel_op, free_vars, bound_vars)?;
                    self.collect_free_variables(graph, *handler, free_vars, bound_vars)?;
                }
                if let Some(def) = default {
                    self.collect_free_variables(graph, *def, free_vars, bound_vars)?;
                }
            }
            Node::Actor { initial_state, handler } => {
                self.collect_free_variables(graph, *initial_state, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *handler, free_vars, bound_vars)?;
            }
            Node::ActorSend { actor, message } => {
                self.collect_free_variables(graph, *actor, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *message, free_vars, bound_vars)?;
            }
            Node::ActorReceive { patterns, timeout } => {
                for (_, handler) in patterns {
                    self.collect_free_variables(graph, *handler, free_vars, bound_vars)?;
                }
                if let Some((duration, handler)) = timeout {
                    self.collect_free_variables(graph, *duration, free_vars, bound_vars)?;
                    self.collect_free_variables(graph, *handler, free_vars, bound_vars)?;
                }
            }
            Node::Become { new_state } => {
                self.collect_free_variables(graph, *new_state, free_vars, bound_vars)?;
            }
            Node::Try { body, catch_branches, finally } => {
                self.collect_free_variables(graph, *body, free_vars, bound_vars)?;
                for (_, handler) in catch_branches {
                    self.collect_free_variables(graph, *handler, free_vars, bound_vars)?;
                }
                if let Some(f) = finally {
                    self.collect_free_variables(graph, *f, free_vars, bound_vars)?;
                }
            }
            Node::Throw { error } => {
                self.collect_free_variables(graph, *error, free_vars, bound_vars)?;
            }
            Node::Promise { body } => {
                self.collect_free_variables(graph, *body, free_vars, bound_vars)?;
            }
            Node::PromiseAll { promises } | Node::PromiseRace { promises } => {
                for promise in promises {
                    self.collect_free_variables(graph, *promise, free_vars, bound_vars)?;
                }
            }
            Node::Timeout { duration, promise, default } => {
                self.collect_free_variables(graph, *duration, free_vars, bound_vars)?;
                self.collect_free_variables(graph, *promise, free_vars, bound_vars)?;
                if let Some(def) = default {
                    self.collect_free_variables(graph, *def, free_vars, bound_vars)?;
                }
            }
            _ => {} // Literals, Channel, etc. have no variables
        }

        Ok(())
    }

    fn compile_captured_variable(&mut self, name: &str) -> Result<()> {
        // Find the variable in outer scopes and emit code to load it
        for (scope_idx, scope) in self.locals.iter().enumerate().rev() {
            if let Some(&rel_pos) = scope.get(name) {
                // Calculate absolute position like in compile_variable
                let abs_pos = self.get_scope_base(scope_idx)? + rel_pos;
                // Fix for issue 26: Properly calculate absolute stack position for captured variables
                
                // Use fast local opcodes for indices 0-3
                match abs_pos {
                    0 => self.emit(Instruction::new(Opcode::LoadLocal0)),
                    1 => self.emit(Instruction::new(Opcode::LoadLocal1)),
                    2 => self.emit(Instruction::new(Opcode::LoadLocal2)),
                    3 => self.emit(Instruction::new(Opcode::LoadLocal3)),
                    _ => self.emit(Instruction::with_arg(Opcode::Load, abs_pos as u32)),
                };
                // Don't dereference cells when capturing - we want to capture the cell itself
                // The dereference will happen when the variable is used
                return Ok(());
            }
        }

        // Check if it's already captured (for nested lambdas)
        for (_scope_idx, scope) in self.captured.iter().enumerate().rev() {
            if let Some(&capture_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(
                    Opcode::LoadCaptured,
                    capture_idx as u32,
                ));
                return Ok(());
            }
        }

        // If not found in locals or captured, it might be a global
        let idx = self.add_constant(Value::String(name.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadGlobal, idx));
        Ok(())
    }

    fn compile_match(
        &mut self,
        graph: &ASTGraph,
        expr: NodeId,
        branches: &[(Pattern, NodeId)],
    ) -> Result<()> {
        // Validate that we have at least one branch
        if branches.is_empty() {
            return Err(anyhow!("Match expression must have at least one branch"));
        }

        // Compile the expression to match
        self.compile_node(graph, expr)?;

        // We'll compile pattern matching as a series of if-else chains
        // Key insight: for literal patterns, we need to preserve the value being matched
        // across multiple tests. We'll use a different strategy:
        // 1. Keep the value on stack throughout
        // 2. For literal patterns, dup the value, push literal, compare
        // 3. Only consume the value when we find a match

        let mut jump_to_ends = Vec::new();

        for (i, (pattern, body)) in branches.iter().enumerate() {
            let is_last = i == branches.len() - 1;

            // Compile pattern test
            // This will leave a boolean on the stack for conditional patterns
            // For always-matching patterns (variable/wildcard), no test is generated
            let (bindings, always_matches) = self.compile_pattern_test(graph, pattern)?;

            // Handle the test result
            let jump_to_next = if always_matches {
                // Pattern always matches - no jump needed
                None
            } else {
                // We have a boolean on the stack from the pattern test
                // Jump to next branch if false
                Some(self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)))
            };

            // At this point, pattern matched (or always matches)
            // The matched value is still on the stack

            // Check for special cons pattern case
            let is_cons_pattern = if let Pattern::Constructor { name, patterns } = pattern {
                (name == "cons" || name == "Cons")
                    && patterns.len() == 2
                    && matches!(&patterns[0], Pattern::Variable(_))
                    && matches!(&patterns[1], Pattern::Variable(_))
            } else {
                false
            };

            // Create new scope for pattern bindings
            if !bindings.is_empty() {
                if is_cons_pattern {
                    // Special handling for cons pattern
                    // At this point, JumpIfNot has already consumed the boolean (if there was one)
                    // The matched value (list) is on top of stack at position stack_depth-1

                    // Save the position where the list currently is
                    let list_position = self.stack_depth - 1;

                    // We need to duplicate the list first before extracting head/tail
                    self.emit(Instruction::new(Opcode::Dup)); // [..., list] -> [..., list, list]
                    // Stack depth is now managed by emit()

                    // Extract head from the duplicated list
                    self.emit(Instruction::new(Opcode::ListHead)); // [..., list, list] -> [..., list, head]
                    
                    // Now we need to get the tail from the original list
                    // First swap to get the original list on top
                    self.emit(Instruction::new(Opcode::Swap)); // [..., list, head] -> [..., head, list]
                    
                    // Extract tail
                    self.emit(Instruction::new(Opcode::ListTail)); // [..., head, list] -> [..., head, tail]

                    // Stack now has [head, tail] where the list used to be
                    // head is at list_position, tail is at list_position + 1

                    // Set up scope for bindings
                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    self.scope_bases.push(list_position); // Base of our new scope
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;

                    // Bind head and tail with their positions relative to scope base
                    // After all operations, head is at list_position and tail at list_position + 1
                    // But we store them as 0 and 1 relative to the scope base
                    if let Pattern::Constructor { patterns, .. } = pattern {
                        if let Pattern::Variable(head_name) = &patterns[0] {
                            // Store position relative to scope base
                            self.locals[scope_idx].insert(head_name.clone(), 0);
                        }
                        if let Pattern::Variable(tail_name) = &patterns[1] {
                            // Store position relative to scope base  
                            self.locals[scope_idx].insert(tail_name.clone(), 1);
                        }
                    }
                } else {
                    // Regular pattern binding
                    // After the pattern test and potential JumpIfNot, the matched value is on top of stack
                    // The bindings contain absolute stack positions, but we need to adjust them
                    // to be relative to our scope base

                    // The matched value is at stack_depth - 1
                    let value_pos = self.stack_depth - 1;

                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    self.scope_bases.push(value_pos);
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;

                    // Add pattern bindings
                    // For simple patterns like (as x 42), the binding position should be 0
                    // relative to the scope base (which is the value position)
                    for (name, abs_pos) in &bindings {
                        // Convert absolute position to relative position
                        let rel_pos = if *abs_pos >= value_pos {
                            *abs_pos - value_pos
                        } else {
                            // This shouldn't happen, but handle it gracefully
                            0
                        };
                        self.locals[scope_idx].insert(name.clone(), rel_pos);
                    }
                }
            } else {
                // No bindings - pop the matched value
                self.emit(Instruction::new(Opcode::Pop));
            }

            // Compile branch body
            self.compile_node(graph, *body)?;

            // Clean up pattern bindings scope
            if !bindings.is_empty() {
                self.locals.pop();
                self.captured.pop();
                self.scope_bases.pop();
                self.cell_vars.pop();

                // Note: For cons patterns, we leave the extracted values on the stack
                // They will be cleaned up when the function returns
                // This is simpler than trying to juggle the stack to preserve the result
            }

            // Jump to end (skip other branches and fallback)
            // We need to jump unless this is the last branch AND it always matches
            // (in which case there's no fallback code to skip)
            if !is_last || !always_matches {
                jump_to_ends.push(self.emit(Instruction::with_arg(Opcode::Jump, 0)));
            }

            // Patch jump to next branch
            if let Some(jump_offset) = jump_to_next {
                let next_branch = self.current_offset();
                self.patch_jump(jump_offset, next_branch);

                // JumpIfNot already consumed the boolean
                // The value remains on stack for the next pattern test
            }
        }

        // If we get here and no pattern matched, we need to handle the error case
        // Check if the last pattern was always-matching
        let last_always_matches = if let Some((pattern, _)) = branches.last() {
            match pattern {
                Pattern::Wildcard | Pattern::Variable(_) => true,
                Pattern::As { pattern, .. } => {
                    // Check if the inner pattern always matches
                    matches!(pattern.as_ref(), Pattern::Wildcard | Pattern::Variable(_))
                }
                _ => false,
            }
        } else {
            false
        };

        if !last_always_matches && !branches.is_empty() {
            // No pattern matched - this is a runtime error
            // For now, push nil as a fallback
            self.emit(Instruction::new(Opcode::Pop)); // Pop the unmatched value
            self.emit(Instruction::new(Opcode::PushNil));
        }

        // Patch all jumps to end (after fallback code)
        let end = self.current_offset();
        for jump_offset in jump_to_ends {
            self.patch_jump(jump_offset, end);
        }

        Ok(())
    }

    fn compile_pattern_test(
        &mut self,
        graph: &ASTGraph,
        pattern: &Pattern,
    ) -> Result<(Vec<(String, usize)>, bool)> {
        let mut bindings = Vec::new();

        match pattern {
            Pattern::Wildcard => {
                // Always matches, no test needed
                // Value remains on stack
                Ok((bindings, true))
            }
            Pattern::Variable(name) => {
                // Always matches, bind the value
                let pos = self.stack_depth - 1; // Value is on top of stack
                bindings.push((name.clone(), pos));
                Ok((bindings, true))
            }
            Pattern::Literal(lit) => {
                // Duplicate the value for comparison
                self.emit(Instruction::new(Opcode::Dup));
                // Push literal and compare
                self.compile_literal(lit)?;
                self.emit(Instruction::new(Opcode::Eq));
                // Stack now has: [value, bool]
                // The boolean will be consumed by JumpIfNot
                // The original value remains for the next test or for the body
                Ok((bindings, false))
            }
            Pattern::Constructor { name, patterns } => {
                // Special handling for list patterns
                if (name == "cons" || name == "Cons")
                    && patterns.len() == 2
                    && matches!(&patterns[0], Pattern::Variable(_))
                    && matches!(&patterns[1], Pattern::Variable(_))
                {
                    // Special case: cons with two variable patterns
                    // First check if the list is non-empty
                    self.emit(Instruction::new(Opcode::Dup)); // [..., list, list]
                    self.emit(Instruction::new(Opcode::ListEmpty)); // [..., list, bool]
                    self.emit(Instruction::new(Opcode::Not)); // [..., list, !bool]

                    // This is handled differently in compile_match
                    if let (Pattern::Variable(head_name), Pattern::Variable(tail_name)) =
                        (&patterns[0], &patterns[1])
                    {
                        // Just return the bindings, compile_match will handle extraction
                        bindings.push((head_name.clone(), 0)); // placeholder positions
                        bindings.push((tail_name.clone(), 1));
                    }
                    Ok((bindings, false)) // false = test was generated
                } else if (name == "cons" || name == "Cons") && patterns.len() == 2 {
                    // Handle cons pattern on lists
                    // Stack before: [..., list]

                    // First check if list is empty
                    self.emit(Instruction::new(Opcode::Dup)); // [..., list, list]
                    self.emit(Instruction::new(Opcode::ListEmpty)); // [..., list, bool]

                    // If empty, fail - the test result (bool) will be left on stack
                    // for JumpIfNot in compile_match
                    // But we need to invert it because empty = fail
                    self.emit(Instruction::new(Opcode::Not)); // [..., list, !bool]
                    Ok((bindings, false))
                } else if (name == "nil" || name == "Nil") && patterns.is_empty() {
                    // Handle nil pattern on lists
                    self.emit(Instruction::new(Opcode::Dup));
                    self.emit(Instruction::new(Opcode::ListEmpty));
                    Ok((bindings, false))
                } else {
                    // Regular tagged value pattern
                    self.emit(Instruction::new(Opcode::Dup)); // Duplicate the value for tag check
                    let tag_idx = self.add_constant(Value::String(name.clone()));
                    self.emit(Instruction::with_arg(Opcode::IsTagged, tag_idx));

                    // If not the right tag, jump to next pattern
                    let fail_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)); // Will patch later

                    // Extract and test sub-patterns
                    for (i, sub_pattern) in patterns.iter().enumerate() {
                        // Duplicate the tagged value
                        self.emit(Instruction::new(Opcode::Dup));
                        // Get the field
                        self.emit(Instruction::with_arg(Opcode::GetTaggedField, i as u32));
                        // Test the sub-pattern
                        let (sub_bindings, _) = self.compile_pattern_test(graph, sub_pattern)?;
                        bindings.extend(sub_bindings);

                        // If sub-pattern failed, clean up stack and fail
                        let _sub_fail_jump =
                            self.bytecode.chunks[self.current_chunk].instructions.len();
                        self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)); // Will patch later

                        // Pop the test result (keep the value for next field or body)
                        self.emit(Instruction::new(Opcode::Pop));
                    }

                    // All sub-patterns matched, push true
                    self.emit(Instruction::new(Opcode::PushTrue));

                    // Jump over the failure case
                    let success_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.emit(Instruction::with_arg(Opcode::Jump, 0)); // Will patch later

                    // Patch the fail jump to here
                    let fail_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.bytecode.chunks[self.current_chunk].patch_jump(fail_jump, fail_target);

                    // Also patch sub-pattern fail jumps
                    for i in 0..patterns.len() {
                        let sub_fail_jump = fail_jump + 2 + (i * 4) + 3; // Calculate the position
                        self.bytecode.chunks[self.current_chunk]
                            .patch_jump(sub_fail_jump, fail_target);
                    }

                    // Failure case: push false
                    self.emit(Instruction::new(Opcode::PushFalse));

                    // Patch success jump
                    let end_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.bytecode.chunks[self.current_chunk].patch_jump(success_jump, end_target);

                    Ok((bindings, false))
                }
            }
            Pattern::Guard { pattern, condition } => {
                // First compile the inner pattern test
                let (inner_bindings, always_matches) = self.compile_pattern_test(graph, pattern)?;

                if !always_matches {
                    // If the pattern doesn't always match, we already have a test result on stack
                    // Need to check it before evaluating the guard
                    let pattern_fail_jump =
                        self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)); // Will patch later

                    // Pattern matched, now evaluate guard condition
                    // Create temporary scope with pattern bindings for guard compilation
                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    self.scope_bases.push(self.stack_depth - 1); // Value is on stack
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;

                    // Add pattern bindings to scope
                    for (name, pos) in &inner_bindings {
                        self.locals[scope_idx].insert(name.clone(), *pos);
                    }

                    // Compile guard condition with bindings in scope
                    self.compile_node(graph, *condition)?;

                    // Remove temporary scope
                    self.locals.pop();
                    self.captured.pop();
                    self.scope_bases.pop();
                    self.cell_vars.pop();

                    // Jump to end
                    let end_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.emit(Instruction::with_arg(Opcode::Jump, 0)); // Will patch later

                    // Pattern failed, push false
                    let pattern_fail_target =
                        self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.bytecode.chunks[self.current_chunk]
                        .patch_jump(pattern_fail_jump, pattern_fail_target);
                    self.emit(Instruction::new(Opcode::PushFalse));

                    // Patch end jump
                    let end_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.bytecode.chunks[self.current_chunk].patch_jump(end_jump, end_target);
                } else {
                    // Pattern always matches, just evaluate the guard
                    // Create temporary scope with pattern bindings for guard compilation
                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    self.scope_bases.push(self.stack_depth - 1); // Value is on stack
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;

                    // Add pattern bindings to scope
                    for (name, pos) in &inner_bindings {
                        self.locals[scope_idx].insert(name.clone(), *pos);
                    }

                    // Compile guard condition with bindings in scope
                    self.compile_node(graph, *condition)?;

                    // Remove temporary scope
                    self.locals.pop();
                    self.captured.pop();
                    self.scope_bases.pop();
                    self.cell_vars.pop();
                }

                let bindings_mut = inner_bindings;
                Ok((bindings_mut, false))
            }
            Pattern::As { binding, pattern } => {
                // As-pattern: bind the value, then test the inner pattern
                // The value is on top of stack
                let value_pos = self.stack_depth - 1;

                // Add the as-binding first
                bindings.push((binding.clone(), value_pos));

                // Compile the inner pattern
                let (inner_bindings, always_matches) = self.compile_pattern_test(graph, pattern)?;

                // Add any inner bindings
                bindings.extend(inner_bindings);

                Ok((bindings, always_matches))
            }
            Pattern::Or(patterns) => {
                // Or-pattern: try each pattern in sequence until one matches
                if patterns.is_empty() {
                    // Empty or-pattern never matches
                    self.emit(Instruction::new(Opcode::PushFalse));
                    return Ok((bindings, false));
                }

                let mut jumps_to_success = Vec::new();

                for (i, pat) in patterns.iter().enumerate() {
                    // The stack should have [value] at this point
                    // compile_pattern_test expects the value on top of stack

                    let (pat_bindings, always_matches) = self.compile_pattern_test(graph, pat)?;

                    if always_matches {
                        // This pattern always matches, no need to check others
                        bindings.extend(pat_bindings);
                        // Push true for the or-pattern result
                        self.emit(Instruction::new(Opcode::PushTrue));
                        return Ok((bindings, false));
                    }

                    // Pattern test left stack as: [value, bool]
                    // We need to check the bool and potentially continue

                    if i < patterns.len() - 1 {
                        // Not the last pattern - need to preserve value for next iteration
                        // Stack: [value, bool]
                        // Duplicate the bool for the jump test
                        self.emit(Instruction::new(Opcode::Dup)); // [value, bool, bool]

                        // If true, jump to success
                        let jump_to_success =
                            self.bytecode.chunks[self.current_chunk].instructions.len();
                        self.emit(Instruction::with_arg(Opcode::JumpIf, 0)); // Will patch later
                        jumps_to_success.push(jump_to_success);
                        // JumpIf consumed one bool, stack: [value, bool]

                        // Test failed, pop the false result
                        self.emit(Instruction::new(Opcode::Pop)); // [value]
                                                                  // Ready for next iteration
                    } else {
                        // Last pattern - the bool result stays on stack
                        // If true, jump to success
                        let jump_to_success =
                            self.bytecode.chunks[self.current_chunk].instructions.len();
                        self.emit(Instruction::with_arg(Opcode::JumpIf, 0)); // Will patch later
                        jumps_to_success.push(jump_to_success);
                        // Stack after jump: [value] (JumpIf consumed the bool)
                    }

                    // Note: We can't use bindings from or-patterns as they may not all bind the same variables
                }

                // After all patterns tried and failed
                // For the last pattern:
                // - If it matched, we jumped to success
                // - If it didn't match, the stack is [value] (JumpIf consumed the false)
                // We need to push false as the or-pattern result
                self.emit(Instruction::new(Opcode::PushFalse)); // [value, false]

                // Jump to end to skip success case
                let jump_to_end = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.emit(Instruction::with_arg(Opcode::Jump, 0)); // Will patch later

                // Success target: when we jump here, stack is [value]
                let success_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                for jump in jumps_to_success {
                    self.bytecode.chunks[self.current_chunk].patch_jump(jump, success_target);
                }
                // Push true as the or-pattern result
                self.emit(Instruction::new(Opcode::PushTrue)); // [value, true]

                // Patch jump to end
                let end_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.bytecode.chunks[self.current_chunk].patch_jump(jump_to_end, end_target);

                // Stack now has: [value, bool] where bool is the or-pattern result
                Ok((bindings, false))
            }
            Pattern::Range(range) => {
                // Range pattern: check if value is within range

                // Duplicate value for comparison
                self.emit(Instruction::new(Opcode::Dup));

                // Check lower bound
                self.compile_literal(&range.start)?;
                self.emit(Instruction::new(Opcode::Ge)); // value >= start

                // Short-circuit if lower bound fails
                let lower_fail_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0)); // Will patch later

                // Check upper bound
                self.emit(Instruction::new(Opcode::Dup)); // Duplicate value again
                self.compile_literal(&range.end)?;
                if range.inclusive {
                    self.emit(Instruction::new(Opcode::Le)); // value <= end
                } else {
                    self.emit(Instruction::new(Opcode::Lt)); // value < end
                }

                // Jump to end
                let end_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.emit(Instruction::with_arg(Opcode::Jump, 0)); // Will patch later

                // Lower bound failed, push false
                let lower_fail_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.bytecode.chunks[self.current_chunk]
                    .patch_jump(lower_fail_jump, lower_fail_target);
                self.emit(Instruction::new(Opcode::PushFalse));

                // Patch end jump
                let end_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                self.bytecode.chunks[self.current_chunk].patch_jump(end_jump, end_target);

                Ok((bindings, false))
            }
            Pattern::View { function, pattern } => {
                // View pattern: apply function then match
                // Stack: [..., value]

                // Duplicate the value for function application
                self.emit(Instruction::new(Opcode::Dup)); // [..., value, value]

                // Compile the function
                self.compile_node(graph, *function)?; // [..., value, value, func]

                // Call expects: function on top, then arguments below
                // Current stack: [..., original_value, dup_value, func]
                // The Call instruction will:
                // 1. Pop func
                // 2. Pop dup_value as the argument
                // 3. Push the result
                // This leaves [..., original_value, result] which is what we want!
                self.emit(Instruction::with_arg(Opcode::Call, 1)); // [..., value, result]

                // Now match the result against the inner pattern
                let (inner_bindings, always_matches) = self.compile_pattern_test(graph, pattern)?;

                // If pattern test left a boolean on stack, we need to handle it
                if !always_matches {
                    // Stack: [..., value, bool]
                    // Swap so value stays on top for potential binding
                    self.emit(Instruction::new(Opcode::Swap)); // [..., bool, value]
                    self.emit(Instruction::new(Opcode::Swap)); // [..., value, bool]
                }

                bindings.extend(inner_bindings);
                Ok((bindings, always_matches))
            }
        }
    }

    // Module compilation methods
    fn compile_module(
        &mut self,
        graph: &ASTGraph,
        name: &str,
        exports: &[String],
        body: NodeId,
    ) -> Result<()> {
        // Begin module scope
        let name_idx = self.add_constant(Value::String(name.to_string()));
        self.emit(Instruction::with_arg(Opcode::BeginModule, name_idx));

        // Compile the module body
        self.compile_node(graph, body)?;

        // Export each binding
        for export in exports {
            let export_idx = self.add_constant(Value::String(export.clone()));
            // Look up the binding value
            self.compile_variable(export)?;
            self.emit(Instruction::with_arg(Opcode::ExportBinding, export_idx));
            self.emit(Instruction::new(Opcode::Pop)); // ExportBinding doesn't consume the value
        }

        // End module scope
        self.emit(Instruction::new(Opcode::EndModule));

        Ok(())
    }

    fn compile_import(
        &mut self,
        module_path: &str,
        import_list: &[fluentai_core::ast::ImportItem],
        import_all: bool,
    ) -> Result<()> {
        // Load the module
        let module_idx = self.add_constant(Value::String(module_path.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadModule, module_idx));

        if import_all {
            // Import all exports from the module
            // The runtime will handle enumerating and binding all exports
            self.emit(Instruction::with_arg(Opcode::ImportAll, module_idx));
        } else {
            // Import specific bindings
            for item in import_list {
                let binding_idx = self.add_constant(Value::String(item.name.clone()));
                // Encode both module and binding indices in arg
                let arg = ((module_idx as u32) << 16) | (binding_idx as u32);
                self.emit(Instruction::with_arg(Opcode::ImportBinding, arg));

                // Store in local scope
                let local_name = item.alias.as_ref().unwrap_or(&item.name);
                let local_idx = self.add_local(local_name.clone())?;
                self.emit(Instruction::with_arg(Opcode::Store, local_idx as u32));
            }
        }

        // Import statements evaluate to nil
        self.emit(Instruction::new(Opcode::PushNil));

        Ok(())
    }

    fn compile_export(&mut self, export_list: &[fluentai_core::ast::ExportItem]) -> Result<()> {
        for item in export_list {
            // Load the value to export
            self.compile_variable(&item.name)?;

            // Export it
            let export_name = item.alias.as_ref().unwrap_or(&item.name);
            let export_idx = self.add_constant(Value::String(export_name.clone()));
            self.emit(Instruction::with_arg(Opcode::ExportBinding, export_idx));
            self.emit(Instruction::new(Opcode::Pop)); // ExportBinding doesn't consume the value
        }

        Ok(())
    }

    fn compile_qualified_variable(&mut self, module_name: &str, variable_name: &str) -> Result<()> {
        let module_idx = self.add_constant(Value::String(module_name.to_string()));
        let var_idx = self.add_constant(Value::String(variable_name.to_string()));

        // Encode both indices in arg
        let arg = ((module_idx as u32) << 16) | (var_idx as u32);
        self.emit(Instruction::with_arg(Opcode::LoadQualified, arg));

        // Stack depth is now managed by emit()

        Ok(())
    }

    fn compile_handler(
        &mut self,
        graph: &ASTGraph,
        handlers: &[(fluentai_core::ast::EffectType, Option<String>, NodeId)],
        body: NodeId,
    ) -> Result<()> {
        // Handler implementation:
        // 1. Compile handler functions and push them onto the stack
        // 2. Create handler table with effect types and optional operation filters
        // 3. Install handler for the dynamic extent of the body
        // 4. Execute body
        // 5. Uninstall handler

        // Save the stack depth before handler setup
        let saved_stack_depth = self.stack_depth;

        // Compile handler functions
        for (effect_type, op_filter, handler_fn) in handlers {
            // Push effect type - use the same case as VM expects
            let effect_str = match effect_type {
                fluentai_core::ast::EffectType::IO => "IO",
                fluentai_core::ast::EffectType::State => "State",
                fluentai_core::ast::EffectType::Error => "Error",
                fluentai_core::ast::EffectType::Time => "Time",
                fluentai_core::ast::EffectType::Network => "Network",
                fluentai_core::ast::EffectType::Random => "Random",
                fluentai_core::ast::EffectType::Dom => "Dom",
                fluentai_core::ast::EffectType::Async => "Async",
                fluentai_core::ast::EffectType::Concurrent => "Concurrent",
                fluentai_core::ast::EffectType::Pure => "Pure",
            }
            .to_string();
            let idx = self.add_constant(Value::String(effect_str));
            self.emit(Instruction::with_arg(Opcode::PushConst, idx));
            // emit() already updates stack_depth

            // Push operation filter (or nil if none)
            if let Some(op) = op_filter {
                let idx = self.add_constant(Value::String(op.clone()));
                self.emit(Instruction::with_arg(Opcode::PushConst, idx));
            } else {
                let nil_idx = self.add_constant(Value::Nil);
                self.emit(Instruction::with_arg(Opcode::PushConst, nil_idx));
            }
            // emit() already updates stack_depth

            // Compile handler function
            self.compile_node(graph, *handler_fn)?;
            // compile_node should have handled stack_depth correctly
        }

        // Create handler with the number of handlers
        // MakeHandler pops 3 values per handler and pushes 1 handler
        self.emit(Instruction::with_arg(
            Opcode::MakeHandler,
            handlers.len() as u32,
        ));
        // emit() will handle the stack depth adjustment

        // Install handler (pops handler, pushes nothing)
        self.emit(Instruction::new(Opcode::InstallHandler));
        // emit() will handle the stack depth adjustment

        // Compile body - the body will produce a value on the stack
        self.compile_node(graph, body)?;

        // Uninstall handler (preserving the body's result)
        self.emit(Instruction::new(Opcode::UninstallHandler));

        // The key insight: after all the handler setup and teardown,
        // we should have exactly one more value on the stack than we started with
        // (the result of the handler expression)
        self.stack_depth = saved_stack_depth + 1;

        Ok(())
    }

    fn add_local(&mut self, name: String) -> Result<usize> {
        let scope_idx = self.locals.len() - 1;
        let pos = if self.stack_depth > 0 {
            self.stack_depth - 1 // Current value on stack
        } else {
            0 // No value on stack yet
        };
        self.locals[scope_idx].insert(name, pos);
        Ok(pos)
    }

    fn enter_scope(&mut self) {
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.scope_bases.push(self.stack_depth);
        self.cell_vars.push(HashSet::new());
    }

    fn exit_scope(&mut self) {
        self.locals.pop();
        self.captured.pop();
        self.scope_bases.pop();
        self.cell_vars.pop();
    }
    
    /// Safe accessor for scope base values with bounds checking.
    /// Returns the base stack position for the given scope index.
    /// 
    /// The scope base represents the stack depth when entering a scope,
    /// used to convert relative variable positions to absolute stack positions.
    /// 
    /// # Errors
    /// Returns an error if scope_idx is out of bounds.
    #[inline]
    fn get_scope_base(&self, scope_idx: usize) -> Result<usize> {
        self.scope_bases.get(scope_idx)
            .copied()
            .ok_or_else(|| anyhow!(
                "Internal compiler error: scope index {} out of bounds (max: {})", 
                scope_idx, 
                self.scope_bases.len().saturating_sub(1)
            ))
    }
    
    /// Check if a variable is a cell variable with bounds checking.
    /// Cell variables are used in letrec bindings and require dereferencing.
    /// 
    /// When a variable is stored in a cell (for recursive bindings), it needs
    /// special handling with CellGet instructions to access the actual value.
    /// 
    /// # Errors
    /// Returns an error if scope_idx is out of bounds.
    #[inline]
    fn is_cell_var(&self, scope_idx: usize, name: &str) -> Result<bool> {
        self.cell_vars.get(scope_idx)
            .map(|cells| cells.contains(name))
            .ok_or_else(|| anyhow!(
                "Internal compiler error: scope index {} out of bounds for cell_vars (max: {})", 
                scope_idx,
                self.cell_vars.len().saturating_sub(1)
            ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluentai_core::ast::{EffectType, Graph};

    #[test]
    fn test_handler_bytecode_generation() {
        // Create a simple handler test case similar to what's in the effects runtime
        let mut graph = Graph::new();

        // Create handler body: just return 42
        let body = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();

        // Create handler function: lambda that takes _ and returns "handled"
        let handler_param = "_".to_string();
        let handler_body = graph
            .add_node(Node::Literal(Literal::String("handled".to_string())))
            .unwrap();
        let handler_fn = graph
            .add_node(Node::Lambda {
                params: vec![handler_param],
                body: handler_body,
            })
            .unwrap();

        // Create the handler node
        let handler = graph
            .add_node(Node::Handler {
                handlers: vec![(EffectType::IO, Some("print".to_string()), handler_fn)],
                body,
            })
            .unwrap();

        graph.root_id = Some(handler);

        // Compile and check bytecode
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph).unwrap();

        // Print the generated bytecode for inspection
        println!("\n=== Handler Bytecode Generation Test ===");
        println!("Generated bytecode:");
        for (i, instr) in bytecode.chunks[bytecode.main_chunk]
            .instructions
            .iter()
            .enumerate()
        {
            println!("  {:3}: {:?}", i, instr);
        }

        // Check for any LoadLocal2 instructions
        let has_load_local2 = bytecode.chunks[bytecode.main_chunk]
            .instructions
            .iter()
            .any(|instr| matches!(instr.opcode, Opcode::LoadLocal2));

        println!("\nContains LoadLocal2: {}", has_load_local2);

        // Also check the constants table
        println!("\nConstants:");
        for (i, val) in bytecode.chunks[bytecode.main_chunk]
            .constants
            .iter()
            .enumerate()
        {
            println!("  [{}]: {:?}", i, val);
        }

        // Assert that we don't have LoadLocal2 (which would be the bug)
        assert!(
            !has_load_local2,
            "Handler bytecode should not contain LoadLocal2"
        );
    }

    #[test]
    fn test_push_catch_scope_basic() {
        // Test basic functionality of push_catch_scope
        let mut compiler = Compiler::new();
        
        // Initial state
        assert_eq!(compiler.locals.len(), 1);
        assert_eq!(compiler.captured.len(), 1);
        assert_eq!(compiler.cell_vars.len(), 1);
        assert_eq!(compiler.scope_bases.len(), 1);
        assert_eq!(compiler.scope_bases[0], 0);
        
        // Set up stack depth to simulate compilation state with 5 values on stack
        compiler.stack_depth = 5;
        
        // Push a catch scope with handler depth 5
        compiler.push_catch_scope(5);
        
        // Verify new scope was created
        assert_eq!(compiler.locals.len(), 2);
        assert_eq!(compiler.captured.len(), 2);
        assert_eq!(compiler.cell_vars.len(), 2);
        assert_eq!(compiler.scope_bases.len(), 2);
        
        // Verify scope base was set to handler depth
        assert_eq!(compiler.scope_bases[1], 5);
        
        // Verify new scope has empty collections
        assert!(compiler.locals[1].is_empty());
        assert!(compiler.captured[1].is_empty());
        assert!(compiler.cell_vars[1].is_empty());
    }

    #[test]
    fn test_push_catch_scope_zero_depth() {
        // Test edge case with handler_depth = 0
        let mut compiler = Compiler::new();
        
        // Push a catch scope with handler depth 0
        compiler.push_catch_scope(0);
        
        // Verify scope base is 0
        assert_eq!(compiler.scope_bases.len(), 2);
        assert_eq!(compiler.scope_bases[1], 0);
        
        // Should still work correctly
        compiler.locals[1].insert("error".to_string(), 0);
        assert_eq!(compiler.locals[1].get("error"), Some(&0));
    }

    #[test]
    fn test_push_catch_scope_nested() {
        // Test multiple nested catch scopes
        let mut compiler = Compiler::new();
        
        // Set up stack depth to allow all handler depths (max is 10)
        compiler.stack_depth = 10;
        
        // Push first catch scope
        compiler.push_catch_scope(3);
        assert_eq!(compiler.scope_bases.len(), 2);
        assert_eq!(compiler.scope_bases[1], 3);
        
        // Push second catch scope
        compiler.push_catch_scope(7);
        assert_eq!(compiler.scope_bases.len(), 3);
        assert_eq!(compiler.scope_bases[2], 7);
        
        // Push third catch scope
        compiler.push_catch_scope(10);
        assert_eq!(compiler.scope_bases.len(), 4);
        assert_eq!(compiler.scope_bases[3], 10);
        
        // Verify all scope bases are preserved
        assert_eq!(compiler.scope_bases[0], 0);
        assert_eq!(compiler.scope_bases[1], 3);
        assert_eq!(compiler.scope_bases[2], 7);
        assert_eq!(compiler.scope_bases[3], 10);
    }

    #[test]
    fn test_push_catch_scope_with_pop() {
        // Test interaction between push_catch_scope and pop_scope
        let mut compiler = Compiler::new();
        
        // Set up stack depth to allow handler depth of 5
        compiler.stack_depth = 5;
        
        // Push a regular scope first
        compiler.push_scope();
        compiler.locals[1].insert("x".to_string(), 0);
        assert_eq!(compiler.scope_bases.len(), 2);
        
        // Push a catch scope
        compiler.push_catch_scope(5);
        compiler.locals[2].insert("e".to_string(), 0);
        assert_eq!(compiler.scope_bases.len(), 3);
        assert_eq!(compiler.scope_bases[2], 5);
        
        // Pop the catch scope
        compiler.pop_scope();
        assert_eq!(compiler.scope_bases.len(), 2);
        assert_eq!(compiler.locals.len(), 2);
        
        // Verify the regular scope is still intact
        assert_eq!(compiler.locals[1].get("x"), Some(&0));
        
        // Pop the regular scope
        compiler.pop_scope();
        assert_eq!(compiler.scope_bases.len(), 1);
        assert_eq!(compiler.locals.len(), 1);
    }

    #[test]
    fn test_catch_scope_variable_resolution() {
        // Test variable resolution within catch scopes
        let mut compiler = Compiler::new();
        
        // Set up initial stack depth
        compiler.stack_depth = 3;  // Changed from 2 to 3 to allow handler depth of 3
        compiler.scope_bases[0] = 0;
        
        // Add some variables to outer scope
        compiler.locals[0].insert("outer".to_string(), 0);
        compiler.locals[0].insert("x".to_string(), 1);
        
        // Push a catch scope at handler depth 3
        compiler.push_catch_scope(3);
        
        // Add catch parameter at relative position 0
        compiler.locals[1].insert("e".to_string(), 0);
        
        // Add another local in catch scope
        compiler.locals[1].insert("inner".to_string(), 1);
        
        // Test variable resolution
        // The catch parameter 'e' should resolve to position 0 relative to scope base 3
        assert_eq!(compiler.locals[1].get("e"), Some(&0));
        
        // The inner variable should be at position 1 relative to scope base 3
        assert_eq!(compiler.locals[1].get("inner"), Some(&1));
        
        // When compiling LoadLocal for these, they would use:
        // - "e": scope_base(3) + relative(0) = absolute position 3
        // - "inner": scope_base(3) + relative(1) = absolute position 4
        
        // Verify scope base is correct
        assert_eq!(compiler.scope_bases[1], 3);
    }
    
    #[test]
    fn test_bounds_checking_get_scope_base() {
        let compiler = Compiler::new();
        
        // Valid access
        assert_eq!(compiler.get_scope_base(0).unwrap(), 0);
        
        // Out of bounds access
        let result = compiler.get_scope_base(1);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("scope index 1 out of bounds"));
    }
    
    #[test]
    fn test_bounds_checking_is_cell_var() {
        let mut compiler = Compiler::new();
        
        // Add a cell variable to the first scope
        // Using direct access here is safe in test setup since we just created the compiler
        if let Some(cell_vars) = compiler.cell_vars.get_mut(0) {
            cell_vars.insert("test_var".to_string());
        } else {
            panic!("Expected at least one scope in newly created compiler");
        }
        
        // Valid access - variable exists
        assert_eq!(compiler.is_cell_var(0, "test_var").unwrap(), true);
        
        // Valid access - variable doesn't exist
        assert_eq!(compiler.is_cell_var(0, "nonexistent").unwrap(), false);
        
        // Out of bounds access
        let result = compiler.is_cell_var(1, "test_var");
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("scope index 1 out of bounds"));
    }
    
    #[test]
    fn test_bounds_checking_deeply_nested() {
        let mut compiler = Compiler::new();
        
        // Create deeply nested scopes
        for i in 0..10 {
            compiler.push_scope();
            compiler.scope_bases[i + 1] = i * 2 + 1;
        }
        
        // All accesses should succeed
        for i in 0..11 {
            let result = compiler.get_scope_base(i);
            assert!(result.is_ok(), "Failed to access scope {} in deeply nested structure", i);
        }
        
        // Out of bounds should fail
        let result = compiler.get_scope_base(11);
        assert!(result.is_err());
    }
    
    #[test]
    fn test_bounds_checking_error_messages() {
        let compiler = Compiler::new();
        
        // Test error message for get_scope_base
        let err = compiler.get_scope_base(5).unwrap_err();
        assert_eq!(
            err.to_string(),
            "Internal compiler error: scope index 5 out of bounds (max: 0)"
        );
        
        // Test error message for is_cell_var
        let err = compiler.is_cell_var(3, "var").unwrap_err();
        assert_eq!(
            err.to_string(),
            "Internal compiler error: scope index 3 out of bounds for cell_vars (max: 0)"
        );
    }
}

impl Compiler {    
    // Helper methods for debug assertions
    #[cfg(debug_assertions)]
    fn verify_stack_invariants(&self) {
        // Verify stack depth is consistent with scope bases
        if let Some(&last_scope_base) = self.scope_bases.last() {
            debug_assert!(
                self.stack_depth >= last_scope_base,
                "Stack depth {} is less than last scope base {}",
                self.stack_depth, last_scope_base
            );
        }
        
        // Verify all data structures have same depth
        debug_assert_eq!(
            self.locals.len(), self.captured.len(),
            "Locals and captured lists should have same length"
        );
        debug_assert_eq!(
            self.locals.len(), self.scope_bases.len(),
            "Locals and scope_bases should have same length"
        );
        debug_assert_eq!(
            self.locals.len(), self.cell_vars.len(),
            "Locals and cell_vars should have same length"
        );
    }
    
    #[cfg(debug_assertions)]
    fn verify_scope_consistency(&self, scope_idx: usize) {
        debug_assert!(
            scope_idx < self.locals.len(),
            "Scope index {} out of bounds for locals (len {})",
            scope_idx, self.locals.len()
        );
        
        debug_assert!(
            scope_idx < self.scope_bases.len(),
            "Scope index {} out of bounds for scope_bases (len {})",
            scope_idx, self.scope_bases.len()
        );
        
        // Verify all variables in scope have valid relative positions
        let scope_base = match self.get_scope_base(scope_idx) {
            Ok(base) => base,
            Err(_) => {
                debug_assert!(false, "Failed to get scope base for index {}", scope_idx);
                return;
            }
        };
        for (name, &rel_pos) in &self.locals[scope_idx] {
            let abs_pos = scope_base + rel_pos;
            debug_assert!(
                abs_pos < self.stack_depth,
                "Variable '{}' at absolute position {} exceeds stack depth {}",
                name, abs_pos, self.stack_depth
            );
        }
    }
    
    #[cfg(not(debug_assertions))]
    fn verify_stack_invariants(&self) {
        // No-op in release builds
    }
    
    #[cfg(not(debug_assertions))]
    fn verify_scope_consistency(&self, _scope_idx: usize) {
        // No-op in release builds
    }
    
    /// Initialize module source map if debug info is enabled
    fn init_module_source_map(&mut self) {
        if self.options.debug_info {
            if self.bytecode.module_source_map.is_none() {
                self.bytecode.module_source_map = Some(ModuleSourceMap::new());
            }
        }
    }
    
    /// Record source location for an instruction
    fn record_source_location(&mut self, instruction_offset: usize, node_id: NodeId) {
        if self.options.debug_info {
            // Ensure source map exists for current chunk
            let chunk = &mut self.bytecode.chunks[self.current_chunk];
            if chunk.source_map.is_none() {
                let mut source_map = SourceMap::new();
                if let Some(ref filename) = self.source_filename {
                    source_map.filename = Some(filename.clone());
                }
                chunk.source_map = Some(source_map);
            }
            
            // Add location mapping
            if let Some(source_map) = &mut chunk.source_map {
                // For now, we don't have actual line/column info from AST nodes
                // This would need to be added during parsing
                // Use node ID's internal value as a placeholder for start/end positions
                let node_id_value = node_id.0.get() as usize;
                source_map.add_instruction_location(instruction_offset, SourceLocation {
                    start: node_id_value,
                    end: node_id_value,
                    line: None,
                    column: None,
                });
                source_map.add_instruction_node(instruction_offset, node_id);
            }
        }
    }
}
