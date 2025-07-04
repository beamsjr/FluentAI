//! Compiler from AST to bytecode

use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode, Value};
use fluentai_core::ast::{Graph as ASTGraph, Node, NodeId, Literal, Pattern};
use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig, OptimizationLevel};
use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};

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
    stack_depth: usize, // Track current stack depth
    scope_bases: Vec<usize>, // Base stack position for each scope
    cell_vars: Vec<HashSet<String>>, // Variables that are cells (for letrec)
    options: CompilerOptions,
    // Tail call optimization tracking
    in_tail_position: bool, // Whether we're compiling in tail position
    current_function: Option<String>, // Name of current function being compiled
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
        }
    }
    
    pub fn compile(mut self, graph: &ASTGraph) -> Result<Bytecode> {
        // Apply optimizations if enabled
        let optimized_graph = if self.options.optimization_level != OptimizationLevel::None {
            let config = OptimizationConfig::for_level(self.options.optimization_level);
            let mut pipeline = OptimizationPipeline::new(config);
            pipeline.optimize(graph)?
        } else {
            graph.clone()
        };
        
        let root_id = optimized_graph.root_id
            .ok_or_else(|| anyhow!("AST graph has no root node"))?;
        
        self.compile_node(&optimized_graph, root_id)?;
        
        // Add halt instruction
        self.emit(Instruction::new(Opcode::Halt));
        
        Ok(self.bytecode)
    }
    
    fn compile_node(&mut self, graph: &ASTGraph, node_id: NodeId) -> Result<()> {
        let node = graph.nodes.get(&node_id)
            .ok_or_else(|| anyhow!("Invalid node ID: {:?}", node_id))?;
        
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
            Node::If { condition, then_branch, else_branch } => {
                self.compile_if(graph, *condition, *then_branch, *else_branch)?;
            }
            Node::List(items) => {
                self.compile_list(graph, items)?;
            }
            Node::Effect { effect_type, operation, args } => {
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
            Node::Channel => {
                self.emit(Instruction::new(Opcode::Channel));
            }
            Node::Send { channel, value } => {
                self.compile_send(graph, *channel, *value)?;
            }
            Node::Receive { channel } => {
                self.compile_receive(graph, *channel)?;
            }
            Node::Match { expr, branches } => {
                self.compile_match(graph, *expr, branches)?;
            }
            Node::Module { name, exports, body } => {
                self.compile_module(graph, name, exports, *body)?;
            }
            Node::Import { module_path, import_list, import_all } => {
                self.compile_import(module_path, import_list, *import_all)?;
            }
            Node::Export { export_list } => {
                self.compile_export(export_list)?;
            }
            Node::QualifiedVariable { module_name, variable_name } => {
                self.compile_qualified_variable(module_name, variable_name)?;
            }
            Node::Contract { .. } => {
                // Contracts are metadata and don't generate bytecode directly
                // They should be attached to functions during compilation
                // For now, we'll emit a no-op
                self.emit(Instruction::new(Opcode::Nop));
            }
        }
        
        Ok(())
    }
    
    fn compile_literal(&mut self, lit: &Literal) -> Result<()> {
        // Don't increment stack_depth here - emit will handle it
        match lit {
            Literal::Integer(n) => {
                match *n {
                    0 => { self.emit(Instruction::new(Opcode::PushInt0)); }
                    1 => { self.emit(Instruction::new(Opcode::PushInt1)); }
                    2 => { self.emit(Instruction::new(Opcode::PushInt2)); }
                    n if n >= 0 && n <= u32::MAX as i64 => {
                        self.emit(Instruction::with_arg(Opcode::PushIntSmall, n as u32));
                    }
                    _ => {
                        let idx = self.add_constant(Value::Int(*n));
                        self.emit(Instruction::with_arg(Opcode::Push, idx));
                    }
                }
            }
            Literal::Float(f) => {
                let idx = self.add_constant(Value::Float(*f));
                self.emit(Instruction::with_arg(Opcode::Push, idx));
            }
            Literal::String(s) => {
                let idx = self.add_constant(Value::String(s.clone()));
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
            if let Some(&abs_pos) = scope.get(name) {
                // The position stored is the absolute stack position
                // For the main function, positions are relative to stack base 0
                // For pattern bindings, they should already be correct
                let rel_pos = abs_pos;
                // Use fast local opcodes for indices 0-3
                match rel_pos {
                    0 => self.emit(Instruction::new(Opcode::LoadLocal0)),
                    1 => self.emit(Instruction::new(Opcode::LoadLocal1)),
                    2 => self.emit(Instruction::new(Opcode::LoadLocal2)),
                    3 => self.emit(Instruction::new(Opcode::LoadLocal3)),
                    _ => self.emit(Instruction::with_arg(Opcode::Load, rel_pos as u32)),
                };
                
                // If this is a cell variable (from letrec), dereference it
                if self.cell_vars[scope_idx].contains(name) {
                    self.emit(Instruction::new(Opcode::CellGet));
                }
                
                return Ok(());
            }
        }
        
        // Look up in captured variables
        for (_scope_idx, scope) in self.captured.iter().enumerate().rev() {
            if let Some(&capture_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(Opcode::LoadCaptured, capture_idx as u32));
                
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
    
    fn compile_application(&mut self, graph: &ASTGraph, func: NodeId, args: &[NodeId]) -> Result<()> {
        // Check if it's a built-in function
        if let Some(node) = graph.nodes.get(&func) {
            if let Node::Variable { name } = node {
                if let Some(opcode) = self.builtin_to_opcode(name) {
                    // For built-in arithmetic/comparison ops, compile args and emit opcode
                    match opcode {
                        // Binary operators
                        Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod |
                        Opcode::Eq | Opcode::Ne | Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge |
                        Opcode::And | Opcode::Or | Opcode::StrConcat | Opcode::ListCons => {
                            if args.len() != 2 {
                                return Err(anyhow!("{} requires exactly 2 arguments", name));
                            }
                            self.compile_node(graph, args[0])?;
                            self.compile_node(graph, args[1])?;
                            self.emit(Instruction::new(opcode));
                            return Ok(());
                        }
                        // Unary operators
                        Opcode::Not | Opcode::ListLen | Opcode::ListEmpty | 
                        Opcode::StrLen | Opcode::StrUpper | Opcode::StrLower |
                        Opcode::ListHead | Opcode::ListTail => {
                            if args.len() != 1 {
                                return Err(anyhow!("{} requires exactly 1 argument", name));
                            }
                            self.compile_node(graph, args[0])?;
                            self.emit(Instruction::new(opcode));
                            return Ok(());
                        }
                        // Variable-arity functions
                        Opcode::MakeList => {
                            // Compile all arguments
                            for &arg in args {
                                self.compile_node(graph, arg)?;
                            }
                            self.emit(Instruction::with_arg(Opcode::MakeList, args.len() as u32));
                            return Ok(());
                        }
                        _ => {
                            // Other opcodes might need special handling
                        }
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
                    let bindings_node = graph.nodes.get(&args[0])
                        .ok_or_else(|| anyhow!("Invalid bindings node in gc:let"))?;
                    
                    // Extract bindings
                    let bindings = if let Node::List(binding_nodes) = bindings_node {
                        let mut result = Vec::new();
                        for &binding_id in binding_nodes {
                            if let Some(Node::List(pair)) = graph.nodes.get(&binding_id) {
                                if pair.len() == 2 {
                                    if let Some(Node::Variable { name }) = graph.nodes.get(&pair[0]) {
                                        result.push((name.clone(), pair[1]));
                                    } else {
                                        return Err(anyhow!("gc:let binding must have variable name"));
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
                        self.stack_depth += 1;
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
        let is_tail_call = self.in_tail_position && 
            self.current_function.is_some() &&
            if let Some(Node::Variable { name }) = graph.nodes.get(&func) {
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
            eprintln!("DEBUG compile_application: tail call detected, stack_depth={}, arg_count={}", self.stack_depth, args.len());
            self.emit(Instruction::with_arg(Opcode::TailCall, args.len() as u32));
        } else {
            eprintln!("DEBUG compile_application: before Call, stack_depth={}, arg_count={}", self.stack_depth, args.len());
            self.emit(Instruction::with_arg(Opcode::Call, args.len() as u32));
        }
        
        Ok(())
    }
    
    fn compile_lambda(&mut self, graph: &ASTGraph, params: &[String], body: NodeId) -> Result<()> {
        // Find free variables - variables used in body but not defined as parameters
        let free_vars = self.find_free_variables(graph, body, params)?;
        eprintln!("DEBUG compile_lambda: found {} free vars: {:?}", free_vars.len(), free_vars);
        
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
        let saved_tail = self.in_tail_position;
        
        // Switch to lambda chunk
        self.current_chunk = chunk_id;
        self.locals = vec![HashMap::new()];
        self.captured = vec![HashMap::new()];
        self.stack_depth = 0; // Lambda starts with fresh stack
        self.scope_bases = vec![0];
        self.cell_vars = vec![HashSet::new()];
        
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
        let saved_tail = self.in_tail_position;
        self.in_tail_position = true;
        self.compile_node(graph, body)?;
        self.in_tail_position = saved_tail;
        self.emit(Instruction::new(Opcode::Return));
        
        // Restore context
        self.current_chunk = saved_chunk;
        self.locals = saved_locals;
        self.captured = saved_captured;
        self.stack_depth = saved_stack_depth;
        self.scope_bases = saved_scope_bases;
        self.cell_vars = saved_cell_vars;
        self.current_function = saved_function;
        self.in_tail_position = saved_tail;
        
        // Push function value with captures
        if free_vars.is_empty() {
            self.emit(Instruction::with_arg(Opcode::MakeFunc, chunk_id as u32));
        } else {
            // Pack chunk_id and capture count into arg
            // Upper 16 bits: chunk_id, Lower 16 bits: capture count
            let packed = ((chunk_id as u32) << 16) | (free_vars.len() as u32);
            self.emit(Instruction::with_arg(Opcode::MakeClosure, packed));
        }
        
        Ok(())
    }
    
    fn compile_let(&mut self, graph: &ASTGraph, bindings: &[(String, NodeId)], body: NodeId) -> Result<()> {
        // Create new scope
        self.locals.push(HashMap::new());
        self.captured.push(HashMap::new());
        self.scope_bases.push(self.stack_depth);
        self.cell_vars.push(HashSet::new());
        let scope_idx = self.locals.len() - 1;
        eprintln!("DEBUG compile_let: new scope {} with base {}", scope_idx, self.stack_depth);
        
        // Compile bindings
        for (_i, (name, value)) in bindings.iter().enumerate() {
            eprintln!("DEBUG: Before compiling {}: stack_depth={}", name, self.stack_depth);
            let before_depth = self.stack_depth;
            self.compile_node(graph, *value)?;
            eprintln!("DEBUG: After compiling {}: stack_depth={}", name, self.stack_depth);
            
            // After compiling the value, it should be on top of the stack
            // We expect exactly one value to be added
            if self.stack_depth != before_depth + 1 {
                eprintln!("WARNING: Expected stack_depth {} but got {}", before_depth + 1, self.stack_depth);
            }
            
            // The value is at position stack_depth - 1
            let abs_pos = self.stack_depth - 1;
            self.locals[scope_idx].insert(name.clone(), abs_pos);
            eprintln!("DEBUG: Stored {} at position {}", name, abs_pos);
        }
        
        // Compile body (preserving tail position - let body is in tail position)
        self.compile_node(graph, body)?;
        
        // Clean up bindings while preserving the result
        if !bindings.is_empty() {
            self.emit(Instruction::with_arg(Opcode::PopN, bindings.len() as u32));
            // PopN already adjusts stack_depth in emit()
        }
        
        // Pop scope
        self.locals.pop();
        self.captured.pop();
        self.scope_bases.pop();
        self.cell_vars.pop();
        
        Ok(())
    }
    
    fn compile_letrec(&mut self, graph: &ASTGraph, bindings: &[(String, NodeId)], body: NodeId) -> Result<()> {
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
        for name in &binding_names {
            self.emit(Instruction::new(Opcode::PushNil));
            self.emit(Instruction::new(Opcode::MakeCell));
            // Cell is now on stack
            let pos = self.stack_depth - 1;
            self.locals[scope_idx].insert(name.clone(), pos);
            self.cell_vars[scope_idx].insert(name.clone());
        }
        
        // Step 2: Compile binding values and store in cells
        for (_i, (name, value)) in bindings.iter().enumerate() {
            // Load the cell - need to look up actual position from locals
            let cell_pos = self.locals[scope_idx].get(name)
                .ok_or_else(|| anyhow!("Cell not found for binding: {}", name))?;
            // Load uses frame-relative indexing, so we use the stored position directly
            self.emit(Instruction::with_arg(Opcode::Load, *cell_pos as u32));
            
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
    
    fn compile_if(&mut self, graph: &ASTGraph, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> Result<()> {
        // Compile condition (not in tail position)
        let saved_tail = self.in_tail_position;
        self.in_tail_position = false;
        self.compile_node(graph, condition)?;
        self.in_tail_position = saved_tail;
        
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
            "car" | "head" | "first" => Some(Opcode::ListHead),
            "cdr" | "tail" | "rest" => Some(Opcode::ListTail),
            "cons" => Some(Opcode::ListCons),
            "str-len" | "string-length" => Some(Opcode::StrLen),
            "str-concat" | "string-append" => Some(Opcode::StrConcat),
            "str-upper" | "string-upcase" => Some(Opcode::StrUpper),
            "str-lower" | "string-downcase" => Some(Opcode::StrLower),
            "list" => Some(Opcode::MakeList),
            "gc-alloc" => Some(Opcode::GcAlloc),
            "gc-deref" => Some(Opcode::GcDeref),
            "gc-set" => Some(Opcode::GcSet),
            "gc-collect" => Some(Opcode::GcCollect),
            "tail-call" => Some(Opcode::TailCall),
            "tail-return" => Some(Opcode::TailReturn),
            // Specialized arithmetic
            "+int" => Some(Opcode::AddInt),
            "-int" => Some(Opcode::SubInt),
            "*int" => Some(Opcode::MulInt),
            "/int" => Some(Opcode::DivInt),
            "+float" => Some(Opcode::AddFloat),
            "-float" => Some(Opcode::SubFloat),
            "*float" => Some(Opcode::MulFloat),
            _ => None,
        }
    }
    
    fn emit(&mut self, instruction: Instruction) -> usize {
        // Adjust stack depth based on instruction
        match instruction.opcode {
            Opcode::Pop => self.stack_depth = self.stack_depth.saturating_sub(1),
            Opcode::PopN => {
                if instruction.arg > 0 {
                    self.stack_depth = self.stack_depth.saturating_sub(instruction.arg as usize - 1); // PopN keeps top value
                }
            }
            Opcode::Dup => self.stack_depth += 1,
            Opcode::Add | Opcode::Sub | Opcode::Mul | Opcode::Div | Opcode::Mod |
            Opcode::Eq | Opcode::Ne | Opcode::Lt | Opcode::Le | Opcode::Gt | Opcode::Ge |
            Opcode::And | Opcode::Or | Opcode::StrConcat | Opcode::ListCons => self.stack_depth = self.stack_depth.saturating_sub(1), // Binary ops consume 2, produce 1
            Opcode::MakeList => self.stack_depth = self.stack_depth.saturating_sub(instruction.arg as usize).saturating_add(1),
            Opcode::Call => {
                // Call pops: the function + all arguments, then pushes the result
                // Net effect: -(arg_count + 1) + 1 = -arg_count
                self.stack_depth = self.stack_depth.saturating_sub(instruction.arg as usize);
                eprintln!("DEBUG emit Call: arg_count={}, new stack_depth={}", instruction.arg, self.stack_depth);
            }
            Opcode::MakeClosure => {
                // MakeClosure consumes N captured values and produces 1 function
                let capture_count = (instruction.arg & 0xFFFF) as usize;
                let old_depth = self.stack_depth;
                self.stack_depth = self.stack_depth.saturating_sub(capture_count).saturating_add(1);
                eprintln!("DEBUG emit MakeClosure: captures={}, stack_depth {} -> {}", capture_count, old_depth, self.stack_depth);
            },
            Opcode::MakeFunc => {
                self.stack_depth += 1;
            }
            Opcode::Load | Opcode::LoadGlobal | Opcode::LoadCaptured => {
                self.stack_depth += 1;
                eprintln!("DEBUG emit {:?}: stack_depth -> {}", instruction.opcode, self.stack_depth);
            }
            Opcode::MakeCell => {
                // Consumes initial value, produces cell
                // No net stack change, but we still have a value on stack
            }
            Opcode::CellGet => {
                // Consumes cell, produces value
                // No net stack change
            }
            Opcode::CellSet => {
                // Consumes cell and value, produces nil
                self.stack_depth = self.stack_depth.saturating_sub(1);
            }
            // GC operations
            Opcode::GcAlloc => {
                // Consumes value, produces GC handle
                // No net stack change
            }
            Opcode::GcDeref => {
                // Consumes GC handle, produces value
                // No net stack change
            }
            Opcode::GcSet => {
                // Consumes GC handle and value, produces nil
                self.stack_depth = self.stack_depth.saturating_sub(1);
            }
            Opcode::GcCollect => {
                // No arguments, produces nil
                self.stack_depth += 1;
            }
            // Tail call operations
            Opcode::TailCall => {
                // Consumes function and arguments
                let arg_count = instruction.arg + 1; // +1 for the function itself
                self.stack_depth = self.stack_depth.saturating_sub(arg_count as usize);
                self.stack_depth += 1; // Result
            }
            Opcode::TailReturn => {
                // Like Return
                self.stack_depth = self.stack_depth.saturating_sub(1);
            }
            Opcode::UpdateLocal => {
                // Consumes value, no net change
                self.stack_depth = self.stack_depth.saturating_sub(1);
            }
            Opcode::LoopStart | Opcode::LoopEnd => {
                // No stack effect
            }
            // Push instructions
            Opcode::Push | Opcode::PushConst | Opcode::PushNil | Opcode::PushTrue | Opcode::PushFalse |
            Opcode::PushInt0 | Opcode::PushInt1 | Opcode::PushInt2 | Opcode::PushIntSmall => {
                self.stack_depth += 1;
            }
            // List operations
            Opcode::ListHead | Opcode::ListTail => {
                // Consumes list, produces head/tail
                // No net stack change
            }
            Opcode::ListEmpty => {
                // Consumes list, produces bool
                // No net stack change
            }
            Opcode::ListLen => {
                // Consumes list, produces int
                // No net stack change
            }
            Opcode::Swap => {
                // Swaps top two values, no net change
            }
            _ => {} // Most instructions don't change stack depth
        }
        self.bytecode.chunks[self.current_chunk].add_instruction(instruction)
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
    
    fn compile_effect(&mut self, graph: &ASTGraph, effect_type: fluentai_core::ast::EffectType, operation: &str, args: &[NodeId]) -> Result<()> {
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
        // For now, async just executes the body
        // In a full implementation, this would create an async context
        self.compile_node(graph, body)?;
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
        // Compile the expression (should be a function)
        self.compile_node(graph, expr)?;
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
    
    fn find_free_variables(&self, graph: &ASTGraph, node_id: NodeId, params: &[String]) -> Result<Vec<String>> {
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
    
    fn collect_free_variables(&self, graph: &ASTGraph, node_id: NodeId, free_vars: &mut HashSet<String>, bound_vars: &mut HashSet<String>) -> Result<()> {
        let node = graph.nodes.get(&node_id)
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
            Node::If { condition, then_branch, else_branch } => {
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
            _ => {} // Literals, Channel, etc. have no variables
        }
        
        Ok(())
    }
    
    fn compile_captured_variable(&mut self, name: &str) -> Result<()> {
        // Find the variable in outer scopes and emit code to load it
        for (_scope_idx, scope) in self.locals.iter().enumerate().rev() {
            if let Some(&local_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(Opcode::Load, local_idx as u32));
                // Don't dereference cells when capturing - we want to capture the cell itself
                // The dereference will happen when the variable is used
                return Ok(());
            }
        }
        
        // Check if it's already captured (for nested lambdas)
        for (_scope_idx, scope) in self.captured.iter().enumerate().rev() {
            if let Some(&capture_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(Opcode::LoadCaptured, capture_idx as u32));
                return Ok(());
            }
        }
        
        // If not found in locals or captured, it might be a global
        let idx = self.add_constant(Value::String(name.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadGlobal, idx));
        Ok(())
    }
    
    fn compile_match(&mut self, graph: &ASTGraph, expr: NodeId, branches: &[(Pattern, NodeId)]) -> Result<()> {
        
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
            let (bindings, always_matches) = self.compile_pattern_test(pattern)?;
            
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
                name == "cons" && patterns.len() == 2 && 
                matches!(&patterns[0], Pattern::Variable(_)) &&
                matches!(&patterns[1], Pattern::Variable(_))
            } else {
                false
            };
            
            // Create new scope for pattern bindings
            if !bindings.is_empty() {
                if is_cons_pattern {
                    // Special handling for cons pattern
                    // The matched value (list) is on top of stack at position stack_depth-1
                    
                    // Extract head (this consumes the list and produces head)
                    self.emit(Instruction::new(Opcode::Dup)); // Duplicate the list
                    self.emit(Instruction::new(Opcode::ListHead)); // Get head
                    
                    // Extract tail (need the list again)
                    self.emit(Instruction::new(Opcode::Swap)); // Swap head and original list
                    self.emit(Instruction::new(Opcode::ListTail)); // Get tail
                    
                    // Stack now has [head, tail] where the list used to be
                    
                    // Set up scope for bindings
                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    
                    // After extraction, head and tail are the top 2 values on the stack
                    // But we need to know their positions relative to the runtime stack
                    // When the match started, the list was the only thing we added to the stack
                    // So at runtime, head will be at position 0 and tail at position 1
                    
                    self.scope_bases.push(0); // Frame starts at 0
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;
                    
                    // Bind head and tail with runtime positions
                    // At runtime, after extracting from the list:
                    // - position 0: head (first item from list)
                    // - position 1: tail (rest of list)
                    if let Pattern::Constructor { patterns, .. } = pattern {
                        if let Pattern::Variable(head_name) = &patterns[0] {
                            self.locals[scope_idx].insert(head_name.clone(), 0);
                        }
                        if let Pattern::Variable(tail_name) = &patterns[1] {
                            self.locals[scope_idx].insert(tail_name.clone(), 1);
                        }
                    }
                } else {
                    // Regular pattern binding
                    self.locals.push(HashMap::new());
                    self.captured.push(HashMap::new());
                    eprintln!("DEBUG compile_match: creating scope with stack_depth={}", self.stack_depth);
                    self.scope_bases.push(self.stack_depth);
                    self.cell_vars.push(HashSet::new());
                    let scope_idx = self.locals.len() - 1;
                    
                    // Add pattern bindings
                    for (name, pos) in &bindings {
                        eprintln!("DEBUG compile_match: binding {} to position {}", name, pos);
                        self.locals[scope_idx].insert(name.clone(), *pos);
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
            if !is_last || jump_to_next.is_some() {
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
            matches!(pattern, Pattern::Wildcard | Pattern::Variable(_))
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
    
    fn compile_pattern_test(&mut self, pattern: &Pattern) -> Result<(Vec<(String, usize)>, bool)> {
        
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
                if name == "cons" && patterns.len() == 2 && 
                         matches!(&patterns[0], Pattern::Variable(_)) &&
                         matches!(&patterns[1], Pattern::Variable(_)) {
                    // Special case: cons with two variable patterns
                    // First check if the list is non-empty
                    self.emit(Instruction::new(Opcode::Dup)); // [..., list, list]
                    self.emit(Instruction::new(Opcode::ListEmpty)); // [..., list, bool]
                    self.emit(Instruction::new(Opcode::Not)); // [..., list, !bool]
                    
                    // This is handled differently in compile_match
                    if let (Pattern::Variable(head_name), Pattern::Variable(tail_name)) = 
                        (&patterns[0], &patterns[1]) {
                        // Just return the bindings, compile_match will handle extraction
                        bindings.push((head_name.clone(), 0)); // placeholder positions
                        bindings.push((tail_name.clone(), 1));
                    }
                    Ok((bindings, false)) // false = test was generated
                } else if name == "cons" && patterns.len() == 2 {
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
                } else if name == "nil" && patterns.is_empty() {
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
                        let (sub_bindings, _) = self.compile_pattern_test(sub_pattern)?;
                        bindings.extend(sub_bindings);
                        
                        // If sub-pattern failed, clean up stack and fail
                        let _sub_fail_jump = self.bytecode.chunks[self.current_chunk].instructions.len();
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
                        self.bytecode.chunks[self.current_chunk].patch_jump(sub_fail_jump, fail_target);
                    }
                    
                    // Failure case: push false
                    self.emit(Instruction::new(Opcode::PushFalse));
                    
                    // Patch success jump
                    let end_target = self.bytecode.chunks[self.current_chunk].instructions.len();
                    self.bytecode.chunks[self.current_chunk].patch_jump(success_jump, end_target);
                    
                    Ok((bindings, false))
                }
            }
        }
    }
    
    // Module compilation methods
    fn compile_module(&mut self, graph: &ASTGraph, name: &str, exports: &[String], body: NodeId) -> Result<()> {
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
    
    fn compile_import(&mut self, module_path: &str, import_list: &[fluentai_core::ast::ImportItem], import_all: bool) -> Result<()> {
        // Load the module
        let module_idx = self.add_constant(Value::String(module_path.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadModule, module_idx));
        
        if import_all {
            // TODO: Implement import * functionality
            // This would require runtime support to enumerate all exports
            return Err(anyhow!("Import * not yet implemented"));
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
}