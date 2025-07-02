//! Compiler from AST to bytecode

use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode, Value};
use claudelang_core::ast::{Graph as ASTGraph, Node, NodeId, Literal};
use anyhow::{anyhow, Result};
use std::collections::{HashMap, HashSet};

pub struct Compiler {
    bytecode: Bytecode,
    current_chunk: usize,
    locals: Vec<HashMap<String, usize>>,
    captured: Vec<HashMap<String, usize>>, // Captured variables per scope
    stack_depth: usize, // Track current stack depth
    scope_bases: Vec<usize>, // Base stack position for each scope
}

impl Compiler {
    pub fn new() -> Self {
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
        }
    }
    
    pub fn compile(mut self, graph: &ASTGraph) -> Result<Bytecode> {
        let root_id = graph.root_id
            .ok_or_else(|| anyhow!("AST graph has no root node"))?;
        
        self.compile_node(graph, root_id)?;
        
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
            _ => return Err(anyhow!("Unimplemented node type: {:?}", node)),
        }
        
        Ok(())
    }
    
    fn compile_literal(&mut self, lit: &Literal) -> Result<()> {
        self.stack_depth += 1;
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
        // Built-in functions are valid values in ClaudeLang (first-class functions)
        // They will be handled specially when applied
        
        // Look up in locals
        for (_scope_idx, scope) in self.locals.iter().enumerate().rev() {
            if let Some(&local_idx) = scope.get(name) {
                // For now, using Load with local index
                // TODO: Implement proper local variable opcodes
                self.emit(Instruction::with_arg(Opcode::Load, local_idx as u32));
                return Ok(());
            }
        }
        
        // Look up in captured variables
        for (_scope_idx, scope) in self.captured.iter().enumerate().rev() {
            if let Some(&capture_idx) = scope.get(name) {
                self.emit(Instruction::with_arg(Opcode::LoadCaptured, capture_idx as u32));
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
                        Opcode::And | Opcode::Or | Opcode::StrConcat => {
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
                        Opcode::StrLen | Opcode::StrUpper | Opcode::StrLower => {
                            if args.len() != 1 {
                                return Err(anyhow!("{} requires exactly 1 argument", name));
                            }
                            self.compile_node(graph, args[0])?;
                            self.emit(Instruction::new(opcode));
                            return Ok(());
                        }
                        _ => {
                            // Other opcodes might need special handling
                        }
                    }
                }
            }
        }
        
        // Regular function call
        for &arg in args {
            self.compile_node(graph, arg)?;
        }
        self.compile_node(graph, func)?;
        self.emit(Instruction::with_arg(Opcode::Call, args.len() as u32));
        
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
        
        // Switch to lambda chunk
        self.current_chunk = chunk_id;
        self.locals = vec![HashMap::new()];
        self.captured = vec![HashMap::new()];
        self.stack_depth = 0; // Lambda starts with fresh stack
        self.scope_bases = vec![0];
        
        // Add parameters to locals
        for (i, param) in params.iter().enumerate() {
            self.locals[0].insert(param.clone(), i);
        }
        
        // Add captured variables to captured map
        for (i, var) in free_vars.iter().enumerate() {
            self.captured[0].insert(var.clone(), i);
        }
        
        // Compile body
        self.compile_node(graph, body)?;
        self.emit(Instruction::new(Opcode::Return));
        
        // Restore context
        self.current_chunk = saved_chunk;
        self.locals = saved_locals;
        self.captured = saved_captured;
        self.stack_depth = saved_stack_depth;
        self.scope_bases = saved_scope_bases;
        
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
        let scope_idx = self.locals.len() - 1;
        
        // Compile bindings
        for (i, (name, value)) in bindings.iter().enumerate() {
            self.compile_node(graph, *value)?;
            // Store absolute position on stack
            let abs_pos = self.scope_bases[scope_idx] + i;
            self.locals[scope_idx].insert(name.clone(), abs_pos);
            self.stack_depth += 1;
        }
        
        // Compile body
        self.compile_node(graph, body)?;
        
        // Clean up bindings while preserving the result
        if !bindings.is_empty() {
            self.emit(Instruction::with_arg(Opcode::PopN, bindings.len() as u32));
            self.stack_depth -= bindings.len();
        }
        
        // Pop scope
        self.locals.pop();
        self.captured.pop();
        self.scope_bases.pop();
        
        Ok(())
    }
    
    fn compile_if(&mut self, graph: &ASTGraph, condition: NodeId, then_branch: NodeId, else_branch: NodeId) -> Result<()> {
        // Compile condition
        self.compile_node(graph, condition)?;
        
        // Jump to else if false
        let jump_to_else = self.emit(Instruction::with_arg(Opcode::JumpIfNot, 0));
        
        // Compile then branch
        self.compile_node(graph, then_branch)?;
        
        // Jump over else
        let jump_over_else = self.emit(Instruction::with_arg(Opcode::Jump, 0));
        
        // Patch jump to else
        let else_start = self.current_offset();
        self.patch_jump(jump_to_else, else_start);
        
        // Compile else branch
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
            "str-len" | "string-length" => Some(Opcode::StrLen),
            "str-concat" | "string-append" => Some(Opcode::StrConcat),
            "str-upper" | "string-upcase" => Some(Opcode::StrUpper),
            "str-lower" | "string-downcase" => Some(Opcode::StrLower),
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
            Opcode::And | Opcode::Or | Opcode::StrConcat => self.stack_depth = self.stack_depth.saturating_sub(1), // Binary ops consume 2, produce 1
            Opcode::MakeList => self.stack_depth = self.stack_depth.saturating_sub(instruction.arg as usize).saturating_add(1),
            Opcode::Call => self.stack_depth = self.stack_depth.saturating_sub(instruction.arg as usize), // Pop args, push result
            Opcode::MakeClosure => {
                // MakeClosure consumes N captured values and produces 1 function
                let capture_count = (instruction.arg & 0xFFFF) as usize;
                self.stack_depth = self.stack_depth.saturating_sub(capture_count).saturating_add(1);
            },
            Opcode::MakeFunc => self.stack_depth += 1,
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
    
    fn compile_effect(&mut self, graph: &ASTGraph, effect_type: claudelang_core::ast::EffectType, operation: &str, args: &[NodeId]) -> Result<()> {
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
                return Ok(());
            }
        }
        
        // If not found in locals, it might be a global
        let idx = self.add_constant(Value::String(name.to_string()));
        self.emit(Instruction::with_arg(Opcode::LoadGlobal, idx));
        Ok(())
    }
}