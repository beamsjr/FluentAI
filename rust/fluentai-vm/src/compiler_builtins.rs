//! Built-in function compilation handlers

use fluentai_bytecode::{Instruction, Opcode};
use crate::compiler::Compiler;
use anyhow::{anyhow, Result};
use fluentai_core::ast::{Graph as ASTGraph, NodeId};

/// Result of compiling a built-in function
pub enum BuiltinResult {
    /// Built-in was handled, skip normal function compilation
    Handled,
    /// Not a built-in, continue with normal compilation
    NotBuiltin,
}

impl Compiler {
    /// Try to compile as a built-in function
    pub(crate) fn try_compile_builtin(
        &mut self,
        graph: &ASTGraph,
        func_name: &str,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        match func_name {
            // Arithmetic operators
            "+" => self.compile_variadic_op(graph, args, Opcode::Add, 2),
            "-" => self.compile_binary_op(graph, args, Opcode::Sub),
            "*" => self.compile_variadic_op(graph, args, Opcode::Mul, 2),
            "/" => self.compile_binary_op(graph, args, Opcode::Div),
            "%" => self.compile_binary_op(graph, args, Opcode::Mod),
            
            // Specialized integer arithmetic
            "+int" => self.compile_binary_op(graph, args, Opcode::AddInt),
            "-int" => self.compile_binary_op(graph, args, Opcode::SubInt),
            "*int" => self.compile_binary_op(graph, args, Opcode::MulInt),
            "/int" => self.compile_binary_op(graph, args, Opcode::DivInt),
            
            // Specialized float arithmetic
            "+float" => self.compile_binary_op(graph, args, Opcode::AddFloat),
            "-float" => self.compile_binary_op(graph, args, Opcode::SubFloat),
            "*float" => self.compile_binary_op(graph, args, Opcode::MulFloat),
            "/float" => self.compile_binary_op(graph, args, Opcode::DivFloat),
            
            // Comparison operators
            "=" | "==" => self.compile_binary_op(graph, args, Opcode::Eq),
            "!=" | "<>" => self.compile_binary_op(graph, args, Opcode::Ne),
            "<" => self.compile_binary_op(graph, args, Opcode::Lt),
            ">" => self.compile_binary_op(graph, args, Opcode::Gt),
            "<=" => self.compile_binary_op(graph, args, Opcode::Le),
            ">=" => self.compile_binary_op(graph, args, Opcode::Ge),
            
            // List operations
            "cons" | "list-cons" => self.compile_binary_op(graph, args, Opcode::ListCons),
            "car" | "head" | "first" => self.compile_unary_op(graph, args, Opcode::ListHead),
            "cdr" | "tail" | "rest" => self.compile_unary_op(graph, args, Opcode::ListTail),
            "list" => self.compile_list_builtin(graph, args),
            "length" | "list-len" => self.compile_unary_op(graph, args, Opcode::ListLen),
            "empty?" | "list-empty?" => self.compile_unary_op(graph, args, Opcode::ListEmpty),
            
            // String operations
            "str-len" | "string-length" => self.compile_unary_op(graph, args, Opcode::StrLen),
            "str-concat" | "string-append" => self.compile_binary_op(graph, args, Opcode::StrConcat),
            "str-upper" | "string-upcase" => self.compile_unary_op(graph, args, Opcode::StrUpper),
            "str-lower" | "string-downcase" => self.compile_unary_op(graph, args, Opcode::StrLower),
            
            // Logical operators
            "and" => self.compile_variadic_op(graph, args, Opcode::And, 2),
            "or" => self.compile_variadic_op(graph, args, Opcode::Or, 2),
            "not" => self.compile_unary_op(graph, args, Opcode::Not),
            
            // GC operations
            "gc-alloc" => self.compile_unary_op(graph, args, Opcode::GcAlloc),
            "gc-deref" => self.compile_unary_op(graph, args, Opcode::GcDeref),
            "gc-set" => self.compile_binary_op(graph, args, Opcode::GcSet),
            "gc-collect" => self.compile_nullary_op(graph, args, Opcode::GcCollect),
            
            // Tail call operations (these might need special handling)
            "tail-call" => Ok(BuiltinResult::NotBuiltin),
            "tail-return" => Ok(BuiltinResult::NotBuiltin),
            
            // Special forms - handled elsewhere
            "gc:let" => Ok(BuiltinResult::NotBuiltin),
            
            // Actor operations
            "CreateActor" => self.compile_create_actor(graph, args),
            "ActorSend" => self.compile_actor_send(graph, args),
            "ActorReceive" => self.compile_actor_receive(graph, args),
            "Become" => self.compile_become(graph, args),
            
            // Channel operations
            "Channel" => self.compile_nullary_op(graph, args, Opcode::Channel),
            "ChannelWithCapacity" => self.compile_unary_op(graph, args, Opcode::ChannelWithCapacity),
            "Send" => self.compile_binary_op(graph, args, Opcode::Send),
            "Receive" => self.compile_unary_op(graph, args, Opcode::Receive),
            
            // Async operations
            "Spawn" => self.compile_unary_op(graph, args, Opcode::Spawn),
            "Await" => self.compile_unary_op(graph, args, Opcode::Await),
            
            _ => Ok(BuiltinResult::NotBuiltin),
        }
    }
    
    fn compile_variadic_op(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
        opcode: Opcode,
        min_args: usize,
    ) -> Result<BuiltinResult> {
        if args.len() < min_args {
            return Err(anyhow!("{:?} requires at least {} arguments", opcode, min_args));
        }
        
        // Compile first argument
        self.compile_node(graph, args[0])?;
        
        // Chain operations for remaining arguments
        for &arg in &args[1..] {
            self.compile_node(graph, arg)?;
            self.emit(Instruction::new(opcode));
        }
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_binary_op(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
        opcode: Opcode,
    ) -> Result<BuiltinResult> {
        if args.len() != 2 {
            return Err(anyhow!("{:?} requires exactly 2 arguments", opcode));
        }
        
        self.compile_node(graph, args[0])?;
        self.compile_node(graph, args[1])?;
        self.emit(Instruction::new(opcode));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_unary_op(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
        opcode: Opcode,
    ) -> Result<BuiltinResult> {
        if args.len() != 1 {
            return Err(anyhow!("{:?} requires exactly 1 argument", opcode));
        }
        
        self.compile_node(graph, args[0])?;
        self.emit(Instruction::new(opcode));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_nullary_op(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
        opcode: Opcode,
    ) -> Result<BuiltinResult> {
        if !args.is_empty() {
            return Err(anyhow!("{:?} takes no arguments", opcode));
        }
        
        self.emit(Instruction::new(opcode));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_list_builtin(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        // Compile all arguments
        for &arg in args {
            self.compile_node(graph, arg)?;
        }
        self.emit(Instruction::with_arg(Opcode::MakeList, args.len() as u32));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_create_actor(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        if args.len() != 2 {
            return Err(anyhow!("CreateActor requires exactly 2 arguments (initial_state, handler)"));
        }
        
        // Compile initial state
        self.compile_node(graph, args[0])?;
        
        // Compile handler function
        self.compile_node(graph, args[1])?;
        
        // Emit CreateActor instruction
        self.emit(Instruction::new(Opcode::CreateActor));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_actor_send(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        if args.len() != 2 {
            return Err(anyhow!("ActorSend requires exactly 2 arguments (actor, message)"));
        }
        
        // Compile actor
        self.compile_node(graph, args[0])?;
        
        // Compile message
        self.compile_node(graph, args[1])?;
        
        // Emit ActorSend instruction
        self.emit(Instruction::new(Opcode::ActorSend));
        
        Ok(BuiltinResult::Handled)
    }
    
    fn compile_actor_receive(
        &mut self,
        _graph: &ASTGraph,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        if args.len() < 1 {
            return Err(anyhow!("ActorReceive requires at least 1 argument"));
        }
        
        // TODO: Implement proper actor receive compilation
        // For now, just compile it as a regular function call
        Ok(BuiltinResult::NotBuiltin)
    }
    
    fn compile_become(
        &mut self,
        graph: &ASTGraph,
        args: &[NodeId],
    ) -> Result<BuiltinResult> {
        if args.len() != 1 {
            return Err(anyhow!("Become requires exactly 1 argument (new_behavior)"));
        }
        
        // Compile new behavior
        self.compile_node(graph, args[0])?;
        
        // Emit Become instruction
        self.emit(Instruction::new(Opcode::Become));
        
        Ok(BuiltinResult::Handled)
    }
}