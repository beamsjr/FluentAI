//! Main native code compiler

use anyhow::{anyhow, Result};
use cranelift::prelude::*;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::settings;
use cranelift_module::{Module, Linkage};
use cranelift_object::{ObjectBuilder, ObjectModule};
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use std::collections::HashMap;
use std::path::Path;

use crate::{CodegenOptions, value_abi::{ValueAbi, RuntimeFunctions}};
use crate::target::CompilationTarget;

/// Native code compiler
pub struct NativeCompiler {
    options: CodegenOptions,
    module: ObjectModule,
    target: CompilationTarget,
}

impl NativeCompiler {
    /// Create a new native compiler
    pub fn new(options: CodegenOptions) -> Result<Self> {
        let target = CompilationTarget::from_triple(&options.target)
            .map_err(|e| anyhow!("Failed to parse target triple: {}", e))?;
        
        // Create ISA
        let mut flag_builder = settings::builder();
        
        // Set optimization level
        let opt_level = match options.opt_level {
            0 => "none",
            1 => "speed",
            2 => "speed",
            3 => "speed_and_size",
            _ => "speed",
        };
        flag_builder.set("opt_level", opt_level)?;
        
        // Enable PIC if needed
        if target.pic {
            flag_builder.set("is_pic", "true")?;
        }
        
        let isa_builder = cranelift_native::builder()
            .map_err(|e| anyhow!("Failed to create ISA builder: {}", e))?;
        let isa = isa_builder.finish(settings::Flags::new(flag_builder))?;
        
        // Create object module
        let builder = ObjectBuilder::new(
            isa,
            "flc_module",
            cranelift_module::default_libcall_names(),
        )?;
        let module = ObjectModule::new(builder);
        
        Ok(Self {
            options,
            module,
            target,
        })
    }
    
    /// Compile an AST to native code
    pub fn compile(mut self, ast: &Graph, output_path: &Path) -> Result<()> {
        // Declare runtime functions
        RuntimeFunctions::declare_in_module(&mut self.module)?;
        
        // Compile main function
        self.compile_main(ast)?;
        
        // Compile all functions in the AST
        self.compile_functions(ast)?;
        
        // Finalize and write object file
        let object = self.module.finish();
        std::fs::write(output_path, object.emit()?)?;
        
        Ok(())
    }
    
    /// Compile multiple modules
    pub fn compile_modules(
        mut self,
        modules: Vec<(&str, &Graph)>,
        output_path: &Path,
    ) -> Result<()> {
        // Declare runtime functions
        RuntimeFunctions::declare_in_module(&mut self.module)?;
        
        // Compile each module
        for (name, ast) in modules {
            self.compile_module(name, ast)?;
        }
        
        // Finalize and write
        let object = self.module.finish();
        std::fs::write(output_path, object.emit()?)?;
        
        Ok(())
    }
    
    /// Compile the main entry point
    fn compile_main(&mut self, ast: &Graph) -> Result<()> {
        let mut sig = self.module.make_signature();
        sig.returns.push(AbiParam::new(types::I32)); // Return code
        
        let func_id = self.module.declare_function("main", Linkage::Export, &sig)?;
        
        let mut ctx = self.module.make_context();
        ctx.func.signature = sig;
        
        let mut func_ctx = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
        
        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);
        
        // Compile the main expression
        if let Some(root_id) = ast.root_id {
            let mut locals = HashMap::new();
            self.compile_node_with_builder(&mut builder, ast, root_id, &mut locals)?;
        }
        
        // Return 0
        let zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[zero]);
        
        builder.finalize();
        
        self.module.define_function(func_id, &mut ctx)?;
        
        Ok(())
    }
    
    /// Compile all function definitions in the AST
    fn compile_functions(&mut self, ast: &Graph) -> Result<()> {
        // TODO: Walk AST and compile each function definition
        Ok(())
    }
    
    /// Compile a module
    fn compile_module(&mut self, _name: &str, ast: &Graph) -> Result<()> {
        self.compile_functions(ast)
    }
    
    /// Compile a node with a given builder
    fn compile_node_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        ast: &Graph,
        node_id: NodeId,
        locals: &mut HashMap<String, Value>,
    ) -> Result<Value> {
        let node = ast.get_node(node_id)
            .ok_or_else(|| anyhow!("Node not found: {:?}", node_id))?;
        
        match node {
            Node::Literal(lit) => self.compile_literal_with_builder(builder, lit),
            Node::Variable { name } => locals.get(name)
                .copied()
                .ok_or_else(|| anyhow!("Undefined variable: {}", name)),
            Node::Application { function, args } => {
                self.compile_application_with_builder(builder, ast, *function, args, locals)
            }
            Node::Let { bindings, body } => {
                self.compile_let_with_builder(builder, ast, bindings, *body, locals)
            }
            Node::Begin { exprs } => {
                self.compile_block_with_builder(builder, ast, exprs, locals)
            }
            Node::If { condition, then_branch, else_branch } => {
                self.compile_if_with_builder(builder, ast, *condition, *then_branch, *else_branch, locals)
            }
            _ => Err(anyhow!("Unsupported node type: {:?}", node)),
        }
    }
    
    fn compile_literal_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        lit: &Literal,
    ) -> Result<Value> {
        match lit {
            Literal::Integer(n) => {
                let value = builder.ins().iconst(types::I64, *n);
                Ok(ValueAbi::make_int(builder, value))
            }
            Literal::Float(_f) => {
                Err(anyhow!("Float literals not yet implemented"))
            }
            Literal::Boolean(b) => {
                let value = builder.ins().iconst(types::I8, if *b { 1 } else { 0 });
                Ok(ValueAbi::make_bool(builder, value))
            }
            Literal::Nil => {
                Ok(ValueAbi::make_nil(builder))
            }
            Literal::String(_s) => {
                Err(anyhow!("String literals not yet implemented"))
            }
            Literal::Symbol(_s) => {
                Err(anyhow!("Symbol literals not yet implemented"))
            }
        }
    }
    
    fn compile_application_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        ast: &Graph,
        function: NodeId,
        args: &[NodeId],
        locals: &mut HashMap<String, Value>,
    ) -> Result<Value> {
        // Check if this is a primitive operation
        if let Some(Node::Variable { name }) = ast.get_node(function) {
            // Handle primitive operations
            match name.as_str() {
                "+" if args.len() == 2 => {
                    let left_val = self.compile_node_with_builder(builder, ast, args[0], locals)?;
                    let right_val = self.compile_node_with_builder(builder, ast, args[1], locals)?;
                    
                    let left_int = ValueAbi::get_payload(builder, left_val);
                    let right_int = ValueAbi::get_payload(builder, right_val);
                    let result = builder.ins().iadd(left_int, right_int);
                    Ok(ValueAbi::make_int(builder, result))
                }
                "-" if args.len() == 2 => {
                    let left_val = self.compile_node_with_builder(builder, ast, args[0], locals)?;
                    let right_val = self.compile_node_with_builder(builder, ast, args[1], locals)?;
                    
                    let left_int = ValueAbi::get_payload(builder, left_val);
                    let right_int = ValueAbi::get_payload(builder, right_val);
                    let result = builder.ins().isub(left_int, right_int);
                    Ok(ValueAbi::make_int(builder, result))
                }
                _ => Err(anyhow!("Unsupported function: {}", name)),
            }
        } else {
            Err(anyhow!("General function calls not yet implemented"))
        }
    }
    
    fn compile_let_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        ast: &Graph,
        bindings: &[(String, NodeId)],
        body: NodeId,
        locals: &mut HashMap<String, Value>,
    ) -> Result<Value> {
        // Compile each binding
        for (name, value_node) in bindings {
            let value_result = self.compile_node_with_builder(builder, ast, *value_node, locals)?;
            locals.insert(name.clone(), value_result);
        }
        
        // Compile the body with all bindings in scope
        self.compile_node_with_builder(builder, ast, body, locals)
    }
    
    fn compile_block_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        ast: &Graph,
        exprs: &[NodeId],
        locals: &mut HashMap<String, Value>,
    ) -> Result<Value> {
        let mut last = ValueAbi::make_nil(builder);
        for expr in exprs {
            last = self.compile_node_with_builder(builder, ast, *expr, locals)?;
        }
        Ok(last)
    }
    
    fn compile_if_with_builder(
        &self,
        builder: &mut FunctionBuilder,
        ast: &Graph,
        test: NodeId,
        then_branch: NodeId,
        else_branch: NodeId,
        locals: &mut HashMap<String, Value>,
    ) -> Result<Value> {
        let cond_val = self.compile_node_with_builder(builder, ast, test, locals)?;
        
        // Check if condition is truthy
        // For now, just check if it's not nil or false
        let tag = ValueAbi::get_tag(builder, cond_val);
        let nil_tag = builder.ins().iconst(types::I64, ValueAbi::TAG_NIL as i64);
        let is_nil = builder.ins().icmp(IntCC::Equal, tag, nil_tag);
        
        let then_block = builder.create_block();
        let else_block = builder.create_block();
        let merge_block = builder.create_block();
        
        builder.ins().brif(is_nil, else_block, &[], then_block, &[]);
        
        // Compile then branch
        builder.switch_to_block(then_block);
        builder.seal_block(then_block);
        let then_val = self.compile_node_with_builder(builder, ast, then_branch, locals)?;
        builder.ins().jump(merge_block, &[then_val]);
        
        // Compile else branch
        builder.switch_to_block(else_block);
        builder.seal_block(else_block);
        let else_val = self.compile_node_with_builder(builder, ast, else_branch, locals)?;
        builder.ins().jump(merge_block, &[else_val]);
        
        // Merge
        builder.switch_to_block(merge_block);
        builder.append_block_param(merge_block, types::I64);
        builder.seal_block(merge_block);
        
        Ok(builder.block_params(merge_block)[0])
    }
}

