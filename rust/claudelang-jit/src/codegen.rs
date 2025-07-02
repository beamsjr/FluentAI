//! Cranelift IR code generation for ClaudeLang bytecode

use cranelift::prelude::*;

use claudelang_vm::bytecode::{BytecodeChunk, Opcode, Value as ClValue};
use anyhow::{Result, anyhow};
use std::collections::HashMap;

/// Build a complete function from bytecode
pub fn build_function(
    func: &mut codegen::ir::Function,
    func_ctx: &mut FunctionBuilderContext,
    chunk: &BytecodeChunk,
) -> Result<()> {
    let mut builder = FunctionBuilder::new(func, func_ctx);
    
    // Create entry block
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);
    
    // Initialize code generator state
    let mut value_stack = Vec::new();
    let mut locals = HashMap::new();
    let mut blocks = HashMap::new();
    
    // Pre-scan for jump targets and create blocks
    for (_pc, instruction) in chunk.instructions.iter().enumerate() {
        match instruction.opcode {
            Opcode::Jump | Opcode::JumpIf | Opcode::JumpIfNot => {
                let target = instruction.arg as usize;
                if !blocks.contains_key(&target) {
                    let block = builder.create_block();
                    blocks.insert(target, block);
                }
            }
            _ => {}
        }
    }
    
    // Allocate locals
    let mut max_local = 0u32;
    for instruction in &chunk.instructions {
        match instruction.opcode {
            Opcode::LoadLocal0 | Opcode::StoreLocal0 => max_local = max_local.max(0),
            Opcode::LoadLocal1 | Opcode::StoreLocal1 => max_local = max_local.max(1),
            Opcode::LoadLocal2 | Opcode::StoreLocal2 => max_local = max_local.max(2),
            Opcode::LoadLocal3 | Opcode::StoreLocal3 => max_local = max_local.max(3),
            Opcode::Load | Opcode::Store => max_local = max_local.max(instruction.arg),
            _ => {}
        }
    }
    
    // Declare variables
    for i in 0..=max_local {
        let var = Variable::new(i as usize);
        builder.declare_var(var, types::I64);
        locals.insert(i, var);
        
        // Initialize to 0
        let zero = builder.ins().iconst(types::I64, 0);
        builder.def_var(var, zero);
    }
    
    // Compile each instruction
    let mut pc = 0;
    while pc < chunk.instructions.len() {
        // Switch to the correct block if this is a jump target
        if let Some(&block) = blocks.get(&pc) {
            if builder.current_block().is_none() {
                builder.switch_to_block(block);
            } else if builder.current_block() != Some(block) {
                // Jump to the block if we're not already there
                builder.ins().jump(block, &[]);
                builder.switch_to_block(block);
            }
            builder.seal_block(block);
        }
        
        let instruction = &chunk.instructions[pc];
        
        // Process instruction
        match instruction.opcode {
            // Stack operations
            Opcode::Push => {
                let constant = chunk.constants.get(instruction.arg as usize)
                    .ok_or_else(|| anyhow!("Invalid constant index"))?;
                let value = match constant {
                    ClValue::Int(n) => builder.ins().iconst(types::I64, *n),
                    ClValue::Float(f) => {
                        let bits = f.to_bits();
                        builder.ins().iconst(types::I64, bits as i64)
                    }
                    ClValue::Bool(b) => builder.ins().iconst(types::I64, if *b { 1 } else { 0 }),
                    ClValue::Nil => builder.ins().iconst(types::I64, 0),
                    _ => return Err(anyhow!("Unsupported constant type in JIT")),
                };
                value_stack.push(value);
            }
            
            Opcode::Pop => {
                value_stack.pop()
                    .ok_or_else(|| anyhow!("Stack underflow"))?;
            }
            
            Opcode::Dup => {
                let value = value_stack.last()
                    .ok_or_else(|| anyhow!("Stack underflow"))?
                    .clone();
                value_stack.push(value);
            }
            
            // Arithmetic operations
            Opcode::Add | Opcode::AddInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().iadd(left, right);
                value_stack.push(result);
            }
            
            Opcode::Sub | Opcode::SubInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().isub(left, right);
                value_stack.push(result);
            }
            
            Opcode::Mul | Opcode::MulInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().imul(left, right);
                value_stack.push(result);
            }
            
            Opcode::Div | Opcode::DivInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().sdiv(left, right);
                value_stack.push(result);
            }
            
            Opcode::Mod => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().srem(left, right);
                value_stack.push(result);
            }
            
            Opcode::Neg => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let zero = builder.ins().iconst(types::I64, 0);
                let result = builder.ins().isub(zero, value);
                value_stack.push(result);
            }
            
            // Comparison operations
            Opcode::Eq => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::Equal, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            Opcode::Ne => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::NotEqual, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            Opcode::Lt | Opcode::LtInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::SignedLessThan, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            Opcode::Le | Opcode::LeInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::SignedLessThanOrEqual, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            Opcode::Gt | Opcode::GtInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::SignedGreaterThan, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            Opcode::Ge | Opcode::GeInt => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, left, right);
                let extended = builder.ins().uextend(types::I64, result);
                value_stack.push(extended);
            }
            
            // Boolean operations
            Opcode::Not => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let one = builder.ins().iconst(types::I64, 1);
                let result = builder.ins().bxor(value, one);
                value_stack.push(result);
            }
            
            Opcode::And => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().band(left, right);
                value_stack.push(result);
            }
            
            Opcode::Or => {
                let right = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let left = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let result = builder.ins().bor(left, right);
                value_stack.push(result);
            }
            
            // Local variable operations
            Opcode::LoadLocal0 | Opcode::LoadLocal1 | Opcode::LoadLocal2 | Opcode::LoadLocal3 | Opcode::Load => {
                let index = match instruction.opcode {
                    Opcode::LoadLocal0 => 0,
                    Opcode::LoadLocal1 => 1,
                    Opcode::LoadLocal2 => 2,
                    Opcode::LoadLocal3 => 3,
                    Opcode::Load => instruction.arg,
                    _ => unreachable!(),
                };
                let var = locals.get(&index)
                    .ok_or_else(|| anyhow!("Invalid local index: {}", index))?;
                let value = builder.use_var(*var);
                value_stack.push(value);
            }
            
            Opcode::StoreLocal0 | Opcode::StoreLocal1 | Opcode::StoreLocal2 | Opcode::StoreLocal3 | Opcode::Store => {
                let index = match instruction.opcode {
                    Opcode::StoreLocal0 => 0,
                    Opcode::StoreLocal1 => 1,
                    Opcode::StoreLocal2 => 2,
                    Opcode::StoreLocal3 => 3,
                    Opcode::Store => instruction.arg,
                    _ => unreachable!(),
                };
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&index)
                    .ok_or_else(|| anyhow!("Invalid local index: {}", index))?;
                builder.def_var(*var, value);
            }
            
            // Specialized constants
            Opcode::PushInt0 => {
                let val = builder.ins().iconst(types::I64, 0);
                value_stack.push(val);
            }
            
            Opcode::PushInt1 => {
                let val = builder.ins().iconst(types::I64, 1);
                value_stack.push(val);
            }
            
            Opcode::PushInt2 => {
                let val = builder.ins().iconst(types::I64, 2);
                value_stack.push(val);
            }
            
            Opcode::PushIntSmall => {
                let val = builder.ins().iconst(types::I64, instruction.arg as i64);
                value_stack.push(val);
            }
            
            Opcode::PushTrue => {
                let val = builder.ins().iconst(types::I64, 1);
                value_stack.push(val);
            }
            
            Opcode::PushFalse => {
                let val = builder.ins().iconst(types::I64, 0);
                value_stack.push(val);
            }
            
            Opcode::PushNil => {
                let val = builder.ins().iconst(types::I64, 0);
                value_stack.push(val);
            }
            
            // Control flow
            Opcode::Jump => {
                let target = instruction.arg as usize;
                let block = blocks.get(&target)
                    .ok_or_else(|| anyhow!("Invalid jump target"))?;
                builder.ins().jump(*block, &[]);
            }
            
            Opcode::JumpIf => {
                let condition = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let target = instruction.arg as usize;
                let then_block = blocks.get(&target)
                    .ok_or_else(|| anyhow!("Invalid jump target"))?;
                let else_block = builder.create_block();
                
                // Branch if condition is non-zero
                builder.ins().brif(condition, *then_block, &[], else_block, &[]);
                
                builder.switch_to_block(else_block);
                builder.seal_block(else_block);
            }
            
            Opcode::JumpIfNot => {
                let condition = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let target = instruction.arg as usize;
                let then_block = blocks.get(&target)
                    .ok_or_else(|| anyhow!("Invalid jump target"))?;
                let else_block = builder.create_block();
                
                // Branch if condition is zero
                builder.ins().brif(condition, else_block, &[], *then_block, &[]);
                
                builder.switch_to_block(else_block);
                builder.seal_block(else_block);
            }
            
            Opcode::Return => {
                // Return is handled by the end of function
            }
            
            Opcode::Halt => {
                let zero = builder.ins().iconst(types::I64, 0);
                builder.ins().return_(&[zero]);
            }
            
            // TODO: Implement remaining opcodes
            _ => {
                // For now, skip unimplemented opcodes
            }
        }
        
        pc += 1;
    }
    
    // Return the top of stack or 0 if empty
    let result = if let Some(value) = value_stack.pop() {
        value
    } else {
        builder.ins().iconst(types::I64, 0)
    };
    
    builder.ins().return_(&[result]);
    
    // Finalize the function
    builder.finalize();
    
    Ok(())
}