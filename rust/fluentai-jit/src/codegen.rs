//! Cranelift IR code generation for FluentAi bytecode
//!
//! This module handles the translation of FluentAI bytecode instructions
//! into Cranelift's intermediate representation (IR). It manages the
//! compile-time stack, generates code for each opcode, and handles
//! the tagged value representation used by the JIT.

use cranelift::prelude::*;
use cranelift_codegen::ir::MemFlags;

use anyhow::{anyhow, Result};
use fluentai_bytecode::{BytecodeChunk, Opcode};
use fluentai_core::value::Value as ClValue;
use std::collections::HashMap;
use crate::value::{TaggedValue, value_to_tagged};

/// Represents a value on the JIT's compile-time stack, tracking both
/// the Cranelift value and its type.
#[derive(Clone, Debug)]
struct JitValue {
    val: Value,
    ty: Type,
}

/// Helper functions for tagged value operations
impl JitValue {
    /// Create a tagged integer from an untagged i64
    fn make_tagged_int(builder: &mut FunctionBuilder, n: Value) -> Self {
        // Shift left by 3 and set integer tag (0b000)
        let shifted = builder.ins().ishl_imm(n, 3);
        JitValue {
            val: shifted,
            ty: types::I64,
        }
    }
    
    /// Extract integer from tagged value
    fn untag_int(&self, builder: &mut FunctionBuilder) -> Value {
        // Shift right by 3 to get the integer value
        builder.ins().sshr_imm(self.val, 3)
    }
    
    /// Check if value has integer tag
    fn has_int_tag(&self, builder: &mut FunctionBuilder) -> Value {
        let tag_mask = builder.ins().iconst(types::I64, 0b111);
        let tag = builder.ins().band(self.val, tag_mask);
        let int_tag = builder.ins().iconst(types::I64, 0);
        builder.ins().icmp(IntCC::Equal, tag, int_tag)
    }
    
    /// Create a tagged boolean value
    fn make_tagged_bool(builder: &mut FunctionBuilder, b: Value) -> Self {
        // For simplicity, represent booleans as tagged integers (0 or 1)
        let extended = builder.ins().uextend(types::I64, b);
        Self::make_tagged_int(builder, extended)
    }
}

/// Build a complete function from bytecode, translating it into Cranelift IR.
pub fn build_function(
    func: &mut codegen::ir::Function,
    func_ctx: &mut FunctionBuilderContext,
    chunk: &BytecodeChunk,
    module: &mut dyn cranelift_module::Module,
) -> Result<()> {
    let mut builder = FunctionBuilder::new(func, func_ctx);

    // Create and seal the entry block
    let entry_block = builder.create_block();
    builder.append_block_params_for_function_params(entry_block);
    builder.switch_to_block(entry_block);
    builder.seal_block(entry_block);  // Seal immediately as it has no predecessors

    // --- Pass 1: Discover all jump targets and create blocks for them ---
    let mut blocks = HashMap::new();
    // Don't insert entry_block for pc=0 since we're already in it

    for (pc, instruction) in chunk.instructions.iter().enumerate() {
        match instruction.opcode {
            Opcode::Jump => {
                let target = instruction.arg as usize;
                if !blocks.contains_key(&target) {
                    let block = builder.create_block();
                    blocks.insert(target, block);
                }
            }
            Opcode::JumpIf | Opcode::JumpIfNot => {
                // Create block for jump target
                let target = instruction.arg as usize;
                if !blocks.contains_key(&target) {
                    let block = builder.create_block();
                    blocks.insert(target, block);
                }
                
                // For conditional branches, we also need a block for the fall-through case
                let fallthrough_pc = pc + 1;
                if !blocks.contains_key(&fallthrough_pc) {
                    let block = builder.create_block();
                    blocks.insert(fallthrough_pc, block);
                }
            }
            _ => {}
        }
    }

    // --- Pass 2: Declare all local variables ---
    let mut locals = HashMap::new();
    // Assuming locals are dense from 0 to N. A more robust method might scan
    // all Load/Store opcodes to find the max index.
    let num_locals = 32; // Placeholder for actual local count from bytecode info
    for i in 0..num_locals {
        // We declare all locals as I64 and will bitcast if we need to store floats.
        let var = Variable::new(i);
        builder.declare_var(var, types::I64);
        locals.insert(i as u32, var);
        let zero = builder.ins().iconst(types::I64, 0);
        builder.def_var(var, zero); // Initialize to 0
    }

    // --- Pass 3: Translate instructions ---
    let mut value_stack: Vec<JitValue> = Vec::new();
    let mut pc = 0;
    let mut block_terminated = false;
    
    while pc < chunk.instructions.len() {
        // If the current PC is a jump target, switch to its block.
        // Skip if pc=0 since we're already in the entry block
        if pc > 0 {
            if let Some(&block) = blocks.get(&pc) {
                if !block_terminated {
                     builder.ins().jump(block, &[]);
                }
                builder.switch_to_block(block);
                block_terminated = false;
            }
        }

        let instruction = &chunk.instructions[pc];

        match instruction.opcode {
            // --- Stack Operations ---
            Opcode::Push => {
                let constant = chunk
                    .constants
                    .get(instruction.arg as usize)
                    .ok_or_else(|| anyhow!("Invalid constant index"))?;

                // Convert the constant to a tagged value
                let tagged = value_to_tagged(constant);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, tagged.0 as i64),
                    ty: types::I64, // All values are represented as tagged i64
                };
                value_stack.push(jit_val);
            }

            Opcode::Pop => {
                value_stack.pop().ok_or_else(|| anyhow!("JIT Error: Stack underflow on POP"))?;
            }
            
            Opcode::PopN => {
                let n = instruction.arg as usize;
                for _ in 0..n {
                    value_stack.pop().ok_or_else(|| anyhow!("JIT Error: Stack underflow on POPN"))?;
                }
            }
            
            Opcode::Dup => {
                let val = value_stack.last()
                    .ok_or_else(|| anyhow!("JIT Error: Stack underflow on DUP"))?
                    .clone();
                value_stack.push(val);
            }
            
            Opcode::Swap => {
                let len = value_stack.len();
                if len < 2 {
                    return Err(anyhow!("JIT Error: Stack underflow on SWAP"));
                }
                value_stack.swap(len - 1, len - 2);
            }
            
            // --- Local Variable Operations ---
            Opcode::LoadLocal0 => {
                let var = locals.get(&0).ok_or_else(|| anyhow!("Undefined local variable 0"))?;
                let val = builder.use_var(*var);
                value_stack.push(JitValue { val, ty: types::I64 });
            }
            
            Opcode::LoadLocal1 => {
                let var = locals.get(&1).ok_or_else(|| anyhow!("Undefined local variable 1"))?;
                let val = builder.use_var(*var);
                value_stack.push(JitValue { val, ty: types::I64 });
            }
            
            Opcode::LoadLocal2 => {
                let var = locals.get(&2).ok_or_else(|| anyhow!("Undefined local variable 2"))?;
                let val = builder.use_var(*var);
                value_stack.push(JitValue { val, ty: types::I64 });
            }
            
            Opcode::LoadLocal3 => {
                let var = locals.get(&3).ok_or_else(|| anyhow!("Undefined local variable 3"))?;
                let val = builder.use_var(*var);
                value_stack.push(JitValue { val, ty: types::I64 });
            }
            
            Opcode::StoreLocal0 => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&0).ok_or_else(|| anyhow!("Undefined local variable 0"))?;
                builder.def_var(*var, value.val);
            }
            
            Opcode::StoreLocal1 => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&1).ok_or_else(|| anyhow!("Undefined local variable 1"))?;
                builder.def_var(*var, value.val);
            }
            
            Opcode::StoreLocal2 => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&2).ok_or_else(|| anyhow!("Undefined local variable 2"))?;
                builder.def_var(*var, value.val);
            }
            
            Opcode::StoreLocal3 => {
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&3).ok_or_else(|| anyhow!("Undefined local variable 3"))?;
                builder.def_var(*var, value.val);
            }
            
            Opcode::LoadLocal => {
                let idx = instruction.arg as u32;
                let var = locals.get(&idx).ok_or_else(|| anyhow!("Undefined local variable {}", idx))?;
                let val = builder.use_var(*var);
                value_stack.push(JitValue { val, ty: types::I64 });
            }
            
            Opcode::StoreLocal => {
                let idx = instruction.arg as u32;
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&idx).ok_or_else(|| anyhow!("Undefined local variable {}", idx))?;
                builder.def_var(*var, value.val);
            }
            
            // --- Specialized Push Operations ---
            Opcode::PushInt0 => {
                let zero = TaggedValue::from_integer(0);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, zero.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushInt1 => {
                let one = TaggedValue::from_integer(1);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, one.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushInt2 => {
                let two = TaggedValue::from_integer(2);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, two.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushIntSmall => {
                let n = instruction.arg as i64;
                let tagged = TaggedValue::from_integer(n);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, tagged.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushTrue => {
                let true_val = TaggedValue::from_integer(1);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, true_val.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushFalse => {
                let false_val = TaggedValue::from_integer(0);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, false_val.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }
            
            Opcode::PushNil => {
                // Use a special tagged value for nil
                let nil_val = value_to_tagged(&ClValue::Nil);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, nil_val.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }

            // --- Global Variable Operations ---
            Opcode::LoadGlobal => {
                // For now, push a placeholder value
                // In a real implementation, we'd look up the global from a registry
                // LoadGlobal test from issue with negation parser - needs proper implementation
                let nil_val = value_to_tagged(&ClValue::Nil);
                let jit_val = JitValue {
                    val: builder.ins().iconst(types::I64, nil_val.0 as i64),
                    ty: types::I64,
                };
                value_stack.push(jit_val);
            }

            // --- Arithmetic Operations ---
            Opcode::Add => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked addition
                let add_func = module.declare_function(
                    "jit_runtime_add_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let add_func_ref = module.declare_func_in_func(add_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(add_func_ref, &[l.val, r.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }

            Opcode::Sub => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked subtraction
                let sub_func = module.declare_function(
                    "jit_runtime_sub_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let sub_func_ref = module.declare_func_in_func(sub_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(sub_func_ref, &[l.val, r.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::Mul => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked multiplication
                let mul_func = module.declare_function(
                    "jit_runtime_mul_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let mul_func_ref = module.declare_func_in_func(mul_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(mul_func_ref, &[l.val, r.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::Div => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked division
                let div_func = module.declare_function(
                    "jit_runtime_div_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let div_func_ref = module.declare_func_in_func(div_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(div_func_ref, &[l.val, r.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::Mod => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked modulo
                let mod_func = module.declare_function(
                    "jit_runtime_mod_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let mod_func_ref = module.declare_func_in_func(mod_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(mod_func_ref, &[l.val, r.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::Neg => {
                let val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function for type-checked negation
                let neg_func = module.declare_function(
                    "jit_runtime_neg_typed",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let neg_func_ref = module.declare_func_in_func(neg_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(neg_func_ref, &[val.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            // --- Comparison Operations ---
            Opcode::Eq => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                // For tagged values, we can compare directly
                let eq = builder.ins().icmp(IntCC::Equal, l.val, r.val);
                let result = JitValue::make_tagged_bool(&mut builder, eq);
                value_stack.push(result);
            }
            
            Opcode::Ne => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let ne = builder.ins().icmp(IntCC::NotEqual, l.val, r.val);
                let result = JitValue::make_tagged_bool(&mut builder, ne);
                value_stack.push(result);
            }
            
            Opcode::Lt => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let lt = builder.ins().icmp(IntCC::SignedLessThan, l_int, r_int);
                let result = JitValue::make_tagged_bool(&mut builder, lt);
                value_stack.push(result);
            }
            
            Opcode::Le => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let le = builder.ins().icmp(IntCC::SignedLessThanOrEqual, l_int, r_int);
                let result = JitValue::make_tagged_bool(&mut builder, le);
                value_stack.push(result);
            }
            
            Opcode::Gt => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let gt = builder.ins().icmp(IntCC::SignedGreaterThan, l_int, r_int);
                let result = JitValue::make_tagged_bool(&mut builder, gt);
                value_stack.push(result);
            }
            
            Opcode::Ge => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let ge = builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, l_int, r_int);
                let result = JitValue::make_tagged_bool(&mut builder, ge);
                value_stack.push(result);
            }
            
            // --- Boolean Operations ---
            Opcode::And => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                // For tagged integers used as bools, we need to check if both are non-zero
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let zero = builder.ins().iconst(types::I64, 0);
                let l_bool = builder.ins().icmp(IntCC::NotEqual, l_int, zero);
                let r_bool = builder.ins().icmp(IntCC::NotEqual, r_int, zero);
                let and_result = builder.ins().band(l_bool, r_bool);
                let result = JitValue::make_tagged_bool(&mut builder, and_result);
                value_stack.push(result);
            }
            
            Opcode::Or => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let zero = builder.ins().iconst(types::I64, 0);
                let l_bool = builder.ins().icmp(IntCC::NotEqual, l_int, zero);
                let r_bool = builder.ins().icmp(IntCC::NotEqual, r_int, zero);
                let or_result = builder.ins().bor(l_bool, r_bool);
                let result = JitValue::make_tagged_bool(&mut builder, or_result);
                value_stack.push(result);
            }
            
            Opcode::Not => {
                let val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let int_val = val.untag_int(&mut builder);
                let zero = builder.ins().iconst(types::I64, 0);
                let is_zero = builder.ins().icmp(IntCC::Equal, int_val, zero);
                let result = JitValue::make_tagged_bool(&mut builder, is_zero);
                value_stack.push(result);
            }

             // --- Local Variable Operations ---
            Opcode::Load => {
                let var = locals.get(&instruction.arg).ok_or_else(|| anyhow!("Invalid local"))?;
                let val = builder.use_var(*var);
                // Here we assume the type based on context, a more advanced JIT might store types of locals
                value_stack.push(JitValue { val, ty: types::I64 });
            }

            Opcode::Store => {
                let jit_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let var = locals.get(&instruction.arg).ok_or_else(|| anyhow!("Invalid local"))?;
                // If storing a float, bitcast it to I64 to fit in the variable slot
                let val_to_store = if jit_val.ty == types::F64 {
                    builder.ins().bitcast(types::I64, MemFlags::new(), jit_val.val)
                } else {
                    jit_val.val
                };
                builder.def_var(*var, val_to_store);
            }

            // --- Control Flow ---
            Opcode::Jump => {
                let target_block = blocks[&(instruction.arg as usize)];
                builder.ins().jump(target_block, &[]);
                block_terminated = true;
            }

            Opcode::JumpIf => {
                let cond = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let cond_int = cond.untag_int(&mut builder);
                let zero = builder.ins().iconst(types::I64, 0);
                let is_truthy = builder.ins().icmp(IntCC::NotEqual, cond_int, zero);
                let target_block = blocks[&(instruction.arg as usize)];
                let fallthrough_block = blocks[&(pc + 1)];
                builder.ins().brif(is_truthy, target_block, &[], fallthrough_block, &[]);
                block_terminated = true;
            }
            
            Opcode::JumpIfNot => {
                let cond = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let cond_int = cond.untag_int(&mut builder);
                let zero = builder.ins().iconst(types::I64, 0);
                let is_falsy = builder.ins().icmp(IntCC::Equal, cond_int, zero);
                let target_block = blocks[&(instruction.arg as usize)];
                let fallthrough_block = blocks[&(pc + 1)];
                builder.ins().brif(is_falsy, target_block, &[], fallthrough_block, &[]);
                block_terminated = true;
            }

            Opcode::Return => {
                let result = value_stack.pop().unwrap_or({
                    // Return properly tagged nil if stack is empty
                    let nil_val = value_to_tagged(&ClValue::Nil);
                    JitValue {
                        val: builder.ins().iconst(types::I64, nil_val.0 as i64),
                        ty: types::I64,
                    }
                });
                builder.ins().return_(&[result.val]);
                block_terminated = true;
            }
            
            // --- Function Calls ---
            Opcode::Call => {
                let arg_count = instruction.arg as usize;
                
                // Pop the function value
                let func_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for function"))?;
                
                // Pop arguments into a vector (in reverse order)
                let mut args = Vec::with_capacity(arg_count);
                for _ in 0..arg_count {
                    args.push(value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for argument"))?);
                }
                args.reverse(); // Restore correct argument order
                
                // Import the runtime call function
                let call_func = module.declare_function(
                    "jit_runtime_call",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // function value
                        sig.params.push(AbiParam::new(types::I64)); // arg count
                        sig.params.push(AbiParam::new(module.target_config().pointer_type())); // args pointer
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let call_func_ref = module.declare_func_in_func(call_func, builder.func);
                
                // Allocate stack space for arguments if needed
                let result = if arg_count > 0 {
                    // Create a stack slot for the arguments array
                    let args_size = (arg_count * 8) as u32; // 8 bytes per i64
                    let args_slot = builder.create_sized_stack_slot(StackSlotData::new(StackSlotKind::ExplicitSlot, args_size));
                    
                    // Store arguments in the stack slot
                    for (i, arg) in args.iter().enumerate() {
                        let offset = (i * 8) as i32;
                        builder.ins().stack_store(arg.val, args_slot, offset);
                    }
                    
                    // Get pointer to the stack slot
                    let args_ptr = builder.ins().stack_addr(module.target_config().pointer_type(), args_slot, 0);
                    let arg_count_val = builder.ins().iconst(types::I64, arg_count as i64);
                    
                    // Call the runtime function
                    let call = builder.ins().call(call_func_ref, &[func_val.val, arg_count_val, args_ptr]);
                    builder.inst_results(call)[0]
                } else {
                    // No arguments - pass null pointer
                    let null_ptr = builder.ins().iconst(module.target_config().pointer_type(), 0);
                    let arg_count_val = builder.ins().iconst(types::I64, 0);
                    
                    // Call the runtime function
                    let call = builder.ins().call(call_func_ref, &[func_val.val, arg_count_val, null_ptr]);
                    builder.inst_results(call)[0]
                };
                
                // Push the result
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }


            // --- Function Operations ---
            Opcode::MakeFunc => {
                // MakeFunc creates a function without captures
                let chunk_id = instruction.arg as usize;
                
                // Create a tagged value representing the function
                let func_tag = TaggedValue::from_integer(chunk_id as i64);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, func_tag.0 as i64),
                    ty: types::I64,
                });
            }
            
            // --- Closure Operations ---
            Opcode::MakeClosure => {
                // MakeClosure: creates a closure capturing N values from stack
                // instruction.arg contains packed data: (chunk_id << 16) | capture_count
                let chunk_id = (instruction.arg >> 16) as usize;
                let capture_count = (instruction.arg & 0xFFFF) as usize;
                
                // Pop captured values into a vector
                let mut captures = Vec::with_capacity(capture_count);
                for _ in 0..capture_count {
                    captures.push(value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for capture"))?);
                }
                captures.reverse(); // Restore correct order
                
                // Import the runtime closure creation function
                let make_closure_func = module.declare_function(
                    "jit_runtime_make_closure",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // chunk_id
                        sig.params.push(AbiParam::new(types::I64)); // capture_count
                        sig.params.push(AbiParam::new(types::I64)); // captures_ptr
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let make_closure_ref = module.declare_func_in_func(make_closure_func, builder.func);
                
                // Allocate stack space for captures
                let stack_slot = if capture_count > 0 {
                    let slot_size = (capture_count * 8) as u32; // 8 bytes per i64
                    let slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        slot_size,
                    ));
                    
                    // Store captures to stack
                    for (i, capture) in captures.iter().enumerate() {
                        let offset = (i * 8) as i32;
                        builder.ins().stack_store(capture.val, slot, offset);
                    }
                    
                    Some(slot)
                } else {
                    None
                };
                
                // Get pointer to captures (or null if no captures)
                let captures_ptr = if let Some(slot) = stack_slot {
                    builder.ins().stack_addr(types::I64, slot, 0)
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                
                // Call runtime function
                let chunk_id_val = builder.ins().iconst(types::I64, chunk_id as i64);
                let capture_count_val = builder.ins().iconst(types::I64, capture_count as i64);
                let call = builder.ins().call(make_closure_ref, &[chunk_id_val, capture_count_val, captures_ptr]);
                let closure_val = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: closure_val,
                    ty: types::I64,
                });
            }
            
            Opcode::LoadCaptured => {
                // Load a value from the captured environment
                // instruction.arg is the index into the captured environment
                let index = instruction.arg as usize;
                
                // We need to get the current closure from somewhere
                // For now, we'll assume it's in a special local variable (local 0)
                // In a real implementation, this would be passed as a parameter or stored in a register
                let closure_val = if let Some(&var) = locals.get(&0) {
                    builder.use_var(var)
                } else {
                    // If no closure is available, return an error
                    let error_msg = "LoadCaptured called without active closure";
                    let error = ClValue::Error {
                        kind: "RuntimeError".to_string(),
                        message: error_msg.to_string(),
                        stack_trace: None,
                    };
                    let error_tagged = value_to_tagged(&error);
                    builder.ins().iconst(types::I64, error_tagged.0 as i64)
                };
                
                // Import the runtime function
                let load_captured_func = module.declare_function(
                    "jit_runtime_closure_get_env",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // closure
                        sig.params.push(AbiParam::new(types::I64)); // index
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let load_captured_ref = module.declare_func_in_func(load_captured_func, builder.func);
                
                // Call runtime function
                let index_val = builder.ins().iconst(types::I64, index as i64);
                let call = builder.ins().call(load_captured_ref, &[closure_val, index_val]);
                let captured_val = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: captured_val,
                    ty: types::I64,
                });
            }
            
            // --- String Operations ---
            Opcode::StrLen => {
                let str_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let len_func = module.declare_function(
                    "jit_runtime_string_len",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let len_func_ref = module.declare_func_in_func(len_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(len_func_ref, &[str_val.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::StrConcat => {
                let b = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let a = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let concat_func = module.declare_function(
                    "jit_runtime_string_concat",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let concat_func_ref = module.declare_func_in_func(concat_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(concat_func_ref, &[a.val, b.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::StrUpper => {
                let str_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let upper_func = module.declare_function(
                    "jit_runtime_string_upper",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let upper_func_ref = module.declare_func_in_func(upper_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(upper_func_ref, &[str_val.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::StrLower => {
                let str_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let lower_func = module.declare_function(
                    "jit_runtime_string_lower",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64));
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let lower_func_ref = module.declare_func_in_func(lower_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(lower_func_ref, &[str_val.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            // --- List Operations ---
            Opcode::MakeList => {
                // Pop N elements from stack and create a list
                let count = instruction.arg as usize;
                let mut items = Vec::with_capacity(count);
                
                // Pop items in reverse order
                for _ in 0..count {
                    items.push(value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?);
                }
                items.reverse();
                
                // Import the runtime list creation function
                let make_list_func = module.declare_function(
                    "jit_runtime_make_list",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // count
                        sig.params.push(AbiParam::new(types::I64)); // items_ptr
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let make_list_ref = module.declare_func_in_func(make_list_func, builder.func);
                
                // Allocate stack space for items
                let stack_slot = if count > 0 {
                    let slot_size = (count * 8) as u32; // 8 bytes per i64
                    let slot = builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        slot_size,
                    ));
                    
                    // Store items to stack
                    for (i, item) in items.iter().enumerate() {
                        let offset = (i * 8) as i32;
                        builder.ins().stack_store(item.val, slot, offset);
                    }
                    
                    Some(slot)
                } else {
                    None
                };
                
                // Get pointer to items (or null if no items)
                let items_ptr = if let Some(slot) = stack_slot {
                    builder.ins().stack_addr(types::I64, slot, 0)
                } else {
                    builder.ins().iconst(types::I64, 0)
                };
                
                // Call runtime function
                let count_val = builder.ins().iconst(types::I64, count as i64);
                let call = builder.ins().call(make_list_ref, &[count_val, items_ptr]);
                let list_val = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: list_val,
                    ty: types::I64,
                });
            }
            
            Opcode::ListHead => {
                // Get the first element of a list
                let list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let head_func = module.declare_function(
                    "jit_runtime_list_head",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // list
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let head_func_ref = module.declare_func_in_func(head_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(head_func_ref, &[list.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::ListTail => {
                // Get all elements except the first
                let list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let tail_func = module.declare_function(
                    "jit_runtime_list_tail",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // list
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let tail_func_ref = module.declare_func_in_func(tail_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(tail_func_ref, &[list.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::ListCons => {
                // Prepend an element to a list
                // Stack order: [head, list] with list on top
                let list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let head = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let cons_func = module.declare_function(
                    "jit_runtime_list_cons",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // element
                        sig.params.push(AbiParam::new(types::I64)); // list
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let cons_func_ref = module.declare_func_in_func(cons_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(cons_func_ref, &[head.val, list.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::ListLen => {
                // Get the length of a list
                let list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let len_func = module.declare_function(
                    "jit_runtime_list_len",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // list
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let len_func_ref = module.declare_func_in_func(len_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(len_func_ref, &[list.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::ListEmpty => {
                // Check if a list is empty
                let list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let empty_func = module.declare_function(
                    "jit_runtime_list_empty",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // list
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let empty_func_ref = module.declare_func_in_func(empty_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(empty_func_ref, &[list.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            // --- Channel Operations ---
            Opcode::Channel | Opcode::MakeChannel => {
                // Create a new channel
                // Import the runtime function
                let make_channel_func = module.declare_function(
                    "jit_runtime_make_channel",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let make_channel_func_ref = module.declare_func_in_func(make_channel_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(make_channel_func_ref, &[]);
                let channel = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: channel,
                    ty: types::I64,
                });
            }
            
            Opcode::Send => {
                // Send a value to a channel
                // Stack: [channel, value] with value on top
                let value = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let channel = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let send_func = module.declare_function(
                    "jit_runtime_channel_send",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // channel
                        sig.params.push(AbiParam::new(types::I64)); // value
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let send_func_ref = module.declare_func_in_func(send_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(send_func_ref, &[channel.val, value.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            Opcode::Receive => {
                // Receive a value from a channel
                let channel = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // Import the runtime function
                let recv_func = module.declare_function(
                    "jit_runtime_channel_recv",
                    cranelift_module::Linkage::Import,
                    &{
                        let mut sig = module.make_signature();
                        sig.params.push(AbiParam::new(types::I64)); // channel
                        sig.returns.push(AbiParam::new(types::I64));
                        sig
                    }
                )?;
                let recv_func_ref = module.declare_func_in_func(recv_func, builder.func);
                
                // Call the runtime function
                let call = builder.ins().call(recv_func_ref, &[channel.val]);
                let result = builder.inst_results(call)[0];
                
                value_stack.push(JitValue {
                    val: result,
                    ty: types::I64,
                });
            }
            
            // --- Halt and Unimplemented Opcodes ---
            Opcode::Halt => {
                // Return the value on top of the stack, or nil if empty
                let result = if let Some(value) = value_stack.last() {
                    value.val
                } else {
                    let nil_val = value_to_tagged(&ClValue::Nil);
                    builder.ins().iconst(types::I64, nil_val.0 as i64)
                };
                builder.ins().return_(&[result]);
                block_terminated = true;
            }

            // All other opcodes are explicitly not supported yet
            _ => return Err(anyhow!("JIT Error: Unsupported opcode {:?}", instruction.opcode)),
        }

        pc += 1;
    }

    // --- Finalization ---
    // In case the last instruction doesn't terminate the block (e.g., not a jump or return)
    if !block_terminated {
         let result = if let Some(value) = value_stack.pop() {
            value.val
        } else {
            // Default return is nil
            let nil_val = value_to_tagged(&ClValue::Nil);
            builder.ins().iconst(types::I64, nil_val.0 as i64)
        };
        builder.ins().return_(&[result]);
    }
    
    // Seal all created blocks now that all predecessors are known.
    for &block in blocks.values() {
        builder.seal_block(block);
    }
    
    builder.finalize();

    Ok(())
}