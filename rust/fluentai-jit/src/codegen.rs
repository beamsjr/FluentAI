//! Cranelift IR code generation for FluentAi bytecode

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
    // Note: Sealing the entry block immediately is fine as it has no predecessors.

    // --- Pass 1: Discover all jump targets and create blocks for them ---
    let mut blocks = HashMap::new();
    blocks.insert(0, entry_block); // The entry point is at pc=0

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
        if let Some(&block) = blocks.get(&pc) {
            if !block_terminated {
                 builder.ins().jump(block, &[]);
            }
            builder.switch_to_block(block);
            block_terminated = false;
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

            // --- Arithmetic Operations ---
            Opcode::Add => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, assume both values are tagged integers
                // TODO: Add runtime type checking and support for other types
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let sum = builder.ins().iadd(l_int, r_int);
                let result = JitValue::make_tagged_int(&mut builder, sum);
                value_stack.push(result);
            }

            Opcode::Sub => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, assume both values are tagged integers
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let diff = builder.ins().isub(l_int, r_int);
                let result = JitValue::make_tagged_int(&mut builder, diff);
                value_stack.push(result);
            }
            
            Opcode::Mul => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let product = builder.ins().imul(l_int, r_int);
                let result = JitValue::make_tagged_int(&mut builder, product);
                value_stack.push(result);
            }
            
            Opcode::Div => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let quotient = builder.ins().sdiv(l_int, r_int);
                let result = JitValue::make_tagged_int(&mut builder, quotient);
                value_stack.push(result);
            }
            
            Opcode::Mod => {
                let r = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let l = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                let l_int = l.untag_int(&mut builder);
                let r_int = r.untag_int(&mut builder);
                let remainder = builder.ins().srem(l_int, r_int);
                let result = JitValue::make_tagged_int(&mut builder, remainder);
                value_stack.push(result);
            }
            
            Opcode::Neg => {
                let val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let int_val = val.untag_int(&mut builder);
                let negated = builder.ins().ineg(int_val);
                let result = JitValue::make_tagged_int(&mut builder, negated);
                value_stack.push(result);
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
                    // Return tagged nil if stack is empty
                    let nil_tagged = TaggedValue::from_integer(0); // Using 0 as nil for now
                    JitValue {
                        val: builder.ins().iconst(types::I64, nil_tagged.0 as i64),
                        ty: types::I64,
                    }
                });
                builder.ins().return_(&[result.val]);
                block_terminated = true;
            }
            
            // --- Function Calls ---
            Opcode::Call => {
                // For now, implement a simplified version that just returns a dummy value
                // Full implementation with runtime dispatch is complex and needs more infrastructure
                let arg_count = instruction.arg as usize;
                
                // Pop the function value
                let _func_val = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for function"))?;
                
                // Pop arguments
                for _ in 0..arg_count {
                    value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for argument"))?;
                }
                
                // For now, just push a dummy result
                // TODO: Implement proper function call dispatch
                let dummy_result = TaggedValue::from_integer(42);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy_result.0 as i64),
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
                
                // Pop captured values
                let mut captures = Vec::with_capacity(capture_count);
                for _ in 0..capture_count {
                    captures.push(value_stack.pop().ok_or_else(|| anyhow!("Stack underflow for capture"))?);
                }
                captures.reverse();
                
                // For now, create a tagged value representing the closure
                // In a real implementation, we'd allocate a closure object on the heap
                // containing the chunk_id and captured environment
                
                // Use chunk_id as a simple closure identifier for now
                let closure_tag = TaggedValue::from_integer(chunk_id as i64);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, closure_tag.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::LoadCaptured => {
                // Load a value from the captured environment
                // instruction.arg is the index into the captured environment
                let _index = instruction.arg as usize;
                
                // For now, just push a dummy value
                // In a real implementation, we'd load from the closure's environment
                let dummy = TaggedValue::from_integer(0);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy.0 as i64),
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
                
                // For now, create a dummy list value
                // In a real implementation, we'd allocate a list on the heap
                let list_id = items.len() as i64; // Use length as a simple ID
                let list_val = TaggedValue::from_integer(list_id);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, list_val.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::ListHead => {
                // Get the first element of a list
                let _list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, return a dummy value
                // In a real implementation, we'd call a runtime function
                let dummy = TaggedValue::from_integer(0);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::ListTail => {
                // Get all elements except the first
                let _list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, return a dummy list
                let dummy = TaggedValue::from_integer(0);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::ListCons => {
                // Prepend an element to a list
                let _list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                let _elem = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, return a dummy list
                let dummy = TaggedValue::from_integer(1);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::ListLen => {
                // Get the length of a list
                let _list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, return a dummy length
                let dummy_len = TaggedValue::from_integer(0);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, dummy_len.0 as i64),
                    ty: types::I64,
                });
            }
            
            Opcode::ListEmpty => {
                // Check if a list is empty
                let _list = value_stack.pop().ok_or_else(|| anyhow!("Stack underflow"))?;
                
                // For now, return true (1)
                let true_val = TaggedValue::from_integer(1);
                value_stack.push(JitValue {
                    val: builder.ins().iconst(types::I64, true_val.0 as i64),
                    ty: types::I64,
                });
            }
            
            // --- Halt and Unimplemented Opcodes ---
            Opcode::Halt => {
                let zero = builder.ins().iconst(types::I64, 0);
                builder.ins().return_(&[zero]);
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
            builder.ins().iconst(types::I64, 0) // Default return
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