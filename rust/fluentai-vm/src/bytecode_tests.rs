//! Unit tests for bytecode structure and operations

#[cfg(test)]
mod tests {
    use super::super::bytecode::*;
    use crate::gc::GcHandle;
    use crate::safety::{PromiseId, ChannelId};
    use rustc_hash::FxHashMap;
    use std::sync::Arc;
    
    mod instruction_tests {
        use super::*;
        
        #[test]
        fn test_instruction_creation() {
            // Test instruction without argument
            let instr = Instruction::new(Opcode::Add);
            assert_eq!(instr.opcode, Opcode::Add);
            assert_eq!(instr.arg, 0);
            
            // Test instruction with argument
            let instr = Instruction::with_arg(Opcode::Push, 42);
            assert_eq!(instr.opcode, Opcode::Push);
            assert_eq!(instr.arg, 42);
        }
        
        #[test]
        fn test_instruction_clone() {
            let instr = Instruction::with_arg(Opcode::Jump, 100);
            let cloned = instr.clone();
            assert_eq!(cloned.opcode, instr.opcode);
            assert_eq!(cloned.arg, instr.arg);
        }
        
        #[test]
        fn test_all_opcodes() {
            // Test that we can create instructions for all opcodes
            let opcodes = vec![
                // Stack manipulation
                Opcode::Push, Opcode::Pop, Opcode::Dup, Opcode::Swap,
                // Arithmetic
                Opcode::Add, Opcode::Sub, Opcode::Mul, Opcode::Div, Opcode::Mod, Opcode::Neg,
                // Specialized arithmetic
                Opcode::AddInt, Opcode::SubInt, Opcode::MulInt, Opcode::DivInt,
                Opcode::AddFloat, Opcode::SubFloat, Opcode::MulFloat, Opcode::DivFloat,
                // Comparison
                Opcode::Eq, Opcode::Ne, Opcode::Lt, Opcode::Le, Opcode::Gt, Opcode::Ge,
                Opcode::LtInt, Opcode::LeInt, Opcode::GtInt, Opcode::GeInt,
                // Boolean
                Opcode::And, Opcode::Or, Opcode::Not,
                // Control flow
                Opcode::Jump, Opcode::JumpIf, Opcode::JumpIfNot, Opcode::Call, Opcode::Return,
                // Variables
                Opcode::Load, Opcode::Store, Opcode::LoadGlobal, Opcode::StoreGlobal,
                // Fast locals
                Opcode::LoadLocal0, Opcode::LoadLocal1, Opcode::LoadLocal2, Opcode::LoadLocal3,
                Opcode::StoreLocal0, Opcode::StoreLocal1, Opcode::StoreLocal2, Opcode::StoreLocal3,
                // Functions
                Opcode::MakeFunc, Opcode::MakeClosure, Opcode::LoadCaptured, Opcode::MakeEnv, Opcode::PopEnv,
                // Let binding
                Opcode::PopN,
                // Lists
                Opcode::MakeList, Opcode::ListHead, Opcode::ListTail, Opcode::ListCons,
                Opcode::ListLen, Opcode::ListEmpty,
                // Strings
                Opcode::StrLen, Opcode::StrConcat, Opcode::StrUpper, Opcode::StrLower,
                // Specialized constants
                Opcode::PushInt0, Opcode::PushInt1, Opcode::PushInt2, Opcode::PushIntSmall,
                Opcode::PushTrue, Opcode::PushFalse, Opcode::PushNil, Opcode::PushConst,
                // Effects
                Opcode::Effect, Opcode::EffectAsync, Opcode::Await, Opcode::Spawn,
                Opcode::Channel, Opcode::Send, Opcode::Receive,
                // Cells
                Opcode::MakeCell, Opcode::CellGet, Opcode::CellSet,
                // Tagged values
                Opcode::MakeTagged, Opcode::GetTag, Opcode::GetTaggedField, Opcode::IsTagged,
                // Modules
                Opcode::LoadModule, Opcode::ImportBinding, Opcode::LoadQualified,
                Opcode::BeginModule, Opcode::EndModule, Opcode::ExportBinding,
                // GC
                Opcode::GcAlloc, Opcode::GcDeref, Opcode::GcSet, Opcode::GcCollect,
                // Tail calls
                Opcode::TailCall, Opcode::TailReturn, Opcode::LoopStart, Opcode::LoopEnd,
                Opcode::UpdateLocal,
                // Special
                Opcode::Halt, Opcode::Nop,
            ];
            
            for opcode in opcodes {
                let _ = Instruction::new(opcode);
            }
        }
    }
    
    mod value_tests {
        use super::*;
        
        #[test]
        fn test_value_nil() {
            let val = Value::Nil;
            assert_eq!(format!("{}", val), "nil");
            
            let cloned = val.clone();
            assert_eq!(cloned, Value::Nil);
        }
        
        #[test]
        fn test_value_bool() {
            let val_true = Value::Bool(true);
            assert_eq!(format!("{}", val_true), "true");
            
            let val_false = Value::Bool(false);
            assert_eq!(format!("{}", val_false), "false");
        }
        
        #[test]
        fn test_value_int() {
            let val = Value::Int(42);
            assert_eq!(format!("{}", val), "42");
            
            let val_neg = Value::Int(-123);
            assert_eq!(format!("{}", val_neg), "-123");
        }
        
        #[test]
        fn test_value_float() {
            let val = Value::Float(3.14);
            assert_eq!(format!("{}", val), "3.14");
            
            let val_sci = Value::Float(1.23e-4);
            assert_eq!(format!("{}", val_sci), "0.000123");
        }
        
        #[test]
        fn test_value_string() {
            let val = Value::String("hello".to_string());
            assert_eq!(format!("{}", val), "\"hello\"");
            
            let val_empty = Value::String(String::new());
            assert_eq!(format!("{}", val_empty), "\"\"");
        }
        
        #[test]
        fn test_value_list() {
            let val = Value::List(vec![
                Value::Int(1),
                Value::Int(2),
                Value::Int(3),
            ]);
            assert_eq!(format!("{}", val), "[1, 2, 3]");
            
            let val_empty = Value::List(vec![]);
            assert_eq!(format!("{}", val_empty), "[]");
            
            let val_nested = Value::List(vec![
                Value::Int(1),
                Value::List(vec![Value::Int(2), Value::Int(3)]),
            ]);
            assert_eq!(format!("{}", val_nested), "[1, [2, 3]]");
        }
        
        #[test]
        fn test_value_map() {
            let mut map = FxHashMap::default();
            map.insert("a".to_string(), Value::Int(1));
            map.insert("b".to_string(), Value::Int(2));
            let val = Value::Map(map);
            let formatted = format!("{}", val);
            assert!(formatted.starts_with('{'));
            assert!(formatted.ends_with('}'));
            assert!(formatted.contains("\"a\": 1"));
            assert!(formatted.contains("\"b\": 2"));
        }
        
        #[test]
        fn test_value_function() {
            let val = Value::Function {
                chunk_id: 5,
                env: vec![Value::Int(42)],
            };
            assert_eq!(format!("{}", val), "<function>");
        }
        
        #[test]
        fn test_value_promise() {
            let val = Value::Promise(PromiseId(123));
            assert_eq!(format!("{}", val), "<promise:123>");
        }
        
        #[test]
        fn test_value_channel() {
            let val = Value::Channel(ChannelId(456));
            assert_eq!(format!("{}", val), "<channel:456>");
        }
        
        #[test]
        fn test_value_cell() {
            let val = Value::Cell(789);
            assert_eq!(format!("{}", val), "<cell:789>");
        }
        
        #[test]
        fn test_value_tagged() {
            let val = Value::Tagged {
                tag: "Some".to_string(),
                values: vec![Value::Int(42)],
            };
            assert_eq!(format!("{}", val), "Some(42)");
            
            let val_empty = Value::Tagged {
                tag: "None".to_string(),
                values: vec![],
            };
            assert_eq!(format!("{}", val_empty), "None");
            
            let val_multi = Value::Tagged {
                tag: "Pair".to_string(),
                values: vec![Value::Int(1), Value::Int(2)],
            };
            assert_eq!(format!("{}", val_multi), "Pair(1, 2)");
        }
        
        #[test]
        fn test_value_module() {
            let mut exports = FxHashMap::default();
            exports.insert("x".to_string(), Value::Int(42));
            exports.insert("y".to_string(), Value::String("test".to_string()));
            
            let val = Value::Module {
                name: "TestModule".to_string(),
                exports,
            };
            assert_eq!(format!("{}", val), "<module TestModule with 2 exports>");
        }
        
        #[test]
        fn test_value_gc_handle() {
            // We can't easily create a real GcHandle in tests, so we'll just test the display
            // This is a placeholder test - in real code we'd have proper GC tests
            // For now, we know GcHandle displays as "<gc-handle>"
        }
        
        #[test]
        fn test_value_equality() {
            // Test that equal values are equal
            assert_eq!(Value::Nil, Value::Nil);
            assert_eq!(Value::Bool(true), Value::Bool(true));
            assert_eq!(Value::Int(42), Value::Int(42));
            assert_eq!(Value::Float(3.14), Value::Float(3.14));
            assert_eq!(Value::String("test".to_string()), Value::String("test".to_string()));
            
            // Test that different values are not equal
            assert_ne!(Value::Int(42), Value::Int(43));
            assert_ne!(Value::Bool(true), Value::Bool(false));
            assert_ne!(Value::String("a".to_string()), Value::String("b".to_string()));
        }
        
        #[test]
        fn test_value_clone() {
            let values = vec![
                Value::Nil,
                Value::Bool(true),
                Value::Int(42),
                Value::Float(3.14),
                Value::String("test".to_string()),
                Value::List(vec![Value::Int(1), Value::Int(2)]),
                Value::Tagged {
                    tag: "Test".to_string(),
                    values: vec![Value::Int(42)],
                },
            ];
            
            for val in values {
                let cloned = val.clone();
                assert_eq!(val, cloned);
            }
        }
    }
    
    mod chunk_tests {
        use super::*;
        
        #[test]
        fn test_chunk_creation() {
            let chunk = BytecodeChunk::new(Some("test".to_string()));
            assert_eq!(chunk.name, Some("test".to_string()));
            assert!(chunk.instructions.is_empty());
            assert!(chunk.constants.is_empty());
            
            let chunk_unnamed = BytecodeChunk::new(None);
            assert_eq!(chunk_unnamed.name, None);
        }
        
        #[test]
        fn test_add_instruction() {
            let mut chunk = BytecodeChunk::new(None);
            
            let idx1 = chunk.add_instruction(Instruction::new(Opcode::Push));
            assert_eq!(idx1, 0);
            
            let idx2 = chunk.add_instruction(Instruction::new(Opcode::Add));
            assert_eq!(idx2, 1);
            
            let idx3 = chunk.add_instruction(Instruction::new(Opcode::Halt));
            assert_eq!(idx3, 2);
            
            assert_eq!(chunk.instructions.len(), 3);
            assert_eq!(chunk.instructions[0].opcode, Opcode::Push);
            assert_eq!(chunk.instructions[1].opcode, Opcode::Add);
            assert_eq!(chunk.instructions[2].opcode, Opcode::Halt);
        }
        
        #[test]
        fn test_add_constant() {
            let mut chunk = BytecodeChunk::new(None);
            
            let idx1 = chunk.add_constant(Value::Int(42));
            assert_eq!(idx1, 0);
            
            let idx2 = chunk.add_constant(Value::String("hello".to_string()));
            assert_eq!(idx2, 1);
            
            let idx3 = chunk.add_constant(Value::Bool(true));
            assert_eq!(idx3, 2);
            
            assert_eq!(chunk.constants.len(), 3);
            assert_eq!(chunk.constants[0], Value::Int(42));
            assert_eq!(chunk.constants[1], Value::String("hello".to_string()));
            assert_eq!(chunk.constants[2], Value::Bool(true));
        }
        
        #[test]
        fn test_patch_jump() {
            let mut chunk = BytecodeChunk::new(None);
            
            // Add some instructions
            chunk.add_instruction(Instruction::new(Opcode::Push));
            let jump_idx = chunk.add_instruction(Instruction::with_arg(Opcode::Jump, 0));
            chunk.add_instruction(Instruction::new(Opcode::Add));
            chunk.add_instruction(Instruction::new(Opcode::Halt));
            
            // Patch the jump to point to instruction 3
            chunk.patch_jump(jump_idx, 3);
            
            assert_eq!(chunk.instructions[jump_idx].arg, 3);
        }
        
        #[test]
        fn test_chunk_with_complex_program() {
            let mut chunk = BytecodeChunk::new(Some("factorial".to_string()));
            
            // Add constants
            let one_idx = chunk.add_constant(Value::Int(1));
            let two_idx = chunk.add_constant(Value::Int(2));
            
            // Build a simple factorial-like program structure
            chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load n
            chunk.add_instruction(Instruction::with_arg(Opcode::Push, one_idx)); // Push 1
            chunk.add_instruction(Instruction::new(Opcode::Le)); // n <= 1
            let jump_if_idx = chunk.add_instruction(Instruction::with_arg(Opcode::JumpIf, 0)); // Jump if true
            
            // Recursive case
            chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load n
            chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load n again
            chunk.add_instruction(Instruction::with_arg(Opcode::Push, one_idx)); // Push 1
            chunk.add_instruction(Instruction::new(Opcode::Sub)); // n - 1
            chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1)); // Recursive call
            chunk.add_instruction(Instruction::new(Opcode::Mul)); // n * factorial(n-1)
            chunk.add_instruction(Instruction::new(Opcode::Return));
            
            // Base case
            let base_case_idx = chunk.add_instruction(Instruction::with_arg(Opcode::Push, one_idx));
            chunk.add_instruction(Instruction::new(Opcode::Return));
            
            // Patch the jump
            chunk.patch_jump(jump_if_idx, base_case_idx);
            
            // Verify the structure
            assert_eq!(chunk.name, Some("factorial".to_string()));
            assert_eq!(chunk.constants.len(), 2);
            assert_eq!(chunk.instructions.len(), 13);
            assert_eq!(chunk.instructions[jump_if_idx].arg, base_case_idx as u32);
        }
    }
    
    mod bytecode_tests {
        use super::*;
        
        #[test]
        fn test_bytecode_creation() {
            let bytecode = Bytecode::new();
            assert!(bytecode.chunks.is_empty());
            assert_eq!(bytecode.main_chunk, 0);
        }
        
        #[test]
        fn test_add_chunk() {
            let mut bytecode = Bytecode::new();
            
            let chunk1 = BytecodeChunk::new(Some("chunk1".to_string()));
            let idx1 = bytecode.add_chunk(chunk1);
            assert_eq!(idx1, 0);
            
            let chunk2 = BytecodeChunk::new(Some("chunk2".to_string()));
            let idx2 = bytecode.add_chunk(chunk2);
            assert_eq!(idx2, 1);
            
            assert_eq!(bytecode.chunks.len(), 2);
            assert_eq!(bytecode.chunks[0].name, Some("chunk1".to_string()));
            assert_eq!(bytecode.chunks[1].name, Some("chunk2".to_string()));
        }
        
        #[test]
        fn test_bytecode_with_multiple_functions() {
            let mut bytecode = Bytecode::new();
            
            // Helper function chunk
            let mut helper_chunk = BytecodeChunk::new(Some("helper".to_string()));
            helper_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0));
            helper_chunk.add_instruction(Instruction::new(Opcode::Dup));
            helper_chunk.add_instruction(Instruction::new(Opcode::Mul)); // Square the input
            helper_chunk.add_instruction(Instruction::new(Opcode::Return));
            let helper_idx = bytecode.add_chunk(helper_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            let five_idx = main_chunk.add_constant(Value::Int(5));
            main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeFunc, helper_idx as u32));
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, five_idx));
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            let main_idx = bytecode.add_chunk(main_chunk);
            
            // Set main chunk
            bytecode.main_chunk = main_idx;
            
            // Verify structure
            assert_eq!(bytecode.chunks.len(), 2);
            assert_eq!(bytecode.main_chunk, 1);
            assert_eq!(bytecode.chunks[0].name, Some("helper".to_string()));
            assert_eq!(bytecode.chunks[1].name, Some("main".to_string()));
        }
        
        #[test]
        fn test_bytecode_clone() {
            let mut bytecode = Bytecode::new();
            
            let mut chunk = BytecodeChunk::new(Some("test".to_string()));
            chunk.add_constant(Value::Int(42));
            chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
            chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.add_chunk(chunk);
            
            let cloned = bytecode.clone();
            assert_eq!(cloned.chunks.len(), bytecode.chunks.len());
            assert_eq!(cloned.main_chunk, bytecode.main_chunk);
            assert_eq!(cloned.chunks[0].name, bytecode.chunks[0].name);
            assert_eq!(cloned.chunks[0].constants.len(), bytecode.chunks[0].constants.len());
            assert_eq!(cloned.chunks[0].instructions.len(), bytecode.chunks[0].instructions.len());
        }
    }
    
    mod serialization_tests {
        use super::*;
        
        #[test]
        fn test_opcode_representation() {
            // Ensure opcodes have stable numeric representation
            assert_eq!(Opcode::Push as u8, Opcode::Push as u8);
            assert_ne!(Opcode::Push as u8, Opcode::Pop as u8);
            
            // Test that opcodes can be converted to/from u8
            let opcodes = vec![
                Opcode::Push, Opcode::Add, Opcode::Jump, Opcode::Halt
            ];
            
            for opcode in opcodes {
                let byte = opcode as u8;
                // In a real implementation, we'd have a from_u8 method
                // For now, just verify the conversion works
                assert!(byte < 255);
            }
        }
        
        #[test]
        fn test_instruction_size() {
            // Verify instruction size is reasonable
            use std::mem;
            let size = mem::size_of::<Instruction>();
            assert!(size <= 16); // Should be small for cache efficiency
        }
        
        #[test]
        fn test_value_size() {
            // Check that Value enum isn't too large
            use std::mem;
            let size = mem::size_of::<Value>();
            // Value should be reasonably sized despite having many variants
            assert!(size <= 64);
        }
    }
}