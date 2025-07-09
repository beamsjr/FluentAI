//! Unit tests for core VM execution

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use fluentai_core::value::Value;
    use crate::error::VMError;
    use std::sync::Arc;
    use fluentai_effects::runtime::EffectRuntime;
    use fluentai_effects::EffectContext;
    
    // Helper functions
    fn create_test_bytecode() -> Bytecode {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(chunk);
        bytecode
    }
    
    fn create_vm_with_program(instructions: Vec<Instruction>) -> VM {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));
        for instr in instructions {
            chunk.add_instruction(instr);
        }
        bytecode.chunks.push(chunk);
        VM::new(bytecode)
    }
    
    fn create_vm_with_constants(instructions: Vec<Instruction>, constants: Vec<Value>) -> VM {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));
        
        for constant in constants {
            chunk.add_constant(constant);
        }
        
        for instr in instructions {
            chunk.add_instruction(instr);
        }
        
        bytecode.chunks.push(chunk);
        VM::new(bytecode)
    }
    
    mod initialization {
        use super::*;
        
        #[test]
        fn test_vm_new() {
            let bytecode = create_test_bytecode();
            let vm = VM::new(bytecode);
            
            assert_eq!(vm.get_stack().len(), 0);
            assert_eq!(vm.get_globals().len(), 0);
            assert_eq!(vm.get_call_stack_depth(), 0);
        }
        
        #[test]
        fn test_enable_trace() {
            let mut vm = VM::new(create_test_bytecode());
            vm.enable_trace();
            // Trace flag is private, but we can verify it doesn't panic
        }
        
        #[test]
        fn test_enable_usage_tracking() {
            let mut vm = VM::new(create_test_bytecode());
            vm.enable_usage_tracking();
            assert!(vm.usage_tracker().is_some());
        }
        
        #[test]
        fn test_set_effect_runtime() {
            let mut vm = VM::new(create_test_bytecode());
            let runtime = Arc::new(EffectRuntime::default());
            vm.set_effect_runtime(runtime);
        }
        
        #[test]
        fn test_set_effect_context() {
            let mut vm = VM::new(create_test_bytecode());
            let context = Arc::new(EffectContext::default());
            vm.set_effect_context(context);
        }
    }
    
    mod stack_operations {
        use super::*;
        
        #[test]
        fn test_push_pop() {
            let mut vm = VM::new(create_test_bytecode());
            
            // Test push
            vm.push(Value::Integer(42)).unwrap();
            assert_eq!(vm.get_stack().len(), 1);
            
            vm.push(Value::String("hello".to_string())).unwrap();
            assert_eq!(vm.get_stack().len(), 2);
            
            // Test pop
            let val = vm.pop().unwrap();
            assert_eq!(val, Value::String("hello".to_string()));
            assert_eq!(vm.get_stack().len(), 1);
            
            let val = vm.pop().unwrap();
            assert_eq!(val, Value::Integer(42));
            assert_eq!(vm.get_stack().len(), 0);
        }
        
        #[test]
        fn test_stack_underflow() {
            let mut vm = VM::new(create_test_bytecode());
            
            // Pop from empty stack should error
            match vm.pop() {
                Err(VMError::StackUnderflow { .. }) => {},
                _ => panic!("Expected stack underflow error"),
            }
        }
        
        #[test]
        fn test_dup_instruction() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::Dup),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
            assert_eq!(vm.get_stack().len(), 1); // One value left after halt pops
        }
        
        #[test]
        fn test_swap_instruction() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Swap),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(1), Value::Integer(2)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(1)); // Top of stack after swap
        }
        
        #[test]
        fn test_pop_n_instruction() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::with_arg(Opcode::Push, 2),
                Instruction::with_arg(Opcode::PopN, 2), // Pop 2, keep top
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(3)); // Only top value remains
        }
    }
    
    mod arithmetic_operations {
        use super::*;
        
        #[test]
        fn test_add() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Add),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(10), Value::Integer(32)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_sub() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Sub),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(50), Value::Integer(8)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_mul() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Mul),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(6), Value::Integer(7)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_div() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Div),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(84), Value::Integer(2)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_div_by_zero() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Div),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42), Value::Integer(0)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            match vm.run() {
                Err(VMError::DivisionByZero { .. }) => {},
                _ => panic!("Expected division by zero error"),
            }
        }
        
        #[test]
        fn test_mod() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Mod),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(17), Value::Integer(5)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(2));
        }
        
        #[test]
        fn test_neg() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::Neg),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(-42));
        }
        
        #[test]
        fn test_float_arithmetic() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Add),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Float(3.14), Value::Float(2.86)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            match result {
                Value::Float(f) => assert!((f - 6.0).abs() < 0.001),
                _ => panic!("Expected float result"),
            }
        }
    }
    
    mod comparison_operations {
        use super::*;
        
        #[test]
        fn test_eq() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Eq),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42), Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_ne() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Ne),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42), Value::Integer(43)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_lt() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Lt),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(10), Value::Integer(20)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_le() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Le),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(10), Value::Integer(10)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_gt() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Gt),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(20), Value::Integer(10)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_ge() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Ge),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(20), Value::Integer(20)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
    }
    
    mod boolean_operations {
        use super::*;
        
        #[test]
        fn test_and() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::And),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Boolean(true), Value::Boolean(false)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(false));
        }
        
        #[test]
        fn test_or() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::Or),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Boolean(true), Value::Boolean(false)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
        
        #[test]
        fn test_not() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::Not),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Boolean(true)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(false));
        }
    }
    
    mod control_flow {
        use super::*;
        
        #[test]
        fn test_jump() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Jump, 2), // Jump to index 2
                Instruction::with_arg(Opcode::Push, 0), // Skipped
                Instruction::with_arg(Opcode::Push, 1), // Execute this
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(1), Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_jump_if() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0), // Push true
                Instruction::with_arg(Opcode::JumpIf, 3), // Jump if true
                Instruction::with_arg(Opcode::Push, 1), // Skipped
                Instruction::with_arg(Opcode::Push, 2), // Execute this
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Boolean(true), Value::Integer(1), Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_jump_if_not() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0), // Push false
                Instruction::with_arg(Opcode::JumpIfNot, 3), // Jump if false
                Instruction::with_arg(Opcode::Push, 1), // Skipped
                Instruction::with_arg(Opcode::Push, 2), // Execute this
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Boolean(false), Value::Integer(1), Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
    }
    
    mod variable_operations {
        use super::*;
        
        // Note: These local variable tests are currently failing because they require
        // specific VM implementation details about how local variables are allocated
        // and accessed within function frames. The tests demonstrate the intended
        // behavior but need adjustment based on actual VM implementation.
        
        #[test]
        fn test_store_load_local() {
            // Create a function that uses local variables
            let mut bytecode = Bytecode::new();
            
            // Create function chunk that uses local variables
            let mut func_chunk = BytecodeChunk::new(Some("test_func".to_string()));
            func_chunk.add_constant(Value::Integer(10));
            func_chunk.add_constant(Value::Integer(32));
            // Pre-allocate space for locals by pushing a nil value
            func_chunk.add_instruction(Instruction::new(Opcode::PushNil)); // Space for local 0
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push 10
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Store, 0)); // Store at local 0
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1)); // Push 32  
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load local 0 (10)
            func_chunk.add_instruction(Instruction::new(Opcode::Add)); // Add them
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Create main chunk that calls the function
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeFunc, 0)); // Make function from chunk 0
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Call, 0)); // Call with 0 arguments
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_store_load_global() {
            // StoreGlobal and LoadGlobal use instruction.arg as index into constants
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0), // Push value 42
                Instruction::with_arg(Opcode::StoreGlobal, 1), // Store with name at constants[1]
                Instruction::with_arg(Opcode::LoadGlobal, 1), // Load with name at constants[1]
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(42), Value::String("test_var".to_string())];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
            assert_eq!(vm.get_globals().get("test_var"), Some(&Value::Integer(42)));
        }
    }
    
    mod list_operations {
        use super::*;
        
        #[test]
        fn test_make_list() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::with_arg(Opcode::Push, 2),
                Instruction::with_arg(Opcode::MakeList, 3), // Make list of 3 elements
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            match result {
                Value::List(items) => {
                    assert_eq!(items.len(), 3);
                    assert_eq!(items[0], Value::Integer(1));
                    assert_eq!(items[1], Value::Integer(2));
                    assert_eq!(items[2], Value::Integer(3));
                },
                _ => panic!("Expected list result"),
            }
        }
        
        #[test]
        fn test_list_head() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::ListHead),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![
                Value::List(vec![Value::Integer(42), Value::Integer(2), Value::Integer(3)])
            ];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_list_tail() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::ListTail),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![
                Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)])
            ];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            match result {
                Value::List(items) => {
                    assert_eq!(items.len(), 2);
                    assert_eq!(items[0], Value::Integer(2));
                    assert_eq!(items[1], Value::Integer(3));
                },
                _ => panic!("Expected list result"),
            }
        }
        
        #[test]
        fn test_list_cons() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0), // Push element
                Instruction::with_arg(Opcode::Push, 1), // Push list
                Instruction::new(Opcode::ListCons),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![
                Value::Integer(42),
                Value::List(vec![Value::Integer(2), Value::Integer(3)])
            ];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            match result {
                Value::List(items) => {
                    assert_eq!(items.len(), 3);
                    assert_eq!(items[0], Value::Integer(42));
                    assert_eq!(items[1], Value::Integer(2));
                    assert_eq!(items[2], Value::Integer(3));
                },
                _ => panic!("Expected list result"),
            }
        }
        
        #[test]
        fn test_list_len() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::ListLen),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![
                Value::List(vec![Value::Integer(1), Value::Integer(2), Value::Integer(3)])
            ];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(3));
        }
        
        #[test]
        fn test_list_empty() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::ListEmpty),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::List(vec![])];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Boolean(true));
        }
    }
    
    mod string_operations {
        use super::*;
        
        #[test]
        fn test_str_len() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::StrLen),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::String("hello".to_string())];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(5));
        }
        
        #[test]
        fn test_str_concat() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Push, 1),
                Instruction::new(Opcode::StrConcat),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![
                Value::String("hello".to_string()),
                Value::String(" world".to_string())
            ];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::String("hello world".to_string()));
        }
        
        #[test]
        fn test_str_upper() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::StrUpper),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::String("hello".to_string())];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::String("HELLO".to_string()));
        }
        
        #[test]
        fn test_str_lower() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::new(Opcode::StrLower),
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::String("HELLO".to_string())];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            let result = vm.run().unwrap();
            assert_eq!(result, Value::String("hello".to_string()));
        }
    }
    
    mod function_operations {
        use super::*;
        
        #[test]
        fn test_make_func_and_call() {
            let mut bytecode = Bytecode::new();
            
            // Create function chunk that adds 1 to its argument
            let mut func_chunk = BytecodeChunk::new(Some("add_one".to_string()));
            func_chunk.add_constant(Value::Integer(1));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load argument from stack base
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push 1
            func_chunk.add_instruction(Instruction::new(Opcode::Add));
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Create main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_constant(Value::Integer(41));
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push argument 41
            main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeFunc, 0)); // Make function from chunk 0
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1)); // Call with 1 argument
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
        
        #[test]
        fn test_make_closure() {
            let mut bytecode = Bytecode::new();
            
            // Create closure chunk that uses captured value
            let mut closure_chunk = BytecodeChunk::new(Some("add_captured".to_string()));
            // The argument will be at stack position 0 when the function is called
            closure_chunk.add_instruction(Instruction::with_arg(Opcode::LoadCaptured, 0)); // Load captured value (10)
            closure_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load argument (32)
            closure_chunk.add_instruction(Instruction::new(Opcode::Add));
            closure_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(closure_chunk);
            
            // Create main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_constant(Value::Integer(10)); // Value to capture
            main_chunk.add_constant(Value::Integer(32)); // Argument
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push value to capture (10)
            // MakeClosure packs chunk_id in upper 16 bits and capture count in lower 16 bits
            let packed_arg = (0 << 16) | 1; // chunk_id=0, capture_count=1
            main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, packed_arg));
            // Call expects: arguments first, then function on top
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1)); // Push argument (32)
            main_chunk.add_instruction(Instruction::new(Opcode::Swap)); // Swap so function is on top
            main_chunk.add_instruction(Instruction::with_arg(Opcode::Call, 1)); // Call with 1 argument
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            let result = vm.run().unwrap();
            assert_eq!(result, Value::Integer(42));
        }
    }
    
    mod usage_tracking {
        use super::*;
        use fluentai_core::ast::NodeId;
        
        #[test]
        fn test_usage_tracking() {
            let mut vm = VM::new(create_test_bytecode());
            vm.enable_usage_tracking();
            
            // Register chunk mapping
            let node_id = NodeId::new(1).unwrap();
            vm.register_chunk_mapping(0, node_id);
            
            // Run VM
            let _ = vm.run();
            
            // Check usage was tracked
            if let Some(tracker) = vm.usage_tracker() {
                let tracker = tracker.read().unwrap();
                let stats = tracker.get_stats(node_id);
                assert!(stats.is_some());
                // Note: exact execution count depends on implementation details
            }
        }
        
        #[test]
        fn test_hot_path_detection() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0),
                Instruction::with_arg(Opcode::Jump, 0), // Infinite loop for testing
            ];
            let constants = vec![Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            vm.enable_usage_tracking();
            
            let node_id = NodeId::new(1).unwrap();
            vm.register_chunk_mapping(0, node_id);
            
            // We can't actually run an infinite loop, but we can test the tracking setup
            if let Some(tracker) = vm.usage_tracker() {
                let mut tracker = tracker.write().unwrap();
                // Simulate many executions
                for _ in 0..1001 {
                    tracker.record_execution(0, 1000);
                }
                
                let stats = tracker.get_stats(node_id).unwrap();
                assert!(stats.is_hot_path);
            }
        }
    }
    
    mod error_handling {
        use super::*;
        
        #[test]
        fn test_invalid_constant_index() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 10), // Invalid index
                Instruction::new(Opcode::Halt),
            ];
            let mut vm = create_vm_with_program(instructions);
            
            match vm.run() {
                Err(VMError::InvalidConstantIndex { .. }) => {},
                _ => panic!("Expected invalid constant index error"),
            }
        }
        
        #[test]
        fn test_type_error() {
            let instructions = vec![
                Instruction::with_arg(Opcode::Push, 0), // String
                Instruction::with_arg(Opcode::Push, 1), // Int
                Instruction::new(Opcode::Add), // Can't add string and int
                Instruction::new(Opcode::Halt),
            ];
            let constants = vec![Value::String("hello".to_string()), Value::Integer(42)];
            let mut vm = create_vm_with_constants(instructions, constants);
            
            match vm.run() {
                Err(VMError::TypeError { .. }) => {},
                _ => panic!("Expected type error"),
            }
        }
        
        // Note: UndefinedVariable error variant test removed as it doesn't exist in VMError enum
    }
}