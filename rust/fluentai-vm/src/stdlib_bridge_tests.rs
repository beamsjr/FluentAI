//! Unit tests for stdlib bridge integration

#[cfg(test)]
mod tests {
    use super::super::*;
    use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    use fluentai_core::value::Value as VMValue;
    use crate::stdlib_bridge::{VMStdlibBridge, VMStdlibExt};
    use crate::vm::VM;
    // Note: These tests assume VM has methods for stdlib value conversion
    // which would need to be implemented in a real scenario
    
    #[allow(unused_imports)]
    use fluentai_stdlib::value::Value as StdlibValue;
    #[allow(unused_imports)]
    use fluentai_stdlib::vm_bridge::VMCallback;
    
    // Helper functions
    fn create_test_vm() -> VM {
        let bytecode = Bytecode::new();
        VM::new(bytecode)
    }
    
    fn create_vm_with_function() -> VM {
        let mut bytecode = Bytecode::new();
        
        // Create a simple function that doubles its input
        let mut func_chunk = BytecodeChunk::new(Some("double".to_string()));
        func_chunk.add_constant(VMValue::Integer(2));
        func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load argument
        func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push 2
        func_chunk.add_instruction(Instruction::new(Opcode::Mul)); // Multiply
        func_chunk.add_instruction(Instruction::new(Opcode::Return));
        bytecode.chunks.push(func_chunk);
        
        // Main chunk
        let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
        main_chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(main_chunk);
        bytecode.main_chunk = 1;
        
        VM::new(bytecode)
    }
    
    mod value_conversion {
        use super::*;
        
        #[test]
        fn test_stdlib_to_vm_value_conversion() {
            let vm = create_test_vm();
            
            // Test nil conversion
            let stdlib_nil = StdlibValue::Nil;
            let vm_nil = vm.stdlib_value_to_vm_value(&stdlib_nil);
            assert_eq!(vm_nil, VMValue::Nil);
            
            // Test bool conversion
            let stdlib_bool = StdlibValue::Boolean(true);
            let vm_bool = vm.stdlib_value_to_vm_value(&stdlib_bool);
            assert_eq!(vm_bool, VMValue::Boolean(true));
            
            // Test int conversion
            let stdlib_int = StdlibValue::Integer(42);
            let vm_int = vm.stdlib_value_to_vm_value(&stdlib_int);
            assert_eq!(vm_int, VMValue::Integer(42));
            
            // Test float conversion
            let stdlib_float = StdlibValue::Float(3.14);
            let vm_float = vm.stdlib_value_to_vm_value(&stdlib_float);
            assert_eq!(vm_float, VMValue::Float(3.14));
            
            // Test string conversion
            let stdlib_string = StdlibValue::String("hello".to_string());
            let vm_string = vm.stdlib_value_to_vm_value(&stdlib_string);
            assert_eq!(vm_string, VMValue::String("hello".to_string()));
            
            // Test list conversion
            let stdlib_list = StdlibValue::List(vec![
                StdlibValue::Integer(1),
                StdlibValue::Integer(2),
                StdlibValue::Integer(3),
            ]);
            let vm_list = vm.stdlib_value_to_vm_value(&stdlib_list);
            match vm_list {
                VMValue::List(items) => {
                    assert_eq!(items.len(), 3);
                    assert_eq!(items[0], VMValue::Integer(1));
                    assert_eq!(items[1], VMValue::Integer(2));
                    assert_eq!(items[2], VMValue::Integer(3));
                },
                _ => panic!("Expected list"),
            }
        }
        
        #[test]
        fn test_vm_to_stdlib_value_conversion() {
            let vm = create_test_vm();
            
            // Test nil conversion
            let vm_nil = VMValue::Nil;
            let stdlib_nil = vm.vm_value_to_stdlib_value(&vm_nil);
            assert_eq!(stdlib_nil, StdlibValue::Nil);
            
            // Test bool conversion
            let vm_bool = VMValue::Boolean(false);
            let stdlib_bool = vm.vm_value_to_stdlib_value(&vm_bool);
            assert_eq!(stdlib_bool, StdlibValue::Boolean(false));
            
            // Test int conversion
            let vm_int = VMValue::Integer(-123);
            let stdlib_int = vm.vm_value_to_stdlib_value(&vm_int);
            assert_eq!(stdlib_int, StdlibValue::Integer(-123));
            
            // Test float conversion
            let vm_float = VMValue::Float(2.718);
            let stdlib_float = vm.vm_value_to_stdlib_value(&vm_float);
            assert_eq!(stdlib_float, StdlibValue::Float(2.718));
            
            // Test string conversion
            let vm_string = VMValue::String("world".to_string());
            let stdlib_string = vm.vm_value_to_stdlib_value(&vm_string);
            assert_eq!(stdlib_string, StdlibValue::String("world".to_string()));
            
            // Test list conversion
            let vm_list = VMValue::List(vec![
                VMValue::Boolean(true),
                VMValue::String("test".to_string()),
            ]);
            let stdlib_list = vm.vm_value_to_stdlib_value(&vm_list);
            match stdlib_list {
                StdlibValue::List(items) => {
                    assert_eq!(items.len(), 2);
                    assert_eq!(items[0], StdlibValue::Boolean(true));
                    assert_eq!(items[1], StdlibValue::String("test".to_string()));
                },
                _ => panic!("Expected list"),
            }
        }
        
        #[test]
        fn test_round_trip_conversion() {
            let vm = create_test_vm();
            
            let values = vec![
                StdlibValue::Nil,
                StdlibValue::Boolean(true),
                StdlibValue::Integer(42),
                StdlibValue::Float(3.14),
                StdlibValue::String("test".to_string()),
                StdlibValue::List(vec![
                    StdlibValue::Integer(1),
                    StdlibValue::String("nested".to_string()),
                ]),
            ];
            
            for original in values {
                let vm_value = vm.stdlib_value_to_vm_value(&original);
                let converted_back = vm.vm_value_to_stdlib_value(&vm_value);
                assert_eq!(original, converted_back);
            }
        }
    }
    
    mod bridge_callback {
        use super::*;
        
        #[test]
        fn test_call_function_simple() {
            let mut vm = create_vm_with_function();
            let mut bridge = VMStdlibBridge::new(&mut vm);
            
            // Create a function value that references our double function
            let func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let args = vec![StdlibValue::Integer(21)];
            let result = bridge.call_function(&func, &args);
            
            // Note: This test assumes VM has proper function value conversion
            // In practice, we'd need to ensure the VM can handle stdlib Function values
            match result {
                Ok(StdlibValue::Integer(42)) => {}, // Expected
                Ok(other) => panic!("Expected Int(42), got {:?}", other),
                Err(e) => panic!("Function call failed: {}", e),
            }
        }
        
        #[test]
        fn test_call_function_with_multiple_args() {
            let mut bytecode = Bytecode::new();
            
            // Create a function that adds three numbers
            let mut func_chunk = BytecodeChunk::new(Some("add3".to_string()));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load first arg
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 1)); // Load second arg
            func_chunk.add_instruction(Instruction::new(Opcode::Add));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 2)); // Load third arg
            func_chunk.add_instruction(Instruction::new(Opcode::Add));
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            let mut bridge = VMStdlibBridge::new(&mut vm);
            
            let func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let args = vec![
                StdlibValue::Integer(10),
                StdlibValue::Integer(20),
                StdlibValue::Integer(12),
            ];
            
            let result = bridge.call_function(&func, &args);
            match result {
                Ok(StdlibValue::Integer(42)) => {}, // Expected
                Ok(other) => panic!("Expected Int(42), got {:?}", other),
                Err(e) => panic!("Function call failed: {}", e),
            }
        }
    }
    
    mod higher_order_functions {
        use super::*;
        
        #[test]
        fn test_map_function() {
            let mut vm = create_vm_with_function();
            
            let double_func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let list = StdlibValue::List(vec![
                StdlibValue::Integer(1),
                StdlibValue::Integer(2),
                StdlibValue::Integer(3),
            ]);
            
            let args = vec![double_func, list];
            let result = vm.call_higher_order_stdlib("map", &args);
            
            match result {
                Ok(StdlibValue::List(items)) => {
                    assert_eq!(items.len(), 3);
                    assert_eq!(items[0], StdlibValue::Integer(2));
                    assert_eq!(items[1], StdlibValue::Integer(4));
                    assert_eq!(items[2], StdlibValue::Integer(6));
                },
                Ok(other) => panic!("Expected list result, got {:?}", other),
                Err(e) => panic!("Map failed: {}", e),
            }
        }
        
        #[test]
        fn test_map_empty_list() {
            let mut vm = create_vm_with_function();
            
            let func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let list = StdlibValue::List(vec![]);
            let args = vec![func, list];
            let result = vm.call_higher_order_stdlib("map", &args);
            
            match result {
                Ok(StdlibValue::List(items)) => {
                    assert_eq!(items.len(), 0);
                },
                Ok(other) => panic!("Expected empty list, got {:?}", other),
                Err(e) => panic!("Map failed: {}", e),
            }
        }
        
        #[test]
        fn test_filter_function() {
            let mut bytecode = Bytecode::new();
            
            // Create a function that checks if number is even
            let mut func_chunk = BytecodeChunk::new(Some("is_even".to_string()));
            func_chunk.add_constant(VMValue::Integer(2));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load argument
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0)); // Push 2
            func_chunk.add_instruction(Instruction::new(Opcode::Mod)); // n % 2
            func_chunk.add_instruction(Instruction::with_arg(Opcode::PushInt0, 0)); // Push 0
            func_chunk.add_instruction(Instruction::new(Opcode::Eq)); // (n % 2) == 0
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            
            let is_even_func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let list = StdlibValue::List(vec![
                StdlibValue::Integer(1),
                StdlibValue::Integer(2),
                StdlibValue::Integer(3),
                StdlibValue::Integer(4),
                StdlibValue::Integer(5),
                StdlibValue::Integer(6),
            ]);
            
            let args = vec![is_even_func, list];
            let result = vm.call_higher_order_stdlib("filter", &args);
            
            match result {
                Ok(StdlibValue::List(items)) => {
                    assert_eq!(items.len(), 3);
                    assert_eq!(items[0], StdlibValue::Integer(2));
                    assert_eq!(items[1], StdlibValue::Integer(4));
                    assert_eq!(items[2], StdlibValue::Integer(6));
                },
                Ok(other) => panic!("Expected filtered list, got {:?}", other),
                Err(e) => panic!("Filter failed: {}", e),
            }
        }
        
        #[test]
        fn test_fold_function() {
            let mut bytecode = Bytecode::new();
            
            // Create a function that adds two numbers
            let mut func_chunk = BytecodeChunk::new(Some("add".to_string()));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load first arg
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 1)); // Load second arg
            func_chunk.add_instruction(Instruction::new(Opcode::Add));
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            
            let add_func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let initial = StdlibValue::Integer(0);
            let list = StdlibValue::List(vec![
                StdlibValue::Integer(1),
                StdlibValue::Integer(2),
                StdlibValue::Integer(3),
                StdlibValue::Integer(4),
            ]);
            
            let args = vec![add_func, initial, list];
            let result = vm.call_higher_order_stdlib("fold", &args);
            
            match result {
                Ok(StdlibValue::Integer(10)) => {}, // 1+2+3+4 = 10
                Ok(other) => panic!("Expected Int(10), got {:?}", other),
                Err(e) => panic!("Fold failed: {}", e),
            }
        }
        
        #[test]
        fn test_fold_with_string_concat() {
            let mut bytecode = Bytecode::new();
            
            // Create a function that concatenates strings
            let mut func_chunk = BytecodeChunk::new(Some("concat".to_string()));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Load first arg
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 1)); // Load second arg
            func_chunk.add_instruction(Instruction::new(Opcode::StrConcat));
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            
            let concat_func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let initial = StdlibValue::String("".to_string());
            let list = StdlibValue::List(vec![
                StdlibValue::String("Hello".to_string()),
                StdlibValue::String(" ".to_string()),
                StdlibValue::String("World".to_string()),
            ]);
            
            let args = vec![concat_func, initial, list];
            let result = vm.call_higher_order_stdlib("fold", &args);
            
            match result {
                Ok(StdlibValue::String(s)) => {
                    assert_eq!(s, "Hello World");
                },
                Ok(other) => panic!("Expected String, got {:?}", other),
                Err(e) => panic!("Fold failed: {}", e),
            }
        }
    }
    
    mod error_handling {
        use super::*;
        
        #[test]
        fn test_map_wrong_arg_count() {
            let mut vm = create_test_vm();
            
            let result = vm.call_higher_order_stdlib("map", &[]);
            assert!(result.is_err());
            
            let result = vm.call_higher_order_stdlib("map", &[StdlibValue::Integer(42)]);
            assert!(result.is_err());
        }
        
        #[test]
        fn test_map_non_list_argument() {
            let mut vm = create_test_vm();
            
            let func = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let not_a_list = StdlibValue::Integer(42);
            let args = vec![func, not_a_list];
            let result = vm.call_higher_order_stdlib("map", &args);
            
            assert!(result.is_err());
            match result {
                Err(e) => assert!(e.to_string().contains("must be a list")),
                Ok(_) => panic!("Expected error for non-list argument"),
            }
        }
        
        #[test]
        fn test_filter_non_boolean_result() {
            let mut bytecode = Bytecode::new();
            
            // Create a function that returns an integer instead of boolean
            let mut func_chunk = BytecodeChunk::new(Some("bad_predicate".to_string()));
            func_chunk.add_instruction(Instruction::with_arg(Opcode::Load, 0)); // Just return the argument
            func_chunk.add_instruction(Instruction::new(Opcode::Return));
            bytecode.chunks.push(func_chunk);
            
            // Main chunk
            let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
            main_chunk.add_instruction(Instruction::new(Opcode::Halt));
            bytecode.chunks.push(main_chunk);
            bytecode.main_chunk = 1;
            
            let mut vm = VM::new(bytecode);
            
            let bad_pred = StdlibValue::Function {
                chunk_id: 0,
                env: vec![],
            };
            
            let list = StdlibValue::List(vec![StdlibValue::Integer(42)]);
            let args = vec![bad_pred, list];
            let result = vm.call_higher_order_stdlib("filter", &args);
            
            assert!(result.is_err());
            match result {
                Err(e) => assert!(e.to_string().contains("must return boolean")),
                Ok(_) => panic!("Expected error for non-boolean predicate result"),
            }
        }
        
        #[test]
        fn test_unknown_higher_order_function() {
            let mut vm = create_test_vm();
            
            let result = vm.call_higher_order_stdlib("unknown_function", &[]);
            assert!(result.is_err());
            match result {
                Err(e) => assert!(e.to_string().contains("Unknown higher-order function")),
                Ok(_) => panic!("Expected error for unknown function"),
            }
        }
    }
}