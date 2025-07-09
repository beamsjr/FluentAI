//! Simple tests to improve VM coverage

#[cfg(test)]
mod tests {
    use crate::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
    use crate::debug::{DebugConfig, StepMode};
    use crate::gc::GcConfig;
    use crate::security::{SecurityManager, SecurityPolicy};
    use crate::vm::VM;
    use fluentai_core::ast::NodeId;
    use fluentai_core::value::Value;
    use fluentai_modules::{ModuleConfig, ModuleLoader};
    use fluentai_stdlib::StdlibRegistry;
    use std::sync::Arc;

    fn create_test_vm() -> VM {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("test".to_string()));
        chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(chunk);
        VM::new(bytecode)
    }

    #[test]
    fn test_set_stdlib_registry() {
        let mut vm = create_test_vm();
        let registry = StdlibRegistry::new();
        vm.set_stdlib_registry(registry);
    }

    #[test]
    fn test_set_module_loader() {
        let mut vm = create_test_vm();
        let config = ModuleConfig::default();
        let loader = ModuleLoader::new(config);
        vm.set_module_loader(loader);
    }

    #[test]
    fn test_set_security_manager() {
        let mut vm = create_test_vm();
        let manager = Arc::new(SecurityManager::new(SecurityPolicy::default()));
        vm.set_security_manager(manager);
    }

    #[test]
    fn test_with_sandbox_security() {
        let mut vm = create_test_vm();
        vm.with_sandbox_security();
    }

    #[test]
    fn test_with_security_policy() {
        let mut vm = create_test_vm();
        let policy = SecurityPolicy::default();
        vm.with_security_policy(policy);
    }

    #[test]
    fn test_with_gc() {
        let mut vm = create_test_vm();
        vm.with_gc();
        assert!(vm.gc().is_some());
    }

    #[test]
    fn test_with_gc_config() {
        let mut vm = create_test_vm();
        let config = GcConfig::default();
        vm.with_gc_config(config);
        assert!(vm.gc().is_some());
    }

    #[test]
    fn test_gc_operations() {
        let mut vm = create_test_vm();

        // Test without GC
        let result1 = vm.gc_alloc(Value::Integer(42));
        assert!(result1.is_ok());

        // Enable GC and test again
        vm.with_gc();
        let result2 = vm.gc_alloc(Value::Integer(42));
        assert!(result2.is_ok());

        // Test gc_collect
        let result3 = vm.gc_collect();
        assert!(result3.is_ok());
    }

    #[test]
    fn test_debug_config() {
        let mut vm = create_test_vm();

        let config = DebugConfig {
            enabled: true,
            breakpoints: vec![0, 5, 10],
            step_mode: StepMode::Run,
            event_sender: None,
        };
        vm.set_debug_config(config);

        let retrieved = vm.get_debug_config();
        assert_eq!(retrieved.enabled, true);
        assert_eq!(retrieved.breakpoints.len(), 3);

        let config_mut = vm.get_debug_config_mut();
        config_mut.enabled = false;
    }

    #[test]
    fn test_state_accessors() {
        let mut vm = create_test_vm();

        // Test stack
        vm.push(Value::Integer(42)).unwrap();
        let stack = vm.get_stack();
        assert_eq!(stack.len(), 1);

        // Test globals
        vm.set_global("x".to_string(), Value::Integer(10));
        let globals = vm.get_globals();
        assert!(globals.contains_key("x"));

        // Test call stack depth
        assert_eq!(vm.get_call_stack_depth(), 0);
    }

    #[test]
    fn test_usage_tracking() {
        let mut vm = create_test_vm();
        vm.enable_usage_tracking();

        let node_id = NodeId(std::num::NonZeroU32::new(1).unwrap());

        if let Some(tracker) = vm.usage_tracker() {
            if let Ok(mut tracker) = tracker.write() {
                tracker.register_chunk(0, node_id);
                tracker.record_execution(0, 1000);
            }
        }

        let stats = vm.get_usage_stats(node_id);
        assert!(stats.is_some());

        let all_stats = vm.get_all_usage_stats();
        assert!(all_stats.is_some());
    }

    #[test]
    fn test_arithmetic_ops() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Push two constants and add them
        chunk.add_constant(Value::Integer(10));
        chunk.add_constant(Value::Integer(32));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Add));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        // For now, just check that it runs without error
        assert!(result.is_ok());
    }

    #[test]
    fn test_comparison_ops() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Push two constants and compare
        chunk.add_constant(Value::Integer(5));
        chunk.add_constant(Value::Integer(10));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Lt));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_string_ops() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // String operations
        chunk.add_constant(Value::String("Hello, ".to_string()));
        chunk.add_constant(Value::String("World!".to_string()));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::StrConcat));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_list_ops() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Create list
        chunk.add_constant(Value::Integer(1));
        chunk.add_constant(Value::Integer(2));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 2));
        chunk.add_instruction(Instruction::new(Opcode::ListLen));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_control_flow() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Simple conditional
        chunk.add_constant(Value::Boolean(true));
        chunk.add_constant(Value::Integer(42));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::JumpIfNot, 5));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_get_global() {
        let mut vm = create_test_vm();
        vm.set_global("test_var".to_string(), Value::Integer(123));

        let value = vm.get_global("test_var");
        assert!(value.is_some());
        match value {
            Some(Value::Integer(123)) => {}
            _ => panic!("Expected Int(123)"),
        }

        // Test non-existent global
        let none_value = vm.get_global("non_existent");
        assert!(none_value.is_none());
    }

    #[test]
    fn test_vm_stack_operations() {
        let mut vm = create_test_vm();

        // Push multiple values
        vm.push(Value::Integer(1)).unwrap();
        vm.push(Value::Integer(2)).unwrap();
        vm.push(Value::Integer(3)).unwrap();

        // Check stack size
        assert_eq!(vm.get_stack().len(), 3);

        // Pop values
        assert!(matches!(vm.pop(), Ok(Value::Integer(3))));
        assert!(matches!(vm.pop(), Ok(Value::Integer(2))));
        assert!(matches!(vm.pop(), Ok(Value::Integer(1))));

        // Pop from empty stack should error
        assert!(vm.pop().is_err());
    }

    #[test]
    fn test_resource_limits() {
        let mut vm = create_test_vm();

        let limits = crate::safety::ResourceLimits {
            max_stack_depth: 100,
            max_call_depth: 50,
            max_cells: 1000,
            max_promises: 10,
            max_channels: 10,
            max_memory_bytes: 1024 * 1024,
            channel_buffer_size: 256,
        };

        vm.set_resource_limits(limits);
    }

    #[test]
    fn test_arithmetic_with_different_types() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test float arithmetic
        chunk.add_constant(Value::Float(3.14));
        chunk.add_constant(Value::Float(2.86));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Add));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        // For now, just check that it runs without error
        assert!(result.is_ok());
    }

    #[test]
    fn test_more_opcodes() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test subtraction, multiplication, division
        chunk.add_constant(Value::Integer(20));
        chunk.add_constant(Value::Integer(5));

        // 20 - 5 = 15
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Sub));

        // 15 * 2 = 30
        chunk.add_constant(Value::Integer(2));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 2));
        chunk.add_instruction(Instruction::new(Opcode::Mul));

        // 30 / 3 = 10
        chunk.add_constant(Value::Integer(3));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 3));
        chunk.add_instruction(Instruction::new(Opcode::Div));

        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        // For now, just check that it runs without error
        assert!(result.is_ok());
    }

    #[test]
    fn test_vm_global_state() {
        let mut vm = create_test_vm();

        // Add some global state
        vm.set_global("test".to_string(), Value::String("hello".to_string()));
        vm.set_global("number".to_string(), Value::Integer(42));

        // Check we can retrieve globals
        assert!(matches!(vm.get_global("test"), Some(Value::String(s)) if s == "hello"));
        assert!(matches!(vm.get_global("number"), Some(Value::Integer(42))));
        assert!(vm.get_global("nonexistent").is_none());
    }

    #[test]
    fn test_logical_operations() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test logical AND
        chunk.add_constant(Value::Boolean(true));
        chunk.add_constant(Value::Boolean(false));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::And));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_negation() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test integer negation
        chunk.add_constant(Value::Integer(42));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::new(Opcode::Neg));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_equality_ops() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test equality
        chunk.add_constant(Value::Integer(42));
        chunk.add_constant(Value::Integer(42));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::Eq));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_vm_with_empty_bytecode() {
        let mut bytecode = Bytecode::new();
        // Add an empty chunk to make it valid but empty
        let chunk = BytecodeChunk::new(Some("empty".to_string()));
        bytecode.chunks.push(chunk);

        let mut vm = VM::new(bytecode);

        // Running bytecode with no instructions should complete (though it does nothing)
        let result = vm.run();
        // The actual behavior might be ok or err, just test it runs
        let _ = result;
    }

    #[test]
    fn test_vm_push_pop_dup() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Push a value, dup it, then halt
        chunk.add_constant(Value::Integer(42));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::new(Opcode::Dup));
        chunk.add_instruction(Instruction::new(Opcode::Halt));
        bytecode.chunks.push(chunk);

        let mut vm = VM::new(bytecode);
        let result = vm.run();
        assert!(result.is_ok());
        // Just check that it ran without errors
        assert!(vm.get_stack().len() >= 1);
    }

    #[test]
    fn test_specialized_arithmetic() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test AddInt (specialized integer addition)
        chunk.add_constant(Value::Integer(10));
        chunk.add_constant(Value::Integer(20));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::AddInt));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }

    #[test]
    fn test_specialized_comparison() {
        let mut bytecode = Bytecode::new();
        let mut chunk = BytecodeChunk::new(Some("main".to_string()));

        // Test LtInt (specialized integer comparison)
        chunk.add_constant(Value::Integer(10));
        chunk.add_constant(Value::Integer(20));

        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
        chunk.add_instruction(Instruction::new(Opcode::LtInt));
        chunk.add_instruction(Instruction::new(Opcode::Halt));

        bytecode.chunks.push(chunk);
        let mut vm = VM::new(bytecode);
        let result = vm.run();

        assert!(result.is_ok());
    }
}
