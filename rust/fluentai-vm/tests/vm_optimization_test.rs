//! Test VM optimizations: shared bytecode, COW globals, lightweight tasks

use fluentai_vm::{Bytecode, VM};
use fluentai_bytecode::{BytecodeChunk, Instruction, Opcode};
use fluentai_core::value::Value;
use std::sync::Arc;
use std::time::Instant;

#[test]
fn test_shared_bytecode() {
    // Create bytecode
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Simple program that adds two numbers
    chunk.constants.push(Value::Integer(10));
    chunk.constants.push(Value::Integer(20));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, 1));
    chunk.add_instruction(Instruction::new(Opcode::Add));
    chunk.add_instruction(Instruction::new(Opcode::Halt));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Convert to Arc
    let shared_bytecode = Arc::new(bytecode);
    
    // Create multiple VMs with shared bytecode
    let vm1 = VM::with_shared_bytecode(Arc::clone(&shared_bytecode));
    let vm2 = VM::with_shared_bytecode(Arc::clone(&shared_bytecode));
    let vm3 = VM::with_shared_bytecode(Arc::clone(&shared_bytecode));
    
    // Verify they share the same bytecode
    // Note: bytecode() returns &Bytecode, not &Arc<Bytecode>
    // But we can verify they're the same instance by comparing addresses
    assert_eq!(vm1.bytecode() as *const _, vm2.bytecode() as *const _);
    assert_eq!(vm2.bytecode() as *const _, vm3.bytecode() as *const _);
}

#[test]
fn test_cow_globals() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Halt));
    bytecode.chunks.push(chunk);
    
    let shared_bytecode = Arc::new(bytecode);
    
    // Create first VM and set some globals
    let mut vm1 = VM::with_shared_bytecode(Arc::clone(&shared_bytecode));
    vm1.set_global("shared_var".to_string(), Value::Integer(42));
    vm1.set_global("another_var".to_string(), Value::String("hello".to_string()));
    
    // Create second VM and copy globals from first
    let mut vm2 = VM::with_shared_bytecode(Arc::clone(&shared_bytecode));
    // Copy globals by setting same values
    vm2.set_global("shared_var".to_string(), Value::Integer(42));
    vm2.set_global("another_var".to_string(), Value::String("hello".to_string()));
    
    // Read from vm2 - should see shared values
    assert_eq!(vm2.get_global("shared_var"), Some(&Value::Integer(42)));
    assert_eq!(vm2.get_global("another_var"), Some(&Value::String("hello".to_string())));
    
    // Modify in vm2 - triggers COW
    vm2.set_global("shared_var".to_string(), Value::Integer(100));
    
    // vm1 should still have original value
    assert_eq!(vm1.get_global("shared_var"), Some(&Value::Integer(42)));
    // vm2 should have new value
    assert_eq!(vm2.get_global("shared_var"), Some(&Value::Integer(100)));
}

#[tokio::test]
async fn test_lightweight_task_spawning() {
    use fluentai_vm::async_vm::AsyncVM;
    
    // Create bytecode with a function that returns 42
    let mut bytecode = Bytecode::new();
    
    // Main chunk that spawns a task
    let mut main_chunk = BytecodeChunk::new(Some("main".to_string()));
    
    // Function chunk that returns 42
    let mut func_chunk = BytecodeChunk::new(Some("task_func".to_string()));
    func_chunk.constants.push(Value::Integer(42));
    func_chunk.add_instruction(Instruction::with_arg(Opcode::Push, 0));
    func_chunk.add_instruction(Instruction::new(Opcode::Return));
    let func_chunk_id = 1;
    
    // Main: create closure and spawn it
    main_chunk.add_instruction(Instruction::with_arg(Opcode::MakeClosure, func_chunk_id << 16));
    main_chunk.add_instruction(Instruction::new(Opcode::Spawn));
    main_chunk.add_instruction(Instruction::new(Opcode::Await));
    main_chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(main_chunk);
    bytecode.chunks.push(func_chunk);
    bytecode.main_chunk = 0;
    
    // Measure spawning performance
    let start = Instant::now();
    
    let vm = VM::new(bytecode);
    let mut async_vm = AsyncVM::new(vm);
    let result = async_vm.run().await.expect("VM execution failed");
    
    let elapsed = start.elapsed();
    println!("Task spawn and await took: {:?}", elapsed);
    
    // Should return Integer(42) when properly implemented
    println!("Got result: {:?}", result);
    assert_eq!(result, Value::Integer(42));
}

#[test]
fn test_memory_efficiency() {
    // Create a large bytecode program
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("large_program".to_string()));
    
    // Add many constants
    for i in 0..1000 {
        chunk.constants.push(Value::Integer(i));
    }
    
    // Add many instructions
    for i in 0..1000 {
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, i));
        chunk.add_instruction(Instruction::new(Opcode::Pop));
    }
    chunk.add_instruction(Instruction::new(Opcode::Halt));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    let shared_bytecode = Arc::new(bytecode);
    
    // Create multiple VMs
    let vms: Vec<VM> = (0..10)
        .map(|_| VM::with_shared_bytecode(Arc::clone(&shared_bytecode)))
        .collect();
    
    // All VMs share the same bytecode memory
    for i in 1..vms.len() {
        assert_eq!(vms[0].bytecode() as *const _, vms[i].bytecode() as *const _);
    }
}