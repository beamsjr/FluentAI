//! Tests for VM safety improvements

use fluentai_core::value::Value;
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use fluentai_vm::compiler::Compiler;
use fluentai_vm::safety::{checked_ops, ResourceLimits};
use fluentai_vm::VM;

#[test]
fn test_integer_overflow_protection() {
    // Test addition overflow
    assert!(checked_ops::add_i64(i64::MAX, 1).is_err());
    assert!(checked_ops::add_i64(i64::MAX - 1, 1).is_ok());

    // Test subtraction overflow
    assert!(checked_ops::sub_i64(i64::MIN, 1).is_err());
    assert!(checked_ops::sub_i64(i64::MIN + 1, 1).is_ok());

    // Test multiplication overflow
    assert!(checked_ops::mul_i64(i64::MAX / 2, 3).is_err());
    assert!(checked_ops::mul_i64(100, 200).is_ok());

    // Test division edge cases
    assert!(checked_ops::div_i64(10, 0).is_err());
    assert!(checked_ops::div_i64(i64::MIN, -1).is_err());
    assert!(checked_ops::div_i64(10, 2).is_ok());

    // Test modulo edge cases
    assert!(checked_ops::mod_i64(10, 0).is_err());
    assert_eq!(checked_ops::mod_i64(i64::MIN, -1).unwrap(), 0);
    assert_eq!(checked_ops::mod_i64(10, 3).unwrap(), 1);

    // Test negation overflow
    assert!(checked_ops::neg_i64(i64::MIN).is_err());
    assert_eq!(checked_ops::neg_i64(5).unwrap(), -5);
}

#[test]
fn test_vm_arithmetic_overflow() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    // Test integer overflow in VM
    // Push MAX and 1, then add
    let max_idx = chunk.add_constant(Value::Integer(i64::MAX));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, max_idx));
    let one_idx = chunk.add_constant(Value::Integer(1));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, one_idx));
    chunk.add_instruction(Instruction::new(Opcode::AddInt));
    chunk.add_instruction(Instruction::new(Opcode::Halt));

    bytecode.add_chunk(chunk);

    let mut vm = VM::new(bytecode);
    let result = vm.run();

    // Should fail with overflow error
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert!(err.to_string().contains("overflow"));
}

#[test]
fn test_resource_limits() {
    // Test cell limits
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    // Try to create many cells
    let val_idx = chunk.add_constant(Value::Integer(42));
    for _ in 0..10 {
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, val_idx));
        chunk.add_instruction(Instruction::new(Opcode::MakeCell));
        chunk.add_instruction(Instruction::new(Opcode::Pop)); // Pop the cell
    }
    let zero_idx = chunk.add_constant(Value::Integer(0));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, zero_idx));
    chunk.add_instruction(Instruction::new(Opcode::Halt));

    bytecode.add_chunk(chunk);

    let mut vm = VM::new(bytecode);

    // Set very restrictive limits
    vm.set_resource_limits(ResourceLimits {
        max_cells: 5,
        ..ResourceLimits::default()
    });

    let result = vm.run();

    // Should fail when trying to create the 6th cell
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert!(err
        .to_string()
        .contains("Resource limit exceeded for cells"));
}

#[test]
fn test_channel_limits() {
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    // Try to create many channels
    for _ in 0..5 {
        chunk.add_instruction(Instruction::new(Opcode::Channel));
        chunk.add_instruction(Instruction::new(Opcode::Pop)); // Pop the channel
    }
    let zero_idx = chunk.add_constant(Value::Integer(0));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, zero_idx));
    chunk.add_instruction(Instruction::new(Opcode::Halt));

    bytecode.add_chunk(chunk);

    let mut vm = VM::new(bytecode);

    // Set very restrictive limits
    vm.set_resource_limits(ResourceLimits {
        max_channels: 3,
        ..ResourceLimits::default()
    });

    let result = vm.run();

    // Should fail when trying to create the 4th channel
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert!(err
        .to_string()
        .contains("Resource limit exceeded for channels"));
}

#[test]
fn test_stack_overflow_protection() {
    // Stack overflow is already tested by existing stack size limit
    // This test verifies it still works
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    // Try to push too many values
    // Pre-create constants to avoid multiple mutable borrows
    let mut const_indices = Vec::new();
    for i in 0..20000 {
        const_indices.push(chunk.add_constant(Value::Integer(i)));
    }
    for idx in const_indices {
        chunk.add_instruction(Instruction::with_arg(Opcode::Push, idx));
    }
    chunk.add_instruction(Instruction::new(Opcode::Halt));

    bytecode.add_chunk(chunk);

    let mut vm = VM::new(bytecode);
    let result = vm.run();

    // Should fail with stack overflow
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert!(err.to_string().contains("Stack overflow"));
}

#[test]
fn test_typed_ids() {
    // Test that promises and channels use typed IDs
    // Create bytecode that uses the Channel opcode directly
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("main".to_string()));

    chunk.add_instruction(Instruction::new(Opcode::Channel));
    chunk.add_instruction(Instruction::new(Opcode::Halt));

    bytecode.add_chunk(chunk);

    let mut vm = VM::new(bytecode);
    let result = vm.run().unwrap();

    // Check that the channel ID has the expected format
    match result {
        Value::Channel(id) => {
            // Channel ID is now a u64
            assert!(id > 0); // Just verify it's a valid ID
        }
        _ => panic!("Expected channel value"),
    }
}

#[test]
fn test_stack_trace_generation() {
    // Create a simple function that will be in the stack trace
    let graph = parse("((x) => x / 0)(42)").unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph).unwrap();

    let mut vm = VM::new(bytecode);
    let result = vm.run();

    // Should fail with division by zero
    assert!(result.is_err());

    // Generate stack trace
    let trace = vm.build_stack_trace();
    assert!(trace.frames.len() > 0);

    // The lambda should be in the trace
    let frame_names: Vec<String> = trace
        .frames
        .iter()
        .map(|f| f.function_name.clone())
        .collect();
    assert!(
        frame_names.contains(&"main".to_string())
            || frame_names.iter().any(|n| n.starts_with("<anonymous"))
    );
}
