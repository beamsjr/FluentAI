//! Tests for symbol interning in JIT compilation

use fluentai_jit::{JitCompiler, value::{TaggedValue, value_to_tagged}};
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use fluentai_core::value::Value;

#[test]
fn test_symbol_interning() {
    let mut jit = JitCompiler::new().unwrap();
    
    // Create bytecode that returns a symbol
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Add a constant symbol
    let symbol_idx = chunk.add_constant(Value::Symbol("test_symbol".to_string()));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, symbol_idx as u32));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Compile and run
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Verify we get the correct symbol back
    match result {
        Value::Symbol(s) => assert_eq!(s, "test_symbol"),
        _ => panic!("Expected symbol, got {:?}", result),
    }
}

#[test]
fn test_symbol_identity() {
    // Test that the same symbol string gets the same ID
    let symbol1 = Value::Symbol("hello".to_string());
    let symbol2 = Value::Symbol("hello".to_string());
    let symbol3 = Value::Symbol("world".to_string());
    
    let tagged1 = value_to_tagged(&symbol1);
    let tagged2 = value_to_tagged(&symbol2);
    let tagged3 = value_to_tagged(&symbol3);
    
    // Same symbols should have same representation
    assert_eq!(tagged1.0, tagged2.0);
    // Different symbols should have different representation
    assert_ne!(tagged1.0, tagged3.0);
    
    // Converting back should preserve the symbol
    assert_eq!(tagged1.to_value(), symbol1);
    assert_eq!(tagged2.to_value(), symbol2);
    assert_eq!(tagged3.to_value(), symbol3);
}

#[test]
fn test_symbol_round_trip() {
    // Test various symbols can round-trip through the tagging system
    let symbols = vec![
        "foo",
        "bar",
        "test_symbol_123",
        "with-dashes",
        "with_underscores",
        "CamelCase",
        "snake_case",
        "SCREAMING_CASE",
        "",  // Empty symbol
        "🦀", // Unicode
    ];
    
    for symbol_str in symbols {
        let value = Value::Symbol(symbol_str.to_string());
        let tagged = value_to_tagged(&value);
        let recovered = tagged.to_value();
        
        assert_eq!(value, recovered, "Symbol '{}' failed round-trip", symbol_str);
    }
}

#[test]
fn test_symbol_in_list() {
    let mut jit = JitCompiler::new().unwrap();
    
    // Create bytecode that makes a list with symbols
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("test".to_string()));
    
    // Push two symbols
    let sym1_idx = chunk.add_constant(Value::Symbol("first".to_string()));
    let sym2_idx = chunk.add_constant(Value::Symbol("second".to_string()));
    
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, sym1_idx as u32));
    chunk.add_instruction(Instruction::with_arg(Opcode::Push, sym2_idx as u32));
    chunk.add_instruction(Instruction::with_arg(Opcode::MakeList, 2));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    
    bytecode.chunks.push(chunk);
    bytecode.main_chunk = 0;
    
    // Compile and run
    let result = jit.compile_and_run(&bytecode).unwrap();
    
    // Verify the list contains the correct symbols
    match result {
        Value::List(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], Value::Symbol("first".to_string()));
            assert_eq!(items[1], Value::Symbol("second".to_string()));
        }
        _ => panic!("Expected list, got {:?}", result),
    }
}