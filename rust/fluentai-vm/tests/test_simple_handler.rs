//! Simple test to debug handler functionality

use fluentai_parser::parse;
use fluentai_vm::{Compiler, Value, VM};

#[test]
fn test_simple_handler() {
    // Test handler expressions using FLC syntax
    let code = r#"
        handle {
            perform Error.raise()
        } with {
            Error.raise() => 99
        }
    "#;

    let graph = parse(code).expect("Should parse");
    println!("AST: {:#?}", graph);

    let bytecode = Compiler::new().compile(&graph).expect("Should compile");
    println!("Bytecode chunks: {}", bytecode.chunks.len());
    for (i, chunk) in bytecode.chunks.iter().enumerate() {
        println!("Chunk {}: {} instructions", i, chunk.instructions.len());
        for (j, instr) in chunk.instructions.iter().enumerate() {
            println!("  [{}] {:?}", j, instr);
        }
        println!("  Constants: {:?}", chunk.constants);
    }

    let mut vm = VM::new(bytecode);
    let result = vm.run().expect("Should execute");

    // Handler should intercept the effect
    match result {
        Value::Integer(99) => {}
        _ => panic!("Expected 99, got {:?}", result),
    }
}
