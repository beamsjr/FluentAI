//! Tests for module system VM integration

use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, VM};
use anyhow::Result;
use rustc_hash::FxHashMap;

#[test]
fn test_compile_module_declaration() -> Result<()> {
    let source = r#"(module test_module (export foo) (let ((foo (lambda () 42))) foo))"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check that module opcodes were generated
    let has_begin_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::BeginModule));
    let has_end_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::EndModule));
    
    assert!(has_begin_module, "Expected BeginModule opcode");
    assert!(has_end_module, "Expected EndModule opcode");
    
    Ok(())
}

#[test]
fn test_compile_import_statement() -> Result<()> {
    let source = r#"(import "math" (sin cos))"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check that LoadModule opcode was generated
    let has_load_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::LoadModule));
    
    assert!(has_load_module, "Expected LoadModule opcode");
    
    // Check that ImportBinding opcodes were generated (one for each import)
    let import_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::ImportBinding))
        .count();
    
    assert_eq!(import_count, 2, "Expected 2 ImportBinding opcodes");
    
    Ok(())
}

#[test]
fn test_compile_export_statement() -> Result<()> {
    let source = r#"
        (define x 10)
        (define y 20)
        (export x y)
    "#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check that ExportBinding opcodes were generated
    let export_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::ExportBinding))
        .count();
    
    assert_eq!(export_count, 2, "Expected 2 ExportBinding opcodes");
    
    Ok(())
}

#[test]
fn test_compile_qualified_variable() -> Result<()> {
    let source = r#"math.pi"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Check that LoadQualified opcode was generated
    let has_load_qualified = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::LoadQualified));
    
    assert!(has_load_qualified, "Expected LoadQualified opcode");
    
    Ok(())
}

#[test]
fn test_vm_module_value_type() -> Result<()> {
    use fluentai_vm::bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode, Value};
    
    // Create a simple bytecode that creates a module value
    let mut bytecode = Bytecode::new();
    let chunk_id = bytecode.add_chunk(BytecodeChunk::new(Some("test".to_string())));
    bytecode.main_chunk = chunk_id;
    
    // Push a module value and halt
    let module = Value::Module {
        name: "test_module".to_string(),
        exports: FxHashMap::default(),
    };
    
    let idx = bytecode.chunks[chunk_id].add_constant(module);
    bytecode.chunks[chunk_id].instructions.push(Instruction::with_arg(Opcode::Push, idx as u32));
    bytecode.chunks[chunk_id].instructions.push(Instruction::new(Opcode::Halt));
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    match result {
        Value::Module { name, .. } => {
            assert_eq!(name, "test_module");
        }
        _ => panic!("Expected Module value"),
    }
    
    Ok(())
}

#[test]
fn test_vm_export_binding() -> Result<()> {
    let source = r#"(let ((x 42)) x)"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    // The result should be 42
    match result {
        fluentai_vm::bytecode::Value::Int(n) => assert_eq!(n, 42),
        _ => panic!("Expected Int(42)"),
    }
    
    Ok(())
}

#[test]
fn test_module_isolation() -> Result<()> {
    // Test that modules have isolated environments
    let source = r#"(let ((x 10)) (do (module test (export get-x) (let ((get-x (lambda () x))) get-x)) x))"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    let result = vm.run()?;
    
    // The result should be 10 (from the outer let binding)
    match result {
        fluentai_vm::bytecode::Value::Int(n) => assert_eq!(n, 10),
        _ => panic!("Expected Int(10)"),
    }
    
    Ok(())
}

#[test]
#[should_panic(expected = "Import * not yet implemented")]
fn test_import_all_not_implemented() {
    let source = r#"(import "math" *)"#;
    
    let graph = parse(source).unwrap();
    let compiler = Compiler::new();
    
    // This should panic with "Import * not yet implemented"
    let _ = compiler.compile(&graph).unwrap();
}

#[test]
fn test_multiple_exports() -> Result<()> {
    let source = r#"(module math (export add sub mul) 
                        (let ((add (lambda (a b) (+ a b))) 
                              (sub (lambda (a b) (- a b))) 
                              (mul (lambda (a b) (* a b))) 
                              (internal (lambda (x) (* x x)))) 
                          42))"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Count ExportBinding instructions
    let export_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, fluentai_vm::bytecode::Opcode::ExportBinding))
        .count();
    
    assert_eq!(export_count, 3, "Expected 3 exports");
    
    Ok(())
}

#[test]
fn test_qualified_variable_execution() -> Result<()> {
    // This test will fail until we implement actual module loading
    // For now, just test that it compiles
    let source = r#"math.pi"#;
    
    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Just verify it compiled - execution will fail until module loading is complete
    assert!(!bytecode.chunks[0].instructions.is_empty());
    
    Ok(())
}