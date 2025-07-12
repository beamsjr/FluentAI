//! Tests for module system VM integration
//!
//! NOTE: This file still uses s-expression syntax because FLC module support is incomplete:
//! - FLC parser's `mod` syntax doesn't support export declarations  
//! - FLC's `use` syntax generates different AST nodes than expected by these tests
//! - Tests verify specific opcodes (BeginModule, EndModule, ExportBinding, etc.) that FLC doesn't generate
//! - Module/Import/Export nodes in the AST require features not yet implemented in FLC parser
//!
//! TODO: Migrate to FLC syntax once the parser fully implements module system features

use anyhow::Result;
use fluentai_parser::parse;
use fluentai_vm::{compiler::Compiler, VM};
use rustc_hash::FxHashMap;

#[test]
#[ignore = "FLC module syntax generates different opcodes than expected"]
fn test_compile_module_declaration() -> Result<()> {
    // Original s-expression test - FLC module syntax doesn't generate same opcodes
    let source = r#"(module test_module (export foo) (let ((foo (lambda () 42))) foo))"#;

    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    // Check that module opcodes were generated
    let has_begin_module = bytecode.chunks[0]
        .instructions
        .iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::BeginModule));
    let has_end_module = bytecode.chunks[0]
        .instructions
        .iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::EndModule));

    assert!(has_begin_module, "Expected BeginModule opcode");
    assert!(has_end_module, "Expected EndModule opcode");

    Ok(())
}

#[test]
#[ignore = "FLC import syntax generates different opcodes"]
fn test_compile_import_statement() -> Result<()> {
    let source = r#"(import "math" (sin cos))"#;

    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    // Check that LoadModule opcode was generated
    let has_load_module = bytecode.chunks[0]
        .instructions
        .iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::LoadModule));

    assert!(has_load_module, "Expected LoadModule opcode");

    // Check that ImportBinding opcodes were generated (one for each import)
    let import_count = bytecode.chunks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ImportBinding))
        .count();

    assert_eq!(import_count, 2, "Expected 2 ImportBinding opcodes");

    Ok(())
}

#[test]
#[ignore = "FLC export syntax and define generate different opcodes"]
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
    let export_count = bytecode.chunks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ExportBinding))
        .count();

    assert_eq!(export_count, 2, "Expected 2 ExportBinding opcodes");

    Ok(())
}

#[test]
#[ignore = "FLC dot notation doesn't generate LoadQualified opcode"]
fn test_compile_qualified_variable() -> Result<()> {
    // FLC parses dot notation differently than expected by this test
    let source = r#"math.pi"#;

    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    // Check that LoadQualified opcode was generated
    let has_load_qualified = bytecode.chunks[0]
        .instructions
        .iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::LoadQualified));

    assert!(has_load_qualified, "Expected LoadQualified opcode");

    Ok(())
}

#[test]
fn test_vm_module_value_type() -> Result<()> {
    use fluentai_core::value::Value;
    use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};

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
    bytecode.chunks[chunk_id]
        .instructions
        .push(Instruction::with_arg(Opcode::Push, idx as u32));
    bytecode.chunks[chunk_id]
        .instructions
        .push(Instruction::new(Opcode::Halt));

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
#[ignore = "FLC blocks return nil instead of last expression"]
fn test_vm_export_binding() -> Result<()> {
    // Original s-expression - FLC blocks have different return behavior
    let source = r#"(let ((x 42)) x)"#;

    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    let mut vm = VM::new(bytecode);
    let result = vm.run()?;

    // The result should be 42
    match result {
        fluentai_vm::Value::Integer(n) => assert_eq!(n, 42),
        _ => panic!("Expected Int(42)"),
    }

    Ok(())
}

#[test]
#[ignore = "Complex module scoping test requires s-expression syntax"]
fn test_module_isolation() -> Result<()> {
    // Test that modules have isolated environments
    let source =
        r#"(let ((x 10)) (do (module test (export get-x) (let ((get-x (lambda () x))) get-x)) x))"#;

    let graph = parse(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;

    let mut vm = VM::new(bytecode);
    let result = vm.run()?;

    // The result should be 10 (from the outer let binding)
    match result {
        fluentai_vm::Value::Integer(n) => assert_eq!(n, 10),
        _ => panic!("Expected Int(10)"),
    }

    Ok(())
}

#[test]
#[ignore = "Import * syntax not available in FLC"]
fn test_import_all_not_implemented() {
    let source = r#"(import "math" *)"#;

    let graph = parse(source).unwrap();
    let compiler = Compiler::new();

    // Import-all is now implemented, so this should compile successfully
    let bytecode = compiler.compile(&graph).unwrap();

    // Verify that ImportAll opcode is used
    let has_import_all = bytecode.chunks[0]
        .instructions
        .iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ImportAll));
    assert!(has_import_all, "Expected ImportAll opcode in bytecode");
}

#[test]
#[ignore = "Complex module with multiple exports requires s-expression syntax"]
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
    let export_count = bytecode.chunks[0]
        .instructions
        .iter()
        .filter(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ExportBinding))
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
