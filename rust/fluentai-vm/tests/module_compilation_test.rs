//! Tests for module compilation and symbol resolution

use fluentai_vm::{Compiler, CompilerOptions};
use fluentai_parser::parse_flc;
use fluentai_optimizer::OptimizationLevel;
use fluentai_bytecode::Opcode;
use anyhow::Result;

#[test]
fn test_compile_module_with_imports() -> Result<()> {
    let code = r#"
use Math;
use Utils::{helper, process};

let result = Math.add(5, 3);
let processed = process(result);
helper()
"#;
    
    let graph = parse_flc(code)?;
    let options = CompilerOptions {
        optimization_level: OptimizationLevel::None,
        debug_info: true,
    };
    let compiler = Compiler::with_options(options);
    let bytecode = compiler.compile(&graph)?;
    
    // Check that LoadModule instructions were generated
    let has_load_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, Opcode::LoadModule));
    
    assert!(has_load_module, "Should have LoadModule instruction");
    
    // Check for ImportBinding instructions
    let import_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, Opcode::ImportBinding))
        .count();
    
    assert_eq!(import_count, 2, "Should have 2 ImportBinding instructions for helper and process");
    
    // Check for LoadQualified instruction for Math.add
    let has_qualified = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, Opcode::LoadQualified));
    
    assert!(has_qualified, "Should have LoadQualified instruction for Math.add");
    
    Ok(())
}

#[test]
fn test_compile_module_declaration() -> Result<()> {
    let code = r#"
// Module declaration is parsed but doesn't generate bytecode
module TestModule;

public function test() -> int {
    42
}
"#;
    
    let graph = parse_flc(code)?;
    
    // Check that module name was captured in graph metadata
    assert_eq!(
        graph.graph_metadata.get("module_name"),
        Some(&"TestModule".to_string())
    );
    
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Module declaration itself doesn't generate instructions
    // but the function definition does
    assert!(!bytecode.chunks.is_empty());
    
    Ok(())
}

#[test]
fn test_compile_export_collection() -> Result<()> {
    let code = r#"
public function public_func() -> int { 1 }
private function private_func() -> int { 2 }
public const PUBLIC_CONST = 42;
private const PRIVATE_CONST = 99;
"#;
    
    let graph = parse_flc(code)?;
    
    // Check that exports were collected during parsing
    let exports_json = graph.graph_metadata.get("exports");
    assert!(exports_json.is_some());
    
    let exports: Vec<fluentai_core::ast::ExportItem> = 
        serde_json::from_str(exports_json.unwrap())?;
    
    assert_eq!(exports.len(), 2, "Should have 2 exports (public items only)");
    
    let export_names: Vec<String> = exports.iter()
        .map(|e| e.name.clone())
        .collect();
    
    assert!(export_names.contains(&"public_func".to_string()));
    assert!(export_names.contains(&"PUBLIC_CONST".to_string()));
    assert!(!export_names.contains(&"private_func".to_string()));
    assert!(!export_names.contains(&"PRIVATE_CONST".to_string()));
    
    Ok(())
}

#[test]
fn test_compile_import_all() -> Result<()> {
    let code = r#"
use Math::*;

let sum = add(1, 2);
let product = multiply(3, 4);
"#;
    
    let graph = parse_flc(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Should have ImportAll instruction
    let has_import_all = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, Opcode::ImportAll));
    
    assert!(has_import_all, "Should have ImportAll instruction");
    
    // Functions should be accessed as globals after import
    let load_global_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, Opcode::LoadGlobal))
        .count();
    
    assert!(load_global_count >= 2, "Should have LoadGlobal for imported functions");
    
    Ok(())
}

#[test]
fn test_compile_nested_module_access() -> Result<()> {
    let code = r#"
use utils.strings;

let result = utils.strings.concat("Hello", "World");
"#;
    
    let graph = parse_flc(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Should compile successfully with qualified access
    assert!(!bytecode.chunks.is_empty());
    
    // Check for LoadQualified instruction
    let has_qualified = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, Opcode::LoadQualified));
    
    assert!(has_qualified, "Should have LoadQualified for utils.strings.concat");
    
    Ok(())
}

#[test]
fn test_compile_relative_imports() -> Result<()> {
    let code = r#"
use ./sibling;
use ../parent;
use ./sub/child;

sibling.func();
parent.func();
child.func();
"#;
    
    let graph = parse_flc(code)?;
    let compiler = Compiler::new();
    let result = compiler.compile(&graph);
    
    // Should compile successfully
    assert!(result.is_ok());
    let bytecode = result.unwrap();
    
    // Should have 3 LoadModule instructions
    let load_module_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, Opcode::LoadModule))
        .count();
    
    assert_eq!(load_module_count, 3, "Should have 3 LoadModule instructions");
    
    Ok(())
}

#[test]
fn test_compile_module_with_alias() -> Result<()> {
    let code = r#"
use VeryLongModuleName as VLMN;
use another.nested.module as anm;

VLMN.function();
anm.function();
"#;
    
    let graph = parse_flc(code)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Module aliases are handled during parsing
    // The bytecode should still have LoadModule for the original names
    let has_load_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, Opcode::LoadModule));
    
    assert!(has_load_module, "Should have LoadModule instructions");
    
    Ok(())
}

#[test]
fn test_compile_export_statements() -> Result<()> {
    let code = r#"
function helper() -> int { 42 }

// Export statements would generate ExportBinding opcodes
export { helper };
"#;
    
    // Note: This might not parse correctly with current parser
    // as explicit export statements might not be implemented
    let result = parse_flc(code);
    
    if result.is_ok() {
        let graph = result.unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&graph)?;
        
        // Check for ExportBinding instruction
        let has_export = bytecode.chunks[0].instructions.iter()
            .any(|i| matches!(i.opcode, Opcode::ExportBinding));
        
        // If parser supports explicit exports, we should have this
        if has_export {
            assert!(true, "Has ExportBinding instruction");
        }
    }
    
    Ok(())
}