//! Integration tests for the module registry

use fluentai_vm::ModuleRegistry;
use fluentai_core::ast::{Graph, ExportItem};
use fluentai_bytecode::{Bytecode, BytecodeChunk, Instruction, Opcode};
use std::sync::Arc;

#[test]
fn test_module_registry_basic() {
    let mut registry = ModuleRegistry::new();
    
    // Create a test module
    let mut graph = Graph::new();
    graph.graph_metadata.insert("module_name".to_string(), "TestModule".to_string());
    
    // Add some exports
    let exports = vec![
        ExportItem { name: "foo".to_string(), alias: None },
        ExportItem { name: "bar".to_string(), alias: None },
    ];
    let exports_json = serde_json::to_string(&exports).unwrap();
    graph.graph_metadata.insert("exports".to_string(), exports_json);
    
    // Create dummy bytecode
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("TestModule".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    bytecode.add_chunk(chunk);
    let bytecode = Arc::new(bytecode);
    
    // Register the module
    let result = registry.register_module(&graph, bytecode, None);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "TestModule");
    
    // Verify module is registered
    assert!(registry.has_module("TestModule"));
    
    // Get module info
    let module = registry.get_module("TestModule").unwrap();
    assert_eq!(module.name, "TestModule");
    assert_eq!(module.exports.len(), 2);
    assert_eq!(module.exports[0].name, "foo");
    assert_eq!(module.exports[1].name, "bar");
}

#[test]
fn test_module_registry_path_deduplication() {
    let mut registry = ModuleRegistry::new();
    
    // First module with path
    let mut graph1 = Graph::new();
    graph1.graph_metadata.insert("module_name".to_string(), "Module1".to_string());
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("Module1".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    bytecode.add_chunk(chunk);
    let bytecode1 = Arc::new(bytecode);
    
    let path = "/path/to/module.flc".to_string();
    let result1 = registry.register_module(&graph1, bytecode1, Some(path.clone()));
    assert!(result1.is_ok());
    
    // Try to register different module with same path
    let mut graph2 = Graph::new();
    graph2.graph_metadata.insert("module_name".to_string(), "Module2".to_string());
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("Module2".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    bytecode.add_chunk(chunk);
    let bytecode2 = Arc::new(bytecode);
    
    let result2 = registry.register_module(&graph2, bytecode2, Some(path));
    assert!(result2.is_err());
    assert!(result2.unwrap_err().to_string().contains("already loaded"));
}

#[test]
fn test_module_registry_exports() {
    let mut registry = ModuleRegistry::new();
    
    // Create module with exports
    let mut graph = Graph::new();
    graph.graph_metadata.insert("module_name".to_string(), "ExportTest".to_string());
    
    let exports = vec![
        ExportItem { name: "publicFunc".to_string(), alias: None },
        ExportItem { name: "PublicType".to_string(), alias: None },
    ];
    let exports_json = serde_json::to_string(&exports).unwrap();
    graph.graph_metadata.insert("exports".to_string(), exports_json);
    
    let mut bytecode = Bytecode::new();
    let mut chunk = BytecodeChunk::new(Some("ExportTest".to_string()));
    chunk.add_instruction(Instruction::new(Opcode::Return));
    bytecode.add_chunk(chunk);
    let bytecode = Arc::new(bytecode);
    
    registry.register_module(&graph, bytecode, None).unwrap();
    
    // Set export values
    use fluentai_core::value::Value;
    let func_value = Value::String("function_placeholder".to_string());
    let type_value = Value::String("type_placeholder".to_string());
    
    registry.set_export_value("ExportTest", "publicFunc", func_value.clone()).unwrap();
    registry.set_export_value("ExportTest", "PublicType", type_value.clone()).unwrap();
    
    // Get export values
    let retrieved_func = registry.get_export("ExportTest", "publicFunc").unwrap();
    assert_eq!(retrieved_func, func_value);
    
    let retrieved_type = registry.get_export("ExportTest", "PublicType").unwrap();
    assert_eq!(retrieved_type, type_value);
    
    // Try to get non-existent export
    let missing = registry.get_export("ExportTest", "nonExistent");
    assert!(missing.is_err());
    assert!(missing.unwrap_err().to_string().contains("does not export"));
}