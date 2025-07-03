//! Integration tests for the MCP server

use claudelang_core::documentation::DocumentationRegistry;
use claudelang_vm::{VM, Bytecode};
use claudelang_stdlib::StdlibRegistry;

#[test]
fn test_documentation_registry_integration() {
    let registry = DocumentationRegistry::new();
    
    // Test that we can search and get documentation
    let search_results = registry.search("lambda");
    assert!(!search_results.is_empty());
    
    let lambda_doc = registry.get("Lambda");
    assert!(lambda_doc.is_some());
}

#[test]
fn test_vm_creation() {
    let bytecode = Bytecode::new();
    let vm = VM::new(bytecode);
    
    // VM should be created successfully
    // This is a basic smoke test
    drop(vm);
}

#[test]
fn test_stdlib_registry() {
    let stdlib = StdlibRegistry::new();
    
    // Registry should be created successfully
    drop(stdlib);
}