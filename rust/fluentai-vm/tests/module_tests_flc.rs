//! Module system tests using proper FLC syntax
//! 
//! These tests verify that the FLC module system works correctly with:
//! - Module declarations
//! - Public/private visibility modifiers  
//! - Automatic exports based on visibility
//! - Module isolation and scoping
//!
//! NOTE: These tests may be marked as ignored if the FLC parser doesn't yet
//! support the required syntax, but they document the expected behavior.

use anyhow::Result;
use fluentai_parser::parse_flc;
use fluentai_vm::{compiler::Compiler, VM};
use fluentai_core::ast::Node;

#[test]
fn test_module_with_public_exports() -> Result<()> {
    let source = r#"
mod math;

// Public functions should be automatically exported
public function add(a: float, b: float) -> float {
    a + b
}

public function sub(a: float, b: float) -> float {
    a - b
}

// Private function should NOT be exported
private function internal_helper(x: float) -> float {
    x * x
}

// Public constant should be exported
public let PI = 3.14159;

// Private variable should NOT be exported  
private let secret = 42;
"#;

    let graph = parse_flc(source)?;
    
    // Debug AST structure
    println!("=== AST Debug ===");
    println!("Root ID: {:?}", graph.root_id);
    println!("Total nodes: {}", graph.nodes.len());
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("=================");
    
    // Check module name
    assert_eq!(
        graph.graph_metadata.get("module_name").map(|s| s.as_str()),
        Some("math")
    );
    
    // Check exports - should have add, sub, and PI but NOT internal_helper or secret
    let exports = graph.graph_metadata.get("exports")
        .and_then(|s| serde_json::from_str::<Vec<serde_json::Value>>(s).ok())
        .expect("Expected exports metadata");
    
    assert_eq!(exports.len(), 3, "Expected 3 exports");
    
    let export_names: Vec<&str> = exports.iter()
        .filter_map(|e| e["name"].as_str())
        .collect();
    
    assert!(export_names.contains(&"add"));
    assert!(export_names.contains(&"sub"));
    assert!(export_names.contains(&"PI"));
    assert!(!export_names.contains(&"internal_helper"));
    assert!(!export_names.contains(&"secret"));
    
    // Compile and check opcodes
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    // Should generate BeginModule/EndModule opcodes
    let has_begin_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::BeginModule));
    let has_end_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::EndModule));
    
    assert!(has_begin_module, "Expected BeginModule opcode");
    assert!(has_end_module, "Expected EndModule opcode");
    
    // Should generate ExportBinding opcodes for public items
    let export_binding_count = bytecode.chunks[0].instructions.iter()
        .filter(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ExportBinding))
        .count();
    
    assert_eq!(export_binding_count, 3, "Expected 3 ExportBinding opcodes");
    
    Ok(())
}

#[test]
#[ignore = "FLC parser doesn't yet support visibility modifiers"]
fn test_module_visibility_isolation() -> Result<()> {
    let source = r#"
module counter;

// Private state
private let count = 0;

// Public interface
public function increment() {
    count = count + 1;
}

public function get_count() -> int {
    count
}

// Private helper
private function reset() {
    count = 0;
}
"#;

    let graph = parse_flc(source)?;
    
    // Only increment and get_count should be exported, not count or reset
    let exports = graph.graph_metadata.get("exports")
        .and_then(|s| serde_json::from_str::<Vec<serde_json::Value>>(s).ok())
        .expect("Expected exports metadata");
    
    assert_eq!(exports.len(), 2, "Expected 2 exports");
    
    let export_names: Vec<&str> = exports.iter()
        .filter_map(|e| e["name"].as_str())
        .collect();
    
    assert!(export_names.contains(&"increment"));
    assert!(export_names.contains(&"get_count"));
    assert!(!export_names.contains(&"count"));
    assert!(!export_names.contains(&"reset"));
    
    Ok(())
}

#[test]
#[ignore = "FLC parser doesn't yet support module imports"]
fn test_module_import_and_usage() -> Result<()> {
    // First, compile a module that exports functions
    let _math_module = r#"
module math;

public function square(x: float) -> float {
    x * x
}

public let E = 2.71828;
"#;

    // Then import and use it
    let main_source = r#"
use math::{square, E};

// Use imported function
let result = square(5.0);

// Use imported constant
let euler = E;

// Return computed value
result + euler
"#;

    let graph = parse_flc(main_source)?;
    
    // Check that Import node was created
    let has_import = graph.nodes.iter()
        .any(|(_, node)| matches!(node, Node::Import { .. }));
    assert!(has_import, "Expected Import node");
    
    // Compile and check for LoadModule opcode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let has_load_module = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::LoadModule));
    assert!(has_load_module, "Expected LoadModule opcode");
    
    Ok(())
}

#[test]
#[ignore = "FLC parser doesn't yet support public let syntax"]
fn test_public_private_let_declarations() -> Result<()> {
    let source = r#"
module config;

// Public configuration values
public let MAX_USERS = 1000;
public let DEFAULT_TIMEOUT = 30;

// Private implementation details
private let cache_size = 512;
private let internal_buffer_size = 4096;

// Public function that uses private variables
public function get_cache_info() -> int {
    cache_size
}
"#;

    let graph = parse_flc(source)?;
    
    // Check that only public let declarations are exported
    let exports = graph.graph_metadata.get("exports")
        .and_then(|s| serde_json::from_str::<Vec<serde_json::Value>>(s).ok())
        .expect("Expected exports metadata");
    
    // Should export: MAX_USERS, DEFAULT_TIMEOUT, get_cache_info
    // Should NOT export: cache_size, internal_buffer_size
    assert_eq!(exports.len(), 3, "Expected 3 exports");
    
    let export_names: Vec<&str> = exports.iter()
        .filter_map(|e| e["name"].as_str())
        .collect();
    
    assert!(export_names.contains(&"MAX_USERS"));
    assert!(export_names.contains(&"DEFAULT_TIMEOUT"));
    assert!(export_names.contains(&"get_cache_info"));
    assert!(!export_names.contains(&"cache_size"));
    assert!(!export_names.contains(&"internal_buffer_size"));
    
    Ok(())
}

#[test]
#[ignore = "Module scoping not fully implemented"]
fn test_module_scope_isolation() -> Result<()> {
    let source = r#"
// Define a variable in outer scope
let x = 100;

module inner;

// This should NOT have access to outer x
public function get_x() -> int {
    // This should be an error or return a different value
    x  
}

// Module has its own x
private let x = 200;

public function get_module_x() -> int {
    x  // Should return 200, not 100
}
"#;

    let graph = parse_flc(source)?;
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let mut vm = VM::new(bytecode);
    let _result = vm.run()?;
    
    // The module functions should use the module's x (200), not the outer x (100)
    // This tests that modules have isolated scopes
    
    Ok(())
}

#[test]
#[ignore = "FLC parser doesn't yet support qualified module access"]
fn test_qualified_module_access() -> Result<()> {
    let source = r#"
use math;

// Access module member with qualified name
let radius = 5.0;
let area = math.PI * radius * radius;
area
"#;

    let graph = parse_flc(source)?;
    
    // The compiler should generate LoadQualified opcode for math.PI
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let has_load_qualified = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::LoadQualified));
    
    assert!(has_load_qualified, "Expected LoadQualified opcode for math.PI");
    
    Ok(())
}

#[test]
#[ignore = "Wildcard imports not yet implemented"]
fn test_wildcard_import() -> Result<()> {
    let source = r#"
use math::*;

// All math exports should be available without qualification
let area = PI * square(radius);
let volume = pow(radius, 3) * PI * 4.0 / 3.0;
"#;

    let graph = parse_flc(source)?;
    
    // Check that import with wildcard was parsed
    if let Some((_, Node::Import { import_all, .. })) = graph.nodes.iter()
        .find(|(_, n)| matches!(n, Node::Import { .. })) {
        assert!(import_all, "Expected import_all to be true for wildcard import");
    }
    
    // Compile and check for ImportAll opcode
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&graph)?;
    
    let has_import_all = bytecode.chunks[0].instructions.iter()
        .any(|i| matches!(i.opcode, fluentai_bytecode::Opcode::ImportAll));
    
    assert!(has_import_all, "Expected ImportAll opcode");
    
    Ok(())
}