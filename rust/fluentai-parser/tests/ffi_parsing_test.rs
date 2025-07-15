//! Tests for FFI (Foreign Function Interface) parsing

use anyhow::Result;
use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

#[test]
fn test_basic_extern_c_block() -> Result<()> {
    let source = r#"
extern "C" {
    private function printf(format: string, value: int) -> int;
    public function malloc(size: size_t) -> pointer;
    private function free(ptr: pointer);
}
"#;

    let graph = parse_flc(source)?;
    
    // Find the Extern node
    let extern_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Extern { .. }))
        .expect("Should find Extern node");
    
    if let Node::Extern { abi, functions } = &extern_node.1 {
        assert_eq!(abi, "C");
        assert_eq!(functions.len(), 3);
        
        // Check printf
        assert_eq!(functions[0].name, "printf");
        assert!(!functions[0].is_public);
        assert_eq!(functions[0].param_types, vec!["string", "int"]);
        assert_eq!(functions[0].return_type, Some("int".to_string()));
        
        // Check malloc
        assert_eq!(functions[1].name, "malloc");
        assert!(functions[1].is_public);
        assert_eq!(functions[1].param_types, vec!["size_t"]);
        assert_eq!(functions[1].return_type, Some("pointer".to_string()));
        
        // Check free
        assert_eq!(functions[2].name, "free");
        assert!(!functions[2].is_public);
        assert_eq!(functions[2].param_types, vec!["pointer"]);
        assert_eq!(functions[2].return_type, None);
    } else {
        panic!("Expected Extern node");
    }
    
    Ok(())
}

#[test]
fn test_extern_with_exports() -> Result<()> {
    let source = r#"
mod math_ffi;

extern "C" {
    public function sin(x: f64) -> f64;
    public function cos(x: f64) -> f64;
    private function internal_math_helper(x: f64) -> f64;
}
"#;

    let graph = parse_flc(source)?;
    
    // Check that public extern functions are exported
    let exports = graph.graph_metadata.get("exports")
        .and_then(|s| serde_json::from_str::<Vec<serde_json::Value>>(s).ok())
        .expect("Expected exports metadata");
    
    let export_names: Vec<&str> = exports.iter()
        .filter_map(|e| e["name"].as_str())
        .collect();
    
    assert!(export_names.contains(&"sin"));
    assert!(export_names.contains(&"cos"));
    assert!(!export_names.contains(&"internal_math_helper"));
    
    Ok(())
}

#[test]
fn test_extern_empty_block() -> Result<()> {
    let source = r#"
extern "C" {
}
"#;

    let graph = parse_flc(source)?;
    
    // Find the Extern node
    let extern_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Extern { .. }))
        .expect("Should find Extern node");
    
    if let Node::Extern { abi, functions } = &extern_node.1 {
        assert_eq!(abi, "C");
        assert_eq!(functions.len(), 0);
    } else {
        panic!("Expected Extern node");
    }
    
    Ok(())
}

#[test]
fn test_extern_function_no_params() -> Result<()> {
    let source = r#"
extern "C" {
    private function get_errno() -> int;
}
"#;

    let graph = parse_flc(source)?;
    
    // Find the Extern node
    let extern_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Extern { .. }))
        .expect("Should find Extern node");
    
    if let Node::Extern { abi, functions } = &extern_node.1 {
        assert_eq!(abi, "C");
        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].name, "get_errno");
        assert_eq!(functions[0].param_types.len(), 0);
        assert_eq!(functions[0].return_type, Some("int".to_string()));
    } else {
        panic!("Expected Extern node");
    }
    
    Ok(())
}

#[test]
fn test_extern_function_void_return() -> Result<()> {
    let source = r#"
extern "C" {
    private function set_flag(flag: bool);
}
"#;

    let graph = parse_flc(source)?;
    
    // Find the Extern node
    let extern_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Extern { .. }))
        .expect("Should find Extern node");
    
    if let Node::Extern { abi, functions } = &extern_node.1 {
        assert_eq!(abi, "C");
        assert_eq!(functions.len(), 1);
        assert_eq!(functions[0].name, "set_flag");
        assert_eq!(functions[0].param_types, vec!["bool"]);
        assert_eq!(functions[0].return_type, None); // No return type means void
    } else {
        panic!("Expected Extern node");
    }
    
    Ok(())
}

#[test]
fn test_different_abi() -> Result<()> {
    let source = r#"
extern "stdcall" {
    private function windows_api_call(hwnd: pointer) -> int;
}
"#;

    let graph = parse_flc(source)?;
    
    // Find the Extern node
    let extern_node = graph.nodes.iter()
        .find(|(_, node)| matches!(node, Node::Extern { .. }))
        .expect("Should find Extern node");
    
    if let Node::Extern { abi, functions } = &extern_node.1 {
        assert_eq!(abi, "stdcall");
        assert_eq!(functions.len(), 1);
    } else {
        panic!("Expected Extern node");
    }
    
    Ok(())
}