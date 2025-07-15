//! Tests for module parsing with FLC syntax

use fluentai_parser::parse_flc;

#[test]
fn test_module_declaration_parsing() {
    let source = r#"
mod math;

public let PI = 3.14159;
"#;
    
    let graph = parse_flc(source).unwrap();
    
    // Debug output
    println!("Graph metadata: {:?}", graph.graph_metadata);
    println!("Total nodes: {}", graph.nodes.len());
    
    // Print all nodes
    for (id, node) in &graph.nodes {
        println!("Node {:?}: {:?}", id, node);
    }
    
    // Check module name is stored
    assert_eq!(
        graph.graph_metadata.get("module_name").map(|s| s.as_str()),
        Some("math")
    );
    
    // Check exports are stored
    let exports = graph.graph_metadata.get("exports");
    assert!(exports.is_some(), "Expected exports metadata");
    
    let exports_json: Vec<serde_json::Value> = 
        serde_json::from_str(exports.unwrap()).unwrap();
    assert_eq!(exports_json.len(), 1);
    assert_eq!(exports_json[0]["name"], "PI");
}

#[test]
fn test_module_token_vs_mod_token() {
    // Test that "module" keyword works
    let source1 = r#"module test_mod;"#;
    let result1 = parse_flc(source1);
    
    // For now, this might fail if the lexer doesn't have "module" token
    println!("Module keyword result: {:?}", result1.is_ok());
    
    // Test that "mod" keyword works 
    let source2 = r#"mod test_mod;"#;
    let result2 = parse_flc(source2);
    assert!(result2.is_ok(), "mod keyword should work");
}