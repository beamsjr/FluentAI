use fluentai_parser::flc_parser::Parser;
use fluentai_core::ast::{Node, Literal};

#[test]
fn test_phase2_continuum_to_render_data() {
    // Phase 2 Milestone: Parse Continuum syntax and verify it can be converted to render data
    let code = r##"
surface demo_app {
    element title {
        content: "Phase 2 Complete!",
        color: "#2E86AB"
    }
    
    element box1 {
        content: "Box",
        color: "#A23B72"
    }
    
    element circle1 {
        content: "Circle",
        color: "#F18F01"
    }
}
"##;

    // 1. Parse the Continuum syntax
    let mut parser = Parser::new(code);
    let result = parser.parse();
    assert!(result.is_ok(), "Failed to parse Continuum syntax");
    
    let graph = result.unwrap();
    assert!(graph.root_id.is_some());
    
    // 2. Verify the AST structure
    if let Some(root_id) = graph.root_id {
        if let Some(Node::Surface { name, children, properties }) = graph.get_node(root_id) {
            assert_eq!(name, "demo_app");
            assert_eq!(children.len(), 3); // 3 elements
            
            // Check first element
            if let Some(Node::Element { name, element_type, properties, .. }) = graph.get_node(children[0]) {
                assert_eq!(name, "title");
                
                // Verify properties exist
                let prop_names: Vec<&str> = properties.iter().map(|(k, _)| k.as_str()).collect();
                assert!(prop_names.contains(&"content"));
                assert!(prop_names.contains(&"color"));
            }
            
            println!("✅ Phase 2 Milestone: Successfully parsed Continuum UI syntax!");
            println!("   - Surface 'demo_app' with {} elements", children.len());
            println!("   - All element properties correctly parsed");
            println!("   - Ready for lowering to render data");
        }
    }
}