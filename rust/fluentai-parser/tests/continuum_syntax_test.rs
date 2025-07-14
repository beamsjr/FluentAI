//! Tests for Continuum UI syntax parsing

use anyhow::Result;
use fluentai_core::ast::Node;
use fluentai_parser::parse_flc;

#[test]
fn test_parse_surface() -> Result<()> {
    let source = r##"
public surface main_window {
    size: (800, 600),
    background: "#f0f0f0",
    
    element title {
        content: "Hello Continuum",
        position: (100, 50),
        color: "#000000"
    }
}
    "##;
    
    let graph = parse_flc(source)?;
    assert!(graph.root_id.is_some());
    
    // Verify the surface node was created
    let root_id = graph.root_id.unwrap();
    if let Some(node) = graph.get_node(root_id) {
        match node {
            Node::Surface { name, .. } => {
                assert_eq!(name, "main_window");
            }
            _ => panic!("Expected Surface node, got {:?}", node),
        }
    }
    
    Ok(())
}

#[test]
fn test_parse_space() -> Result<()> {
    let source = r#"
private space ar_scene {
    anchor: world_floor,
    size: (2, 2, 2)
}
    "#;
    
    let graph = parse_flc(source)?;
    assert!(graph.root_id.is_some());
    
    Ok(())
}

#[test]
fn test_parse_state_field() -> Result<()> {
    let source = r#"
public state_field menu_open: bool = false
    "#;
    
    let graph = parse_flc(source)?;
    assert!(graph.root_id.is_some());
    
    let root_id = graph.root_id.unwrap();
    if let Some(node) = graph.get_node(root_id) {
        match node {
            Node::StateField { name, field_type, initial } => {
                assert_eq!(name, "menu_open");
                assert_eq!(field_type.as_deref(), Some("bool"));
                assert!(initial.is_some());
            }
            _ => panic!("Expected StateField node, got {:?}", node),
        }
    }
    
    Ok(())
}

#[test]
fn test_parse_element_with_when() -> Result<()> {
    let source = r##"
public surface app {
    element menu {
        when menu_open == true {
            visible: true,
            opacity: 1.0
        }
        when menu_open == false {
            visible: false,
            opacity: 0.0
        }
    }
}
    "##;
    
    let graph = parse_flc(source)?;
    assert!(graph.root_id.is_some());
    
    Ok(())
}

#[test]
fn test_parse_disturb() -> Result<()> {
    let source = r##"
public surface app {
    element button {
        content: "Toggle Menu",
        on_click: disturb menu_state
    }
}
    "##;
    
    let graph = parse_flc(source)?;
    assert!(graph.root_id.is_some());
    
    Ok(())
}