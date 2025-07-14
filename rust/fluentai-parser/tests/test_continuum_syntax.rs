use fluentai_parser::flc_parser::Parser;

#[test]
fn test_simple_surface() {
    let code = r#"
surface app {
    element button {
        content: "Click me"
    }
}
"#;

    let mut parser = Parser::new(code);
    let result = parser.parse();
    
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    
    let graph = result.unwrap();
    assert!(graph.root_id.is_some());
    
    println!("Successfully parsed Continuum surface!");
}

#[test]
fn test_state_field() {
    let code = r#"
private state_field counter: int = 0
"#;

    let mut parser = Parser::new(code);
    let result = parser.parse();
    
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    println!("Successfully parsed state field!");
}

#[test]
fn test_surface_with_state() {
    let code = r#"
private state_field count: int = 0

surface counter_app {
    element text {
        content: "Count: {count}"
    }
    element button {
        content: "Increment",
        on_click: () => disturb count
    }
}
"#;

    let mut parser = Parser::new(code);
    let result = parser.parse();
    
    assert!(result.is_ok(), "Failed to parse: {:?}", result.err());
    println!("Successfully parsed surface with state!");
}