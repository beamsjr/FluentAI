//! Test case to demonstrate if statement semicolon handling

use fluentai_parser::parse_flc;

#[test]
fn test_if_with_multiple_statements_no_semicolons() {
    // This is the pattern used in existing tests
    let source = r#"
        let x = 10;
        let y = 20;
        
        if (x > 5) {
            y = y + 1
            x = x - 1
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse if with multiple statements: {:?}", result);
}

#[test]
fn test_if_with_semicolons_between_statements() {
    // This is what the user thinks should work
    let source = r#"
        let x = 10;
        let y = 20;
        
        if (x > 5) {
            y = y + 1;
            x = x - 1
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse if with semicolon between statements: {:?}", result);
}

#[test]
fn test_if_with_all_semicolons() {
    // Traditional JavaScript-like syntax
    let source = r#"
        let x = 10;
        let y = 20;
        
        if (x > 5) {
            y = y + 1;
            x = x - 1;
        }
    "#;
    
    let result = parse_flc(source);
    // This might fail based on current parser behavior
    println!("Result with all semicolons: {:?}", result);
}

#[test]
fn test_particle_system_if_statement() {
    // The exact pattern from the particle system demo
    let source = r#"
        let p = {"x": 100, "y": 200, "vx": 1.5, "vy": 2.0};
        let dist = 15;
        
        if (dist > 10) {
            p.vx = p.vx + 1 / dist * 0.05
            p.vy = p.vy + 2 / dist * 0.05
        }
    "#;
    
    let result = parse_flc(source);
    assert!(result.is_ok(), "Failed to parse particle system if statement: {:?}", result);
}