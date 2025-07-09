use fluentai_parser::parse;

fn main() {
    println!("Testing pattern matching parsing...\n");
    
    // Test 1: Simple match with literal patterns (should work)
    let code1 = r#"
(match 1
  (0 "zero")
  (1 "one")
  (_ "other"))
"#;
    println!("Test 1 - Simple literals:");
    test_parse(code1);
    
    // Test 2: Match with Cons/Nil patterns
    let code2 = r#"
(match (list 1 2 3)
  (Nil "empty")
  ((Cons x xs) x))
"#;
    println!("\nTest 2 - Cons/Nil patterns:");
    test_parse(code2);
    
    // Test 3: Just the Cons pattern part
    let code3 = r#"
(match lst
  ((Cons x xs) x))
"#;
    println!("\nTest 3 - Just Cons pattern:");
    test_parse(code3);
    
    // Test 4: Simpler version
    let code4 = r#"
(match lst
  (Cons x))
"#;
    println!("\nTest 4 - Cons without parens:");
    test_parse(code4);
}

fn test_parse(code: &str) {
    match parse(code) {
        Ok(graph) => {
            println!("✓ Parse successful!");
            println!("  Graph: {} nodes", graph.nodes.len());
        }
        Err(e) => {
            println!("✗ Parse error: {:?}", e);
        }
    }
}