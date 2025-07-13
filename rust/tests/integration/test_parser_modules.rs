//! Test module parsing


fn main() {
    // Test the exact syntax from the parser test
    let test_cases = vec![
        r#"(module math (export sin cos) (lambda (x) x))"#,
        r#"(module utils (+ 1 2))"#,
        r#"(import "math" (sin cos))"#,
        r#"(export add multiply)"#,
        r#"math.pi"#,
    ];
    
    for (i, source) in test_cases.iter().enumerate() {
        println!("\nTest case {}: {}", i + 1, source);
        match parse(source) {
            Ok(graph) => {
                println!("  ✓ Parsed successfully");
                if let Some(root_id) = graph.root_id {
                    let node = graph.get_node(root_id).unwrap();
                    println!("  Root node: {:?}", node);
                }
            }
            Err(e) => {
                println!("  ✗ Parse error: {:?}", e);
            }
        }
    }
}