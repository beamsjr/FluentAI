use fluentai_parser::parse_flc;

fn main() {
    let source = r#"
public let PI = 3.14159;
"#;
    
    match parse_flc(source) {
        Ok(graph) => {
            println!("✓ Parsed successfully!");
            println!("  Total nodes: {}", graph.nodes.len());
            
            // Print all nodes
            for (id, node) in &graph.nodes {
                println!("  Node {:?}: {:?}", id, std::mem::discriminant(node));
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
            
            // Try to find where the error is
            let lines: Vec<&str> = source.lines().collect();
            for (i, line) in lines.iter().enumerate() {
                println!("{:3}: {}", i + 1, line);
            }
        }
    }
}