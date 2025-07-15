use fluentai_parser::parse_flc;

fn main() {
    let source = r#"
module math;

public let PI = 3.14159;
"#;
    
    match parse_flc(source) {
        Ok(graph) => {
            println!("✓ Parsed successfully!");
            println!("  Module name: {:?}", graph.graph_metadata.get("module_name"));
            println!("  Exports: {:?}", graph.graph_metadata.get("exports"));
            println!("  Total nodes: {}", graph.nodes.len());
            
            // Print all nodes
            for (id, node) in &graph.nodes {
                println!("  Node {:?}: {:?}", id, std::mem::discriminant(node));
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}