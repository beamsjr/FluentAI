use fluentai_parser::parse_flc;
use fluentai_core::ast::Node;

fn main() {
    let source = std::fs::read_to_string("test_module_parser.flc").unwrap();
    
    match parse_flc(&source) {
        Ok(graph) => {
            println!("✓ Parsed successfully!");
            println!("  Total nodes: {}", graph.nodes.len());
            
            // Check module metadata
            if let Some(module_name) = graph.graph_metadata.get("module_name") {
                println!("  Module name: {}", module_name);
            } else {
                println!("  ❌ No module name found in metadata");
            }
            
            // Check exports metadata
            if let Some(exports_json) = graph.graph_metadata.get("exports") {
                match serde_json::from_str::<Vec<serde_json::Value>>(exports_json) {
                    Ok(exports) => {
                        println!("  Exports: {} items", exports.len());
                        for export in &exports {
                            if let Some(name) = export.get("name").and_then(|v| v.as_str()) {
                                println!("    - {}", name);
                            }
                        }
                    }
                    Err(e) => println!("  ❌ Failed to parse exports: {}", e),
                }
            } else {
                println!("  ❌ No exports found in metadata");
            }
            
            // Count Define nodes
            let define_count = graph.nodes.iter()
                .filter(|(_, node)| matches!(node, Node::Define { .. }))
                .count();
            println!("  Define nodes: {}", define_count);
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}