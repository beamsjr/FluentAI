use fluentai_parser::parse_flc;
use std::fs;

fn main() {
    println!("Testing RL optimization demo parsing...");
    
    let source = match fs::read_to_string("examples/rl_optimization_demo.flc") {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Failed to read demo file: {}", e);
            return;
        }
    };
    
    match parse_flc(&source) {
        Ok(graph) => {
            println!("✅ Demo parsed successfully!");
            println!("Graph has {} nodes", graph.nodes.len());
            if let Some(root) = graph.root_id {
                println!("Root node: {:?}", root);
            }
        }
        Err(e) => {
            eprintln!("❌ Parse error: {:?}", e);
            eprintln!("This indicates a parser issue that needs to be fixed");
        }
    }
}