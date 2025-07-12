use fluentai_parser::flc_parser::Parser as FlcParser;
use fluentai_core::ast::{Graph, Node};

fn main() {
    let input = r#"
let x = 10;
x := 42
"#;

    let mut parser = FlcParser::new(input);
    let result = parser.parse();
    
    match result {
        Ok(graph) => {
            println!("Parse successful!");
            println!("Graph nodes:");
            for (id, node) in &graph.nodes {
                println!("  NodeId({}) => {:?}", id.0, node);
            }
            println!("Root: {:?}", graph.root_id);
            
            // Check if all node references are valid
            for (id, node) in &graph.nodes {
                match node {
                    Node::Assignment { target, value } => {
                        println!("\nAssignment node {}:", id.0);
                        println!("  target: NodeId({})", target.0);
                        println!("  value: NodeId({})", value.0);
                        
                        if !graph.nodes.contains_key(target) {
                            println!("  ERROR: target NodeId({}) not found in graph!", target.0);
                        }
                        if !graph.nodes.contains_key(value) {
                            println!("  ERROR: value NodeId({}) not found in graph!", value.0);
                        }
                    }
                    _ => {}
                }
            }
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}