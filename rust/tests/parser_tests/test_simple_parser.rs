// Simple test to verify parser output
use fluentai_parser::parse_flc;
use fluentai_core::ast::{Node, Literal};

fn main() {
    let code = "1 + 2";
    match parse_flc(code) {
        Ok(graph) => {
            println!("Parsed '{}' successfully!", code);
            println!("Graph has {} nodes", graph.nodes.len());
            
            if let Some(root_id) = graph.root_id {
                println!("Root node ID: {:?}", root_id);
                if let Some(root) = graph.get_node(root_id) {
                    match root {
                        Node::Application { function, args } => {
                            println!("Root is Application node");
                            println!("  Function ID: {:?}", function);
                            println!("  Args: {:?}", args);
                            
                            // Check function node
                            if let Some(func_node) = graph.get_node(*function) {
                                println!("  Function node: {:?}", func_node);
                            }
                            
                            // Check arg nodes
                            for (i, arg_id) in args.iter().enumerate() {
                                if let Some(arg_node) = graph.get_node(*arg_id) {
                                    println!("  Arg {}: {:?}", i, arg_node);
                                }
                            }
                        }
                        _ => println!("Root node: {:?}", root),
                    }
                }
            }
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}