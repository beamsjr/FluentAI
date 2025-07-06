use fluentai_parser::parse;
use fluentai_core::ast::{Node, EffectType};

fn main() {
    // Test how "do" expressions are parsed
    println!("=== Testing 'do' expression parsing ===");
    let code = "(do (effect IO:print \"hello\") 42)";
    let graph = parse(code).unwrap();
    
    println!("Code: {}", code);
    println!("\nParsed graph:");
    println!("  Total nodes: {}", graph.nodes.len());
    println!("  Root ID: {:?}", graph.root_id);
    
    println!("\nAll nodes:");
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    // Let's trace through the structure
    if let Some(root_id) = graph.root_id {
        println!("\nTracing from root:");
        trace_node(&graph, root_id, 0);
    }
}

fn trace_node(graph: &fluentai_core::ast::Graph, node_id: fluentai_core::ast::NodeId, indent: usize) {
    let prefix = "  ".repeat(indent);
    
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Let { bindings, body } => {
                println!("{}Let:", prefix);
                for (name, binding_id) in bindings {
                    println!("{}  Binding '{}' = NodeId({})", prefix, name, binding_id.get());
                    trace_node(graph, *binding_id, indent + 2);
                }
                println!("{}  Body = NodeId({})", prefix, body.get());
                trace_node(graph, *body, indent + 2);
            }
            Node::Effect { effect_type, operation, args } => {
                println!("{}Effect: {:?}:{} with {} args", prefix, effect_type, operation, args.len());
                for (i, arg) in args.iter().enumerate() {
                    println!("{}  Arg[{}] = NodeId({})", prefix, i, arg.get());
                    trace_node(graph, *arg, indent + 2);
                }
            }
            Node::Literal(lit) => {
                println!("{}Literal: {:?}", prefix, lit);
            }
            _ => {
                println!("{}{:?}", prefix, node);
            }
        }
    }
}