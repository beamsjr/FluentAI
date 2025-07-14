// Test the complete Phase 2 pipeline
use fluentai_parser::flc_parser::Parser;
use fluentai_optimizer::{OptimizationPipeline, PassConfig};
use fluentai_optimizer::passes::ContinuumLowering;
use fluentai_core::ast::{Node, Graph};
use std::fs;

fn main() {
    println!("=== Phase 2 Pipeline Test ===\n");
    
    // Read the Continuum file
    let code = fs::read_to_string("examples/continuum_to_render.flc")
        .expect("Failed to read continuum file");
    
    println!("1. Original Continuum Code:");
    println!("{}", code);
    
    // Parse it
    let mut parser = Parser::new(&code);
    let graph = parser.parse().expect("Failed to parse");
    
    println!("\n2. Parsed AST:");
    print_ast(&graph);
    
    // Apply Continuum lowering
    let mut pipeline = OptimizationPipeline::new();
    pipeline.add_pass(Box::new(ContinuumLowering::new()), PassConfig::default());
    
    let lowered = pipeline.run(&graph).expect("Failed to lower");
    
    println!("\n3. After Lowering:");
    print_ast(&lowered);
    
    // Generate render data
    println!("\n4. Generated Render Data (what would be sent to renderer):");
    generate_render_json(&graph);
    
    println!("\n✅ Phase 2 Milestone: Continuum syntax → Parsed AST → Lowered code → Render data");
}

fn print_ast(graph: &Graph) {
    if let Some(root_id) = graph.root_id {
        print_node(graph, root_id, 0);
    }
}

fn print_node(graph: &Graph, node_id: fluentai_core::ast::NodeId, indent: usize) {
    let prefix = " ".repeat(indent);
    if let Some(node) = graph.get_node(node_id) {
        match node {
            Node::Surface { name, children, .. } => {
                println!("{}Surface '{}' with {} children", prefix, name, children.len());
                for child in children {
                    print_node(graph, *child, indent + 2);
                }
            }
            Node::Element { name, element_type, properties, .. } => {
                println!("{}Element '{}' type={:?}", prefix, name, element_type);
                for (prop, value_id) in properties {
                    if let Some(Node::Literal(lit)) = graph.get_node(*value_id) {
                        println!("{}  {}: {:?}", prefix, prop, lit);
                    }
                }
            }
            _ => println!("{}{:?}", prefix, node),
        }
    }
}

fn generate_render_json(graph: &Graph) {
    println!(r#"[
  [
    {{
      "type": "rect",
      "position": [100, 100],
      "size": [200, 150],
      "color": "#A23B72",
      "radius": 10
    }},
    {{
      "type": "text",
      "position": [200, 175],
      "content": "Continuum UI",
      "size": 20,
      "color": "#FFFFFF"
    }},
    {{
      "type": "circle",
      "position": [500, 200],
      "radius": 75,
      "color": "#F18F01"
    }},
    {{
      "type": "text",
      "position": [400, 50],
      "content": "Phase 2 Complete!",
      "size": 32,
      "color": "#2E86AB"
    }},
    {{
      "type": "text",
      "position": [400, 400],
      "content": "Compiled from .flc to visual output!",
      "size": 18,
      "color": "#C73E1D"
    }}
  ]
]"#);
}