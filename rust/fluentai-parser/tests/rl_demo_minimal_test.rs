use fluentai_parser::parse_flc;

#[test]
fn test_rl_optimization_demo_minimal() {
    let source = include_str!("../../examples/rl_optimization_demo_minimal.flc");
    
    match parse_flc(source) {
        Ok(graph) => {
            println!("✅ Minimal demo parsed successfully!");
            println!("Graph has {} nodes", graph.nodes.len());
            
            // Check for critical nodes
            let mut has_functions = false;
            let mut has_effects = false;
            
            for node in graph.nodes.values() {
                match node {
                    fluentai_core::ast::Node::Lambda { .. } => has_functions = true,
                    fluentai_core::ast::Node::Effect { .. } => has_effects = true,
                    _ => {}
                }
            }
            
            println!("Has functions: {}", has_functions);
            println!("Has effects: {}", has_effects);
        }
        Err(e) => {
            panic!("❌ Parse error: {:?}\nThis indicates a parser issue that needs to be fixed", e);
        }
    }
}