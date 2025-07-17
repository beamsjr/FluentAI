use fluentai_parser::parse_flc;

#[test]
fn test_rl_optimization_demo_simple() {
    let source = include_str!("../../examples/rl_optimization_demo_simple.flc");
    
    match parse_flc(source) {
        Ok(graph) => {
            println!("✅ Simple demo parsed successfully!");
            println!("Graph has {} nodes", graph.nodes.len());
        }
        Err(e) => {
            panic!("❌ Parse error: {:?}\nThis indicates a parser issue that needs to be fixed", e);
        }
    }
}