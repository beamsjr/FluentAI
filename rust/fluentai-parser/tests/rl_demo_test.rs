use fluentai_parser::parse_flc;

#[test]
fn test_rl_optimization_demo() {
    let source = include_str!("../../examples/rl_optimization_demo.flc");
    
    match parse_flc(source) {
        Ok(graph) => {
            println!("✅ Demo parsed successfully!");
            println!("Graph has {} nodes", graph.nodes.len());
        }
        Err(e) => {
            panic!("❌ Parse error: {:?}\nThis indicates a parser issue that needs to be fixed", e);
        }
    }
}