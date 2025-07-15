use fluentai_parser::parse_flc;

fn main() {
    let code = "1 + 2";
    match parse_flc(code) {
        Ok(graph) => {
            println!("Graph nodes:");
            for (id, node) in &graph.nodes {
                println!("  ID {:?}: {:?}", id, node);
            }
            println!("Root ID: {:?}", graph.root_id);
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}