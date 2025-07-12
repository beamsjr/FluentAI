use fluentai_parser::parse_flc;

fn main() {
    let code = r#"
// Simple finally test
let result = try {
    42
} finally {
    // Finally block should execute but not affect the return value
    1 + 1
};

$(f"Result: {result}").print()
"#;

    match parse_flc(code) {
        Ok(graph) => {
            println!("Parse succeeded!");
            println!("Root: {:?}", graph.root_id);
            println!("Nodes:");
            for (id, node) in graph.nodes() {
                println!("  {:?}: {:?}", id, node);
            }
        }
        Err(e) => {
            println!("Parse error: {:?}", e);
        }
    }
}