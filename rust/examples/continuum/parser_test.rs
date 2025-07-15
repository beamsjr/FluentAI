use fluentai_parser::Parser;

fn main() {
    let code = r#"
surface app {
    element button {
        content: "Click me"
    }
}
"#;

    println!("Parsing Continuum code:");
    println!("{}", code);
    
    let mut parser = Parser::new(code);
    match parser.parse() {
        Ok(graph) => {
            println!("Success! Parsed graph with {} nodes", graph.graph.len());
            if let Some(root) = graph.root_id {
                println!("Root node ID: {:?}", root);
                if let Some(node) = graph.get_node(root) {
                    println!("Root node type: {:?}", node);
                }
            }
        }
        Err(e) => {
            println!("Parse error: {}", e);
        }
    }
}