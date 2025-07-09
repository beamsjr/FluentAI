//! Debug AST structure to understand how functions are stored

use fluentai_parser::parse;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = r#"
        (define (abs x)
          (if (< x 0)
              (- 0 x)
              x))
    "#;

    let graph = parse(program)?;

    println!("Graph nodes:");
    for (id, node) in &graph.nodes {
        println!("{}: {:?}", id, node);
    }

    println!("\nRoot ID: {:?}", graph.root_id);

    Ok(())
}
