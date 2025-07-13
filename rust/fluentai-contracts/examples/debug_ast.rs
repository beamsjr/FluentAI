//! Debug AST structure to understand how functions are stored
use fluentai_parser::parse_flc;



fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program = r#"
        (define (abs x)
          (if (< x 0)
              (- 0 x)
              x))
    "#;

    let graph = parse_flc(program)?;

    println!("Graph nodes:");
    for (id, node) in &graph.nodes {
        println!("{}: {:?}", id, node);
    }

    println!("\nRoot ID: {:?}", graph.root_id);

    Ok(())
}
