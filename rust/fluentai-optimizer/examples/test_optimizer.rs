//! Test the optimizer with a simple example

use fluentai_optimizer::GraphOptimizer;
use fluentai_parser::parse_flc;

fn main() {
    let code = "(+ (* 2 3) (- 10 5))";
    println!("Original code: {}", code);

    let graph = parse_flc(code).unwrap();
    println!("Original graph nodes: {}", graph.nodes.len());

    let mut optimizer = GraphOptimizer::new();
    let optimized = optimizer.optimize(&graph).unwrap();

    println!("Optimized graph nodes: {}", optimized.nodes.len());
    println!("Stats: {:?}", optimizer.stats());

    // Print the optimized result
    if let Some(root) = optimized.root_id {
        if let Some(node) = optimized.get_node(root) {
            println!("Result: {:?}", node);
        }
    }
}
