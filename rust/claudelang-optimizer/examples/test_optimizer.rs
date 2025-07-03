//! Test the optimizer with a simple example

use claudelang_optimizer::GraphOptimizer;
use claudelang_parser::parse;

fn main() {
    let code = "(+ (* 2 3) (- 10 5))";
    println!("Original code: {}", code);
    
    let graph = parse(code).unwrap();
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