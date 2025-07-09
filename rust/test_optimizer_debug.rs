use fluentai_parser::parse;
use fluentai_optimizer::GraphOptimizer;

fn main() {
    let code = "(module test-module (export x) (define x 10))";
    let graph = parse(code).unwrap();
    
    println!("Original graph:");
    println!("  Root: {:?}", graph.root_id);
    for (id, node) in &graph.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    let mut optimizer = GraphOptimizer::new();
    match optimizer.optimize(&graph) {
        Ok(optimized) => {
            println!("\nOptimized graph:");
            println!("  Root: {:?}", optimized.root_id);
            for (id, node) in &optimized.nodes {
                println!("  {:?}: {:?}", id, node);
            }
        }
        Err(e) => {
            println!("\nOptimization failed: {}", e);
        }
    }
}