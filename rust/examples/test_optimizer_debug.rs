use fluentai_core::ast::{Graph, Node, Literal};
use fluentai_optimizer::{OptimizationPipeline, OptimizationConfig, OptimizationLevel};

fn main() {
    println!("Testing optimizer with List containing variables...");
    
    let mut graph = Graph::new();
    
    // Create (let ((x 1) (y 2) (z 3)) (list x y z))
    let x_val = graph.add_node(Node::Literal(Literal::Integer(1)));
    let y_val = graph.add_node(Node::Literal(Literal::Integer(2)));
    let z_val = graph.add_node(Node::Literal(Literal::Integer(3)));
    
    let x_var = graph.add_node(Node::Variable { name: "x".to_string() });
    let y_var = graph.add_node(Node::Variable { name: "y".to_string() });
    let z_var = graph.add_node(Node::Variable { name: "z".to_string() });
    
    let list_node = graph.add_node(Node::List(vec![x_var, y_var, z_var]));
    
    let let_node = graph.add_node(Node::Let {
        bindings: vec![
            ("x".to_string(), x_val),
            ("y".to_string(), y_val),
            ("z".to_string(), z_val),
        ],
        body: list_node,
    });
    graph.root_id = Some(let_node);
    
    println!("Original graph has {} nodes", graph.nodes.len());
    
    // Try to optimize
    let config = OptimizationConfig::for_level(OptimizationLevel::Standard);
    let mut pipeline = OptimizationPipeline::new(config);
    
    println!("Running optimizer...");
    match pipeline.optimize(&graph) {
        Ok(optimized) => {
            println!("Optimization succeeded! Result has {} nodes", optimized.nodes.len());
        }
        Err(e) => {
            println!("Optimization failed: {:?}", e);
        }
    }
}