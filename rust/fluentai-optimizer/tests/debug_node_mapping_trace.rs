use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;
use fluentai_core::ast::{Graph, Node, NodeId, Literal};
use rustc_hash::FxHashMap;

// A wrapper to trace optimization steps
struct TracingOptimizer {
    inner: AdvancedOptimizer,
    trace: Vec<String>,
}

impl TracingOptimizer {
    fn new() -> Self {
        Self {
            inner: AdvancedOptimizer::new(),
            trace: Vec::new(),
        }
    }
    
    fn optimize(&mut self, graph: &Graph) -> Graph {
        // We can't actually trace inside AdvancedOptimizer without modifying it
        // So let's just do a before/after analysis
        self.inner.optimize(graph).unwrap()
    }
}

#[test]
fn debug_node_mapping_trace() {
    let code = "(let ((x 5) (y (+ x 2))) (* y 3))";
    let ast = parse(code).unwrap();
    
    println!("=== Analyzing Original AST ===");
    
    // Find the Let node and its bindings
    for (id, node) in &ast.nodes {
        if let Node::Let { bindings, body } = node {
            println!("Found Let node at {:?}", id);
            for (name, value_id) in bindings {
                println!("  Original binding: '{}' -> {:?}", name, value_id);
                if let Some(value_node) = ast.get_node(*value_id) {
                    println!("    which is: {:?}", value_node);
                }
            }
            println!("  Body: {:?}", body);
        }
    }
    
    // Now optimize
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    println!("\n=== Analyzing Optimized AST ===");
    
    // Find the Let node in optimized graph
    for (id, node) in &optimized.nodes {
        if let Node::Let { bindings, body } = node {
            println!("Found Let node at {:?}", id);
            for (name, value_id) in bindings {
                println!("  Optimized binding: '{}' -> {:?}", name, value_id);
                if let Some(value_node) = optimized.get_node(*value_id) {
                    println!("    which is: {:?}", value_node);
                } else {
                    println!("    ERROR: NodeId {:?} not found in optimized graph!", value_id);
                    
                    // Check if this NodeId exists in the original graph
                    if let Some(orig_node) = ast.get_node(*value_id) {
                        println!("    BUT it exists in original graph as: {:?}", orig_node);
                        println!("    This suggests the binding is using an unmapped NodeId!");
                    }
                }
            }
            println!("  Body: {:?}", body);
        }
    }
    
    // Let's also check what NodeId(2) is in both graphs
    println!("\n=== NodeId(2) Analysis ===");
    println!("In original graph: {:?}", ast.get_node(NodeId(std::num::NonZeroU32::new(2).unwrap())));
    println!("In optimized graph: {:?}", optimized.get_node(NodeId(std::num::NonZeroU32::new(2).unwrap())));
}