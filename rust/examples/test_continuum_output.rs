// Quick test to show what Continuum produces right now
use fluentai_parser::flc_parser::Parser;
use fluentai_optimizer::{OptimizationPipeline, PassConfig};
use fluentai_optimizer::passes::ContinuumLowering;

fn main() {
    let code = r#"
surface demo {
    element text {
        content: "Hello Continuum!"
    }
    element button {
        content: "Click me",
        on_click: () => perform IO.println("Clicked!")
    }
}
"#;

    // Parse the Continuum code
    let mut parser = Parser::new(code);
    let graph = parser.parse().expect("Failed to parse");
    
    println!("=== Original AST ===");
    println!("{:#?}", graph.get_node(graph.root_id.unwrap()));
    
    // Apply Continuum lowering
    let mut pipeline = OptimizationPipeline::new();
    pipeline.add_pass(Box::new(ContinuumLowering::new()), PassConfig::default());
    
    let optimized = pipeline.run(&graph).expect("Failed to optimize");
    
    println!("\n=== After Continuum Lowering ===");
    println!("{:#?}", optimized.get_node(optimized.root_id.unwrap()));
    
    println!("\n=== What would happen at runtime ===");
    println!("DOM: Creating surface 'demo'");
    println!("DOM: Creating div element");
    println!("DOM: Setting property content = 'Hello Continuum!'");
    println!("DOM: Creating div element");  
    println!("DOM: Setting property content = 'Click me'");
    println!("DOM: Adding click handler");
}