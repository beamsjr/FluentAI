use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse_flc;

#[test]
fn debug_effect_preservation() {
    let code = r#"{
        let x = perform IO.print("hello");
        1 + 2
    }"#;
    let ast = parse_flc(code).unwrap();

    println!("Original AST ({} nodes):", ast.nodes.len());
    for (id, node) in &ast.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}\n", ast.root_id);

    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();

    println!("Optimized AST ({} nodes):", optimized.nodes.len());
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    println!("Root: {:?}", optimized.root_id);
    println!("Stats: {:?}", optimizer.stats());

    // The print effect should still be in the graph
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, fluentai_core::ast::Node::Effect { 
            effect_type: fluentai_core::ast::EffectType::IO, 
            operation, 
            .. 
        } if operation == "print")
    });
    println!("\nHas print effect: {}", has_print);
    
    assert!(has_print, "Print effect should be preserved during optimization");
}
