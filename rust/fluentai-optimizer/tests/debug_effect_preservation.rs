use fluentai_optimizer::AdvancedOptimizer;
use fluentai_parser::parse;

#[test]
fn debug_effect_preservation() {
    let code = r#"
        (let ((x (print "hello")))
          (+ 1 2))
    "#;
    let ast = parse(code).unwrap();

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

    // The print should still be in the graph
    let has_print = optimized.nodes.values().any(|node| {
        matches!(node, fluentai_core::ast::Node::Application { function, .. } if {
            optimized.get_node(*function)
                .map(|n| matches!(n, fluentai_core::ast::Node::Variable { name } if name == "print"))
                .unwrap_or(false)
        })
    });
    println!("\nHas print: {}", has_print);
}
