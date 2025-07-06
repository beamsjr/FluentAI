use fluentai_optimizer::{AdvancedOptimizer};
use fluentai_parser::parse;

fn main() {
    let code = r#"
        (let ((add (lambda (x y) (+ x y)))
              (mul (lambda (x y) (* x y))))
          (+ (add 2 3) (mul 4 5)))
    "#;
    
    let ast = parse(code).unwrap();
    
    println!("Original AST:");
    println!("Root: {:?}", ast.root_id);
    for (id, node) in &ast.nodes {
        println!("  {:?}: {:?}", id, node);
    }
    
    let mut optimizer = AdvancedOptimizer::new();
    let optimized = optimizer.optimize(&ast).unwrap();
    
    println!("\nOptimized AST:");
    println!("Root: {:?}", optimized.root_id);
    for (id, node) in &optimized.nodes {
        println!("  {:?}: {:?}", id, node);
    }
}