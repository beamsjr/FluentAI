//! Showcase the FluentAi optimizer capabilities

use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_optimizer::{OptimizationConfig, OptimizationPipeline};
use fluentai_parser::parse;

fn main() {
    println!("=== FluentAi Optimizer Showcase ===\n");

    // Example 1: Constant folding
    showcase_optimization(
        "Constant Folding",
        "(+ (* 2 3) (- 10 5))",
        OptimizationLevel::Basic,
    );

    // Example 2: Dead code elimination
    showcase_optimization(
        "Dead Code Elimination",
        "(let ((x 1) (y 2) (unused 3) (also-unused 4)) (+ x y))",
        OptimizationLevel::Basic,
    );

    // Example 3: Branch elimination
    showcase_optimization(
        "Branch Elimination",
        "(if #t (+ 1 2) (error \"unreachable\"))",
        OptimizationLevel::Standard,
    );

    // Example 4: Complex expression optimization
    showcase_optimization(
        "Complex Expression",
        r#"(let ((x 5) (y 10))
             (+ (* x 2) 
                (* y 3) 
                (if (> x 0) 
                    (- 20 5) 
                    0)))"#,
        OptimizationLevel::Aggressive,
    );

    // Example 5: Nested constant expressions
    showcase_optimization(
        "Nested Constants",
        "(+ (+ (+ 1 2) (+ 3 4)) (+ (+ 5 6) (+ 7 8)))",
        OptimizationLevel::Standard,
    );

    // Example 6: Beta reduction (lambda application)
    showcase_optimization(
        "Beta Reduction",
        "((lambda (x y) (+ x y)) 10 20)",
        OptimizationLevel::Aggressive,
    );
}

fn showcase_optimization(name: &str, code: &str, level: OptimizationLevel) {
    println!("### {} ###", name);
    println!("Original: {}", code);

    let graph = match parse(code) {
        Ok(g) => g,
        Err(e) => {
            println!("Parse error: {}", e);
            return;
        }
    };

    println!("Original nodes: {}", graph.nodes.len());

    let config = OptimizationConfig::for_level(level);
    let mut pipeline = OptimizationPipeline::new(config);

    match pipeline.optimize(&graph) {
        Ok(optimized) => {
            println!("Optimized nodes: {}", optimized.nodes.len());
            println!(
                "Reduction: {:.1}%",
                (1.0 - optimized.nodes.len() as f64 / graph.nodes.len() as f64) * 100.0
            );

            let stats = pipeline.stats();
            println!("Stats:");
            if stats.constant_folded > 0 {
                println!("  - Constants folded: {}", stats.constant_folded);
            }
            if stats.dead_code_eliminated > 0 {
                println!("  - Dead code eliminated: {}", stats.dead_code_eliminated);
            }
            if stats.branches_eliminated > 0 {
                println!("  - Branches eliminated: {}", stats.branches_eliminated);
            }
            if stats.pure_expressions_evaluated > 0 {
                println!(
                    "  - Pure expressions evaluated: {}",
                    stats.pure_expressions_evaluated
                );
            }
            if stats.inlined_expressions > 0 {
                println!("  - Expressions inlined: {}", stats.inlined_expressions);
            }

            // Show the optimized result if it's a single literal
            if optimized.nodes.len() == 1 {
                if let Some(root) = optimized.root_id {
                    if let Some(node) = optimized.get_node(root) {
                        println!("Result: {:?}", node);
                    }
                }
            }
        }
        Err(e) => {
            println!("Optimization error: {}", e);
        }
    }

    println!();
}
