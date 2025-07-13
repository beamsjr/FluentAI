//! Example of using the FluentAi optimizer

use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_optimizer::{OptimizationConfig, OptimizationPipeline};
use fluentai_parser::parse_flc;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let code = if args.len() > 1 {
        args[1].clone()
    } else {
        // Default example program
        r#"
        (let ((x 5) (y 10) (unused 20))
            (+ (* x 2) (* y 3) (if #t 7 (error "unreachable"))))
        "#
        .to_string()
    };

    println!("Original program:");
    println!("{}", code);
    println!();

    // Parse the program
    let graph = match parse_flc(&code) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    println!("Original AST has {} nodes", graph.nodes.len());
    println!();

    // Try different optimization levels
    for level in vec![
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ] {
        println!("Optimization level: {:?}", level);

        let config = OptimizationConfig::for_level(level);
        let mut pipeline = OptimizationPipeline::new(config);

        match pipeline.optimize(&graph) {
            Ok(optimized) => {
                let stats = pipeline.stats();
                println!(
                    "  Nodes: {} -> {} ({:.1}% reduction)",
                    stats.nodes_before,
                    stats.nodes_after,
                    stats.reduction_percentage()
                );
                println!(
                    "  Time: {:.3}ms",
                    stats.optimization_time_us as f64 / 1000.0
                );

                if stats.total_optimizations() > 0 {
                    println!("  Optimizations performed:");
                    if stats.constant_folded > 0 {
                        println!("    - Constant folding: {}", stats.constant_folded);
                    }
                    if stats.dead_code_eliminated > 0 {
                        println!("    - Dead code eliminated: {}", stats.dead_code_eliminated);
                    }
                    if stats.pure_expressions_evaluated > 0 {
                        println!(
                            "    - Pure expressions evaluated: {}",
                            stats.pure_expressions_evaluated
                        );
                    }
                    if stats.branches_eliminated > 0 {
                        println!("    - Branches eliminated: {}", stats.branches_eliminated);
                    }
                    if stats.cse_eliminated > 0 {
                        println!("    - CSE eliminated: {}", stats.cse_eliminated);
                    }
                }
            }
            Err(e) => {
                eprintln!("  Optimization error: {}", e);
            }
        }

        println!();
    }
}
