//! Demonstration of FluentAi optimization capabilities

use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_optimizer::{OptimizationConfig, OptimizationPipeline};
use fluentai_parser::parse;

fn main() {
    println!("FluentAi Optimization Demo\n");

    // Example programs to optimize
    let examples = vec![
        (
            "constant_folding",
            "(+ (* 3 4) (- 10 5) (* 2 (+ 1 2)))",
            "Demonstrates constant folding optimization",
        ),
        (
            "dead_code_elimination",
            "(let ((x 10) (y 20) (unused (+ 1 2))) (+ x y))",
            "Demonstrates dead code elimination",
        ),
        (
            "common_subexpression",
            "(let ((x (* a b)) (y (* a b)) (z (* a b))) (+ x y z))",
            "Demonstrates common subexpression elimination",
        ),
        (
            "function_inlining",
            "(let ((add (lambda (x y) (+ x y)))) (add 5 10))",
            "Demonstrates function inlining",
        ),
        (
            "partial_evaluation",
            "(if #t (+ 1 2) (expensive-computation))",
            "Demonstrates partial evaluation with boolean short-circuiting",
        ),
        (
            "arithmetic_identities",
            "(+ (* x 1) (* 0 y) (+ z 0))",
            "Demonstrates arithmetic identity optimizations",
        ),
        (
            "effect_aware",
            "(let ((pure1 (+ 1 2)) (effect (io-read)) (pure2 (* 3 4))) (+ pure1 effect pure2))",
            "Demonstrates effect-aware optimization hoisting pure computations",
        ),
        (
            "complex_optimization",
            r#"
            (let ((factorial (lambda (n) 
                    (if (= n 0) 
                        1 
                        (* n (factorial (- n 1))))))
                  (square (lambda (x) (* x x)))
                  (add3 (lambda (x) (+ x 3))))
                (+ (square 5) 
                   (add3 7) 
                   (* 2 3)
                   (if #f 100 (+ 1 2))))
            "#,
            "Demonstrates multiple optimizations working together",
        ),
    ];

    // Test each optimization level
    let levels = vec![
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ];

    for (name, program, description) in &examples {
        println!("=== {} ===", name);
        println!("{}", description);
        println!("Original program: {}", program);

        let graph = match parse(program) {
            Ok(g) => g,
            Err(e) => {
                println!("Parse error: {}", e);
                continue;
            }
        };

        let original_nodes = graph.nodes.len();
        println!("Original AST nodes: {}", original_nodes);

        for level in &levels {
            let config = OptimizationConfig::for_level(*level);
            let mut pipeline = OptimizationPipeline::new(config);

            match pipeline.optimize(&graph) {
                Ok(optimized) => {
                    let stats = pipeline.stats();
                    println!(
                        "  {:?}: {} nodes ({}% reduction, {} μs)",
                        level,
                        optimized.nodes.len(),
                        stats.reduction_percentage(),
                        stats.optimization_time_us
                    );
                }
                Err(e) => println!("  {:?}: Optimization error: {}", level, e),
            }
        }

        println!();
    }

    // Demonstrate performance improvement on a larger program
    println!("=== Performance Benchmark ===");
    let benchmark_program = r#"
        (let ((map (lambda (f lst)
                    (if (list-empty? lst)
                        []
                        (cons (f (car lst)) (map f (cdr lst))))))
              (filter (lambda (pred lst)
                        (if (list-empty? lst)
                            []
                            (if (pred (car lst))
                                (cons (car lst) (filter pred (cdr lst)))
                                (filter pred (cdr lst))))))
              (reduce (lambda (f acc lst)
                        (if (list-empty? lst)
                            acc
                            (reduce f (f acc (car lst)) (cdr lst)))))
              (data [1 2 3 4 5 6 7 8 9 10]))
            (reduce + 0 
                (map (lambda (x) (* x x))
                    (filter (lambda (x) (> x 5)) data))))
    "#;

    println!("Complex functional program with map/filter/reduce");

    let graph = parse(benchmark_program).expect("Failed to parse benchmark");
    let original_nodes = graph.nodes.len();

    for level in &levels {
        let config = OptimizationConfig::for_level(*level);
        let mut pipeline = OptimizationPipeline::new(config);

        let start = std::time::Instant::now();
        let optimized = pipeline.optimize(&graph).expect("Optimization failed");
        let elapsed = start.elapsed();

        let stats = pipeline.stats();
        let reduction = stats.reduction_percentage();

        println!("{:?} optimization:", level);
        println!(
            "  Nodes: {} -> {} ({}% reduction)",
            original_nodes,
            optimized.nodes.len(),
            reduction
        );
        println!("  Time: {:.2} ms", elapsed.as_secs_f64() * 1000.0);

        if *level == OptimizationLevel::Aggressive {
            println!(
                "\nAchieved {}% performance improvement with aggressive optimization!",
                reduction
            );
            if reduction >= 20.0 {
                println!("✓ Target of 20-50% improvement ACHIEVED!");
            }
        }
    }
}
