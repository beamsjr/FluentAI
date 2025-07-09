use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use fluentai_optimizer::pipeline::OptimizationLevel;
use fluentai_optimizer::{OptimizationConfig, OptimizationPipeline, OptimizationStats};
use fluentai_parser::parse;

fn create_test_programs() -> Vec<(&'static str, &'static str)> {
    vec![
        ("arithmetic", "(+ (* 3 4) (- 10 5))"),
        ("nested_if", "(if (> 5 3) (+ 1 2) (- 4 1))"),
        ("let_binding", "(let ((x 10) (y 20)) (+ x y))"),
        ("function_app", "((lambda (x y) (+ x y)) 5 10)"),
        ("list_ops", "(car (cons 1 (cons 2 (cons 3 []))))"),
        (
            "complex",
            r#"
            (let ((factorial (lambda (n)
                (if (= n 0)
                    1
                    (* n (factorial (- n 1)))))))
                (factorial 5))
        "#,
        ),
    ]
}

fn benchmark_optimization_levels(c: &mut Criterion) {
    let programs = create_test_programs();
    let levels = vec![
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ];

    let mut group = c.benchmark_group("optimization_levels");

    for (name, program) in programs {
        let graph = parse(program).expect("Failed to parse program");

        for level in &levels {
            group.bench_with_input(
                BenchmarkId::new(name, format!("{:?}", level)),
                &(&graph, level),
                |b, (graph, level)| {
                    b.iter(|| {
                        let config = OptimizationConfig::for_level(**level);
                        let mut pipeline = OptimizationPipeline::new(config);
                        let _optimized = pipeline.optimize(graph);
                    });
                },
            );
        }
    }

    group.finish();
}

fn benchmark_individual_passes(c: &mut Criterion) {
    let programs = create_test_programs();

    let mut group = c.benchmark_group("individual_passes");

    for (name, program) in programs {
        let graph = parse(program).expect("Failed to parse program");

        // Benchmark constant folding
        group.bench_with_input(
            BenchmarkId::new("constant_folding", name),
            &graph,
            |b, graph| {
                b.iter(|| {
                    let mut config = OptimizationConfig::for_level(OptimizationLevel::None);
                    config.constant_folding = true;
                    let mut pipeline = OptimizationPipeline::new(config);
                    let _optimized = pipeline.optimize(graph);
                });
            },
        );

        // Benchmark dead code elimination
        group.bench_with_input(
            BenchmarkId::new("dead_code_elimination", name),
            &graph,
            |b, graph| {
                b.iter(|| {
                    let mut config = OptimizationConfig::for_level(OptimizationLevel::None);
                    config.dead_code_elimination = true;
                    let mut pipeline = OptimizationPipeline::new(config);
                    let _optimized = pipeline.optimize(graph);
                });
            },
        );

        // Benchmark CSE
        group.bench_with_input(BenchmarkId::new("cse", name), &graph, |b, graph| {
            b.iter(|| {
                let mut config = OptimizationConfig::for_level(OptimizationLevel::None);
                config.cse = true;
                let mut pipeline = OptimizationPipeline::new(config);
                let _optimized = pipeline.optimize(graph);
            });
        });
    }

    group.finish();
}

fn benchmark_optimization_effectiveness(c: &mut Criterion) {
    let programs = vec![
        ("constant_heavy", "(+ (+ 1 2) (+ 3 4) (+ 5 6) (+ 7 8))"),
        (
            "dead_code_heavy",
            "(let ((x 1) (y 2) (z 3) (unused 4)) (+ x y))",
        ),
        ("cse_heavy", "(+ (* x y) (* x y) (* x y) (* x y))"),
    ];

    let mut group = c.benchmark_group("effectiveness");

    for (name, program) in programs {
        let graph = parse(program).expect("Failed to parse program");

        group.bench_with_input(BenchmarkId::new("aggressive", name), &graph, |b, graph| {
            b.iter(|| {
                let config = OptimizationConfig::for_level(OptimizationLevel::Aggressive);
                let mut pipeline = OptimizationPipeline::new(config);
                let optimized = pipeline.optimize(graph).unwrap();

                // Return stats to ensure optimization happened
                black_box(pipeline.stats().reduction_percentage());
            });
        });
    }

    group.finish();
}

criterion_group!(
    benches,
    benchmark_optimization_levels,
    benchmark_individual_passes,
    benchmark_optimization_effectiveness
);
criterion_main!(benches);
