use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use fluentai_parser::parse;

const SIMPLE_EXPR: &str = "(+ 1 2)";
const NESTED_EXPR: &str = "(* (+ 1 2) (- 4 3))";
const LAMBDA_EXPR: &str = "(lambda (x y) (+ x y))";
const LET_EXPR: &str = "(let ((x 10) (y 20)) (+ x y))";
const LIST_EXPR: &str = "[1 2 3 4 5 6 7 8 9 10]";
const COMPLEX_EXPR: &str = r#"
(let ((map (lambda (f lst)
            (if (empty? lst)
                []
                (cons (f (head lst))
                      (map f (tail lst)))))))
  (map (lambda (x) (* x 2)) [1 2 3 4 5]))
"#;

const LARGE_PROGRAM: &str = r#"
(let ((quicksort (lambda (lst)
                  (if (empty? lst)
                      []
                      (let ((pivot (head lst))
                            (rest (tail lst))
                            (less (filter (lambda (x) (< x pivot)) rest))
                            (greater (filter (lambda (x) (>= x pivot)) rest)))
                        (append (quicksort less)
                                (cons pivot (quicksort greater))))))))
  (quicksort [3 1 4 1 5 9 2 6 5 3 5]))
"#;

fn parser_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser");

    // Simple expression
    group.throughput(Throughput::Bytes(SIMPLE_EXPR.len() as u64));
    group.bench_function("simple_expr", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(SIMPLE_EXPR)));
        });
    });

    // Nested expression
    group.throughput(Throughput::Bytes(NESTED_EXPR.len() as u64));
    group.bench_function("nested_expr", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(NESTED_EXPR)));
        });
    });

    // Lambda expression
    group.throughput(Throughput::Bytes(LAMBDA_EXPR.len() as u64));
    group.bench_function("lambda", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(LAMBDA_EXPR)));
        });
    });

    // Let binding
    group.throughput(Throughput::Bytes(LET_EXPR.len() as u64));
    group.bench_function("let_binding", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(LET_EXPR)));
        });
    });

    // List literal
    group.throughput(Throughput::Bytes(LIST_EXPR.len() as u64));
    group.bench_function("list_literal", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(LIST_EXPR)));
        });
    });

    // Complex expression
    group.throughput(Throughput::Bytes(COMPLEX_EXPR.len() as u64));
    group.bench_function("complex_expr", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(COMPLEX_EXPR)));
        });
    });

    // Large program
    group.throughput(Throughput::Bytes(LARGE_PROGRAM.len() as u64));
    group.bench_function("large_program", |b| {
        b.iter(|| {
            let _ = black_box(parse(black_box(LARGE_PROGRAM)));
        });
    });

    group.finish();
}

fn scaling_benchmarks(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_scaling");

    // Test parser performance with different input sizes
    for size in [10, 100, 1000, 10000].iter() {
        let input = generate_nested_expr(*size);
        group.throughput(Throughput::Bytes(input.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), &input, |b, input| {
            b.iter(|| {
                let _ = black_box(parse(black_box(input)));
            });
        });
    }

    group.finish();
}

fn generate_nested_expr(depth: usize) -> String {
    if depth == 0 {
        "1".to_string()
    } else {
        format!("(+ {} {})", generate_nested_expr(depth - 1), depth)
    }
}

criterion_group!(benches, parser_benchmarks, scaling_benchmarks);
criterion_main!(benches);
