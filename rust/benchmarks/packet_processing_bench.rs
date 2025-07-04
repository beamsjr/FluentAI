//! Benchmarks for packet processing optimizations

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fluentai_parser::parse;
use fluentai_vm::{VM, VMBuilder, Compiler, VMConfig};
use fluentai_optimizer::OptimizationLevel;

fn benchmark_tail_recursion(c: &mut Criterion) {
    let mut group = c.benchmark_group("tail_recursion");
    
    // Tail-recursive packet counter
    let tail_recursive_code = r#"
    (letrec ((count-packets
              (lambda (packets acc)
                (if (empty? packets)
                    acc
                    (count-packets (rest packets) (+ acc 1))))))
      (count-packets (list 1 2 3 4 5 6 7 8 9 10) 0))
    "#;
    
    // Non-tail-recursive version
    let non_tail_recursive_code = r#"
    (letrec ((count-packets
              (lambda (packets)
                (if (empty? packets)
                    0
                    (+ 1 (count-packets (rest packets)))))))
      (count-packets (list 1 2 3 4 5 6 7 8 9 10)))
    "#;
    
    group.bench_function("tail_recursive", |b| {
        let ast = parse(tail_recursive_code).unwrap();
        let bytecode = Compiler::new().compile(&ast).unwrap();
        b.iter(|| {
            let mut vm = VMBuilder::new()
                .with_bytecode(bytecode.clone())
                .build()
                .unwrap();
            black_box(vm.run().unwrap())
        })
    });
    
    group.bench_function("non_tail_recursive", |b| {
        let ast = parse(non_tail_recursive_code).unwrap();
        let bytecode = Compiler::new().compile(&ast).unwrap();
        b.iter(|| {
            let mut vm = VMBuilder::new()
                .with_bytecode(bytecode.clone())
                .build()
                .unwrap();
            black_box(vm.run().unwrap())
        })
    });
    
    group.finish();
}

fn benchmark_unboxed_arithmetic(c: &mut Criterion) {
    let mut group = c.benchmark_group("unboxed_arithmetic");
    
    // Checksum calculation with specialized ops
    let optimized_code = r#"
    (let ((sum 0))
      (set! sum (+int sum 0xFF))
      (set! sum (+int sum 0x45))
      (set! sum (+int sum 0x00))
      (set! sum (+int sum 0x34))
      (set! sum (+int sum 0x12))
      (set! sum (+int sum 0x34))
      (set! sum (+int sum 0x40))
      (set! sum (+int sum 0x00))
      sum)
    "#;
    
    // Generic arithmetic
    let generic_code = r#"
    (let ((sum 0))
      (set! sum (+ sum 0xFF))
      (set! sum (+ sum 0x45))
      (set! sum (+ sum 0x00))
      (set! sum (+ sum 0x34))
      (set! sum (+ sum 0x12))
      (set! sum (+ sum 0x34))
      (set! sum (+ sum 0x40))
      (set! sum (+ sum 0x00))
      sum)
    "#;
    
    group.bench_function("specialized_int", |b| {
        let ast = parse(optimized_code).unwrap();
        let bytecode = Compiler::new().compile(&ast).unwrap();
        b.iter(|| {
            let mut vm = VMBuilder::new()
                .with_bytecode(bytecode.clone())
                .build()
                .unwrap();
            black_box(vm.run().unwrap())
        })
    });
    
    group.bench_function("generic_add", |b| {
        let ast = parse(generic_code).unwrap();
        let bytecode = Compiler::new().compile(&ast).unwrap();
        b.iter(|| {
            let mut vm = VMBuilder::new()
                .with_bytecode(bytecode.clone())
                .build()
                .unwrap();
            black_box(vm.run().unwrap())
        })
    });
    
    group.finish();
}

fn benchmark_channel_types(c: &mut Criterion) {
    let mut group = c.benchmark_group("channels");
    
    // Test different channel implementations
    let channel_code = r#"
    (let ((ch (chan)))
      (spawn (send! ch 42))
      (recv! ch))
    "#;
    
    for size in [0, 10, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("buffered", size),
            size,
            |b, &size| {
                let code = if size == 0 {
                    channel_code.to_string()
                } else {
                    format!("(let ((ch (chan {}))) (spawn (send! ch 42)) (recv! ch))", size)
                };
                
                let ast = parse(&code).unwrap();
                let bytecode = Compiler::new().compile(&ast).unwrap();
                
                b.iter(|| {
                    let mut vm = VMBuilder::new()
                        .with_bytecode(bytecode.clone())
                        .build()
                        .unwrap();
                    black_box(vm.run().unwrap())
                })
            },
        );
    }
    
    group.finish();
}

fn benchmark_optimization_levels(c: &mut Criterion) {
    let mut group = c.benchmark_group("optimization_levels");
    
    // Complex code that benefits from optimization
    let code = r#"
    (letrec ((factorial
              (lambda (n acc)
                (if (<= n 1)
                    acc
                    (factorial (- n 1) (* n acc)))))
             (sum-factorials
              (lambda (n acc)
                (if (<= n 0)
                    acc
                    (sum-factorials (- n 1) 
                                   (+ acc (factorial n 1)))))))
      (sum-factorials 10 0))
    "#;
    
    let ast = parse(code).unwrap();
    
    for level in [
        OptimizationLevel::None,
        OptimizationLevel::Basic,
        OptimizationLevel::Standard,
        OptimizationLevel::Aggressive,
    ] {
        group.bench_with_input(
            BenchmarkId::new("level", format!("{:?}", level)),
            &level,
            |b, &level| {
                let config = VMConfig::default().with_optimization_level(level);
                let compiler = Compiler::with_options(
                    fluentai_vm::CompilerOptions {
                        optimization_level: level,
                        debug_info: false,
                    }
                );
                let bytecode = compiler.compile(&ast).unwrap();
                
                b.iter(|| {
                    let mut vm = VMBuilder::new()
                        .with_config(config.clone())
                        .with_bytecode(bytecode.clone())
                        .build()
                        .unwrap();
                    black_box(vm.run().unwrap())
                })
            },
        );
    }
    
    group.finish();
}

criterion_group!(
    benches,
    benchmark_tail_recursion,
    benchmark_unboxed_arithmetic,
    benchmark_channel_types,
    benchmark_optimization_levels
);
criterion_main!(benches);