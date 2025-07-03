//! JIT compilation benchmarks

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use fluentai_parser::parse;
use fluentai_vm::compiler::Compiler;
use fluentai_vm::vm::VM;
use fluentai_jit::JitCompiler;

fn benchmark_jit_compilation(c: &mut Criterion) {
    let mut group = c.benchmark_group("jit_compilation");
    
    let test_cases = vec![
        ("simple_arithmetic", "(+ 1 2)"),
        ("nested_arithmetic", "(+ (* 2 3) (- 5 1))"),
        ("factorial", "(let ((fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))) (fact 5))"),
    ];
    
    for (name, source) in test_cases {
        group.bench_with_input(BenchmarkId::new("compile", name), &source, |b, &source| {
            b.iter(|| {
                let mut jit = JitCompiler::new().unwrap();
                
                let ast = parse(source).unwrap();
                let compiler = Compiler::new();
                let bytecode = compiler.compile(&ast).unwrap();
                
                black_box(jit.compile(&bytecode, 0).unwrap());
            });
        });
    }
    
    group.finish();
}

fn benchmark_jit_vs_vm_execution(c: &mut Criterion) {
    let mut group = c.benchmark_group("execution");
    
    let test_cases = vec![
        ("simple_arithmetic", "(+ 1 2)"),
        ("loop_sum", "(let ((sum 0) (i 0)) (while (< i 100) (set! sum (+ sum i)) (set! i (+ i 1))) sum)"),
        ("recursive_fib", "(let ((fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))) (fib 10))"),
    ];
    
    for (name, source) in test_cases {
        // Prepare bytecode
        let ast = parse(source).unwrap();
        let compiler = Compiler::new();
        let bytecode = compiler.compile(&ast).unwrap();
        
        // Benchmark VM execution
        group.bench_with_input(BenchmarkId::new("vm", name), &bytecode, |b, bytecode| {
            b.iter(|| {
                let mut vm = VM::new(bytecode.clone());
                black_box(vm.run().unwrap());
            });
        });
        
        // Benchmark JIT execution
        group.bench_with_input(BenchmarkId::new("jit", name), &bytecode, |b, bytecode| {
            let mut jit = JitCompiler::new().unwrap();
            let func = jit.compile(bytecode, 0).unwrap();
            
            b.iter(|| {
                black_box(func());
            });
        });
    }
    
    group.finish();
}

fn benchmark_jit_warmup(c: &mut Criterion) {
    let mut group = c.benchmark_group("jit_warmup");
    
    let source = "(+ (* 2 3) (- 5 1))";
    let ast = parse(source).unwrap();
    let compiler = Compiler::new();
    let bytecode = compiler.compile(&ast).unwrap();
    
    group.bench_function("cold_start", |b| {
        b.iter(|| {
            let mut jit = JitCompiler::new().unwrap();
            black_box(jit.compile_and_run(&bytecode).unwrap());
        });
    });
    
    group.bench_function("warm_cache", |b| {
        let mut jit = JitCompiler::new().unwrap();
        
        // Warm up the cache
        jit.compile(&bytecode, 0).unwrap();
        
        b.iter(|| {
            black_box(jit.compile_and_run(&bytecode).unwrap());
        });
    });
    
    group.finish();
}

criterion_group!(benches, 
    benchmark_jit_compilation,
    benchmark_jit_vs_vm_execution,
    benchmark_jit_warmup
);
criterion_main!(benches);