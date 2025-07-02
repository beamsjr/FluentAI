//! End-to-end benchmarks measuring parse + compile + execute performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use claudelang_parser::parse;
use claudelang_vm::{Compiler, VM};

fn benchmark_e2e_simple(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_simple");
    
    // Integer literal
    group.bench_function("integer", |b| {
        b.iter(|| {
            let code = black_box("42");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // Simple arithmetic
    group.bench_function("arithmetic", |b| {
        b.iter(|| {
            let code = black_box("(+ 1 2)");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // List creation
    group.bench_function("list", |b| {
        b.iter(|| {
            let code = black_box("[1 2 3 4 5]");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    group.finish();
}

fn benchmark_e2e_complex(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_complex");
    
    // Nested arithmetic
    group.bench_function("nested_math", |b| {
        b.iter(|| {
            let code = black_box("(+ (* 2 3) (- (* 4 5) (/ 10 2)))");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // Lambda definition
    group.bench_function("lambda", |b| {
        b.iter(|| {
            let code = black_box("(lambda (x y) (+ x y))");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // Let binding
    group.bench_function("let_binding", |b| {
        b.iter(|| {
            let code = black_box("(let ((x 10) (y 20)) (+ x y))");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // Large list
    group.bench_function("large_list", |b| {
        b.iter(|| {
            let code = black_box("[1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20]");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    group.finish();
}

fn benchmark_e2e_programs(c: &mut Criterion) {
    let mut group = c.benchmark_group("e2e_programs");
    
    // Factorial-like computation
    group.bench_function("factorial_5", |b| {
        b.iter(|| {
            let code = black_box("(* 5 (* 4 (* 3 (* 2 1))))");
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    // String manipulation
    group.bench_function("string_concat", |b| {
        b.iter(|| {
            let code = black_box(r#"(str-concat (str-concat "hello" " ") "world")"#);
            let ast = parse(code).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            vm.run().unwrap()
        });
    });
    
    group.finish();
}

criterion_group!(
    benches,
    benchmark_e2e_simple,
    benchmark_e2e_complex,
    benchmark_e2e_programs
);
criterion_main!(benches);