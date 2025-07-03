//! Benchmarks for FluentAi VM execution performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fluentai_parser::parse;
use fluentai_vm::{Compiler, VM};

fn benchmark_vm_arithmetic(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_arithmetic");
    
    // Simple addition
    group.bench_function("simple_add", |b| {
        b.iter(|| {
            let ast = parse("(+ 1 2)").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    // Nested arithmetic
    group.bench_function("nested_arithmetic", |b| {
        b.iter(|| {
            let ast = parse("(* (+ 1 2) (- 4 3))").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    // Complex expression
    group.bench_function("complex_arithmetic", |b| {
        b.iter(|| {
            let ast = parse("(+ (* 2 3) (/ 8 2))").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    group.finish();
}

fn benchmark_vm_literals(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_literals");
    
    group.bench_function("integer", |b| {
        b.iter(|| {
            let ast = parse("42").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    group.bench_function("float", |b| {
        b.iter(|| {
            let ast = parse("3.14159").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    group.bench_function("string", |b| {
        b.iter(|| {
            let ast = parse(r#""hello world""#).unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    group.bench_function("list", |b| {
        b.iter(|| {
            let ast = parse("[1 2 3 4 5]").unwrap();
            let compiler = Compiler::new();
            let bytecode = compiler.compile(&ast).unwrap();
            let mut vm = VM::new(bytecode);
            black_box(vm.run().unwrap())
        });
    });
    
    group.finish();
}

fn benchmark_vm_control_flow(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_control_flow");
    
    // TODO: Implement control flow once built-in functions are fixed
    
    group.finish();
}

fn benchmark_vm_functions(c: &mut Criterion) {
    let mut group = c.benchmark_group("vm_functions");
    
    // TODO: Implement function benchmarks once built-in functions are fixed
    
    group.finish();
}

criterion_group!(
    benches,
    benchmark_vm_arithmetic,
    benchmark_vm_literals,
    benchmark_vm_control_flow,
    benchmark_vm_functions
);
criterion_main!(benches);