use criterion::{criterion_group, criterion_main, Criterion};

fn vm_benchmarks(_c: &mut Criterion) {
    // TODO: Implement VM benchmarks
}

criterion_group!(benches, vm_benchmarks);
criterion_main!(benches);