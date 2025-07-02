use criterion::{criterion_group, criterion_main, Criterion};

fn end_to_end_benchmarks(_c: &mut Criterion) {
    // TODO: Implement end-to-end benchmarks
}

criterion_group!(benches, end_to_end_benchmarks);
criterion_main!(benches);