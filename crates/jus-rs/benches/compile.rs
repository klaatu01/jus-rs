use criterion::{black_box, criterion_group, criterion_main, Criterion};
use jus_rs::Jus;

fn bench_jus_compilation(c: &mut Criterion) {
    let jus_schema_str = include_str!("../data/schema/complex.jus");

    c.bench_function("jus_compilation", |b| {
        b.iter(|| {
            let _ = Jus::compile(black_box(jus_schema_str)).expect("Failed to compile Jus schema");
        })
    });
}

criterion_group!(benches, bench_jus_compilation);
criterion_main!(benches);
