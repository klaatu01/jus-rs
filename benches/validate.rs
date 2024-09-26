use criterion::{black_box, criterion_group, criterion_main, Criterion};
use jus_rs::Jus;

fn bench_jus_validation(c: &mut Criterion) {
    let jus_schema_str = include_str!("../data/schema/complex.jus");
    let jus = Jus::compile(jus_schema_str).expect("Failed to compile Jus schema");

    let json_data = serde_json::json!({
        "id": "123",
        "name": "John Doe",
        "username": "johndoe",
        "contactInfo": {
            "email": "test@test.com",
            "phone": "123-456-7890",
            "socialMedia": {
                "twitter": "@johndoe",
                "linkedin": "johndoe",
                "github": "johndoe",
            },
        },
        "friends": ["456", "789"],
        "status": "active",
        "bio": "Hello, world!",
        "preferences": {
            "theme": "light",
            "notifications": {
                "email": true,
                "sms": false,
                "push": true,
            },
        },
    });

    c.bench_function("jus_validation", |b| {
        b.iter(|| {
            let result = jus.validate(black_box(&json_data));
            assert!(result);
        })
    });
}

criterion_group!(benches, bench_jus_validation);
criterion_main!(benches);
