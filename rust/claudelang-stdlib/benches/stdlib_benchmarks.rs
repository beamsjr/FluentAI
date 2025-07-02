//! Benchmarks for ClaudeLang standard library functions

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use claudelang_stdlib::{init_stdlib, value::Value};

fn benchmark_list_operations(c: &mut Criterion) {
    let stdlib = init_stdlib();
    
    c.bench_function("list_append_small", |b| {
        let append = stdlib.get("append").unwrap();
        let list = Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        b.iter(|| {
            append.call(&[black_box(list.clone()), black_box(Value::Int(4))]).unwrap()
        });
    });
    
    c.bench_function("list_reverse_100", |b| {
        let reverse = stdlib.get("reverse").unwrap();
        let list = Value::List((0..100).map(|i| Value::Int(i)).collect());
        b.iter(|| {
            reverse.call(&[black_box(list.clone())]).unwrap()
        });
    });
    
    c.bench_function("list_length_1000", |b| {
        let length = stdlib.get("length").unwrap();
        let list = Value::List((0..1000).map(|i| Value::Int(i)).collect());
        b.iter(|| {
            length.call(&[black_box(list.clone())]).unwrap()
        });
    });
    
    c.bench_function("range_1000", |b| {
        let range = stdlib.get("range").unwrap();
        b.iter(|| {
            range.call(&[black_box(Value::Int(1000))]).unwrap()
        });
    });
}

fn benchmark_string_operations(c: &mut Criterion) {
    let stdlib = init_stdlib();
    
    c.bench_function("string_concat_10", |b| {
        let concat = stdlib.get("string-concat").unwrap();
        let strings: Vec<Value> = (0..10)
            .map(|i| Value::String(format!("string{}", i)))
            .collect();
        b.iter(|| {
            concat.call(&black_box(strings.clone())).unwrap()
        });
    });
    
    c.bench_function("string_split_csv", |b| {
        let split = stdlib.get("string-split").unwrap();
        let csv = Value::String("a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z".to_string());
        let delim = Value::String(",".to_string());
        b.iter(|| {
            split.call(&[black_box(csv.clone()), black_box(delim.clone())]).unwrap()
        });
    });
    
    c.bench_function("string_upcase_long", |b| {
        let upcase = stdlib.get("string-upcase").unwrap();
        let text = Value::String("the quick brown fox jumps over the lazy dog".repeat(10));
        b.iter(|| {
            upcase.call(&[black_box(text.clone())]).unwrap()
        });
    });
}

fn benchmark_math_operations(c: &mut Criterion) {
    let stdlib = init_stdlib();
    
    c.bench_function("math_add_100_ints", |b| {
        let add = stdlib.get("+").unwrap();
        let numbers: Vec<Value> = (0..100).map(|i| Value::Int(i)).collect();
        b.iter(|| {
            add.call(&black_box(numbers.clone())).unwrap()
        });
    });
    
    c.bench_function("math_sqrt_float", |b| {
        let sqrt = stdlib.get("sqrt").unwrap();
        b.iter(|| {
            sqrt.call(&[black_box(Value::Float(16384.0))]).unwrap()
        });
    });
    
    c.bench_function("math_trig_sin_cos_tan", |b| {
        let sin = stdlib.get("sin").unwrap();
        let cos = stdlib.get("cos").unwrap();
        let tan = stdlib.get("tan").unwrap();
        let angle = Value::Float(1.5708); // π/2
        b.iter(|| {
            let s = sin.call(&[black_box(angle.clone())]).unwrap();
            let c = cos.call(&[black_box(angle.clone())]).unwrap();
            let t = tan.call(&[black_box(angle.clone())]).unwrap();
            (s, c, t)
        });
    });
}

fn benchmark_collection_operations(c: &mut Criterion) {
    let stdlib = init_stdlib();
    
    c.bench_function("map_set_get_100", |b| {
        let make_map = stdlib.get("make-map").unwrap();
        let map_set = stdlib.get("map-set").unwrap();
        let map_get = stdlib.get("map-get").unwrap();
        
        b.iter(|| {
            let mut map = make_map.call(&[]).unwrap();
            // Set 100 key-value pairs
            for i in 0..100 {
                let key = Value::String(format!("key{}", i));
                let val = Value::Int(i);
                map = map_set.call(&[map, key, val]).unwrap();
            }
            // Get all values
            for i in 0..100 {
                let key = Value::String(format!("key{}", i));
                map_get.call(&[map.clone(), key]).unwrap();
            }
            map
        });
    });
    
    c.bench_function("list_flatten_nested", |b| {
        let flatten = stdlib.get("flatten").unwrap();
        // Create deeply nested list [[1,2],[3,4],[5,6],...] 
        let nested = Value::List(
            (0..50).map(|i| {
                Value::List(vec![Value::Int(i*2), Value::Int(i*2+1)])
            }).collect()
        );
        b.iter(|| {
            flatten.call(&[black_box(nested.clone())]).unwrap()
        });
    });
}

fn benchmark_type_checking(c: &mut Criterion) {
    let stdlib = init_stdlib();
    
    c.bench_function("type_predicates_mixed", |b| {
        let is_int = stdlib.get("int?").unwrap();
        let is_string = stdlib.get("string?").unwrap();
        let is_list = stdlib.get("list?").unwrap();
        let is_nil = stdlib.get("nil?").unwrap();
        
        let values = vec![
            Value::Int(42),
            Value::String("hello".to_string()),
            Value::List(vec![]),
            Value::Nil,
            Value::Float(3.14),
        ];
        
        b.iter(|| {
            for val in &values {
                is_int.call(&[black_box(val.clone())]).unwrap();
                is_string.call(&[black_box(val.clone())]).unwrap();
                is_list.call(&[black_box(val.clone())]).unwrap();
                is_nil.call(&[black_box(val.clone())]).unwrap();
            }
        });
    });
}

criterion_group!(
    benches,
    benchmark_list_operations,
    benchmark_string_operations,
    benchmark_math_operations,
    benchmark_collection_operations,
    benchmark_type_checking
);
criterion_main!(benches);