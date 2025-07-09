//! Comprehensive tests for FluentAI VM SIMD operations

use fluentai_vm::{
    simd::{PortableSimd, SimdOp, SimdOps},
    Value,
};
use std::f64;

#[test]
fn test_simd_add_f64_arrays() {
    if !SimdOps::is_supported() {
        println!("SIMD not supported on this CPU, using fallback");
    }

    // Test various array sizes
    let test_cases = vec![
        // Size 4 - perfect SIMD alignment
        (vec![1.0, 2.0, 3.0, 4.0], vec![5.0, 6.0, 7.0, 8.0]),
        // Size 8 - multiple SIMD operations
        (vec![1.0; 8], vec![2.0; 8]),
        // Size 7 - not aligned, needs scalar handling
        (vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0], vec![1.0; 7]),
        // Size 1 - all scalar
        (vec![42.0], vec![58.0]),
        // Large array
        (vec![1.5; 100], vec![2.5; 100]),
    ];

    for (a, b) in test_cases {
        let mut result = vec![0.0; a.len()];

        unsafe {
            SimdOps::add_f64_arrays(&a, &b, &mut result).unwrap();
        }

        // Verify results
        for i in 0..a.len() {
            assert_eq!(result[i], a[i] + b[i], "Mismatch at index {}", i);
        }
    }
}

#[test]
fn test_simd_mul_f64_arrays() {
    let a = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0];
    let b = vec![2.0, 2.0, 2.0, 2.0, 0.5, 0.5, 0.5, 0.5];
    let mut result = vec![0.0; 8];

    unsafe {
        SimdOps::mul_f64_arrays(&a, &b, &mut result).unwrap();
    }

    let expected = vec![2.0, 4.0, 6.0, 8.0, 2.5, 3.0, 3.5, 4.0];
    for i in 0..8 {
        assert_eq!(result[i], expected[i]);
    }
}

#[test]
fn test_simd_add_i64_arrays() {
    let a = vec![1i64, 2, 3, 4, 5, 6, 7, 8];
    let b = vec![8i64, 7, 6, 5, 4, 3, 2, 1];
    let mut result = vec![0i64; 8];

    unsafe {
        SimdOps::add_i64_arrays(&a, &b, &mut result).unwrap();
    }

    for i in 0..8 {
        assert_eq!(result[i], 9);
    }
}

#[test]
fn test_simd_dot_product() {
    if !SimdOps::is_supported() {
        println!("SIMD not supported on this CPU, using fallback");
    }

    // Test cases with known results
    let test_cases = vec![
        (vec![1.0, 2.0, 3.0, 4.0], vec![4.0, 3.0, 2.0, 1.0], 20.0),
        (vec![1.0; 8], vec![1.0; 8], 8.0),
        (vec![2.0; 100], vec![3.0; 100], 600.0),
        (vec![1.0, 0.0, -1.0], vec![1.0, 2.0, 3.0], -2.0),
    ];

    for (a, b, expected) in test_cases {
        unsafe {
            let result = SimdOps::dot_product_f64(&a, &b).unwrap();
            assert!(
                (result - expected).abs() < f64::EPSILON * 10.0,
                "Expected {}, got {}",
                expected,
                result
            );
        }
    }
}

#[test]
fn test_simd_length_mismatch() {
    let a = vec![1.0, 2.0, 3.0];
    let b = vec![1.0, 2.0];
    let mut result = vec![0.0; 3];

    unsafe {
        assert!(SimdOps::add_f64_arrays(&a, &b, &mut result).is_err());
        assert!(SimdOps::mul_f64_arrays(&a, &b, &mut result).is_err());
        assert!(SimdOps::dot_product_f64(&a, &b).is_err());
    }
}

#[test]
fn test_simd_with_values_float_lists() {
    let a = Value::List(vec![
        Value::Float(1.0),
        Value::Float(2.0),
        Value::Float(3.0),
        Value::Float(4.0),
    ]);
    let b = Value::List(vec![
        Value::Float(5.0),
        Value::Float(6.0),
        Value::Float(7.0),
        Value::Float(8.0),
    ]);

    // Test addition
    let result = SimdOps::apply_simd_op(SimdOp::Add, &a, &b).unwrap();
    match result {
        Value::List(list) => {
            assert_eq!(list.len(), 4);
            assert_eq!(list[0], Value::Float(6.0));
            assert_eq!(list[1], Value::Float(8.0));
            assert_eq!(list[2], Value::Float(10.0));
            assert_eq!(list[3], Value::Float(12.0));
        }
        _ => panic!("Expected list result"),
    }

    // Test multiplication
    let result = SimdOps::apply_simd_op(SimdOp::Mul, &a, &b).unwrap();
    match result {
        Value::List(list) => {
            assert_eq!(list.len(), 4);
            assert_eq!(list[0], Value::Float(5.0));
            assert_eq!(list[1], Value::Float(12.0));
            assert_eq!(list[2], Value::Float(21.0));
            assert_eq!(list[3], Value::Float(32.0));
        }
        _ => panic!("Expected list result"),
    }

    // Test dot product
    let result = SimdOps::apply_simd_op(SimdOp::DotProduct, &a, &b).unwrap();
    assert_eq!(result, Value::Float(70.0)); // 1*5 + 2*6 + 3*7 + 4*8
}

#[test]
fn test_simd_with_values_int_lists() {
    let a = Value::List(vec![
        Value::Integer(1),
        Value::Integer(2),
        Value::Integer(3),
        Value::Integer(4),
    ]);
    let b = Value::List(vec![
        Value::Integer(10),
        Value::Integer(20),
        Value::Integer(30),
        Value::Integer(40),
    ]);

    // Test addition (should work for integers)
    let result = SimdOps::apply_simd_op(SimdOp::Add, &a, &b).unwrap();
    match result {
        Value::List(list) => {
            assert_eq!(list.len(), 4);
            assert_eq!(list[0], Value::Integer(11));
            assert_eq!(list[1], Value::Integer(22));
            assert_eq!(list[2], Value::Integer(33));
            assert_eq!(list[3], Value::Integer(44));
        }
        _ => panic!("Expected list result"),
    }

    // Multiplication not supported for integer arrays
    assert!(SimdOps::apply_simd_op(SimdOp::Mul, &a, &b).is_err());

    // Dot product converts to float
    let result = SimdOps::apply_simd_op(SimdOp::DotProduct, &a, &b).unwrap();
    assert_eq!(result, Value::Float(300.0)); // 1*10 + 2*20 + 3*30 + 4*40
}

#[test]
fn test_simd_mixed_numeric_lists() {
    let a = Value::List(vec![
        Value::Integer(1),
        Value::Float(2.0),
        Value::Integer(3),
        Value::Float(4.0),
    ]);
    let b = Value::List(vec![
        Value::Float(5.0),
        Value::Integer(6),
        Value::Float(7.0),
        Value::Integer(8),
    ]);

    // Mixed lists should fail
    assert!(SimdOps::apply_simd_op(SimdOp::Add, &a, &b).is_err());
}

#[test]
fn test_simd_non_numeric_lists() {
    let a = Value::List(vec![
        Value::String("hello".to_string()),
        Value::String("world".to_string()),
    ]);
    let b = Value::List(vec![
        Value::String("foo".to_string()),
        Value::String("bar".to_string()),
    ]);

    // Non-numeric lists should fail
    assert!(SimdOps::apply_simd_op(SimdOp::Add, &a, &b).is_err());
}

#[test]
fn test_simd_empty_lists() {
    let a = Value::List(vec![]);
    let b = Value::List(vec![]);

    // Empty lists should work but return empty
    let result = SimdOps::apply_simd_op(SimdOp::Add, &a, &b).unwrap();
    match result {
        Value::List(list) => assert_eq!(list.len(), 0),
        _ => panic!("Expected list result"),
    }
}

#[test]
fn test_simd_mismatched_list_lengths() {
    let a = Value::List(vec![Value::Float(1.0), Value::Float(2.0)]);
    let b = Value::List(vec![
        Value::Float(3.0),
        Value::Float(4.0),
        Value::Float(5.0),
    ]);

    assert!(SimdOps::apply_simd_op(SimdOp::Add, &a, &b).is_err());
}

#[test]
fn test_simd_non_list_values() {
    let a = Value::Float(1.0);
    let b = Value::Float(2.0);

    assert!(SimdOps::apply_simd_op(SimdOp::Add, &a, &b).is_err());
}

#[test]
fn test_portable_simd_fallback() {
    // Test the fallback implementations directly
    let a = vec![1.0, 2.0, 3.0, 4.0];
    let b = vec![5.0, 6.0, 7.0, 8.0];
    let mut result = vec![0.0; 4];

    // Test add fallback
    PortableSimd::add_arrays_fallback(&a, &b, &mut result).unwrap();
    assert_eq!(result, vec![6.0, 8.0, 10.0, 12.0]);

    // Test multiply fallback
    PortableSimd::mul_arrays_fallback(&a, &b, &mut result).unwrap();
    assert_eq!(result, vec![5.0, 12.0, 21.0, 32.0]);

    // Test dot product fallback
    let dot = PortableSimd::dot_product_fallback(&a, &b).unwrap();
    assert_eq!(dot, 70.0);
}

#[test]
fn test_simd_edge_cases() {
    // Test with special float values
    let a = vec![0.0, f64::INFINITY, -f64::INFINITY, f64::NAN];
    let b = vec![1.0, 2.0, 3.0, 4.0];
    let mut result = vec![0.0; 4];

    unsafe {
        SimdOps::add_f64_arrays(&a, &b, &mut result).unwrap();
    }

    assert_eq!(result[0], 1.0);
    assert!(result[1].is_infinite() && result[1].is_sign_positive());
    assert!(result[2].is_infinite() && result[2].is_sign_negative());
    assert!(result[3].is_nan());
}

#[test]
fn test_simd_i64_wrapping() {
    // Test integer wrapping behavior
    let a = vec![i64::MAX, i64::MIN, 0, 1];
    let b = vec![1, 1, i64::MAX, i64::MAX];
    let mut result = vec![0i64; 4];

    unsafe {
        SimdOps::add_i64_arrays(&a, &b, &mut result).unwrap();
    }

    // Verify wrapping behavior
    assert_eq!(result[0], i64::MIN); // MAX + 1 wraps to MIN
    assert_eq!(result[1], i64::MIN + 1); // MIN + 1
    assert_eq!(result[2], i64::MAX); // 0 + MAX
    assert_eq!(result[3], i64::MIN); // 1 + MAX wraps
}

#[test]
#[ignore = "Performance test - run manually"]
fn test_simd_performance_comparison() {
    use std::time::Instant;

    let size = 1_000_000;
    let a = vec![1.5; size];
    let b = vec![2.5; size];
    let mut result = vec![0.0; size];

    // Measure SIMD performance
    let start = Instant::now();
    unsafe {
        SimdOps::add_f64_arrays(&a, &b, &mut result).unwrap();
    }
    let simd_time = start.elapsed();

    // Measure fallback performance
    let start = Instant::now();
    PortableSimd::add_arrays_fallback(&a, &b, &mut result).unwrap();
    let fallback_time = start.elapsed();

    println!("SIMD time: {:?}", simd_time);
    println!("Fallback time: {:?}", fallback_time);
    println!(
        "Speedup: {:.2}x",
        fallback_time.as_secs_f64() / simd_time.as_secs_f64()
    );

    // Test dot product performance
    let start = Instant::now();
    unsafe {
        let _ = SimdOps::dot_product_f64(&a, &b).unwrap();
    }
    let simd_dot_time = start.elapsed();

    let start = Instant::now();
    let _ = PortableSimd::dot_product_fallback(&a, &b).unwrap();
    let fallback_dot_time = start.elapsed();

    println!("Dot product SIMD time: {:?}", simd_dot_time);
    println!("Dot product fallback time: {:?}", fallback_dot_time);
    println!(
        "Dot product speedup: {:.2}x",
        fallback_dot_time.as_secs_f64() / simd_dot_time.as_secs_f64()
    );
}
