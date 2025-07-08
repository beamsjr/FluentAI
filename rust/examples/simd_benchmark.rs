//! SIMD Performance Benchmark
//! 
//! Demonstrates FluentAI's SIMD operations achieving 4-8x speedup
//! for vectorized numeric computations.

use fluentai_vm::simd::{SimdOps, simd_available};
use std::time::Instant;

const ARRAY_SIZE: usize = 1024;
const ITERATIONS: usize = 10_000;

fn benchmark_scalar_ops(a: &[f64], b: &[f64]) -> (Vec<f64>, f64) {
    let start = Instant::now();
    
    for _ in 0..ITERATIONS {
        let mut result = vec![0.0; ARRAY_SIZE];
        for i in 0..ARRAY_SIZE {
            result[i] = a[i] + b[i];
        }
        
        // Prevent optimization
        std::hint::black_box(&result);
    }
    
    let duration = start.elapsed();
    
    // Final computation for verification
    let mut result = vec![0.0; ARRAY_SIZE];
    for i in 0..ARRAY_SIZE {
        result[i] = a[i] + b[i];
    }
    
    (result, duration.as_secs_f64())
}

fn benchmark_simd_ops(a: &[f64], b: &[f64]) -> (Vec<f64>, f64) {
    let simd = SimdOps::new();
    let start = Instant::now();
    
    for _ in 0..ITERATIONS {
        let mut result = vec![0.0; ARRAY_SIZE];
        simd.add_f64_arrays(a, b, &mut result);
        
        // Prevent optimization
        std::hint::black_box(&result);
    }
    
    let duration = start.elapsed();
    
    // Final computation for verification
    let mut result = vec![0.0; ARRAY_SIZE];
    simd.add_f64_arrays(a, b, &mut result);
    
    (result, duration.as_secs_f64())
}

fn benchmark_dot_product_scalar(a: &[f64], b: &[f64]) -> (f64, f64) {
    let start = Instant::now();
    
    let mut sum = 0.0;
    for _ in 0..ITERATIONS {
        sum = 0.0;
        for i in 0..ARRAY_SIZE {
            sum += a[i] * b[i];
        }
        std::hint::black_box(sum);
    }
    
    let duration = start.elapsed();
    (sum, duration.as_secs_f64())
}

fn benchmark_dot_product_simd(a: &[f64], b: &[f64]) -> (f64, f64) {
    let simd = SimdOps::new();
    let start = Instant::now();
    
    let mut result = 0.0;
    for _ in 0..ITERATIONS {
        result = simd.dot_product_f64(a, b);
        std::hint::black_box(result);
    }
    
    let duration = start.elapsed();
    (result, duration.as_secs_f64())
}

fn main() {
    println!("FluentAI SIMD Performance Benchmark");
    println!("===================================");
    println!("Array size: {} elements", ARRAY_SIZE);
    println!("Iterations: {}\n", ITERATIONS);
    
    // Check SIMD availability
    if !simd_available() {
        println!("WARNING: SIMD not available on this platform!");
        println!("Results will show fallback performance.\n");
    }
    
    // Initialize test arrays
    let a: Vec<f64> = (0..ARRAY_SIZE).map(|i| i as f64 * 0.5).collect();
    let b: Vec<f64> = (0..ARRAY_SIZE).map(|i| i as f64 * 0.3).collect();
    
    // Benchmark array addition
    println!("Array Addition (a + b):");
    println!("-----------------------");
    let (scalar_result, scalar_time) = benchmark_scalar_ops(&a, &b);
    let (simd_result, simd_time) = benchmark_simd_ops(&a, &b);
    
    // Verify results match
    let results_match = scalar_result.iter()
        .zip(simd_result.iter())
        .all(|(a, b)| (a - b).abs() < 1e-10);
    
    println!("Scalar time: {:.4} seconds", scalar_time);
    println!("SIMD time:   {:.4} seconds", simd_time);
    println!("Speedup:     {:.1}x", scalar_time / simd_time);
    println!("Results match: {}", if results_match { "✓" } else { "✗" });
    
    // Benchmark dot product
    println!("\nDot Product (a · b):");
    println!("--------------------");
    let (scalar_dot, scalar_dot_time) = benchmark_dot_product_scalar(&a, &b);
    let (simd_dot, simd_dot_time) = benchmark_dot_product_simd(&a, &b);
    
    println!("Scalar time: {:.4} seconds", scalar_dot_time);
    println!("SIMD time:   {:.4} seconds", simd_dot_time);
    println!("Speedup:     {:.1}x", scalar_dot_time / simd_dot_time);
    println!("Results match: {}", if (scalar_dot - simd_dot).abs() < 1e-10 { "✓" } else { "✗" });
    
    // Summary
    println!("\n========================================");
    let avg_speedup = ((scalar_time / simd_time) + (scalar_dot_time / simd_dot_time)) / 2.0;
    println!("Average SIMD speedup: {:.1}x", avg_speedup);
    
    if avg_speedup >= 4.0 {
        println!("\n✓ VERIFIED: FluentAI SIMD operations achieve 4-8x speedup");
    } else if simd_available() {
        println!("\n⚠ SIMD speedup below expected range (4-8x)");
    } else {
        println!("\n⚠ SIMD not available - using fallback implementation");
    }
}