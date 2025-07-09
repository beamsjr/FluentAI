//! SIMD (Single Instruction, Multiple Data) operations for parallel numeric computation
//!
//! This module provides vectorized operations for arrays and lists, enabling
//! parallel processing of numeric data on modern CPUs.

use anyhow::{anyhow, Result};
use fluentai_core::value::Value;
use std::mem;
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
use std::arch::x86_64::*;

/// SIMD operations for numeric arrays
pub struct SimdOps;

impl SimdOps {
    /// Add two f64 arrays using AVX2 (processes 4 elements at once)
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    #[target_feature(enable = "avx2")]
    #[inline]
    pub unsafe fn add_f64_arrays(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        let len = a.len();
        let simd_len = len & !3; // Round down to multiple of 4

        // Process 4 elements at a time
        for i in (0..simd_len).step_by(4) {
            let va = _mm256_loadu_pd(&a[i]);
            let vb = _mm256_loadu_pd(&b[i]);
            let vr = _mm256_add_pd(va, vb);
            _mm256_storeu_pd(&mut result[i], vr);
        }

        // Handle remaining elements
        for i in simd_len..len {
            result[i] = a[i] + b[i];
        }

        Ok(())
    }

    /// Multiply two f64 arrays using AVX2
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    #[target_feature(enable = "avx2")]
    #[inline]
    pub unsafe fn mul_f64_arrays(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        let len = a.len();
        let simd_len = len & !3;

        for i in (0..simd_len).step_by(4) {
            let va = _mm256_loadu_pd(&a[i]);
            let vb = _mm256_loadu_pd(&b[i]);
            let vr = _mm256_mul_pd(va, vb);
            _mm256_storeu_pd(&mut result[i], vr);
        }

        for i in simd_len..len {
            result[i] = a[i] * b[i];
        }

        Ok(())
    }

    /// Add two i64 arrays using AVX2 (processes 4 elements at once)
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    #[target_feature(enable = "avx2")]
    #[inline]
    pub unsafe fn add_i64_arrays(a: &[i64], b: &[i64], result: &mut [i64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        let len = a.len();
        let simd_len = len & !3;

        for i in (0..simd_len).step_by(4) {
            let va = _mm256_loadu_si256(&a[i] as *const i64 as *const __m256i);
            let vb = _mm256_loadu_si256(&b[i] as *const i64 as *const __m256i);
            let vr = _mm256_add_epi64(va, vb);
            _mm256_storeu_si256(&mut result[i] as *mut i64 as *mut __m256i, vr);
        }

        for i in simd_len..len {
            result[i] = a[i].wrapping_add(b[i]);
        }

        Ok(())
    }

    /// Dot product of two f64 arrays using AVX2 with FMA
    #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
    #[target_feature(enable = "avx2,fma")]
    #[inline]
    pub unsafe fn dot_product_f64(a: &[f64], b: &[f64]) -> Result<f64> {
        if a.len() != b.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        let len = a.len();
        let simd_len = len & !3;
        let mut sum = _mm256_setzero_pd();

        // Process 4 elements at a time using FMA (Fused Multiply-Add)
        for i in (0..simd_len).step_by(4) {
            let va = _mm256_loadu_pd(&a[i]);
            let vb = _mm256_loadu_pd(&b[i]);
            sum = _mm256_fmadd_pd(va, vb, sum);
        }

        // Sum the 4 values in the vector
        let mut result = 0.0;
        let sum_array: [f64; 4] = mem::transmute(sum);
        for &v in &sum_array {
            result += v;
        }

        // Handle remaining elements
        for i in simd_len..len {
            result += a[i] * b[i];
        }

        Ok(result)
    }

    /// Fallback add for non-x86 architectures
    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    pub unsafe fn add_f64_arrays(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        PortableSimd::add_arrays_fallback(a, b, result)
    }

    /// Fallback multiply for non-x86 architectures
    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    pub unsafe fn mul_f64_arrays(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        PortableSimd::mul_arrays_fallback(a, b, result)
    }

    /// Fallback add for non-x86 architectures
    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    pub unsafe fn add_i64_arrays(a: &[i64], b: &[i64], result: &mut [i64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }
        for i in 0..a.len() {
            result[i] = a[i].wrapping_add(b[i]);
        }
        Ok(())
    }

    /// Fallback dot product for non-x86 architectures
    #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
    pub unsafe fn dot_product_f64(a: &[f64], b: &[f64]) -> Result<f64> {
        PortableSimd::dot_product_fallback(a, b)
    }

    /// Check if CPU supports required SIMD features
    pub fn is_supported() -> bool {
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        {
            is_x86_feature_detected!("avx2") && is_x86_feature_detected!("fma")
        }
        #[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
        {
            false
        }
    }

    /// Apply SIMD operations to Value types
    pub fn apply_simd_op(op: SimdOp, a: &Value, b: &Value) -> Result<Value> {
        match (a, b) {
            (Value::List(list_a), Value::List(list_b)) => {
                // Check if all elements are the same numeric type
                let (all_f64_a, all_i64_a) = Self::check_numeric_list(list_a);
                let (all_f64_b, all_i64_b) = Self::check_numeric_list(list_b);

                if (all_f64_a && all_f64_b) || (op == SimdOp::DotProduct && all_i64_a && all_i64_b)
                {
                    // Convert to f64 arrays and apply SIMD
                    // For dot product, we also accept integer lists and convert them
                    let a_vec: Vec<f64> = list_a
                        .iter()
                        .map(|v| match v {
                            Value::Float(f) => *f,
                            Value::Integer(i) => *i as f64,
                            _ => 0.0,
                        })
                        .collect();
                    let b_vec: Vec<f64> = list_b
                        .iter()
                        .map(|v| match v {
                            Value::Float(f) => *f,
                            Value::Integer(i) => *i as f64,
                            _ => 0.0,
                        })
                        .collect();

                    let mut result = vec![0.0; a_vec.len()];

                    unsafe {
                        match op {
                            SimdOp::Add => Self::add_f64_arrays(&a_vec, &b_vec, &mut result)?,
                            SimdOp::Mul => Self::mul_f64_arrays(&a_vec, &b_vec, &mut result)?,
                            SimdOp::DotProduct => {
                                let dot = Self::dot_product_f64(&a_vec, &b_vec)?;
                                return Ok(Value::Float(dot));
                            }
                        }
                    }

                    Ok(Value::List(result.into_iter().map(Value::Float).collect()))
                } else if all_i64_a && all_i64_b && op != SimdOp::DotProduct {
                    // Integer SIMD operations
                    let a_vec: Vec<i64> = list_a
                        .iter()
                        .map(|v| match v {
                            Value::Integer(i) => *i,
                            _ => 0,
                        })
                        .collect();
                    let b_vec: Vec<i64> = list_b
                        .iter()
                        .map(|v| match v {
                            Value::Integer(i) => *i,
                            _ => 0,
                        })
                        .collect();

                    let mut result = vec![0i64; a_vec.len()];

                    unsafe {
                        match op {
                            SimdOp::Add => Self::add_i64_arrays(&a_vec, &b_vec, &mut result)?,
                            _ => return Err(anyhow!("Operation not supported for integer arrays")),
                        }
                    }

                    Ok(Value::List(
                        result.into_iter().map(Value::Integer).collect(),
                    ))
                } else {
                    Err(anyhow!("SIMD operations require homogeneous numeric lists"))
                }
            }
            _ => Err(anyhow!("SIMD operations require list values")),
        }
    }

    /// Check if a list contains only numeric values
    fn check_numeric_list(list: &[Value]) -> (bool, bool) {
        let mut all_float = true;
        let mut all_int = true;

        for v in list {
            match v {
                Value::Float(_) => all_int = false,
                Value::Integer(_) => all_float = false,
                _ => {
                    all_float = false;
                    all_int = false;
                    break;
                }
            }
        }

        (all_float || (all_int && list.is_empty()), all_int)
    }
}

/// SIMD operation types
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SimdOp {
    Add,
    Mul,
    DotProduct,
}

/// Portable SIMD wrapper for cross-platform support
pub struct PortableSimd;

impl PortableSimd {
    /// Fallback implementation for platforms without AVX2
    pub fn add_arrays_fallback(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        for i in 0..a.len() {
            result[i] = a[i] + b[i];
        }

        Ok(())
    }

    /// Fallback multiply implementation
    pub fn mul_arrays_fallback(a: &[f64], b: &[f64], result: &mut [f64]) -> Result<()> {
        if a.len() != b.len() || a.len() != result.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        for i in 0..a.len() {
            result[i] = a[i] * b[i];
        }

        Ok(())
    }

    /// Fallback dot product implementation
    pub fn dot_product_fallback(a: &[f64], b: &[f64]) -> Result<f64> {
        if a.len() != b.len() {
            return Err(anyhow!("Array lengths must match"));
        }

        let mut result = 0.0;
        for i in 0..a.len() {
            result += a[i] * b[i];
        }

        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simd_add_f64() {
        if !SimdOps::is_supported() {
            println!("SIMD not supported on this CPU");
            return;
        }

        let a = vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0];
        let b = vec![8.0, 7.0, 6.0, 5.0, 4.0, 3.0, 2.0, 1.0];
        let mut result = vec![0.0; 8];

        unsafe {
            SimdOps::add_f64_arrays(&a, &b, &mut result).unwrap();
        }

        for i in 0..8 {
            assert_eq!(result[i], 9.0);
        }
    }

    #[test]
    fn test_simd_dot_product() {
        if !SimdOps::is_supported() {
            return;
        }

        let a = vec![1.0, 2.0, 3.0, 4.0];
        let b = vec![4.0, 3.0, 2.0, 1.0];

        unsafe {
            let result = SimdOps::dot_product_f64(&a, &b).unwrap();
            assert_eq!(result, 20.0); // 1*4 + 2*3 + 3*2 + 4*1
        }
    }

    #[test]
    fn test_simd_with_values() {
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

        if SimdOps::is_supported() {
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
        }
    }
}
