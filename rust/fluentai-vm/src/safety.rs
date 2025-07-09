//! Safety improvements for the VM
//!
//! This module contains safety-related types and functions to improve
//! the robustness of the VM implementation.

use anyhow::{anyhow, Result};
use std::sync::atomic::{AtomicU64, Ordering};

/// Type-safe ID generation for promises, channels, etc.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PromiseId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChannelId(pub u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TaskId(pub u64);

/// ID generator using atomic counter
pub struct IdGenerator {
    counter: AtomicU64,
}

impl IdGenerator {
    pub fn new() -> Self {
        Self {
            counter: AtomicU64::new(1),
        }
    }

    pub fn next_promise_id(&self) -> PromiseId {
        PromiseId(self.counter.fetch_add(1, Ordering::Relaxed))
    }

    pub fn next_channel_id(&self) -> ChannelId {
        ChannelId(self.counter.fetch_add(1, Ordering::Relaxed))
    }

    pub fn next_task_id(&self) -> TaskId {
        TaskId(self.counter.fetch_add(1, Ordering::Relaxed))
    }
}

impl Default for IdGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for PromiseId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "promise:{}", self.0)
    }
}

impl std::fmt::Display for ChannelId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "channel:{}", self.0)
    }
}

// Conversion functions for compatibility with string-based IDs
impl PromiseId {
    pub fn from_string(s: &str) -> Option<Self> {
        s.strip_prefix("promise:")
            .and_then(|id_str| id_str.parse::<u64>().ok())
            .map(PromiseId)
    }
}

impl ChannelId {
    pub fn from_string(s: &str) -> Option<Self> {
        s.strip_prefix("channel:")
            .and_then(|id_str| id_str.parse::<u64>().ok())
            .map(ChannelId)
    }
}

impl std::fmt::Display for TaskId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "task:{}", self.0)
    }
}

/// Safe arithmetic operations with overflow checking
pub mod checked_ops {
    use super::*;

    #[inline]
    pub fn add_i64(a: i64, b: i64) -> Result<i64> {
        a.checked_add(b)
            .ok_or_else(|| anyhow!("Integer overflow in addition: {} + {}", a, b))
    }

    #[inline]
    pub fn sub_i64(a: i64, b: i64) -> Result<i64> {
        a.checked_sub(b)
            .ok_or_else(|| anyhow!("Integer overflow in subtraction: {} - {}", a, b))
    }

    #[inline]
    pub fn mul_i64(a: i64, b: i64) -> Result<i64> {
        a.checked_mul(b)
            .ok_or_else(|| anyhow!("Integer overflow in multiplication: {} * {}", a, b))
    }

    #[inline]
    pub fn div_i64(a: i64, b: i64) -> Result<i64> {
        if b == 0 {
            return Err(anyhow!("Division by zero"));
        }
        // Check for i64::MIN / -1 which would overflow
        if a == i64::MIN && b == -1 {
            return Err(anyhow!("Integer overflow in division: {} / {}", a, b));
        }
        Ok(a / b)
    }

    #[inline]
    pub fn mod_i64(a: i64, b: i64) -> Result<i64> {
        if b == 0 {
            return Err(anyhow!("Modulo by zero"));
        }
        // Check for i64::MIN % -1 which would overflow
        if a == i64::MIN && b == -1 {
            return Ok(0);
        }
        Ok(a % b)
    }

    #[inline]
    pub fn neg_i64(a: i64) -> Result<i64> {
        a.checked_neg()
            .ok_or_else(|| anyhow!("Integer overflow in negation: -{}", a))
    }
}

/// Resource limits for VM execution
#[derive(Debug, Clone)]
pub struct ResourceLimits {
    pub max_stack_depth: usize,
    pub max_call_depth: usize,
    pub max_cells: usize,
    pub max_promises: usize,
    pub max_channels: usize,
    pub max_memory_bytes: usize,
    pub channel_buffer_size: usize,
}

impl Default for ResourceLimits {
    fn default() -> Self {
        Self {
            max_stack_depth: 10_000,
            max_call_depth: 1_000,
            max_cells: 100_000,
            max_promises: 10_000,
            max_channels: 1_000,
            max_memory_bytes: 100 * 1024 * 1024, // 100MB
            channel_buffer_size: 1024,
        }
    }
}

impl ResourceLimits {
    /// Create limits suitable for untrusted code
    pub fn sandboxed() -> Self {
        Self {
            max_stack_depth: 1_000,
            max_call_depth: 100,
            max_cells: 1_000,
            max_promises: 100,
            max_channels: 10,
            max_memory_bytes: 10 * 1024 * 1024, // 10MB
            channel_buffer_size: 100,
        }
    }

    /// Create limits suitable for testing
    pub fn testing() -> Self {
        Self {
            max_stack_depth: 100,
            max_call_depth: 20,
            max_cells: 100,
            max_promises: 10,
            max_channels: 5,
            max_memory_bytes: 1024 * 1024, // 1MB
            channel_buffer_size: 10,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_id_generation() {
        let gen = IdGenerator::new();
        let id1 = gen.next_promise_id();
        let id2 = gen.next_promise_id();
        assert_ne!(id1, id2);
        assert_eq!(format!("{}", id1), "promise:1");
        assert_eq!(format!("{}", id2), "promise:2");
    }

    #[test]
    fn test_checked_arithmetic() {
        use checked_ops::*;

        // Normal operations
        assert_eq!(add_i64(2, 3).unwrap(), 5);
        assert_eq!(sub_i64(5, 3).unwrap(), 2);
        assert_eq!(mul_i64(3, 4).unwrap(), 12);
        assert_eq!(div_i64(12, 3).unwrap(), 4);
        assert_eq!(mod_i64(10, 3).unwrap(), 1);
        assert_eq!(neg_i64(5).unwrap(), -5);

        // Overflow cases
        assert!(add_i64(i64::MAX, 1).is_err());
        assert!(sub_i64(i64::MIN, 1).is_err());
        assert!(mul_i64(i64::MAX, 2).is_err());
        assert!(neg_i64(i64::MIN).is_err());

        // Division by zero
        assert!(div_i64(10, 0).is_err());
        assert!(mod_i64(10, 0).is_err());

        // Special division case
        assert!(div_i64(i64::MIN, -1).is_err());
        assert_eq!(mod_i64(i64::MIN, -1).unwrap(), 0);
    }
}
