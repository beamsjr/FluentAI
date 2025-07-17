//! Core profiling data structures that can be shared between VM and optimizer
//!
//! This module defines the data structures for runtime profiling information
//! without any implementation details, allowing both the VM and optimizer
//! to work with profiling data without circular dependencies.

use crate::ast::NodeId;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Profile data for a single function
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FunctionProfileData {
    /// Unique identifier for the function (chunk_id in VM)
    pub chunk_id: usize,
    /// Optional function name
    pub name: Option<String>,
    /// Number of times the function was called
    pub call_count: u64,
    /// Total time spent in the function (in nanoseconds)
    pub total_time_ns: u64,
    /// Average time per call (in nanoseconds)
    pub avg_time_ns: u64,
}

/// Profile data for value observations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueProfileData {
    /// Node that produces the value
    pub node_id: NodeId,
    /// Total number of observations
    pub total_count: u64,
    /// Most common value and its count
    pub most_common: Option<(String, u64)>,
    /// Percentage of observations that are the most common value
    pub skew_percentage: f64,
}

/// Profile data for loops
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LoopProfileData {
    /// Loop identifier
    pub loop_id: NodeId,
    /// Number of times the loop was executed
    pub execution_count: u64,
    /// Average iterations per execution
    pub avg_iterations: f64,
    /// Maximum iterations observed
    pub max_iterations: u64,
}

/// Interface for accessing profiling data
pub trait ProfileDataProvider {
    /// Get hot functions above a threshold
    fn get_hot_functions(&self, threshold: u64) -> Vec<FunctionProfileData>;
    
    /// Get values with high skew
    fn get_skewed_values(&self, threshold: f64) -> Vec<ValueProfileData>;
    
    /// Get hot loops
    fn get_hot_loops(&self, threshold: f64) -> Vec<LoopProfileData>;
}