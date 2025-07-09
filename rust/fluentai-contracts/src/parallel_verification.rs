//! Parallel contract verification for improved performance
//!
//! This module enables verification of multiple contracts in parallel,
//! significantly improving performance on multi-core systems.

use crate::{
    contract::Contract,
    errors::{ContractError, ContractResult},
    incremental::IncrementalVerifier,
    static_verification::{StaticVerifier, VerificationResult},
};
use fluentai_core::ast::Graph;
use rayon::prelude::*;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Configuration for parallel verification
#[derive(Debug, Clone)]
pub struct ParallelVerificationConfig {
    /// Number of threads to use (0 = auto-detect)
    pub num_threads: usize,

    /// Maximum contracts to verify in parallel
    pub max_parallel_contracts: usize,

    /// Enable work stealing between threads
    pub enable_work_stealing: bool,

    /// Batch size for contract distribution
    pub batch_size: usize,

    /// Priority-based scheduling
    pub enable_priority_scheduling: bool,
}

impl Default for ParallelVerificationConfig {
    fn default() -> Self {
        Self {
            num_threads: 0, // Auto-detect
            max_parallel_contracts: 100,
            enable_work_stealing: true,
            batch_size: 4,
            enable_priority_scheduling: true,
        }
    }
}

/// Parallel contract verifier
pub struct ParallelVerifier<'a> {
    graph: &'a Graph,
    config: ParallelVerificationConfig,
}

impl<'a> ParallelVerifier<'a> {
    /// Create a new parallel verifier
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            config: ParallelVerificationConfig::default(),
        }
    }

    /// Set configuration
    pub fn with_config(mut self, config: ParallelVerificationConfig) -> Self {
        self.config = config;
        self
    }

    /// Verify contracts in parallel
    pub fn verify_contracts_parallel(
        &self,
        contracts: &HashMap<String, Contract>,
    ) -> ContractResult<HashMap<String, VerificationResult>> {
        // Configure thread pool
        let pool = rayon::ThreadPoolBuilder::new()
            .num_threads(if self.config.num_threads == 0 {
                num_cpus::get()
            } else {
                self.config.num_threads
            })
            .build()
            .map_err(|e| {
                ContractError::VerificationError(format!("Failed to create thread pool: {}", e))
            })?;

        // Prepare contracts for parallel processing
        let contract_vec: Vec<(&String, &Contract)> = if self.config.enable_priority_scheduling {
            // Sort by priority (complexity, dependencies, etc.)
            let mut sorted: Vec<_> = contracts.iter().collect();
            sorted.sort_by_key(|(name, contract)| self.compute_contract_priority(name, contract));
            sorted
        } else {
            contracts.iter().collect()
        };

        // Verify in parallel
        let results = pool.install(|| {
            contract_vec
                .par_iter()
                .chunks(self.config.batch_size)
                .map(|batch| {
                    let mut batch_results = HashMap::new();

                    for (name, contract) in batch {
                        let result = self.verify_single_contract(contract);
                        batch_results.insert((*name).clone(), result);
                    }

                    batch_results
                })
                .reduce(HashMap::new, |mut acc, batch| {
                    acc.extend(batch);
                    acc
                })
        });

        Ok(results)
    }

    /// Verify a single contract (thread-safe)
    fn verify_single_contract(&self, contract: &Contract) -> VerificationResult {
        // Create thread-local verifier
        let verifier = StaticVerifier::new();

        match verifier.verify_contract(contract) {
            Ok(result) => result,
            Err(e) => VerificationResult::Unknown(format!("Verification error: {}", e)),
        }
    }

    /// Compute priority for contract verification
    fn compute_contract_priority(&self, _name: &str, contract: &Contract) -> i32 {
        let mut priority = 0;

        // Higher priority for contracts with more conditions
        priority += contract.preconditions.len() as i32 * 10;
        priority += contract.postconditions.len() as i32 * 20;
        priority += contract.invariants.len() as i32 * 30;

        // Higher priority for contracts with quantifiers (more complex)
        // This would require analyzing the contract expressions

        -priority // Negative so higher priority contracts are verified first
    }

    /// Verify contracts with dependency-aware parallelization
    pub fn verify_with_dependencies(
        &self,
        contracts: &HashMap<String, Contract>,
        incremental: &IncrementalVerifier,
    ) -> ContractResult<HashMap<String, VerificationResult>> {
        // Group contracts by dependency level
        let dependency_levels = self.compute_dependency_levels(contracts, incremental)?;

        let mut all_results = HashMap::new();

        // Process each level in parallel
        for level_contracts in dependency_levels {
            let level_results = self.verify_contracts_parallel(&level_contracts)?;
            all_results.extend(level_results);
        }

        Ok(all_results)
    }

    /// Compute dependency levels for contracts
    fn compute_dependency_levels(
        &self,
        contracts: &HashMap<String, Contract>,
        _incremental: &IncrementalVerifier,
    ) -> ContractResult<Vec<HashMap<String, Contract>>> {
        // Simplified implementation - in reality would use dependency graph
        // For now, just return all contracts in one level
        Ok(vec![contracts.clone()])
    }
}

/// Statistics for parallel verification
#[derive(Debug, Clone)]
pub struct ParallelVerificationStats {
    pub total_contracts: usize,
    pub verified_contracts: usize,
    pub failed_contracts: usize,
    pub time_elapsed_ms: u64,
    pub threads_used: usize,
    pub speedup_factor: f64,
}

/// Parallel verification coordinator with advanced features
pub struct ParallelCoordinator<'a> {
    graph: &'a Graph,
    config: ParallelVerificationConfig,
    stats: Arc<Mutex<ParallelVerificationStats>>,
}

impl<'a> ParallelCoordinator<'a> {
    pub fn new(graph: &'a Graph) -> Self {
        Self {
            graph,
            config: ParallelVerificationConfig::default(),
            stats: Arc::new(Mutex::new(ParallelVerificationStats {
                total_contracts: 0,
                verified_contracts: 0,
                failed_contracts: 0,
                time_elapsed_ms: 0,
                threads_used: 0,
                speedup_factor: 1.0,
            })),
        }
    }

    /// Verify with progress reporting
    pub fn verify_with_progress<F>(
        &self,
        contracts: &HashMap<String, Contract>,
        progress_callback: F,
    ) -> ContractResult<HashMap<String, VerificationResult>>
    where
        F: Fn(usize, usize) + Send + Sync,
    {
        let total = contracts.len();
        let completed = Arc::new(Mutex::new(0usize));
        let callback = Arc::new(progress_callback);

        let results: HashMap<String, VerificationResult> = contracts
            .par_iter()
            .map(|(name, contract)| {
                let verifier = ParallelVerifier::new(self.graph);
                let result = verifier.verify_single_contract(contract);

                // Update progress
                let mut count = completed.lock().unwrap();
                *count += 1;
                callback(*count, total);

                (name.clone(), result)
            })
            .collect();

        Ok(results)
    }

    /// Get verification statistics
    pub fn get_stats(&self) -> ParallelVerificationStats {
        self.stats.lock().unwrap().clone()
    }
}

/// Work-stealing queue for dynamic load balancing
struct WorkStealingQueue<T> {
    queues: Vec<Arc<Mutex<VecDeque<T>>>>,
}

impl<T: Send + Clone> WorkStealingQueue<T> {
    fn new(num_queues: usize) -> Self {
        let mut queues = Vec::with_capacity(num_queues);
        for _ in 0..num_queues {
            queues.push(Arc::new(Mutex::new(VecDeque::new())));
        }
        Self { queues }
    }

    fn push(&self, queue_id: usize, item: T) {
        if let Some(queue) = self.queues.get(queue_id) {
            queue.lock().unwrap().push_back(item);
        }
    }

    fn steal(&self, thief_id: usize) -> Option<T> {
        // Try to steal from other queues
        for (i, queue) in self.queues.iter().enumerate() {
            if i != thief_id {
                if let Ok(mut q) = queue.try_lock() {
                    if q.len() > 1 {
                        return q.pop_front();
                    }
                }
            }
        }
        None
    }

    fn pop(&self, queue_id: usize) -> Option<T> {
        if let Some(queue) = self.queues.get(queue_id) {
            queue.lock().unwrap().pop_front()
        } else {
            None
        }
    }
}

use std::collections::VecDeque;

#[cfg(test)]
mod tests {
    use super::*;
    use std::num::NonZeroU32;

    #[test]
    fn test_parallel_config() {
        let config = ParallelVerificationConfig::default();
        assert_eq!(config.num_threads, 0); // Auto-detect
        assert!(config.enable_work_stealing);
    }

    #[test]
    fn test_parallel_verifier_creation() {
        let graph = Graph::new();
        let verifier = ParallelVerifier::new(&graph);

        let contracts = HashMap::new();
        let results = verifier.verify_contracts_parallel(&contracts).unwrap();
        assert_eq!(results.len(), 0);
    }
}
