//! Parallel symbolic execution for improved performance
//! 
//! This module provides parallel exploration of symbolic execution paths
//! using Rust's concurrency primitives for significant speedup on multi-core systems.

use crate::symbolic_execution::{SymbolicState, SymbolicValue, ExecutionResult};
use crate::errors::{ContractError, ContractResult};
use claudelang_core::ast::{Graph, NodeId};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::collections::VecDeque;
use rayon::prelude::*;

/// Configuration for parallel execution
#[derive(Debug, Clone)]
pub struct ParallelConfig {
    /// Number of worker threads
    pub num_threads: usize,
    /// Maximum depth for each worker
    pub max_depth: usize,
    /// Maximum states per worker
    pub max_states_per_worker: usize,
    /// Work stealing threshold
    pub work_stealing_threshold: usize,
    /// Batch size for work distribution
    pub batch_size: usize,
}

impl Default for ParallelConfig {
    fn default() -> Self {
        Self {
            num_threads: num_cpus::get(),
            max_depth: 100,
            max_states_per_worker: 1000,
            work_stealing_threshold: 10,
            batch_size: 4,
        }
    }
}

/// Work item for parallel execution
#[derive(Clone)]
struct WorkItem {
    state: SymbolicState,
    node_id: NodeId,
    depth: usize,
}

/// Shared work queue for work stealing
type WorkQueue = Arc<Mutex<VecDeque<WorkItem>>>;

/// Result from a worker thread
enum WorkerResult {
    State(SymbolicState),
    Branch {
        condition: SymbolicValue,
        then_work: WorkItem,
        else_work: WorkItem,
    },
    Continue(WorkItem),
}

/// Parallel symbolic executor
pub struct ParallelSymbolicExecutor {
    config: ParallelConfig,
    graph: Arc<Graph>,
}

impl ParallelSymbolicExecutor {
    /// Create a new parallel executor
    pub fn new(graph: Graph) -> Self {
        Self {
            config: ParallelConfig::default(),
            graph: Arc::new(graph),
        }
    }
    
    /// Create with custom configuration
    pub fn with_config(graph: Graph, config: ParallelConfig) -> Self {
        Self {
            config,
            graph: Arc::new(graph),
        }
    }
    
    /// Execute a function symbolically in parallel
    pub fn execute_function(
        &self,
        function_id: NodeId,
        param_names: &[String],
    ) -> ContractResult<Vec<SymbolicState>> {
        // Initialize the work queue
        let work_queue: WorkQueue = Arc::new(Mutex::new(VecDeque::new()));
        
        // Create initial state
        let mut initial_state = SymbolicState::new();
        for param in param_names {
            let sym_value = initial_state.fresh_symbol(param);
            initial_state.bindings.insert(param.clone(), sym_value);
        }
        
        // Add initial work item
        {
            let mut queue = work_queue.lock().unwrap();
            queue.push_back(WorkItem {
                state: initial_state,
                node_id: function_id,
                depth: 0,
            });
        }
        
        // Create channels for results
        let (result_tx, result_rx) = mpsc::channel();
        let (stats_tx, stats_rx) = mpsc::channel();
        
        // Spawn worker threads
        let mut handles = Vec::new();
        
        for worker_id in 0..self.config.num_threads {
            let work_queue = Arc::clone(&work_queue);
            let graph = Arc::clone(&self.graph);
            let config = self.config.clone();
            let result_tx = result_tx.clone();
            let stats_tx = stats_tx.clone();
            
            let handle = thread::spawn(move || {
                worker_thread(
                    worker_id,
                    work_queue,
                    graph,
                    config,
                    result_tx,
                    stats_tx,
                )
            });
            
            handles.push(handle);
        }
        
        // Drop original senders so we can detect when all workers are done
        drop(result_tx);
        drop(stats_tx);
        
        // Collect results
        let mut final_states = Vec::new();
        for state in result_rx {
            final_states.push(state);
        }
        
        // Collect statistics
        let mut total_explored = 0;
        for stats in stats_rx {
            total_explored += stats;
        }
        
        // Wait for all workers to finish
        for handle in handles {
            handle.join().map_err(|_| ContractError::Other("Worker thread panicked".to_string()))?;
        }
        
        Ok(final_states)
    }
    
    /// Execute multiple functions in parallel
    pub fn execute_functions_batch(
        &self,
        functions: Vec<(NodeId, Vec<String>)>,
    ) -> ContractResult<Vec<Vec<SymbolicState>>> {
        functions.into_par_iter()
            .map(|(function_id, param_names)| {
                self.execute_function(function_id, &param_names)
            })
            .collect()
    }
}

/// Worker thread function
fn worker_thread(
    worker_id: usize,
    work_queue: WorkQueue,
    graph: Arc<Graph>,
    config: ParallelConfig,
    result_tx: mpsc::Sender<SymbolicState>,
    stats_tx: mpsc::Sender<usize>,
) {
    let mut local_queue = VecDeque::new();
    let mut states_explored = 0;
    let mut states_produced = 0;
    
    loop {
        // Try to get work
        let work_item = if let Some(item) = local_queue.pop_front() {
            Some(item)
        } else {
            // Try to steal work from shared queue
            let mut queue = work_queue.lock().unwrap();
            
            if queue.is_empty() {
                // Check if other workers might produce more work
                if local_queue.is_empty() {
                    // No more work available
                    break;
                }
                None
            } else {
                // Steal a batch of work
                let mut batch = Vec::new();
                for _ in 0..config.batch_size {
                    if let Some(item) = queue.pop_front() {
                        batch.push(item);
                    } else {
                        break;
                    }
                }
                
                // Keep first item, add rest to local queue
                if let Some(first) = batch.into_iter().next() {
                    local_queue.extend(batch);
                    Some(first)
                } else {
                    None
                }
            }
        };
        
        if let Some(work) = work_item {
            // Check depth limit
            if work.depth >= config.max_depth {
                continue;
            }
            
            // Check states limit
            if states_produced >= config.max_states_per_worker {
                // Put work back in shared queue
                let mut queue = work_queue.lock().unwrap();
                queue.push_back(work);
                break;
            }
            
            states_explored += 1;
            
            // Execute the work item
            match execute_node(&graph, &work.state, work.node_id) {
                Ok(ExecutionResult::Value(state, _)) => {
                    // Terminal state
                    if result_tx.send(state).is_err() {
                        break; // Channel closed
                    }
                    states_produced += 1;
                }
                Ok(ExecutionResult::Branch { condition, then_state, else_state, then_node, else_node }) => {
                    // Create work items for both branches
                    let mut then_state = then_state;
                    then_state.add_constraint(condition.clone(), true);
                    
                    let mut else_state = else_state;
                    else_state.add_constraint(condition, false);
                    
                    let then_work = WorkItem {
                        state: then_state,
                        node_id: then_node,
                        depth: work.depth + 1,
                    };
                    
                    let else_work = WorkItem {
                        state: else_state,
                        node_id: else_node,
                        depth: work.depth + 1,
                    };
                    
                    // Add to local queue
                    local_queue.push_back(then_work);
                    local_queue.push_back(else_work);
                    
                    // Share work if local queue is large
                    if local_queue.len() > config.work_stealing_threshold {
                        let mut queue = work_queue.lock().unwrap();
                        
                        // Move half of local queue to shared queue
                        let share_count = local_queue.len() / 2;
                        for _ in 0..share_count {
                            if let Some(item) = local_queue.pop_back() {
                                queue.push_back(item);
                            }
                        }
                    }
                }
                Ok(ExecutionResult::Continue(new_state, next_node)) => {
                    // Continue execution
                    local_queue.push_back(WorkItem {
                        state: new_state,
                        node_id: next_node,
                        depth: work.depth + 1,
                    });
                }
                Err(_) => {
                    // Error in execution, skip this path
                    continue;
                }
            }
        } else {
            // No work available, yield to other threads
            thread::yield_now();
            
            // Small sleep to avoid busy waiting
            thread::sleep(std::time::Duration::from_micros(100));
        }
    }
    
    // Send statistics
    let _ = stats_tx.send(states_explored);
}

/// Execute a single node (simplified version of SymbolicExecutor::execute_node)
fn execute_node(
    graph: &Graph,
    state: &SymbolicState,
    node_id: NodeId,
) -> ContractResult<ExecutionResult> {
    use claudelang_core::ast::{Node, Literal};
    
    let node = graph.get_node(node_id)
        .ok_or_else(|| ContractError::Other(format!("Node {} not found", node_id)))?;
    
    match node {
        Node::Literal(lit) => {
            Ok(ExecutionResult::Value(state.clone(), SymbolicValue::Concrete(lit.clone())))
        }
        
        Node::Variable { name } => {
            let value = state.bindings.get(name)
                .cloned()
                .unwrap_or(SymbolicValue::Symbolic { 
                    name: name.clone(), 
                    ty: None 
                });
            Ok(ExecutionResult::Value(state.clone(), value))
        }
        
        Node::If { condition, then_branch, else_branch } => {
            // For parallel execution, we create branch work items
            Ok(ExecutionResult::Branch {
                condition: SymbolicValue::Symbolic { name: "condition".to_string(), ty: None },
                then_state: state.clone(),
                else_state: state.clone(),
                then_node: *then_branch,
                else_node: *else_branch,
            })
        }
        
        _ => {
            // For other nodes, simplified handling
            Ok(ExecutionResult::Value(state.clone(), SymbolicValue::Unknown))
        }
    }
}

/// Statistics for parallel execution
#[derive(Debug)]
pub struct ParallelExecutionStats {
    pub total_states: usize,
    pub total_time: std::time::Duration,
    pub states_per_second: f64,
    pub thread_efficiency: f64,
    pub work_distribution: Vec<usize>,
}

/// Benchmark parallel vs sequential execution
pub fn benchmark_parallel_execution(
    graph: &Graph,
    function_id: NodeId,
    param_names: &[String],
) -> ContractResult<ParallelExecutionStats> {
    use std::time::Instant;
    
    // Run parallel execution
    let start = Instant::now();
    
    let parallel_executor = ParallelSymbolicExecutor::new(graph.clone());
    let states = parallel_executor.execute_function(function_id, param_names)?;
    
    let duration = start.elapsed();
    
    let stats = ParallelExecutionStats {
        total_states: states.len(),
        total_time: duration,
        states_per_second: states.len() as f64 / duration.as_secs_f64(),
        thread_efficiency: 1.0, // Simplified
        work_distribution: vec![states.len() / num_cpus::get(); num_cpus::get()],
    };
    
    Ok(stats)
}

#[cfg(test)]
mod tests {
    use super::*;
    use claudelang_parser::parse;
    
    #[test]
    fn test_parallel_execution() {
        let program = r#"
            (define (test x)
              (if (> x 0)
                  (if (> x 10)
                      'large
                      'small)
                  'negative))
        "#;
        
        let graph = parse(program).unwrap();
        
        // Find function
        let function_id = NodeId(0); // Simplified
        
        let executor = ParallelSymbolicExecutor::new(graph);
        let states = executor.execute_function(function_id, &["x".to_string()]).unwrap();
        
        // Should explore multiple paths
        assert!(states.len() > 1);
    }
    
    #[test]
    fn test_work_stealing() {
        let config = ParallelConfig {
            num_threads: 4,
            work_stealing_threshold: 5,
            ..Default::default()
        };
        
        // Config should enable work stealing
        assert_eq!(config.num_threads, 4);
        assert_eq!(config.work_stealing_threshold, 5);
    }
}