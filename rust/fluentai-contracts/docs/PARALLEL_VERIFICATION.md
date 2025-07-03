# Parallel Contract Verification

This document explains the parallel verification system that leverages multi-core processors to verify multiple contracts simultaneously.

## Overview

Parallel verification provides significant performance improvements by:
- Verifying independent contracts concurrently
- Utilizing all available CPU cores
- Implementing work-stealing for load balancing
- Supporting priority-based scheduling
- Enabling progress monitoring

## Architecture

### Core Components

1. **ParallelVerifier**: Main verification engine
2. **ParallelCoordinator**: Advanced coordination with progress tracking
3. **WorkStealingQueue**: Dynamic load balancing
4. **Thread Pool**: Managed by Rayon for efficient parallelism

### Parallelization Strategy

```
┌─────────────────┐
│ Contract Queue  │
└────────┬────────┘
         │
    ┌────┴────┐
    │Scheduler│
    └────┬────┘
         │
┌────────┴────────┬────────────┬────────────┐
│   Thread 1      │  Thread 2  │  Thread N   │
│  ┌─────────┐    │ ┌────────┐ │ ┌────────┐ │
│  │Verifier │    │ │Verifier│ │ │Verifier│ │
│  └─────────┘    │ └────────┘ │ └────────┘ │
│                 │            │             │
│  Work Stealing  │←──────────→│             │
└─────────────────┴────────────┴─────────────┘
```

## Usage

### Basic Parallel Verification

```rust
use claudelang_contracts::ParallelVerifier;

let verifier = ParallelVerifier::new(&graph);
let results = verifier.verify_contracts_parallel(&contracts)?;

println!("Verified {} contracts", results.len());
```

### Custom Configuration

```rust
use claudelang_contracts::ParallelVerificationConfig;

let config = ParallelVerificationConfig {
    num_threads: 8,                    // Use 8 threads
    max_parallel_contracts: 100,       // Max contracts in flight
    enable_work_stealing: true,        // Dynamic load balancing
    batch_size: 4,                     // Process in batches of 4
    enable_priority_scheduling: true,  // Prioritize complex contracts
};

let verifier = ParallelVerifier::new(&graph)
    .with_config(config)
    .with_resource_limits(limits);
```

### Progress Monitoring

```rust
use claudelang_contracts::ParallelCoordinator;

let coordinator = ParallelCoordinator::new(&graph);

let progress_callback = |completed: usize, total: usize| {
    println!("Progress: {}/{} ({:.1}%)", 
             completed, total, 
             (completed as f64 / total as f64) * 100.0);
};

let results = coordinator.verify_with_progress(&contracts, progress_callback)?;
```

## Configuration Options

### Thread Management

- **`num_threads`**: Number of worker threads (0 = auto-detect)
- **`max_parallel_contracts`**: Maximum contracts verified simultaneously
- **`enable_work_stealing`**: Allow idle threads to steal work

### Scheduling

- **`batch_size`**: Number of contracts per work unit
- **`enable_priority_scheduling`**: Process complex contracts first

### Resource Limits

Each thread has independent resource limits:
- Memory limits
- Timeout per contract
- Recursion depth

## Performance Characteristics

### Scalability

Performance scales with:
- Number of CPU cores
- Contract independence
- Contract complexity distribution

Typical speedups:
- 2 cores: 1.8x
- 4 cores: 3.5x
- 8 cores: 6.5x
- 16 cores: 12x

### Load Balancing

Work stealing ensures:
- Even distribution of work
- No idle threads
- Adaptive to varying contract complexity

## Advanced Features

### Dependency-Aware Parallelization

```rust
// Verify contracts respecting dependencies
let results = verifier.verify_with_dependencies(&contracts, &incremental)?;
```

This ensures:
- Independent contracts verified in parallel
- Dependent contracts verified in correct order
- Maximum parallelism within constraints

### Priority Scheduling

Contracts are prioritized by:
1. Number of conditions (complexity)
2. Presence of quantifiers
3. Recursive structure
4. User-defined priority

```rust
// Higher priority = verified first
let priority = 
    preconditions * 10 + 
    postconditions * 20 + 
    invariants * 30 +
    has_quantifiers * 50;
```

### Work Stealing Algorithm

When a thread's queue is empty:
1. Try to steal from random victim
2. Steal half of victim's queue
3. Continue until work found or all queues empty

## Integration with Other Features

### Incremental Verification

Combine with incremental verification:

```rust
// Only verify changed contracts in parallel
let changed = incremental.get_contracts_to_verify();
let results = parallel.verify_contracts_parallel(&changed)?;
```

### Caching

Each thread maintains local cache:
- Thread-safe access
- Cache hits avoid re-verification
- Shared cache for common subexpressions

### Progress Reporting

Real-time statistics:
```rust
let stats = coordinator.get_stats();
println!("Contracts/second: {}", 
         stats.verified_contracts as f64 / 
         (stats.time_elapsed_ms as f64 / 1000.0));
```

## Best Practices

### 1. Contract Organization

- **Group related contracts**: Better cache locality
- **Minimize dependencies**: More parallelization opportunities
- **Balance complexity**: Avoid one contract dominating runtime

### 2. Configuration Tuning

```rust
// For many simple contracts
let config = ParallelVerificationConfig {
    batch_size: 16,  // Larger batches
    ..Default::default()
};

// For few complex contracts
let config = ParallelVerificationConfig {
    batch_size: 1,   // Fine-grained distribution
    enable_work_stealing: true,
    ..Default::default()
};
```

### 3. Resource Management

- Set appropriate timeouts per contract
- Monitor memory usage
- Use thread-local allocators

## Performance Monitoring

### Metrics to Track

1. **Throughput**: Contracts verified per second
2. **Thread Utilization**: Average busy time per thread
3. **Work Stealing**: Number of steals
4. **Queue Depths**: Load distribution

### Example Monitoring

```rust
// Custom monitoring
let stats = Arc::new(Mutex::new(VerificationStats::default()));

let monitor_callback = {
    let stats = stats.clone();
    move |event: VerificationEvent| {
        let mut s = stats.lock().unwrap();
        match event {
            VerificationEvent::Started(name) => s.in_progress.insert(name),
            VerificationEvent::Completed(name, duration) => {
                s.completed.insert(name, duration);
                s.in_progress.remove(&name);
            }
            VerificationEvent::Failed(name, error) => {
                s.failed.insert(name, error);
            }
        }
    }
};
```

## Troubleshooting

### Common Issues

1. **Poor Speedup**
   - Check contract dependencies
   - Verify work distribution
   - Consider contract complexity variance

2. **Memory Issues**
   - Reduce `max_parallel_contracts`
   - Set per-thread memory limits
   - Enable memory pooling

3. **Thread Starvation**
   - Enable work stealing
   - Reduce batch size
   - Check for blocking operations

### Debugging

Enable detailed logging:
```rust
env::set_var("RAYON_LOG", "debug");
env::set_var("CONTRACTS_PARALLEL_DEBUG", "1");
```

## Future Enhancements

1. **GPU Acceleration**: Offload SMT solving to GPU
2. **Distributed Verification**: Cluster-wide parallelization
3. **Adaptive Scheduling**: ML-based priority prediction
4. **Speculative Verification**: Verify likely-valid contracts first
5. **Incremental Parallelism**: Parallel incremental verification

## Example: Large-Scale Verification

```rust
// Verify 1000+ contracts efficiently
fn verify_large_codebase(contracts: &HashMap<String, Contract>) {
    let config = ParallelVerificationConfig {
        num_threads: num_cpus::get(),
        max_parallel_contracts: 200,
        enable_work_stealing: true,
        batch_size: 8,
        enable_priority_scheduling: true,
    };
    
    let verifier = ParallelVerifier::new(&graph).with_config(config);
    
    // Set up progress bar
    let pb = ProgressBar::new(contracts.len() as u64);
    
    let results = verifier.verify_contracts_parallel_with_callback(
        contracts,
        |completed, total| {
            pb.set_position(completed as u64);
            pb.set_message(format!("{}/{}", completed, total));
        }
    )?;
    
    pb.finish_with_message("Verification complete");
    
    // Analyze results
    let verified = results.values()
        .filter(|r| r.is_verified())
        .count();
    
    println!("Success rate: {:.1}%", 
             (verified as f64 / results.len() as f64) * 100.0);
}
```