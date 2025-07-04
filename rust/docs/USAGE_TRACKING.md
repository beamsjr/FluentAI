# Runtime Usage Tracking

This document describes the runtime usage tracking feature implemented in the FluentAi VM.

## Overview

The VM can track execution statistics for functions during runtime, providing valuable data for:
- Performance optimization
- Hot path detection
- Error rate monitoring
- Context-aware compilation decisions

## Architecture

### 1. UsageTracker

The `UsageTracker` struct in the VM maintains:
- Mapping from chunk IDs to AST NodeIds
- Execution statistics per node
- Execution time history (last 100 samples)

```rust
pub struct UsageTracker {
    chunk_to_node: FxHashMap<usize, NodeId>,
    stats: FxHashMap<NodeId, UsageStatistics>,
    execution_times: FxHashMap<NodeId, Vec<u64>>,
}
```

### 2. CallFrame Enhancement

Each `CallFrame` now includes an optional `start_time` field to track when the function started executing:

```rust
pub struct CallFrame {
    chunk_id: usize,
    ip: usize,
    stack_base: usize,
    env: Vec<Value>,
    start_time: Option<Instant>, // Track execution start time
}
```

### 3. Integration Points

Usage tracking is integrated at these VM operations:

- **Function Call (Call opcode)**: Records start time when entering a function
- **Function Return (Return opcode)**: Calculates elapsed time and updates statistics
- **Tail Call (TailCall opcode)**: Preserves timing across tail-recursive calls
- **Error Handling**: Increments error count when functions fail

## Usage

### Enabling Usage Tracking

```rust
let mut vm = VM::new(bytecode);
vm.enable_usage_tracking();
```

### Registering Chunk Mappings

Map bytecode chunks to AST nodes for tracking:

```rust
vm.register_chunk_mapping(chunk_id, node_id);
```

### Retrieving Statistics

```rust
// Get stats for a specific node
if let Some(stats) = vm.get_usage_stats(node_id) {
    println!("Execution count: {}", stats.execution_count);
    println!("Average time: {}ns", stats.avg_execution_time_ns);
}

// Get all statistics
if let Some(all_stats) = vm.get_all_usage_stats() {
    for (node_id, stats) in all_stats {
        // Process statistics...
    }
}
```

## Performance Impact

When disabled (default):
- Zero memory overhead (Option<UsageTracker> is None)
- No timing instrumentation
- No performance impact

When enabled:
- ~100 bytes per tracked function
- Timing overhead: ~50ns per function call
- Memory for execution history (max 100 samples per function)

## Integration with Context Memory

Usage statistics can be synchronized with the AST's ContextMemory:

```rust
// Update AST node with runtime statistics
if let Some(mut metadata) = node.metadata().cloned() {
    metadata.update_usage_stats(stats);
    graph.set_metadata(node_id, metadata);
}
```

This enables:
- Persistence of usage data across runs
- Integration with the optimizer for better decisions
- Context-aware code generation in future compilations

## Future Enhancements

1. **Sampling Mode**: Track only a percentage of executions to reduce overhead
2. **Call Graph Tracking**: Record caller-callee relationships
3. **Memory Usage Tracking**: Monitor heap allocations per function
4. **JIT Compilation Hints**: Use hot path data to guide JIT decisions
5. **Distributed Statistics**: Aggregate usage data across multiple VM instances