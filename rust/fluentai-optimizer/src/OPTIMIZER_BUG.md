# Optimizer Bug: Dangling Node References

## Issue Description

The advanced optimizer creates dangling node references when optimizing certain AST patterns. This is particularly evident with Assignment nodes.

## Example

Given this code:
```
let x = 10; x := 42
```

The optimizer produces:
```
Root: NodeId(1)
NodeId(1): Let { bindings: [], body: NodeId(3) }
NodeId(3): Assignment { target: NodeId(4), value: NodeId(5) }
```

But NodeId(4) and NodeId(5) don't exist in the optimized graph!

## Root Cause

The issue occurs in the `optimize_node_iterative` method of `AdvancedOptimizer`. When processing nodes:

1. Each node gets a placeholder ID when it starts processing
2. If the node optimization returns Some(node), it's kept
3. If it returns None, the placeholder is removed
4. But parent nodes might still reference the removed placeholder IDs

## Affected Patterns

- Assignment nodes where the target or value fail to optimize
- Any node that references child nodes that get optimized away

## Workaround

Use OptimizationLevel::None to disable optimization, or use Basic level which doesn't use the advanced optimizer.

## Proper Fix

The optimizer needs to ensure that all nodes referenced by included nodes are also included in the final graph. This requires either:
1. A post-processing pass to fix up references
2. A different optimization strategy that tracks dependencies
3. Keeping nodes that are referenced even if they "optimize to None"