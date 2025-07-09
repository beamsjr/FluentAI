# Complete Loop Optimization Implementation

## Overview

Implement advanced loop optimizations in the optimizer to improve performance of iterative code patterns.

## Current State

The optimizer mentions loop optimizations in future work but only has basic tail call detection. We need comprehensive loop optimization including unrolling, fusion, and invariant hoisting.

## Optimizations to Implement

### 1. Loop Unrolling

```lisp
;; Before
(define sum-array (lambda (arr n)
  (let loop ((i 0) (sum 0))
    (if (< i n)
        (loop (+ i 1) (+ sum (array-ref arr i)))
        sum))))

;; After (unrolled by 4)
(define sum-array (lambda (arr n)
  (let ((limit (- n (mod n 4))))
    (let loop ((i 0) (sum 0))
      (if (< i limit)
          (loop (+ i 4)
                (+ sum 
                   (array-ref arr i)
                   (array-ref arr (+ i 1))
                   (array-ref arr (+ i 2))
                   (array-ref arr (+ i 3))))
          ;; Handle remainder
          (let remainder ((j i) (s sum))
            (if (< j n)
                (remainder (+ j 1) (+ s (array-ref arr j)))
                s)))))))
```

### 2. Loop Fusion

```lisp
;; Before - Two separate loops
(let ((squared (map (lambda (x) (* x x)) numbers))
      (doubled (map (lambda (x) (* x 2)) squared)))
  doubled)

;; After - Single fused loop
(map (lambda (x) (* (* x x) 2)) numbers)
```

### 3. Loop Invariant Code Motion (LICM)

```lisp
;; Before
(let loop ((i 0) (sum 0))
  (if (< i n)
      (let ((factor (* scale (+ offset 1)))) ; Invariant
        (loop (+ i 1) 
              (+ sum (* (array-ref arr i) factor))))
      sum))

;; After - Invariant hoisted
(let ((factor (* scale (+ offset 1))))  ; Moved outside
  (let loop ((i 0) (sum 0))
    (if (< i n)
        (loop (+ i 1) 
              (+ sum (* (array-ref arr i) factor)))
        sum)))
```

### 4. Loop Strength Reduction

```lisp
;; Before - Multiplication in loop
(let loop ((i 0))
  (when (< i n)
    (array-set! arr i (* i stride))
    (loop (+ i 1))))

;; After - Addition instead of multiplication
(let loop ((i 0) (val 0))
  (when (< i n)
    (array-set! arr i val)
    (loop (+ i 1) (+ val stride))))
```

### 5. Loop Vectorization (Preparation)

```lisp
;; Mark loops suitable for SIMD
(let loop ((i 0))
  (when (< i n)
    ;; Optimizer marks this as vectorizable
    (array-set! c i 
      (+ (array-ref a i) (array-ref b i)))
    (loop (+ i 1))))

;; Generates metadata for JIT to use SIMD instructions
```

## Implementation Design

### Loop Detection

```rust
enum LoopPattern {
    TailRecursive {
        function: NodeId,
        params: Vec<String>,
        base_case: NodeId,
        recursive_call: NodeId,
    },
    MapPattern {
        function: NodeId,
        list: NodeId,
    },
    FoldPattern {
        function: NodeId,
        accumulator: String,
        list: NodeId,
    },
    WhileLoop {
        condition: NodeId,
        body: NodeId,
        updates: Vec<(String, NodeId)>,
    },
}

impl LoopDetector {
    fn detect_loops(&self, graph: &Graph) -> Vec<(NodeId, LoopPattern)> {
        let mut loops = Vec::new();
        
        // Detect tail recursion
        for (id, node) in &graph.nodes {
            if let Some(pattern) = self.detect_tail_recursion(graph, id, node) {
                loops.push((*id, pattern));
            }
        }
        
        // Detect higher-order patterns
        self.detect_map_patterns(graph, &mut loops);
        self.detect_fold_patterns(graph, &mut loops);
        
        loops
    }
}
```

### Optimization Strategies

```rust
trait LoopOptimization {
    fn applicable(&self, loop_info: &LoopPattern) -> bool;
    fn apply(&self, graph: &Graph, loop_id: NodeId, pattern: &LoopPattern) -> Result<Graph>;
    fn cost_benefit(&self, loop_info: &LoopPattern) -> f64;
}

struct LoopUnroller {
    unroll_factor: usize,
}

impl LoopOptimization for LoopUnroller {
    fn applicable(&self, loop_info: &LoopPattern) -> bool {
        match loop_info {
            LoopPattern::TailRecursive { .. } => true,
            LoopPattern::MapPattern { list, .. } => {
                // Only unroll if list size is known or small
                self.is_small_constant_list(list)
            }
            _ => false,
        }
    }
    
    fn apply(&self, graph: &Graph, loop_id: NodeId, pattern: &LoopPattern) -> Result<Graph> {
        // Implementation of unrolling transformation
        self.unroll_loop(graph, loop_id, pattern, self.unroll_factor)
    }
}
```

### Loop Optimization Pipeline

```rust
pub struct LoopOptimizer {
    optimizations: Vec<Box<dyn LoopOptimization>>,
}

impl LoopOptimizer {
    pub fn new() -> Self {
        Self {
            optimizations: vec![
                Box::new(LoopInvariantHoisting::new()),
                Box::new(LoopFusion::new()),
                Box::new(LoopUnroller::new(4)),
                Box::new(StrengthReduction::new()),
                Box::new(VectorizationMarker::new()),
            ],
        }
    }
    
    pub fn optimize_loops(&self, graph: &Graph) -> Result<Graph> {
        let detector = LoopDetector::new();
        let loops = detector.detect_loops(graph);
        
        let mut optimized = graph.clone();
        
        for (loop_id, pattern) in loops {
            // Apply optimizations in order
            for opt in &self.optimizations {
                if opt.applicable(&pattern) {
                    if opt.cost_benefit(&pattern) > 1.0 {
                        optimized = opt.apply(&optimized, loop_id, &pattern)?;
                    }
                }
            }
        }
        
        Ok(optimized)
    }
}
```

## Implementation Tasks

### Phase 1: Loop Detection
- [ ] Implement tail recursion detection
- [ ] Detect map/filter/fold patterns
- [ ] Identify loop bounds and trip counts
- [ ] Build loop dependency graph
- [ ] Detect nested loops

### Phase 2: Basic Optimizations
- [ ] Implement loop unrolling
- [ ] Add loop invariant hoisting
- [ ] Implement strength reduction
- [ ] Add dead iteration elimination
- [ ] Support partial unrolling

### Phase 3: Advanced Optimizations
- [ ] Implement loop fusion
- [ ] Add loop fission (splitting)
- [ ] Implement loop interchange
- [ ] Add loop tiling for cache
- [ ] Support loop peeling

### Phase 4: Vectorization
- [ ] Mark vectorizable loops
- [ ] Generate SIMD hints
- [ ] Align data for SIMD
- [ ] Handle reduction operations
- [ ] Support masked operations

### Phase 5: Integration
- [ ] Integrate with main optimizer
- [ ] Add cost models
- [ ] Profile-guided optimization
- [ ] Debugging/visualization
- [ ] Performance benchmarks

## Test Cases

```lisp
;; Test loop unrolling
(define test-unroll
  (lambda (n)
    (let loop ((i 0) (sum 0))
      (if (< i n)
          (loop (+ i 1) (+ sum i))
          sum))))

;; Test loop fusion
(define test-fusion
  (lambda (lst)
    (map square (map double lst))))  ; Should fuse

;; Test invariant hoisting
(define test-licm
  (lambda (arr n scale)
    (let loop ((i 0))
      (when (< i n)
        (let ((factor (* scale (expensive-computation))))
          (array-set! arr i (* (array-ref arr i) factor)))
        (loop (+ i 1))))))

;; Test strength reduction
(define test-strength
  (lambda (n)
    (let loop ((i 0) (result '()))
      (if (< i n)
          (loop (+ i 1) (cons (* i 8) result))  ; 8*i -> shift
          result))))
```

## Performance Targets

| Optimization | Expected Speedup | Use Case |
|--------------|-----------------|----------|
| Unrolling (4x) | 1.5-2x | Small fixed loops |
| Loop fusion | 1.3-1.8x | Multiple passes over data |
| LICM | 1.2-3x | Loops with expensive invariants |
| Strength reduction | 1.1-1.5x | Index calculations |
| Vectorization prep | 2-8x (with SIMD) | Numeric arrays |

## Priority

**Medium** - Significant performance gains for numeric/scientific code

## Labels

- enhancement
- optimizer
- performance
- loops