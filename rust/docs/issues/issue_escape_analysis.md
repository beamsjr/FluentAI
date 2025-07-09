# Implement Escape Analysis for Memory Optimization

## Overview

Implement escape analysis to determine which values can be stack-allocated instead of heap-allocated, reducing GC pressure and improving cache locality.

## Motivation

Escape analysis can provide significant performance benefits:
- Stack allocation is faster than heap allocation
- Reduces garbage collection pressure
- Improves cache locality
- Enables further optimizations (scalar replacement)

## Design

### What is Escape Analysis?

Escape analysis determines whether an object's lifetime is bounded by its creating function:
- **Does not escape**: Can be stack-allocated
- **Escapes to heap**: Must be heap-allocated
- **Escapes to caller**: Returned or stored in parameter

### Examples

```lisp
;; Does not escape - can be stack allocated
(define process-point (lambda (x y)
  (let ((point {:x x :y y}))        ; point doesn't escape
    (+ (get point :x) (get point :y)))))

;; Escapes to caller - needs heap allocation
(define make-point (lambda (x y)
  {:x x :y y}))                     ; Returned to caller

;; Escapes via closure - needs heap allocation
(define make-adder (lambda (x)
  (lambda (y) (+ x y))))            ; x escapes into closure

;; Partial escape - can optimize parts
(define process-data (lambda (items)
  (let ((temp (map (lambda (x) (* x 2)) items))  ; temp doesn't escape
        (result (filter even? temp)))             ; result doesn't escape
    (length result))))                            ; Only length escapes
```

### Analysis Algorithm

```rust
#[derive(Debug, Clone, PartialEq)]
enum EscapeState {
    NoEscape,           // Can be stack allocated
    EscapeToHeap,       // Must be heap allocated  
    EscapeToReturn,     // Returned from function
    EscapeToClosure,    // Captured by closure
    EscapeToGlobal,     // Stored in global
    Unknown,            // Conservative: assume escapes
}

struct EscapeAnalysis {
    escape_states: HashMap<NodeId, EscapeState>,
    value_origins: HashMap<NodeId, NodeId>,  // Track where values come from
    closure_captures: HashMap<NodeId, HashSet<String>>,
}

impl EscapeAnalysis {
    fn analyze(&mut self, graph: &Graph) -> Result<()> {
        // Phase 1: Build value origin graph
        self.build_value_origins(graph)?;
        
        // Phase 2: Find all allocation sites
        let allocations = self.find_allocations(graph);
        
        // Phase 3: Analyze each allocation
        for alloc_id in allocations {
            let state = self.analyze_allocation(graph, alloc_id)?;
            self.escape_states.insert(alloc_id, state);
        }
        
        // Phase 4: Propagate escape information
        self.propagate_escapes(graph)?;
        
        Ok(())
    }
    
    fn analyze_allocation(&self, graph: &Graph, alloc_id: NodeId) -> Result<EscapeState> {
        let mut state = EscapeState::NoEscape;
        let mut work_list = vec![alloc_id];
        let mut visited = HashSet::new();
        
        while let Some(node_id) = work_list.pop() {
            if !visited.insert(node_id) {
                continue;
            }
            
            // Check all uses of this value
            for use_id in self.find_uses(graph, node_id) {
                match self.analyze_use(graph, use_id, node_id)? {
                    EscapeState::NoEscape => {
                        // Continue analyzing
                        work_list.push(use_id);
                    }
                    new_state => {
                        // Value escapes
                        state = self.merge_states(state, new_state);
                        if state == EscapeState::EscapeToHeap {
                            return Ok(state); // Worst case
                        }
                    }
                }
            }
        }
        
        Ok(state)
    }
}
```

### Optimization Transformations

#### 1. Stack Allocation
```lisp
;; Before
(let ((point (make-map :x 10 :y 20)))
  (distance-to-origin point))

;; After analysis: point doesn't escape
;; Generate stack allocation hint
(let ((point (stack-alloc-map :x 10 :y 20)))  
  (distance-to-origin point))
```

#### 2. Scalar Replacement
```lisp
;; Before  
(let ((pair (cons a b)))
  (+ (car pair) (cdr pair)))

;; After: pair doesn't escape, replace with scalars
(+ a b)  ; Eliminated allocation entirely
```

#### 3. Allocation Sinking
```lisp
;; Before
(define process (lambda (flag)
  (let ((buffer (make-array 1000)))  ; Always allocated
    (if flag
        (use-buffer buffer)
        0))))

;; After: sink allocation into branch
(define process (lambda (flag)
  (if flag
      (let ((buffer (make-array 1000)))  ; Only allocated if needed
        (use-buffer buffer))
      0)))
```

### Integration with VM

```rust
// VM stack frame with escape analysis info
struct StackFrame {
    locals: Vec<Value>,
    stack: Vec<Value>,
    escape_info: EscapeInfo,
    stack_allocations: Vec<StackAllocation>,
}

struct StackAllocation {
    offset: usize,
    size: usize,
    value_type: ValueType,
}

// New opcodes for optimized allocation
enum OpCode {
    // Regular allocation
    AllocMap(usize),          // Heap allocate map with n slots
    AllocList(usize),         // Heap allocate list with n items
    
    // Stack allocation (from escape analysis)
    StackAllocMap(usize),     // Stack allocate map
    StackAllocList(usize),    // Stack allocate list
    StackAllocClosure(usize), // Stack allocate closure
    
    // Scalar replacement
    LoadField(usize),         // Load field from scalar-replaced object
    StoreField(usize),        // Store field to scalar-replaced object
}
```

## Implementation Tasks

### Phase 1: Basic Analysis
- [ ] Implement allocation site detection
- [ ] Build use-def chains
- [ ] Track value flow through variables
- [ ] Detect return values
- [ ] Handle simple no-escape cases

### Phase 2: Advanced Analysis  
- [ ] Handle closures and captures
- [ ] Track values through collections
- [ ] Analyze loops and recursion
- [ ] Support interprocedural analysis
- [ ] Handle effect boundaries

### Phase 3: Optimizations
- [ ] Implement stack allocation
- [ ] Add scalar replacement
- [ ] Implement allocation sinking
- [ ] Support partial escape
- [ ] Add allocation elimination

### Phase 4: VM Integration
- [ ] Add stack allocation opcodes
- [ ] Implement stack frame extensions
- [ ] Add scalar replacement opcodes
- [ ] Update GC to ignore stack allocs
- [ ] Handle stack overflow gracefully

### Phase 5: Advanced Features
- [ ] Profile-guided refinement
- [ ] Speculative optimization
- [ ] Escape analysis for arrays
- [ ] Lock elision optimization
- [ ] Integration with JIT

## Test Cases

```lisp
;; Test 1: Basic non-escaping allocation
(define test-no-escape ()
  (let ((temp {:x 1 :y 2}))
    (+ (get temp :x) (get temp :y))))
;; Expected: temp is stack allocated

;; Test 2: Escaping via return
(define test-escape-return (x y)
  {:sum (+ x y) :product (* x y)})
;; Expected: map is heap allocated

;; Test 3: Partial escape
(define test-partial-escape (items)
  (let ((processed (map process items))
        (filtered (filter valid? processed)))
    (length filtered)))
;; Expected: processed and filtered are stack allocated

;; Test 4: Closure capture
(define test-closure-escape (x)
  (let ((multiplier x))
    (lambda (y) (* multiplier y))))
;; Expected: multiplier escapes to closure

;; Test 5: Scalar replacement candidate
(define test-scalar-replace (a b)
  (let ((pair (cons a b)))
    (if (> (car pair) (cdr pair))
        (car pair)
        (cdr pair))))
;; Expected: pair eliminated, use a and b directly
```

## Performance Impact

Expected improvements:
- **Allocation cost**: 10-100x faster for stack vs heap
- **GC pressure**: 20-50% reduction in GC time
- **Cache locality**: 2-5x better for stack allocations
- **Overall speedup**: 10-30% for allocation-heavy code

## Integration Points

1. **Optimizer**: Run after other optimizations
2. **Type Analysis**: Use type info to refine analysis  
3. **Effect Analysis**: Consider effect boundaries
4. **JIT Compiler**: Generate optimized native code
5. **Profiler**: Collect allocation profiles

## Priority

**Medium** - Significant performance gains for allocation-heavy code

## Labels

- enhancement
- optimizer
- performance
- memory-optimization