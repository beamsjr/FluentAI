# Native Code Generation Strategy for FluentAI

## Overview

To achieve our target of being only 10x slower than Python (from current 18x), we need native code generation. This document outlines our strategy for implementing both JIT and AOT compilation.

## Current Architecture

```
Source → Parser → AST → Optimizer → Bytecode → VM (Python)
                                                 ↓
                                              ~18x slower
```

## Target Architecture

```
Source → Parser → AST → Optimizer → IR → Native Code
                                      ↓
                                   LLVM IR → Machine Code (~10x slower)
                                      ↓
                                   Hot Path JIT → Optimized Native (~5x slower)
```

## Implementation Strategy

### Phase 1: LLVM Backend (AOT Compilation)

1. **Intermediate Representation (IR)**
   - Convert optimized AST to LLVM IR
   - Type inference for better code generation
   - Effect annotations for optimization hints

2. **LLVM Code Generation**
   - Map FluentAI primitives to LLVM intrinsics
   - Implement runtime support library in C/Rust
   - Generate position-independent code

3. **Runtime Support**
   - Garbage collection (Boehm GC initially)
   - Effect handlers
   - Native list/string operations

### Phase 2: JIT Compilation

1. **Profiling Infrastructure**
   - Track hot functions
   - Collect type information
   - Monitor call patterns

2. **Tiered Compilation**
   - Tier 0: Bytecode interpreter (current)
   - Tier 1: Simple JIT (no optimizations)
   - Tier 2: Optimizing JIT with type specialization

3. **Optimization Strategies**
   - Type specialization for numeric operations
   - Inline caching for method dispatch
   - Loop unrolling for known bounds

## Technical Design

### Type Inference System

```python
@dataclass
class InferredType:
    base_type: str  # int, float, string, list, function
    is_constant: bool
    value_range: Optional[Tuple[Any, Any]]
    element_type: Optional['InferredType']  # For lists
    
@dataclass
class TypeInfo:
    node_types: Dict[str, InferredType]
    constraints: List[TypeConstraint]
```

### LLVM IR Generation Example

FluentAI code:
```lisp
(defun add (x y)
  (+ x y))
```

Generated LLVM IR:
```llvm
define i64 @fluentai_add(i64 %x, i64 %y) {
entry:
  %result = add i64 %x, %y
  ret i64 %result
}
```

### JIT Compilation Triggers

1. **Function call count > threshold** (default: 1000)
2. **Loop iteration count > threshold** (default: 10000)
3. **Total execution time in function > threshold** (default: 100ms)

## Implementation Plan

### Milestone 1: Basic LLVM Backend (2-3x improvement)
- [ ] LLVM IR generator for basic operations
- [ ] Runtime library in Rust
- [ ] AOT compilation script
- [ ] Benchmark against VM

### Milestone 2: Type Inference (1.5x improvement)
- [ ] Basic type inference algorithm
- [ ] Type-specialized code generation
- [ ] Numeric operation optimization
- [ ] Remove unnecessary boxing

### Milestone 3: JIT Compilation (2x improvement)
- [ ] Profiling infrastructure
- [ ] Basic JIT using LLVM MCJIT
- [ ] Hot path detection
- [ ] Inline caching

### Milestone 4: Advanced Optimizations (1.5x improvement)
- [ ] Escape analysis
- [ ] Stack allocation for non-escaping objects
- [ ] SIMD vectorization
- [ ] Profile-guided optimization

## Code Structure

```
src/
├── codegen/
│   ├── llvm_codegen.py      # LLVM IR generation
│   ├── type_inference.py    # Type inference engine
│   ├── jit_compiler.py      # JIT compilation logic
│   └── runtime_support.rs   # Rust runtime library
├── profiler/
│   ├── execution_profiler.py
│   └── type_profiler.py
```

## Performance Targets

| Component | Current | Target | Improvement |
|-----------|---------|--------|-------------|
| Arithmetic | 18x | 5x | 3.6x |
| List operations | 45x | 15x | 3x |
| Function calls | 25x | 8x | 3.1x |
| Overall | 18x | 10x | 1.8x |

## Risks and Mitigations

1. **Compilation overhead**
   - Mitigation: Lazy compilation, caching compiled code

2. **Memory usage**
   - Mitigation: Tiered compilation, evict cold code

3. **Complexity**
   - Mitigation: Start simple, incremental improvements

4. **Platform compatibility**
   - Mitigation: Fallback to VM, support major platforms first

## Example: Fibonacci with Native Code

```python
# Current VM execution: ~20x slower than Python
# With LLVM backend: ~7x slower
# With JIT + type specialization: ~3x slower

def compile_fibonacci_to_llvm(ast):
    """
    (defun fib (n)
      (if (<= n 1)
          n
          (+ (fib (- n 1))
             (fib (- n 2)))))
    """
    # Type inference determines n is always int
    # Generate specialized code without boxing
    # Tail call optimization where possible
    return llvm_ir
```

## Next Steps

1. Set up LLVM Python bindings (llvmlite)
2. Create minimal runtime support library
3. Implement IR generation for arithmetic operations
4. Benchmark native code vs VM
5. Iterate on optimizations