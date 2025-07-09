# Complete JIT Compiler Integration

## Overview

The Cranelift JIT infrastructure exists but is not fully integrated with the VM. Completing this integration would provide 10-50x performance improvements for hot code paths.

## Current State

### What Exists ✅
- Cranelift backend setup
- Basic x86_64 code generation
- JIT module structure

### What's Missing ❌
- VM integration for JIT compilation
- Automatic hot function detection
- JIT cache persistence
- Profile-guided optimization
- Multi-architecture support

## Design

### JIT Compilation Triggers

```lisp
;; Manual JIT compilation
(jit-compile factorial)  ; Compile specific function

;; Automatic compilation based on call count
(with-jit-threshold 100  ; Compile after 100 calls
  (map expensive-function large-list))

;; Profile-guided JIT
(with-profiling
  (run-application)
  (jit-compile-hot-functions :threshold 0.8)) ; Top 20% of CPU time
```

### JIT Configuration

```rust
// VM Builder configuration
let vm = VMBuilder::new()
    .with_jit_config(JitConfig {
        enabled: true,
        threshold: 100,        // Call count before JIT
        cache_dir: Some("/tmp/fluentai-jit-cache"),
        optimization_level: OptLevel::Speed,
        target_cpu: "native",  // Or specific: "x86-64-v3"
    })
    .build()?;
```

### Implementation Architecture

```
VM Execution Flow:
1. Interpret bytecode
2. Track function call counts
3. When threshold reached:
   - Generate Cranelift IR from bytecode
   - Optimize IR (inlining, constant propagation)
   - Compile to native code
   - Replace bytecode with native function pointer
4. Future calls use native code directly
```

## Implementation Tasks

### Phase 1: Basic Integration
- [ ] Add call counting to VM
- [ ] Implement bytecode → Cranelift IR translation
- [ ] Create native function calling convention
- [ ] Replace hot functions with JIT versions
- [ ] Add safety checks and guards

### Phase 2: Optimization
- [ ] Implement type specialization
- [ ] Add inline caching for method calls
- [ ] Optimize arithmetic operations
- [ ] Implement loop optimizations
- [ ] Add SIMD vectorization

### Phase 3: Advanced Features
- [ ] Profile-guided optimization
- [ ] Tiered compilation (interpreter → baseline JIT → optimized JIT)
- [ ] On-stack replacement (OSR)
- [ ] Deoptimization support
- [ ] Speculative optimizations

### Phase 4: Production Features
- [ ] JIT cache persistence
- [ ] Multi-architecture support (ARM64, WASM)
- [ ] Memory usage controls
- [ ] Debugging support (source maps)
- [ ] Performance monitoring

### Phase 5: Integration
- [ ] Integration with optimizer
- [ ] Effect-aware compilation
- [ ] Module boundary optimization
- [ ] Cross-function inlining
- [ ] Whole-program optimization

## Bytecode to Cranelift IR Mapping

```rust
// Example translation
match opcode {
    OpCode::Add => {
        let (b, a) = (builder.pop(), builder.pop());
        let result = builder.ins().iadd(a, b);
        builder.push(result);
    }
    OpCode::Call(func_id) => {
        // Check if function is JIT compiled
        if let Some(native_func) = jit_cache.get(func_id) {
            // Direct native call
            builder.ins().call_indirect(native_func, args);
        } else {
            // Fallback to interpreter
            builder.ins().call_import("vm_interpret_call", args);
        }
    }
    // ... other opcodes
}
```

## Performance Targets

| Operation | Interpreted | JIT | Expected Speedup |
|-----------|------------|-----|------------------|
| Arithmetic | 30 ns | 2 ns | 15x |
| Function call | 50 ns | 5 ns | 10x |
| Loop iteration | 100 ns | 10 ns | 10x |
| Pattern match | 200 ns | 20 ns | 10x |
| Numeric algorithms | 1 µs | 20 ns | 50x |

## Testing Strategy

1. **Correctness Tests**: Ensure JIT produces same results as interpreter
2. **Performance Tests**: Verify speedup targets are met
3. **Stress Tests**: Large programs, deep recursion, many functions
4. **Security Tests**: Ensure no code injection vulnerabilities
5. **Compatibility Tests**: Multiple architectures and OS platforms

## Examples

```lisp
;; Mandelbrot set calculation - ideal for JIT
(define mandelbrot (lambda (x y max-iter)
  (let loop ((zr 0.0) (zi 0.0) (i 0))
    (if (or (>= i max-iter)
            (> (+ (* zr zr) (* zi zi)) 4.0))
        i
        (loop (+ (- (* zr zr) (* zi zi)) x)
              (+ (* 2.0 zr zi) y)
              (+ i 1))))))

;; With JIT: ~50x faster than interpreted
(time (render-mandelbrot 1920 1080))
;; Interpreted: 2.5s
;; JIT compiled: 0.05s
```

## Priority

**High** - Massive performance gains for compute-intensive code

## Labels

- enhancement
- performance
- jit
- compiler