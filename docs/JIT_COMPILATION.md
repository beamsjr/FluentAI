# JIT Compilation in ClaudeLang

ClaudeLang includes a Just-In-Time (JIT) compiler that can significantly improve performance for hot code paths.

## Overview

The JIT compiler works by:
1. **Profiling** - Tracking execution counts and performance of functions
2. **Hot Function Detection** - Identifying functions that are executed frequently
3. **Native Code Generation** - Compiling hot functions to x86-64 machine code
4. **Guard-based Optimization** - Specializing code based on runtime type information
5. **Deoptimization** - Falling back to interpreter when guards fail

## Architecture

### Components

1. **ExecutionProfile** - Tracks function execution statistics
2. **JITCompiler** - Main JIT compilation engine
3. **X86_64Assembler** - Generates native machine code
4. **CompiledCode** - Manages executable memory and guards
5. **TraceJIT** - Trace-based compilation for hot loops

### Compilation Pipeline

```
Source Code → Parser → AST → Bytecode → Profiling → JIT → Native Code
                                ↑                             ↓
                                └─────── Interpreter ←────────┘
                                         (fallback)
```

## Usage

### Enable JIT in VM

```python
from src.vm import VM

# JIT is enabled by default
vm = VM(enable_jit=True)

# Execute with function ID for profiling
result = vm.execute(bytecode_chunk, function_id="my_function")
```

### Check JIT Statistics

```python
stats = vm.get_jit_stats()
print(f"Compiled functions: {stats['compiled_functions']}")
print(f"Hot functions: {stats['hot_functions']}")
```

## Configuration

### Hotspot Thresholds

```python
from src.jit import HotspotThreshold

# Compile after 1000 function calls (default)
HotspotThreshold.FUNCTION_CALLS = 1000

# Compile loops after 10000 iterations
HotspotThreshold.LOOP_ITERATIONS = 10000

# Maximum trace length
HotspotThreshold.TRACE_LENGTH = 50
```

## Example: Fibonacci with JIT

```lisp
; This function will be JIT compiled after 1000 calls
(define (fibonacci n)
  (if (<= n 1)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

; Call it many times to trigger JIT
(let ((results []))
  (dotimes (i 2000)
    (push! results (fibonacci 20)))
  results)
```

## Performance Impact

### Expected Speedups

- **Arithmetic-heavy code**: 5-10x faster
- **List operations**: 3-5x faster  
- **Recursive functions**: 4-8x faster
- **String manipulation**: 2-4x faster

### Compilation Overhead

- **Cold start**: First execution includes profiling overhead
- **Compilation time**: ~10-50ms for typical functions
- **Memory usage**: ~1-10KB per compiled function

## Guard-based Type Specialization

The JIT compiler uses sophisticated guard-based specialization to generate optimal code for different type profiles:

### Type Profiling

During execution, the JIT tracks:
- Parameter types and their frequencies
- Constant values that appear frequently  
- Numeric value ranges
- Collection shapes and sizes
- Return types

### Guard Types

```python
# Type guards - optimize for specific types
GuardType.TYPE_CHECK     # Exact type match (e.g., int, string)
GuardType.POLYMORPHIC    # Small set of types
GuardType.MONOMORPHIC    # Single receiver type

# Value guards - optimize based on values
GuardType.CONSTANT       # Specific constant value
GuardType.RANGE_CHECK    # Numeric range (min, max)
GuardType.NULL_CHECK     # Null/None checking

# Shape guards - optimize data structures  
GuardType.SHAPE_CHECK    # Fixed collection size
GuardType.LENGTH_CHECK   # Collection size range
```

### Specialization Examples

#### Monomorphic Integer Addition
```lisp
(define (add x y) (+ x y))

; After profiling shows x and y are always integers:
; Generated code skips type checks and uses direct ADD instruction
```

#### Constant Folding
```lisp
(define (scale x) (* x 10))

; After profiling shows second operand is always 10:
; Generated code can use shifts: (x << 3) + (x << 1)
```

#### Range-based Division
```lisp
(define (safe-div x y) (/ x y))

; After profiling shows y is always positive:
; Generated code skips zero check
```

### Multiple Specializations

Functions can have multiple specialized versions:

```python
def add(x, y):
    return x + y

# Specialization 1: (int, int) -> int
# Specialization 2: (float, float) -> float  
# Specialization 3: (string, string) -> string
```

Each specialization has its own guards and optimized code.

### Guard Failure Handling

When guards fail:
1. Specialized code returns sentinel value (-1)
2. JIT falls back to interpreter
3. After repeated failures, specialization is evicted
4. New specialization may be generated

### Specialization Statistics

Track specialization effectiveness:
```json
{
  "specialization_count": 3,
  "total_hits": 9500,
  "total_misses": 500,
  "hit_rate": 0.95,
  "specializations": [
    {
      "id": "add_int_int",
      "guards": 2,
      "hits": 8000,
      "misses": 100,
      "success_rate": 0.987
    }
  ]
}
```

## Native Code Generation

The JIT generates x86-64 machine code with complete support for:

### Arithmetic Operations
- ADD, SUB, MUL, DIV, MOD, NEG
- Integer overflow handling
- Division by zero protection

### Comparison Operations  
- EQ, NE, LT, LE, GT, GE
- Efficient flag-based comparisons
- Boolean result generation

### Boolean Operations
- AND, OR, NOT
- Short-circuit evaluation
- Truthiness conversion

### Control Flow
- JUMP, JUMP_IF, JUMP_IF_NOT
- Label-based branching
- Stack-based condition testing

### Stack Management
- Simulated VM stack using native stack
- R15 as stack pointer
- Efficient push/pop operations

Example generated code for `(+ a b)`:
```assembly
; Pop operands from simulated stack
dec    %r15
mov    %r15, %rax
shl    $3, %rax      ; RAX = R15 * 8
add    %rsp, %rax
mov    (%rax), %rsi  ; b in RSI

dec    %r15
mov    %r15, %rax
shl    $3, %rax
add    %rsp, %rax
mov    (%rax), %rdi  ; a in RDI

; Perform addition
add    %rsi, %rdi

; Push result
mov    %r15, %rax
shl    $3, %rax
add    %rsp, %rax
mov    %rdi, (%rax)
inc    %r15
```

## Trace-based JIT

For hot loops, ClaudeLang can record execution traces:

```lisp
; This loop will be trace-compiled
(let ((sum 0))
  (dotimes (i 1000000)
    (set! sum (+ sum i)))
  sum)
```

The trace JIT linearizes the loop, eliminating branches.

## Platform Support

Currently supported platforms:
- **x86-64 Linux** - Full support
- **x86-64 macOS** - Full support  
- **x86-64 Windows** - Experimental support
- **ARM64** - Planned

## Debugging JIT Compilation

### Enable JIT Logging

```python
import logging
logging.getLogger('claudelang.jit').setLevel(logging.DEBUG)
```

### Inspect Compiled Code

```python
# Get compiled code for a function
compiled = vm.jit_compiler.compiled_code.get("my_function")
if compiled:
    print(f"Code size: {compiled.size} bytes")
    print(f"Guards: {compiled.guards}")
```

### Force Compilation

```python
# Lower threshold for testing
HotspotThreshold.FUNCTION_CALLS = 1

# Function will compile on second call
vm.execute(chunk, function_id="test_func")
vm.execute(chunk, function_id="test_func")  # Triggers JIT
```

## Limitations

1. **Not all bytecode ops supported** - Complex operations fall back to interpreter
2. **Limited type specialization** - Currently only basic types
3. **No vectorization** - SIMD instructions not yet generated
4. **Single-threaded** - No parallel compilation

## Future Improvements

1. **Inline caching** - Cache method lookups
2. **Escape analysis** - Optimize object allocations
3. **Loop unrolling** - Optimize small loops
4. **SIMD generation** - Vectorize array operations
5. **Background compilation** - Compile in separate thread

## Benchmarks

Run JIT benchmarks:

```bash
python3 benchmarks/benchmark_jit.py
```

Example output:
```
Fibonacci(20):
  No JIT:    0.1234s
  JIT Cold:  0.1245s (0.99x)
  JIT Hot:   0.0247s (5.00x)

Arithmetic Loop (1000 iterations):
  No JIT:    0.0567s
  JIT Cold:  0.0572s (0.99x)
  JIT Hot:   0.0081s (7.00x)
```

## Integration with AI Features

The JIT compiler integrates with ClaudeLang's AI-first features:

1. **Automatic proof generation** - Proves JIT optimizations correct
2. **ML-guided compilation** - Learn which functions benefit from JIT
3. **Behavioral versioning** - Version based on JIT performance characteristics
4. **Profile-guided optimization** - Use execution traces to guide compilation