# ClaudeLang: An AI-First Programming Language

ClaudeLang is an experimental programming language that explores what happens when we design a language specifically for AI systems rather than humans. It features a graph-based AST, explicit semantics, and advanced AI-driven optimization capabilities.

## Table of Contents

- [Key Features](#key-features)
- [Quick Example](#quick-example)
- [Installation](#installation)
- [Language Features](#language-features)
- [AI-First Features](#advanced-ai-first-features-new)
- [Performance](#performance)
- [Documentation](#documentation)
- [Contributing](#contributing)

## Key Features

### 🧠 AI-First Design
- **Graph-based AST**: Programs as directed graphs, not text
- **Explicit semantics**: All effects and dependencies declared
- **Machine-readable specs**: Formal specifications embedded in code
- **Semantic versioning**: Version numbers based on behavior, not syntax

### ⚡ Performance & Optimization
- **27-83x performance improvement** from baseline
- **Multiple backends**: Bytecode VM, C, and LLVM
- **Automatic proof generation**: Every optimization is verified
- **ML-driven optimization**: Learns patterns from execution

### 🔧 Modern Language Features
- **Pattern matching**: ML-style with exhaustiveness checking
- **Effect system**: Explicit tracking of IO, State, Random, etc.
- **Module system**: Namespaces and dependency management
- **Metaprogramming**: Runtime graph queries and transformations

### 📊 Advanced Capabilities
- **Behavioral contracts**: Pre/postconditions and invariants
- **Execution trace analysis**: Generate docs from runtime behavior
- **Graph queries**: Analyze and transform program structure
- **Proof export**: Generate Coq/Lean proofs

## Quick Example

```lisp
; Define a function with behavioral contract
(spec:contract fibonacci
  :requires [(>= n 0)]
  :ensures [(>= result 0)]
  :complexity "O(2^n)")

(define (fibonacci n)
  (match n
    (0 1)
    (1 1)
    (n (+ (fibonacci (- n 1))
          (fibonacci (- n 2))))))

; Automatic optimization with proof
(define fibonacci-opt 
  (optimize:memoize fibonacci))

; Query the function's structure
(graph:query (graph:of fibonacci)
  (select 'application)
  (where recursive?)
  (count))  ; => 2 recursive calls
```

## Installation

```bash
# Clone the repository
git clone https://github.com/beamsjr/claudelang.git
cd claudelang

# Install dependencies (optional, for numpy-based ML features)
pip install numpy

# Run a program
python3 -m src.interpreter examples/fibonacci.cl

# Start the interactive REPL
python3 -m src.repl

# Run with VM optimization
python3 -m src.vm.vm_runner examples/fibonacci.cl
```

### Hello World

```lisp
(print "Hello, World!")
```

### Example: Fibonacci

```lisp
(let ((fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1))
                   (fib (- n 2)))))))
  (fib 10))  ; Returns 55
```

## Project Structure

```
claudelang/
├── src/
│   ├── core/          # AST and language primitives
│   ├── parser/        # S-expression parser
│   ├── interpreter/   # Tree-walking interpreter
│   ├── vm/           # Bytecode VM with optimizations
│   ├── optimizer/    # Graph-based optimizer
│   ├── types/        # Type inference system
│   ├── codegen/      # Native code generation
│   └── stdlib/       # Standard library
├── docs/             # Documentation
├── examples/         # Example programs
├── tests/           # Test suite
└── benchmarks/      # Performance tests
```

## Language Features

### Interactive REPL

ClaudeLang now includes a fully-featured REPL (Read-Eval-Print Loop) for interactive development:

```bash
$ python3 -m src.repl
ClaudeLang REPL v1.0
Type :help for help, :quit to exit

claudelang> (+ 2 3)
5

claudelang> (let ((square (lambda (x) (* x x)))) 
...         (square 7))
49

claudelang> :help
```

REPL features:
- Multi-line input support
- Command history (up/down arrows)
- Special commands (:help, :load, :ast on/off, :vm on/off)
- Toggle between VM and interpreter execution
- Show AST, bytecode, and type information

### Effect System

ClaudeLang features a comprehensive effect system with composable handlers:

```lisp
; Effect primitives
(io:print "Hello, World!")              ; IO effect
(state:set "counter" 0)                 ; State effect  
(state:update "counter" (lambda (x) (+ x 1)))
(time:now)                              ; Time effect
(random:int 1 10)                       ; Random effect

; Transactions
(state:begin-transaction)
(state:set "balance" 100)
(state:rollback-transaction)            ; Undo changes

; Error handling
(error:catch "div-zero" 
  (lambda (err) 
    (io:print "Caught:" err)
    0))
```

See [Effect System Documentation](docs/EFFECT_SYSTEM.md) for details.

### Type Inference
```lisp
(+ 2 3)  ; Inferred as Int + Int -> Int
         ; Optimized to constant 5
```

### List Operations
```lisp
(map (lambda (x) (* x 2)) [1 2 3])  ; [2 4 6]
(filter (lambda (x) (> x 2)) [1 2 3 4])  ; [3 4]
(fold + 0 [1 2 3 4 5])  ; 15
```

## Performance

### Current Performance
- **Baseline interpreter**: 500-1500x slower than Python
- **With all optimizations**: ~18x slower than Python
- **Target**: 10x slower (achievable with JIT)

### Optimization Example
```lisp
; This entire expression is computed at compile time
(if (> 10 5)
    (* 2 50)
    (+ 100 200))
; Optimizes to just: 100
```

## Documentation

- [Quick Start Guide](docs/QUICK_START.md) - Get started quickly
- [Language Specification](docs/LANGUAGE_SPECIFICATION.md) - Complete reference
- [Optimization Journey](OPTIMIZATION_JOURNEY.md) - How we achieved 27-83x speedup
- [Performance Report](PERFORMANCE_REPORT.md) - Detailed benchmarks
- [Examples](examples/) - Sample programs

## Running Tests

```bash
# Run test suite
python3 -m tests.test_core

# Run benchmarks
python3 benchmark_all.py

# Run specific example
python3 run_claudelang.py examples/list_operations.cl
```

## Standard Library

ClaudeLang includes a comprehensive standard library:

- **Core** - List operations, numeric functions, type checking
- **Strings** - String manipulation and formatting
- **IO** - File and console I/O with explicit effects
- **Math** - Trigonometry, logarithms, statistics
- **Data** - Dictionaries, sets, queues
- **Functional** - Composition, partial application, higher-order functions
- **DateTime** - Date/time manipulation and formatting

See [Standard Library Documentation](docs/STANDARD_LIBRARY.md) for details.

## Recent Additions

### Pattern Matching (NEW!)
```lisp
(match x
  (0 "zero")
  ([x, y] (+ x y))
  ((Some v) v)
  (_ "default"))
```

### Module System (NEW!)
```lisp
(import "modules/math_utils" (square cube))
(import "modules/prelude" *)
```

### Enhanced Error Diagnostics (NEW!)
```
error[E002]: Undefined variable: squaer
  --> example.cl:5:10
   5 | (squaer 5)
     |  ^^^^^^
help: Did you mean: square?
```

## Advanced AI-First Features (NEW!)

### Semantic Versioning Based on Behavior
```lisp
; Two implementations with same behavior get compatible versions
(semantic:version bubble-sort)  ; => 1.0.0+abc123
(semantic:version quick-sort)   ; => 1.0.1+def456
```

### Automatic Proof Generation
```lisp
; Optimizations come with correctness proofs
(prove:equivalent
  (lambda (lst) (fold + 0 lst))
  (lambda (lst) (reduce + lst)))
; => Proof by induction on list structure
```

### Machine-Learnable Optimization
```lisp
; System learns from execution patterns
(optimize:hint 'vectorize
  (map square data))
; => Learns: "Vectorize arithmetic maps on numeric data"
```

### Runtime Graph Queries
```lisp
; Query and transform program structure
(graph:query (graph:of my-function)
  (select 'application)
  (where (lambda (n) (recursive? n)))
  (transform memoize))
```

### Formal Specifications
```lisp
; Embed mathematical specs in code
(spec:contract sort
  :requires [(list? input)]
  :ensures [(sorted? result) (permutation? input result)])
```

### Documentation from Traces
```lisp
; Generate docs from runtime behavior
(trace:analyze my-program)
; => Performance profiles, call graphs, optimization opportunities
```

See [AI-First Features Documentation](docs/AI_FIRST_FEATURES.md) for details.

## Future Work

- **JIT Compilation**: Expected 3-5x speedup
- **Concurrency**: Async/await and parallel primitives
- **Package Manager**: Dependency management and registry
- **Developer Tools**: LSP, formatter, linter
- **Formal Verification**: Prove program properties

## Contributing

Contributions welcome! See [CONTRIBUTING.md](CONTRIBUTING.md) for guidelines.

## License

MIT License - see [LICENSE](LICENSE) for details.

## Design Philosophy

ClaudeLang prioritizes:
- **Machine readability** over human aesthetics
- **Explicitness** over brevity
- **Optimization** over flexibility
- **Correctness** over convenience

By making semantics explicit and optimization-friendly, we enable aggressive compile-time optimization while maintaining correctness. The result is a language that serves as a bridge between human intent and machine execution.