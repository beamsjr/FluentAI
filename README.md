# ClaudeLang: An AI-First Programming Language

[![Python Version](https://img.shields.io/badge/python-3.9%2B-blue)](https://www.python.org/downloads/)
[![License](https://img.shields.io/badge/license-MIT-green)](LICENSE)

ClaudeLang is an experimental programming language that explores what happens when we design a language specifically for AI systems rather than humans. It features a graph-based AST, explicit semantics, and advanced AI-driven optimization capabilities.

## Table of Contents

- [Key Features](#key-features)
- [Quick Example](#quick-example)
- [Installation](#installation)
- [Testing](#testing)
- [Language Features](#language-features)
- [AI-First Features](#ai-first-features)
- [Performance](#performance)
- [Documentation](#documentation)
- [Project Structure](#project-structure)
- [Contributing](#contributing)

## Key Features

### 🧠 AI-First Design
- **Graph-based AST**: Programs as directed graphs, not text
- **Explicit semantics**: All effects and dependencies declared
- **Machine-readable specs**: Formal specifications embedded in code
- **Semantic versioning**: Version numbers based on behavior, not syntax

### ⚡ Performance & Optimization
- **27-83x performance improvement** from baseline
- **Multiple backends**: Bytecode VM, JIT compiler, and LLVM
- **Automatic proof generation**: Every optimization is verified
- **ML-driven optimization**: Learns patterns from execution

### 🔧 Modern Language Features
- **Pattern matching**: ML-style with exhaustiveness checking
- **Algebraic data types**: Sum and product types with pattern matching
- **Effect system**: Explicit tracking of IO, State, Error, etc.
- **Module system**: Namespaces and dependency management
- **Type annotations**: Optional type ascription for clarity and optimization

### 📊 Advanced Capabilities
- **Behavioral contracts**: Pre/postconditions and invariants
- **Property-based testing**: Automatic test generation with Hypothesis
- **Execution trace analysis**: Generate docs from runtime behavior
- **Graph queries**: Analyze and transform program structure
- **Proof export**: Generate formal proofs for optimizations

## Quick Example

```lisp
;; Define an algebraic data type
(data List a
  (Nil)
  (Cons a (List a)))

;; Pattern matching with type annotation
(: (lambda (xs)
     (match xs
       ((Nil) 0)
       ((Cons x xs) (+ 1 (length xs)))))
   (Function (List a) Int))

;; Contracts with formal specifications
(spec:contract map
  :requires [(function? f) (list? xs)]
  :ensures [(= (length result) (length xs))]
  :where [(f : (Function a b))
          (xs : (List a))
          (result : (List b))])
```

## Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/claudelang.git
cd claudelang

# Install dependencies
pip install -r requirements.txt

# Optional: Install Hypothesis for property-based testing
pip install hypothesis

# Run the REPL
python -m src.repl
```

## Testing

```bash
# Run all tests
python -m unittest discover tests

# Run property-based tests
python -m unittest tests.test_properties

# Run specific test module
python -m unittest tests.test_parser

# Run with verbose output
python -m unittest discover tests -v
```

## Language Features

### S-Expression Syntax
```lisp
;; Basic expressions
(+ 1 2)                          ; => 3
(lambda (x) (* x x))             ; Square function
(let ((x 5)) (+ x 1))           ; Let binding

;; Lists and pattern matching
[1 2 3 4]                        ; List literal
(match lst
  ([] 0)                         ; Empty list
  ([x, ... xs] (+ x (sum xs))))  ; Head and tail

;; Effects
(effect io:print "Hello!")       ; IO effect
(effect state:set counter 0)     ; State effect
```

### Type System
```lisp
;; Type annotations
(: 42 Int)
(: "hello" String)
(: (lambda (x) x) (Function a a))

;; Algebraic data types
(data Option a
  (None)
  (Some a))

(data Tree a
  (Leaf a)
  (Node (Tree a) a (Tree a)))
```

### Module System
```lisp
;; Define a module
(module math
  (export add multiply square)
  
  (define add (lambda (x y) (+ x y)))
  (define multiply (lambda (x y) (* x y)))
  (define square (lambda (x) (* x x))))

;; Import from module
(import math (add square))
(import math :as m)              ; Qualified import
```

## AI-First Features

### Graph-Based AST
Programs are represented as directed graphs, enabling:
- Sophisticated program analysis
- Safe transformations and optimizations
- Pattern recognition across code structures

### Behavioral Versioning
Version numbers are computed from actual behavior:
```lisp
;; Version 1.0.0
(define add (lambda (x y) (+ x y)))

;; Still version 1.0.0 (same behavior)
(define add (lambda (a b) (+ a b)))

;; Version 2.0.0 (different behavior)
(define add (lambda (x y) (+ x y 1)))
```

### Proof Generation
Every optimization generates a machine-checkable proof:
```lisp
;; Optimizer proves: (map f (map g xs)) = (map (compose f g) xs)
;; Generates Coq/Lean proof of equivalence
```

## Performance

Comprehensive benchmarking shows significant improvements:

| Optimization | Speedup | Description |
|-------------|---------|-------------|
| Baseline | 1.0x | Tree-walking interpreter |
| Bytecode VM | 2.8x | Stack-based virtual machine |
| JIT Compiler | 15.2x | Native code generation |
| Type Specialization | 27.4x | Type-guided optimization |
| LLVM Backend | 83.3x | Full optimization pipeline |

## Documentation

- [Language Specification](docs/LANGUAGE_SPECIFICATION.md) - Complete language reference
- [Quick Start Guide](docs/QUICK_START.md) - Getting started tutorial
- [Effect System](docs/EFFECT_SYSTEM.md) - Effect handling and handlers
- [Pattern Matching](docs/PATTERN_MATCHING.md) - Pattern matching guide
- [Module System](docs/MODULE_SYSTEM.md) - Modules and imports
- [AI-First Features](docs/AI_FIRST_FEATURES.md) - Advanced AI capabilities
- [Property Testing](docs/PROPERTY_TESTING.md) - Property-based testing guide

## Project Structure

```
claudelang/
├── src/                 # Source code
│   ├── core/           # Core language (AST, primitives)
│   ├── parser/         # S-expression parser
│   ├── interpreter/    # Tree-walking interpreter
│   ├── compiler/       # Bytecode compiler
│   ├── vm/            # Virtual machine
│   ├── jit/           # JIT compiler
│   ├── optimizer/     # Graph optimizer
│   ├── effects/       # Effect system
│   ├── types/         # Type system
│   ├── modules/       # Module system
│   ├── contracts/     # Contract verification
│   └── stdlib/        # Standard library
├── tests/              # Test suite
├── tools/              # Development tools
│   ├── benchmark*.py  # Performance benchmarks
│   ├── analyze_traces.py # Trace analysis
│   └── verify_contracts.py # Contract verification
├── docs/               # Documentation
└── examples/           # Example programs
```

## Recent Updates

### Property-Based Testing (Latest)
- Added comprehensive property-based tests using Hypothesis
- Fixed string escaping and scientific notation parsing
- Improved robustness of parser and interpreter
- All 20 property tests now pass

### Type Annotations
- Added type ascription syntax: `(: expr Type)`
- Support for complex type annotations
- Integration with optimization pipeline

### Algebraic Data Types
- Full support for sum types with pattern matching
- Recursive data types (List, Tree, etc.)
- Constructor functions with proper arity checking

### Performance Optimizations
- JIT compiler with guard-based specialization
- LLVM backend integration
- Reference counting garbage collection
- Bytecode caching system

## Contributing

Contributions are welcome! Please see our [Contributing Guide](CONTRIBUTING.md) for details.

### Development Setup
```bash
# Install development dependencies
pip install -r requirements-dev.txt

# Run tests
python -m pytest

# Run linter
python -m flake8 src tests

# Run type checker
python -m mypy src
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

ClaudeLang explores ideas from:
- Scheme/Lisp (S-expressions, functional programming)
- ML/Haskell (type system, pattern matching)
- Koka/Frank (effect system)
- Lean/Coq (proof generation)
- Various AI/ML optimization techniques

Special thanks to the programming language theory community for inspiration and ideas.
