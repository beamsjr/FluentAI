# ClaudeLang Tutorial

Welcome to ClaudeLang, an AI-first programming language designed for clarity, explicitness, and machine understanding.

## Table of Contents

1. [Introduction](#introduction)
2. [Core Concepts](#core-concepts)
3. [Basic Syntax](#basic-syntax)
4. [Functions](#functions)
5. [Effects System](#effects-system)
6. [Type System](#type-system)
7. [Advanced Features](#advanced-features)
8. [Best Practices](#best-practices)

## Introduction

ClaudeLang prioritizes:
- **Unambiguous syntax**: One way to express each concept
- **Explicit effects**: All side effects are visible in the code
- **Graph-based representation**: Code as data structures
- **AI-friendly design**: Optimized for machine processing

## Core Concepts

### Everything is an Expression

In ClaudeLang, everything evaluates to a value:

```clojure
(+ 1 2)           ; => 3
(if true 5 10)    ; => 5
(let ((x 42)) x)  ; => 42
```

### Explicit Effects

Side effects are never hidden:

```clojure
; Pure computation - no effects
(+ 2 3)

; IO effect - explicitly marked
(effect :io (print "Hello"))

; State effect - mutation is visible
(effect :state (set "counter" 5))
```

### Immutability by Default

All values are immutable. State changes create new values:

```clojure
(let ((lst [1 2 3])
      (new-lst (cons 0 lst)))
  lst)  ; => [1 2 3] (unchanged)
```

## Basic Syntax

### Literals

```clojure
42              ; Integer
3.14            ; Float
"hello"         ; String
true            ; Boolean
[1 2 3]         ; List
```

### Variables and Binding

Use `let` to bind variables:

```clojure
(let ((x 10)
      (y 20))
  (+ x y))  ; => 30
```

### Conditionals

`if` requires both then and else branches:

```clojure
(if (> x 0)
    "positive"
    "non-positive")
```

## Functions

### Lambda Functions

Functions are first-class values:

```clojure
(lambda (x) (+ x 1))  ; Anonymous function

(let ((inc (lambda (x) (+ x 1))))
  (inc 5))  ; => 6
```

### Higher-Order Functions

Functions can take and return functions:

```clojure
(let ((twice (lambda (f x) (f (f x))))
      (inc (lambda (x) (+ x 1))))
  (twice inc 5))  ; => 7
```

### Recursion

Recursive functions with explicit termination:

```clojure
(let ((factorial (lambda (n)
                   (if (<= n 1)
                       1
                       (* n (factorial (- n 1)))))))
  (factorial 5))  ; => 120
```

## Effects System

### IO Effects

Input/output operations:

```clojure
(do
  (effect :io (print "Enter name: "))
  (let ((name (effect :io (read))))
    (effect :io (print (concat "Hello, " name)))))
```

### Error Effects

Explicit error handling:

```clojure
(let ((safe-head (lambda (lst)
                   (if (empty? lst)
                       (effect :error "Empty list")
                       (head lst)))))
  (safe-head [1 2 3]))  ; => 1
```

### State Effects

Controlled mutation:

```clojure
(do
  (effect :state (set "counter" 0))
  (effect :state (set "counter" 
    (+ 1 (effect :state (get "counter")))))
  (effect :state (get "counter")))  ; => 1
```

## Type System

### Basic Types

- `Int`: Integers
- `Float`: Floating-point numbers
- `String`: Text
- `Bool`: true/false
- `List[T]`: Homogeneous lists
- `Function[A, B]`: Functions from A to B

### Effect Types

Types include effect information:

```clojure
; Pure function type
(-> Int Int)

; Function with IO effects
(-> String Unit ~{IO})

; Function with multiple effects
(-> Int Int ~{IO, State})
```

### Probabilistic Types

Uncertainty as a first-class concept:

```clojure
; 80% confident it's an integer
Uncertain<Int, 0.8>
```

## Advanced Features

### Parallel Evaluation

Explicit parallelism:

```clojure
(parallel
  (expensive-computation-1)
  (expensive-computation-2)
  (expensive-computation-3))  ; All run in parallel
```

### Uncertainty

Probabilistic programming:

```clojure
(uncertain
  (0.6 "sunny")
  (0.3 "cloudy")
  (0.1 "rainy"))  ; Weather prediction
```

### Pattern Matching (Future)

Exhaustive pattern matching:

```clojure
(match value
  [:ok result] result
  [:error msg] (effect :io (print msg)))
```

## Best Practices

### 1. Make Effects Explicit

Always use effect markers:

```clojure
; Bad - hidden effect
(define print-value (lambda (x) (print x)))

; Good - explicit effect
(define print-value 
  (lambda (x) (effect :io (print x))))
```

### 2. Prefer Pure Functions

Isolate effects at the boundaries:

```clojure
; Core logic - pure
(define process-data
  (lambda (data)
    (map (lambda (x) (* x 2)) 
         (filter (lambda (x) (> x 0)) data))))

; Effects at the boundary
(let ((input (effect :io (read-data)))
      (result (process-data input)))
  (effect :io (write-data result)))
```

### 3. Use Types for Documentation

Type annotations clarify intent:

```clojure
(define add
  :type (-> Int Int Int)
  :effects #{:pure}
  (lambda (x y) (+ x y)))
```

### 4. Handle All Error Cases

Explicit error handling:

```clojure
(define safe-divide
  :type (-> Float Float (Result Float Error))
  (lambda (x y)
    (if (== y 0)
        [:error "Division by zero"]
        [:ok (/ x y)])))
```

## Running ClaudeLang

### REPL

Start the interactive interpreter:

```bash
python -m claudelang.repl
```

### Running Files

Execute a ClaudeLang file:

```bash
python -m claudelang.run examples/hello.cl
```

### Compilation (Future)

Compile to efficient targets:

```bash
claudelang compile program.cl --target wasm
```

## Next Steps

1. Explore the [examples](../examples/) directory
2. Read the [language specification](./language_spec.md)
3. Learn about [AI integration](./ai_integration.md)
4. Contribute to the [standard library](../stdlib/)

Remember: ClaudeLang is designed for clarity and machine understanding. When in doubt, be explicit!