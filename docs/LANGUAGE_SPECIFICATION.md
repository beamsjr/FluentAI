# FluentAI Language Specification v1.0

## Table of Contents
1. [Introduction](#introduction)
2. [Syntax](#syntax)
3. [Type System](#type-system)
4. [Effects System](#effects-system)
5. [Standard Library](#standard-library)
6. [Optimization Guarantees](#optimization-guarantees)
7. [Implementation Notes](#implementation-notes)

## Introduction

FluentAI is an AI-first programming language designed for machine understanding and optimization. It prioritizes unambiguous syntax, explicit semantics, and compile-time optimization over human convenience.

### Design Principles
- **Unambiguous Parsing**: S-expression syntax with no precedence rules
- **Explicit Semantics**: All effects and types are explicit
- **Graph-Based AST**: Programs are graphs, not text
- **Optimization-Friendly**: Designed for aggressive compile-time optimization
- **Machine-Readable**: Documentation and types are first-class

## Syntax

### Basic Syntax

FluentAI uses S-expressions (symbolic expressions) for all constructs:

```lisp
(operator argument1 argument2 ...)
```

### Literals

```lisp
; Numbers
42          ; Integer
3.14        ; Float
-17         ; Negative integer

; Booleans
#t          ; True
#f          ; False

; Strings
"hello"     ; String literal
"multi\nline" ; With escape sequences

; Lists
[]          ; Empty list
[1 2 3]     ; List literal
```

### Variables and Binding

```lisp
; Variable reference
x

; Let binding
(let ((x 10)
      (y 20))
  (+ x y))

; Recursive let
(let ((fact (lambda (n)
             (if (== n 0)
                 1
                 (* n (fact (- n 1)))))))
  (fact 5))
```

### Functions

```lisp
; Lambda expression
(lambda (x y) (+ x y))

; Function application
(f arg1 arg2)

; Higher-order functions
(map (lambda (x) (* x 2)) [1 2 3])
```

### Conditionals

```lisp
; If expression
(if condition
    then-expr
    else-expr)

; Example
(if (> x 0)
    "positive"
    "non-positive")
```

### Sequences

```lisp
; Sequential evaluation
(begin
  (print "First")
  (print "Second")
  42)  ; Returns last value
```

## Type System

### Primitive Types

- `Int`: Integer numbers
- `Float`: Floating-point numbers
- `Bool`: Boolean values (#t, #f)
- `String`: Text strings
- `List[T]`: Homogeneous lists
- `Function[T1, T2, ..., Tn] -> R`: Function types

### Type Annotations (Future)

```lisp
; Type annotations
(lambda ((x : Int) (y : Int)) : Int
  (+ x y))

; Type aliases
(type Point [Int Int])
```

### Type Inference

FluentAI performs type inference for optimization purposes:
- Constant propagation
- Type specialization
- Dead code elimination

## Effects System

FluentAI features a comprehensive effect system with first-class effect primitives.

### Effect Types

- `PURE` - No side effects
- `IO` - Input/output operations  
- `STATE` - Mutable state access
- `ERROR` - Error handling
- `TIME` - Time-dependent operations
- `NETWORK` - Network operations
- `RANDOM` - Non-deterministic operations

### Effect Primitives

```lisp
; IO effects
(io:print "Hello, World!")              ; Print to stdout
(io:read-line)                          ; Read from stdin
(io:open-file "data.txt" "r")          ; Open file
(io:read-file file-handle)              ; Read file
(io:write-file file-handle "content")   ; Write file
(io:close-file file-handle)             ; Close file

; State effects
(state:set "counter" 0)                 ; Set state
(state:get "counter")                   ; Get state
(state:update "counter" (lambda (x) (+ x 1)))  ; Update with function
(state:delete "counter")                ; Delete state

; Transactions
(state:begin-transaction)
(state:set "x" 100)
(state:rollback-transaction)            ; Undo changes

; Error effects
(error:raise "div-by-zero" "Cannot divide by zero")
(error:catch "div-by-zero" (lambda (err) 0))  ; Install handler

; Time effects
(time:now)                              ; Current timestamp
(time:sleep 1.0)                        ; Sleep duration
(time:elapsed)                          ; Time since start

; Random effects
(random:float)                          ; Random [0,1)
(random:int 1 10)                       ; Random integer
(random:choice ["a" "b" "c"])          ; Random selection

; Network effects (simplified)
(network:fetch "https://api.example.com/data")
```

### Legacy Effect Syntax

```lisp
(effect io:print "Hello")               ; Explicit effect
(effect state:set "x" 42)
```

### Pure vs Effectful

```lisp
; Pure - can be optimized/memoized
(lambda (x) (* x 2))

; Effectful - must be executed
(lambda (x) 
  (io:print x)
  (* x 2))
```

## Standard Library

### Arithmetic Operations

```lisp
(+ a b)         ; Addition
(- a b)         ; Subtraction
(* a b)         ; Multiplication
(/ a b)         ; Division
(mod a b)       ; Modulo
(abs x)         ; Absolute value
```

### Comparison Operations

```lisp
(< a b)         ; Less than
(> a b)         ; Greater than
(<= a b)        ; Less than or equal
(>= a b)        ; Greater than or equal
(== a b)        ; Equal
(!= a b)        ; Not equal
```

### Boolean Operations

```lisp
(and a b)       ; Logical AND
(or a b)        ; Logical OR
(not x)         ; Logical NOT
```

### List Operations

```lisp
(cons x lst)    ; Prepend element
(head lst)      ; First element
(tail lst)      ; Rest of list
(length lst)    ; List length
(empty? lst)    ; Check if empty
(nth lst n)     ; Nth element
(append l1 l2)  ; Concatenate lists
```

### String Operations

```lisp
(string-length str)     ; String length
(concat s1 s2)         ; Concatenate strings
(string-upcase str)    ; Convert to uppercase
(string-downcase str)  ; Convert to lowercase
(substring str start end) ; Extract substring
```

### Higher-Order Functions

```lisp
(map f lst)           ; Apply function to each element
(filter pred lst)     ; Keep elements satisfying predicate
(fold f init lst)     ; Reduce list with function
(any? pred lst)       ; True if any element satisfies predicate
(all? pred lst)       ; True if all elements satisfy predicate
```

## Optimization Guarantees

### Constant Folding

Any expression involving only constants is evaluated at compile time:

```lisp
(+ 2 3)              ; Optimized to 5
(* (+ 1 2) (- 10 5)) ; Optimized to 15
```

### Dead Code Elimination

Unreachable code is removed:

```lisp
(if #t
    42
    (error "unreachable"))  ; Error branch removed
```

### Pure Expression Evaluation

Pure expressions with constant inputs are evaluated at compile time:

```lisp
(length [1 2 3 4 5])  ; Optimized to 5
(string-upcase "hello") ; Optimized to "HELLO"
```

### Type Specialization

Operations on known types use specialized instructions:

```lisp
(+ 10 20)  ; Uses ADD_INT instead of generic ADD
```

## Implementation Notes

### Graph-Based AST

Programs are represented as directed graphs:
- Nodes represent operations
- Edges represent data flow
- Enables non-local optimizations

### Bytecode VM

- Stack-based virtual machine
- 30+ opcodes including specialized variants
- Type-specialized instructions for performance

### Optimization Pipeline

1. Parse to AST graph
2. Type inference
3. Graph optimization (constant folding, DCE)
4. Bytecode generation with specialization
5. Optional native code generation

### Memory Management

- Reference counting for most objects
- Native lists for performance
- Future: Generational GC for cycles

## Examples

### Hello World

```lisp
(effect io:print "Hello, World!")
```

### Factorial

```lisp
(let ((fact (lambda (n)
             (if (<= n 1)
                 1
                 (* n (fact (- n 1)))))))
  (fact 10))
```

### Map Implementation

```lisp
(let ((map (lambda (f lst)
            (if (empty? lst)
                []
                (cons (f (head lst))
                      (map f (tail lst)))))))
  (map (lambda (x) (* x 2)) [1 2 3 4 5]))
```

### Effect Example

```lisp
(let ((counter (effect state:new 0)))
  (effect state:update counter (lambda (x) (+ x 1)))
  (effect state:get counter))
```

## Future Extensions

- **Algebraic Data Types**: Sum and product types
- **Pattern Matching**: Destructuring and matching
- **Module System**: Namespace management
- **Gradual Typing**: Optional static types
- **Metaprogramming**: Compile-time code generation
- **Parallel Constructs**: Explicit parallelism
- **Contracts**: Runtime and static verification

## Conclusion

FluentAI demonstrates that languages designed for machines can achieve both clarity and performance. By making semantics explicit and optimization-friendly, we enable aggressive compile-time optimization while maintaining correctness.