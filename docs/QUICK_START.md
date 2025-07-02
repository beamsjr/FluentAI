# ClaudeLang Quick Start Guide

## Installation

```bash
# Clone the repository
git clone https://github.com/anthropic/claudelang.git
cd claudelang

# Install dependencies (optional, for benchmarks)
pip install matplotlib numpy

# Run the REPL
python3 -m src.repl
```

## Your First ClaudeLang Program

### Hello World

```lisp
(print "Hello, World!")
```

### Basic Arithmetic

```lisp
(+ 2 3)           ; Returns 5
(* 4 5)           ; Returns 20
(- 10 3)          ; Returns 7
(/ 20 4)          ; Returns 5
```

### Variables and Functions

```lisp
; Define variables
(let ((x 10)
      (y 20))
  (+ x y))        ; Returns 30

; Define functions
(let ((double (lambda (x) (* x 2))))
  (double 21))    ; Returns 42
```

## Running ClaudeLang Code

### Using the REPL

```bash
$ python3 -m src.repl
ClaudeLang REPL v1.0
Type 'exit' to quit

> (+ 2 3)
5

> (let ((x 10)) (* x x))
100

> exit
```

### Running Files

```bash
# Run a ClaudeLang file
python3 run_claudelang.py examples/fibonacci.cl

# With optimization stats
python3 run_claudelang.py --verbose examples/optimization_demo.cl
```

### Using as a Library

```python
from src.parser import parse
from src.interpreter import Interpreter
from src.vm import BytecodeCompiler, VM
from src.optimizer import AdvancedGraphOptimizer

# Method 1: Interpreter (simple)
interp = Interpreter()
result = interp.eval("(+ 2 3)")
print(result.data)  # 5

# Method 2: VM (faster)
compiler = BytecodeCompiler()
vm = VM()
graph = parse("(+ 2 3)")
bytecode = compiler.compile(graph)
result = vm.execute(bytecode)
print(result)  # 5

# Method 3: Optimized VM (fastest)
optimizer = AdvancedGraphOptimizer()
optimized = optimizer.optimize(graph)
opt_bytecode = compiler.compile(optimized)
result = vm.execute(opt_bytecode)
print(result)  # 5
```

## Language Basics

### Data Types

```lisp
; Numbers
42              ; Integer
3.14            ; Float

; Booleans
#t              ; True
#f              ; False

; Strings
"hello"         ; String

; Lists
[]              ; Empty list
[1 2 3]         ; List of numbers
```

### Control Flow

```lisp
; Conditionals
(if (> x 0)
    "positive"
    "non-positive")

; Pattern: cond-like behavior
(let ((classify (lambda (x)
                 (if (> x 0)
                     "positive"
                     (if (< x 0)
                         "negative"
                         "zero")))))
  (classify -5))
```

### Lists and Recursion

```lisp
; List operations
(head [1 2 3])    ; Returns 1
(tail [1 2 3])    ; Returns [2 3]
(cons 0 [1 2 3])  ; Returns [0 1 2 3]
(length [1 2 3])  ; Returns 3

; Recursive list processing
(let ((sum (lambda (lst)
            (if (empty? lst)
                0
                (+ (head lst) (sum (tail lst)))))))
  (sum [1 2 3 4 5]))  ; Returns 15
```

### Higher-Order Functions

```lisp
; Map - transform each element
(map (lambda (x) (* x 2)) [1 2 3])  ; Returns [2 4 6]

; Filter - select elements
(filter (lambda (x) (> x 2)) [1 2 3 4])  ; Returns [3 4]

; Fold - reduce to single value
(fold + 0 [1 2 3 4 5])  ; Returns 15
```

## Common Patterns

### Factorial

```lisp
(let ((fact (lambda (n)
             (if (<= n 1)
                 1
                 (* n (fact (- n 1)))))))
  (fact 5))  ; Returns 120
```

### Fibonacci

```lisp
(let ((fib (lambda (n)
            (if (<= n 1)
                n
                (+ (fib (- n 1))
                   (fib (- n 2)))))))
  (fib 10))  ; Returns 55
```

### List Range

```lisp
(let ((range (lambda (start end)
              (if (> start end)
                  []
                  (cons start (range (+ start 1) end))))))
  (range 1 5))  ; Returns [1 2 3 4 5]
```

## Performance Tips

### 1. Use Native Lists
```lisp
; Good - uses native lists
[1 2 3 4 5]

; Less efficient - builds cons cells
(cons 1 (cons 2 (cons 3 [])))
```

### 2. Let the Optimizer Work
```lisp
; This entire expression is computed at compile time
(+ (* 2 3) (* 4 5))  ; Optimized to 26
```

### 3. Prefer Tail Recursion
```lisp
; Tail-recursive sum
(let ((sum-tail (lambda (lst acc)
                 (if (empty? lst)
                     acc
                     (sum-tail (tail lst) 
                              (+ acc (head lst)))))))
  (sum-tail [1 2 3 4 5] 0))
```

## Debugging

### Check Types
```lisp
; Type predicates
(number? 42)       ; #t
(string? "hello")  ; #t
(list? [1 2 3])   ; #t
```

### Print Debugging
```lisp
(let ((debug-sum (lambda (lst)
                  (print (concat "Summing: " (to-string lst)))
                  (let ((result (fold + 0 lst)))
                    (print (concat "Result: " (to-string result)))
                    result))))
  (debug-sum [1 2 3]))
```

## Next Steps

1. Explore the [examples/](../examples/) directory
2. Read the [Language Specification](LANGUAGE_SPECIFICATION.md)
3. Learn about [optimization](OPTIMIZATION_JOURNEY.md)
4. Contribute to the project!

## Common Issues

### "Unbound variable" Error
Make sure variables are defined before use:
```lisp
; Wrong
(+ x 10)  ; x not defined

; Right
(let ((x 5))
  (+ x 10))
```

### List vs Cons
Native lists `[1 2 3]` are more efficient than cons cells:
```lisp
; Prefer this
[1 2 3 4 5]

; Over this
(cons 1 (cons 2 (cons 3 (cons 4 (cons 5 [])))))
```

### Recursion Depth
For deep recursion, use tail recursion or iterative approaches.