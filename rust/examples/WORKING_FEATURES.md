# FluentAI Working Features

This document lists what currently works in the FluentAI implementation.

## Tested and Working

### Core Language
- **Arithmetic**: `+`, `-`, `*`, `/`, `%`
- **Comparisons**: `=`, `<`, `>`, `<=`, `>=`  
- **Booleans**: `true`, `false`
- **Numbers**: Integers and floats
- **Strings**: String literals
- **Lists**: `list`, `cons`, `head`, `tail`, `length`, `empty?`

### Control Flow
- **Conditionals**: `if` expressions (must have both then and else)
- **Pattern Matching**: `match` with literal patterns and wildcard `_`
  - List patterns with `Cons` and `Nil` (but may not work as expected)

### Bindings and Functions
- **Let**: `let` bindings (single expression in body)
- **Letrec**: `letrec` for recursive bindings (has bugs with recursion)
- **Lambda**: Anonymous functions `(lambda (x) ...)`
- **Function Application**: Calling functions with arguments

### Built-in Functions  
- **Print**: Outputs values (returns nil)
- **List Operations**: As mentioned above
- **Type Predicates**: Various `?` functions

### Effect System (Partially Working!)
- **Effects**: `effect` form with default handlers works perfectly
  - Syntax: `(effect type:operation args...)`
  - Built-in effects:
    - `(effect io:print "message")` - prints to stdout
    - `(effect error:raise "error")` - raises runtime error
    - `(effect state:set "key" value)` - stores state
    - `(effect state:get "key")` - retrieves state
    - `(effect time:now)` - current timestamp
    - `(effect random:float)` - random float [0, 1)
    - `(effect random:int min max)` - random integer
- **Custom Handlers**: `handler` form compiles but has runtime bug (stack underflow)

## Not Working or Not Implemented

### Major Features
- **Module System**: `module`, `import`, `export`, `define`
- **Async/Concurrent**: `async`, `await`, `spawn`, `channel`, `send`, `receive`
- **Begin Blocks**: `begin` for sequencing (doesn't work properly)
- **Define**: Top-level definitions

### Known Bugs
- **Recursion**: Factorial and other recursive functions return incorrect results
- **Pattern Matching**: List pattern matching with `Cons` doesn't extract values correctly
- **Multiple Expressions**: Let bodies can only have one expression

## Example Usage

```scheme
; Working example
(let ((x 10)
      (y 20))
  (+ x y))  ; Returns 30

; Working pattern match
(match 42
  (0 "zero")
  (42 "forty-two")
  (_ "other"))  ; Returns "forty-two"

; Working lambda
(let ((square (lambda (x) (* x x))))
  (square 5))  ; Returns 25

; Working lists
(cons 1 (list 2 3 4))  ; Returns [1, 2, 3, 4]

; Working effects with default handlers
(effect io:print "Hello!")  ; Prints: Hello!
(effect state:set "x" 10)
(effect state:get "x")  ; Returns 10
(effect time:now)  ; Returns timestamp
(effect random:int 1 10)  ; Returns random int

; Custom handlers compile but have runtime bug
; (handler ((error (lambda (e) 99))) 
;   (effect error:raise "test"))  ; Stack underflow error
```