# S-Expression to FLC Migration Guide

This guide helps developers migrate FluentAI code from s-expression syntax to FLC (Fluent Lambda Chain) syntax.

## Basic Syntax Comparison

### Variables and Literals

**S-Expression:**
```lisp
42
"hello"
true
x
```

**FLC:**
```flc
42
"hello"
true
x
```

### Function Calls

**S-Expression:**
```lisp
(add 1 2)
(print "Hello")
```

**FLC:**
```flc
add(1, 2)
print("Hello")
```

### Lambda Expressions

**S-Expression:**
```lisp
(lambda (x) (* x 2))
(lambda (x y) (+ x y))
(() => 42)  ; alternative syntax
```

**FLC:**
```flc
x => x * 2
(x, y) => x + y
() => 42
```

### Let Bindings

**S-Expression:**
```lisp
(let ((x 10)
      (y 20))
  (+ x y))
```

**FLC:**
```flc
let x = 10;
let y = 20;
x + y
```

### Function Definitions

**S-Expression:**
```lisp
(define (add x y)
  (+ x y))

(define factorial
  (lambda (n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))
```

**FLC:**
```flc
private function add(x, y) {
    x + y
}

private function factorial(n) {
    if (n <= 1) {
        1
    } else {
        n * factorial(n - 1)
    }
}
```

### Control Flow

**S-Expression:**
```lisp
(if (> x 0)
    "positive"
    "non-positive")

(begin
  (print "First")
  (print "Second")
  42)
```

**FLC:**
```flc
if (x > 0) {
    "positive"
} else {
    "non-positive"
}

{
    print("First");
    print("Second");
    42
}
```

### Pattern Matching

**S-Expression:**
```lisp
(match value
  ((Some x) (* x 2))
  (None 0))
```

**FLC:**
```flc
value.match()
    .case(Some(x), x * 2)
    .case(None, 0)
    .get()

// Or using match expression:
match value {
    Some(x) => x * 2,
    None => 0
}
```

### Method Chaining

**S-Expression:**
```lisp
(map (filter users (lambda (u) (> (get-age u) 18)))
     (lambda (u) (get-name u)))
```

**FLC:**
```flc
users
    .filter(u => u.age > 18)
    .map(u => u.name)
```

### Async/Await

**S-Expression:**
```lisp
(await (spawn (lambda () (fetch-data))))
(spawn (lambda () 42))
```

**FLC:**
```flc
// Spawn is now implemented in FLC:
spawn { fetch_data() }.await()
spawn { () => 42 }
spawn(() => compute())

// With async function:
private async function get_data() {
    fetch_data().await()
}

// In blocks:
{
    let task = spawn { expensive_computation() };
    // Do other work...
    let result = task.await();
    result
}
```

### Collections

**S-Expression:**
```lisp
(list 1 2 3)
'(1 2 3)
(make-map "key" "value")
(make-set 1 2 3)
```

**FLC:**
```flc
[1, 2, 3]
{"key": "value"}
#{1, 2, 3}

// With spread operator:
[1, ...other_items, 3]
#{...set1, ...set2}
```

### Contracts

**S-Expression:**
```lisp
(define/contract (sqrt x)
  (requires (>= x 0))
  (ensures (>= result 0))
  (math-sqrt x))
```

**FLC:**
```flc
@requires(x >= 0)
@ensures(result >= 0)
private function sqrt(x) {
    math_sqrt(x)
}
```

### Modules

**S-Expression:**
```lisp
(import "std/list" (map filter))
(import "http" :as http)
```

**FLC:**
```flc
use std::list::{map, filter};
use http as http;
use collections::*;
```

### Error Handling

**S-Expression:**
```lisp
(try
  (risky-operation)
  (catch (e)
    (handle-error e))
  (finally
    (cleanup)))
```

**FLC:**
```flc
try {
    risky_operation()
} catch (e) {
    handle_error(e)
} finally {
    cleanup()
}
```

## Complete Example Migration

### Before (S-Expression):
```lisp
(begin
  (define (process-users users)
    (map (filter users 
                 (lambda (user) (>= (get user 'age) 18)))
         (lambda (user) 
           (format "~a <~a>" 
                   (get user 'name) 
                   (get user 'email)))))
  
  (let ((users (list (make-map 'name "Alice" 'age 25 'email "alice@example.com")
                     (make-map 'name "Bob" 'age 17 'email "bob@example.com")
                     (make-map 'name "Charlie" 'age 30 'email "charlie@example.com"))))
    (process-users users)))
```

### After (FLC):
```flc
private function process_users(users) {
    users
        .filter(user => user.age >= 18)
        .map(user => f"{user.name} <{user.email}>")
}

let users = [
    {name: "Alice", age: 25, email: "alice@example.com"},
    {name: "Bob", age: 17, email: "bob@example.com"},
    {name: "Charlie", age: 30, email: "charlie@example.com"}
];

process_users(users)
```

## Migration Tips

1. **Start with simple expressions**: Begin by migrating literals and basic function calls.

2. **Use method chaining**: Replace nested function calls with fluent method chains for better readability.

3. **Leverage type inference**: FLC has better type inference, so many type annotations are optional.

4. **Use modern features**: Take advantage of FLC features like:
   - Optional chaining (`.?`)
   - Spread operator (`...`)
   - Destructuring (`let {x, y} = point`)
   - F-strings (`f"Hello {name}"`)

5. **Test incrementally**: Migrate and test small pieces at a time to ensure correctness.

## Common Pitfalls

1. **Parentheses**: Remember that FLC uses `()` for function calls, not for grouping expressions like s-expressions.

2. **Semicolons**: Statements in FLC need semicolons, but the last expression in a block doesn't.

3. **Keywords**: Some s-expression function names are keywords in FLC (e.g., `match`, `case`).

4. **Operator precedence**: FLC follows standard operator precedence, unlike s-expressions which have explicit precedence through parentheses.

## Automated Migration

While manual migration is recommended for complex code, simple patterns can be automated. The FluentAI project is working on migration tools to help with the transition.