# S-Expression to FLC Syntax Mapping

This document provides a comprehensive mapping from FluentAI's current s-expression syntax to the new FLC (Fluent Lambda Chain) syntax.

## Basic Expressions

### Literals

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `42` | `42` | Integer literal |
| `3.14` | `3.14` | Float literal |
| `"hello"` | `"hello"` | String literal |
| `true` | `true` | Boolean literal |
| `false` | `false` | Boolean literal |
| `nil` | `nil` | Nil/null value |

### Variables and Identifiers

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `foo` | `foo` | Variable reference |
| `user-name` | `user_name` | Naming convention change |
| `MAX-SIZE` | `MAX_SIZE` | Constants use SCREAMING_SNAKE_CASE |

## Function Application

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(print "hello")` | `print("hello")` | Simple function call |
| `(+ 1 2)` | `1 + 2` | Binary operators |
| `(map f list)` | `list.map(f)` | Method chaining |
| `(filter pred (map f list))` | `list.map(f).filter(pred)` | Chained methods |

## Lambda Expressions

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(lambda (x) (* x 2))` | `{ \|x\| x * 2 }` | Simple lambda |
| `(lambda (x y) (+ x y))` | `{ \|x, y\| x + y }` | Multiple parameters |
| `(lambda () 42)` | `{ \|\| 42 }` | No parameters |

## Let Bindings

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(let ((x 10)) x)` | `let x = 10; x` | Single binding |
| `(let ((x 10) (y 20)) (+ x y))` | `let x = 10; let y = 20; x + y` | Multiple bindings |
| `(letrec ((f (lambda (n) ...))) ...)` | `let rec f = { \|n\| ... }; ...` | Recursive binding |

## Control Flow

### If Expressions

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(if cond then else)` | `if (cond) { then } else { else }` | Standard if |
| `(if cond then)` | `if (cond) { then }` | No else branch |
| `(cond ((test1) act1) ((test2) act2))` | `match() { case(test1, act1), case(test2, act2) }` | Multi-branch |

### Pattern Matching

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(match expr ((pat1) res1) ((pat2) res2))` | `expr.match().case(pat1, res1).case(pat2, res2).run()` | Pattern matching |
| `(match expr ((:ok val) val) ((:err e) e))` | `expr.match().case(Ok(val), val).case(Err(e), e).run()` | Result matching |

## Function Definitions

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(define foo 42)` | `def foo = 42;` | Value definition |
| `(define (add x y) (+ x y))` | `def fn add(x: int, y: int) -> int { x + y }` | Function definition |
| `(define (map f list) ...)` | `def fn map<T, U>(f: fn(T) -> U, list: List<T>) -> List<U> { ... }` | Generic function |

## Structs and Types

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(struct user ((name string) (age int)))` | `def struct User { name: string, age: int }` | Struct definition |
| `(make-user :name "Alice" :age 30)` | `User { name: "Alice", age: 30 }` | Struct construction |
| `(user-name u)` | `u.name` | Field access |

## Collections

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(list 1 2 3)` | `[1, 2, 3]` | List literal |
| `(cons 1 (list 2 3))` | `[1] ++ [2, 3]` or `1 :: [2, 3]` | List cons |
| `(map :key "value")` | `{"key": "value"}` | Map/dictionary |
| `(set 1 2 3)` | `#{1, 2, 3}` | Set literal |

## Module System

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(module foo ...)` | `mod foo { ... }` | Module definition |
| `(export foo bar)` | `pub use {foo, bar};` | Export symbols |
| `(import (module path) foo)` | `use module::path::foo;` | Import specific |
| `(import (module path) *)` | `use module::path::*;` | Import all |

## Async/Concurrent Features

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(async expr)` | `async { expr }` | Async block |
| `(await promise)` | `promise.await()` | Await expression |
| `(spawn expr)` | `spawn { expr }` | Spawn task |
| `(chan)` | `Channel::new()` | Create channel |
| `(send! ch val)` | `ch.send(val)` | Send to channel |
| `(recv! ch)` | `ch.recv()` | Receive from channel |

## Error Handling

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(try expr (catch e handler))` | `try { expr } catch (e) { handler }` | Try-catch |
| `(finally expr cleanup)` | `try { expr } finally { cleanup }` | Finally block |
| `(throw msg)` | `throw Error(msg)` | Throw exception |

## Effects and Handlers

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(effect IO ((print string)))` | `def effect IO { fn print(s: string); }` | Effect definition |
| `(handle expr ((IO print) handler))` | `expr.handle { IO::print => handler }` | Effect handler |
| `(perform IO print "hello")` | `perform IO::print("hello")` | Perform effect |

## Method Chaining and Pipes

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(-> val (f1) (f2) (f3))` | `val.f1().f2().f3()` | Thread-first macro |
| `(->> val (f1) (f2) (f3))` | `val \|> f1 \|> f2 \|> f3` | Pipe operator |
| `(doto obj (method1) (method2))` | `obj.method1(); obj.method2(); obj` | Side effects |

## String Interpolation

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(format "Hello, %s!" name)` | `f"Hello, {name}!"` | F-string interpolation |
| `(str "Total: " count)` | `f"Total: {count}"` | String concatenation |

## Type Annotations

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(: expr type)` | `expr: type` | Type annotation |
| `(fn [x :int] :int body)` | `fn(x: int) -> int { body }` | Function types |
| `(list-of int)` | `List<int>` | Generic types |

## Traits and Implementations

| S-Expression | FLC Syntax | Notes |
|--------------|------------|-------|
| `(trait Show (show self))` | `def trait Show { fn show(self) -> string; }` | Trait definition |
| `(impl Show User ...)` | `def impl Show for User { ... }` | Trait implementation |

## Complete Example Translation

### S-Expression Version:
```lisp
(module user-service
  (import (std io) print)
  (import (std collections) map filter)
  
  (struct user ((id int) (name string) (age int)))
  
  (define (create-user name age)
    (make-user :id (generate-id) 
               :name name 
               :age age))
  
  (define (adult? user)
    (>= (user-age user) 18))
  
  (define (main)
    (let ((users (list 
                   (create-user "Alice" 25)
                   (create-user "Bob" 17)
                   (create-user "Charlie" 30))))
      (->> users
           (filter adult?)
           (map user-name)
           (map (lambda (name) (format "Adult: %s" name)))
           (map print)))))
```

### FLC Version:
```flc
mod user_service {
  use std::io::print;
  use std::collections::{map, filter};
  
  def struct User {
    id: int,
    name: string,
    age: int
  }
  
  def fn create_user(name: string, age: int) -> User {
    User {
      id: generate_id(),
      name: name,
      age: age
    }
  }
  
  def fn is_adult(user: User) -> bool {
    user.age >= 18
  }
  
  def fn main() {
    let users = [
      create_user("Alice", 25),
      create_user("Bob", 17),
      create_user("Charlie", 30)
    ];
    
    users
      .filter(is_adult)
      .map { |u| u.name }
      .map { |name| f"Adult: {name}" }
      .for_each(print);
  }
}
```

## Migration Notes

1. **Naming Conventions**: Convert kebab-case to snake_case for identifiers
2. **Method Chaining**: Prefer `.method()` over `(method obj)` for better readability
3. **Type Annotations**: FLC requires more explicit type information for clarity
4. **Module System**: FLC uses Rust-style module paths with `::`
5. **Pattern Matching**: FLC's match is method-based for fluent chaining
6. **String Formatting**: Use f-strings instead of format functions