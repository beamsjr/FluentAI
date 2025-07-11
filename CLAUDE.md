  1. Always run the full workspace test suite after making any changes, no matter how isolated they seem
  2. Before declaring any fix complete, verify that all tests across the entire codebase still pass
  3. If a change breaks tests in other modules, investigate why and either:
    - Fix the broken tests if they were relying on incorrect behavior
    - Reconsider the change if it breaks legitimate functionality
    - Find a solution that satisfies all parts of the system

 This is especially important in a complex system like FluentAI where modules are interconnected in subtle ways.
 The LoadGlobal change was a perfect example - what seemed like a simple fix in the VM module ended up  breaking functionality in the effects module.

  Please make sure to follow this workflow for all future changes:
  1. Make change
  2. Run local module tests
  3. Run full workspace tests
  4. Only proceed if ALL tests pass
  5. If any test fails, investigate and resolve before moving on

Also very important for development, if you change something in critical parts of the system that heavely depends on order, please leave a comment on what test you changed somthing for, this way If we find later that this change broke something else we know why we made the change so we can make sure to get both issues resolved. 

## Common Pitfalls to Watch For

### Catch-all Pattern in Match Statements
When adding new cases to match statements (especially in optimizers or analyzers), be aware of catch-all patterns like `_ => {}`. These can silently ignore new node types if they're placed before your new case. For example:

```rust
match node {
    Node::Application { .. } => { /* handle */ }
    Node::Lambda { .. } => { /* handle */ }
    _ => {} // This catches everything else!
    Node::Channel { .. } => { /* This will NEVER be reached! */ }
}
```

Always check if there's a catch-all pattern and ensure your new cases are added BEFORE it. This is particularly important in:
- Dead code elimination (mark_reachable methods)
- Effect analysis 
- Node copying/transformation functions
- Any visitor pattern implementation

---

# FLC Language Specification

This document defines the Fluent Lambda Chain (FLC) language syntax that will replace the current s-expression syntax in FluentAI. FLC is designed for clarity, expressiveness, and safety, with method chaining as the primary mode of expression.

## Refined FLC: The Complete Style Guide

### 1. Core Philosophy

Refined FLC (Fluent Lambda Chain) is a general-purpose programming language designed for clarity, expressiveness, and safety. Its core philosophy is that all operations should, whenever possible, be expressed as a linear, left-to-right chain of transformations.

This is achieved by making method chaining the primary mode of expression, enhanced by powerful, inline lambda blocks ({...}) that allow for complex logic without breaking the fluent flow. The syntax prioritizes consistency, unambiguity, and human ergonomics, making it ideal for both developers and AI-driven tooling.

### 2. Lexical Structure

This section defines the basic grammar and tokens of the language.

**Comments:**
```
// Single-line comment
/* Multi-line comment */
```

**Identifiers & Naming Conventions:**
- `snake_case`: For variables and function names (`user_name`, `calculate_total`).
- `PascalCase`: For all types (structs, enums, traits, actors) (`User`, `RequestState`).
- `SCREAMING_SNAKE_CASE`: For constants and enum variants (`MAX_USERS`, `RequestState.Success`).

**Keywords:**
- Definitions: `def`, `pub`, `fn`, `struct`, `enum`, `trait`, `impl`, `type`, `actor`, `effect`, `macro`, `extern`
- Control Flow: `if`, `else`, `match`, `case`, `for`, `while`, `in`, `let`, `const`
- Concurrency: `async`, `await`, `parallel`, `handle`
- Error Handling: `try`, `catch`, `finally`
- Modules: `use`, `mod`
- Other: `true`, `false`, `self`, `super`, `unsafe`, `dyn`, `with`, `perform`

**Operators:**
- Chaining: `.` (method call), `|>` (pipe-to), `.?` (optional chaining)
- Arithmetic: `+`, `-`, `*`, `/`, `%` (modulo)
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&` (and), `||` (or), `!` (not)
- Assignment: `=`

### 3. Primitive Types & Literals

**Primitive Types:**
- `int`: Integer (e.g., `10`, `-5`).
- `float`: Floating-point number (e.g., `3.14`).
- `string`: UTF-8 text (e.g., `"hello"`).
- `bool`: Boolean (`true` or `false`).

**Collection Literals:**
- List: `[1, 2, 3]`
- Map: `{"key1": "value1", "key2": "value2"}`
- Set: `#{1, 2, 3}`

**String Formatting:** F-strings are used for easy interpolation.
```flc
let name = "Alice";
let message = f"Hello, {name}!"; // "Hello, Alice!"
```

### 4. Modules and Visibility

Modules provide encapsulation and organization. Visibility is controlled with the `pub` keyword.

**Defining Modules:** A file implicitly defines a module. Directories can also define modules.

**Importing:** The `use` keyword brings items into scope.
```flc
// Import the entire module
use my_app::user_service;

// Import specific items
use my_app::models::{User, ApiError};

// Import with aliasing
use my_app::database as db;
```

**Visibility:**
- `def ...`: Default visibility. The definition is private to the current module.
- `def pub ...`: Public visibility. The definition can be imported and used by other modules.

### 5. Definitions & Declarations

All top-level definitions use the `def` keyword for consistency.

**Functions:**
```flc
def pub fn create_user(name: string) -> User { ... }
```

**Structs (Value Types):**
```flc
def pub struct User {
    id: Uuid, // Private field
    pub name: string, // Public field
}
```

**Enums:**
```flc
def pub enum RequestState { ... }
```

**Traits (Interfaces):**
```flc
def pub trait Serializable { ... }
```

### 6. Chaining, Lambdas, and Control Flow

The core of FLC is the fluent chain.

**Lambda Blocks & Destructuring:**
```flc
users
    .filter { |user| user.age > 18 }
    .map { |{name, email}| f"{name} <{email}>" }
```

**Control Flow as Chainable Expressions:**
```flc
let message = if (user.is_active) { "Active" } else { "Inactive" };
let status_text = response.match().case(200, "OK").get();
```

### 7. Traits & Polymorphism

Polymorphism is the ability to write code that operates on values of different types, achieved through traits.

**Static Polymorphism via Generics:** Zero-cost, compile-time polymorphism.
```flc
def fn log_as_json<T: Serializable>(item: T) {
    item.to_json() |> print()
}
```

**Dynamic Polymorphism via Trait Objects:** Runtime flexibility using `dyn<Trait>`.
```flc
let ui_elements: List<Box<dyn<Drawable>>> = [ ... ];
for element in ui_elements {
    element.draw(); // Call is dispatched at runtime
}
```

### 8. Concurrency

**Async/Await:**
```flc
def async fn fetch_user_data(id: Uuid) -> User {
    http.get(f"/users/{id}").await()
}
```

**Actor Model:**
```flc
def actor Counter {
    count: int = 0;
    def handle Inc(|amount: int|) { self.count += amount; }
}
```

### 9. Metaprogramming & Advanced Features

**Derive Attributes:**
```flc
def struct User { ... }.derive(Debug, Clone)
```

**FFI (Foreign Function Interface):**
```flc
extern "C" {
    def fn c_lib_function(input: i32) -> i32;
}
```

**Effect Systems:**
```flc
def effect Database { ... }
def fn get_user(id: Uuid).with(Database) { ... }
```

### 10. Complete Example: Console Application

This example demonstrates a simple console application that fetches user data from a mock API, showcasing many of the language's features working together.

```flc
// main.flc

// === Definitions ===
def pub struct User { pub id: int, pub name: string }
    .derive(Debug, Clone)

def pub enum ApiError { NotFound, Network(string) }
    .derive(Debug)

def pub trait ApiClient {
    def async fn fetch_user(self, id: int) -> Result<User, ApiError>;
}

def struct MockApiClient { db: HashMap<int, User> }

// === Implementations ===
def impl MockApiClient {
    def fn new() -> Self {
        let db = HashMap.new()
            .insert(1, User{id: 1, name: "Alice"})
            .insert(2, User{id: 2, name: "Bob"});
        Self{db: db}
    }
}

def impl ApiClient for MockApiClient {
    def async fn fetch_user(self, id: int) -> Result<User, ApiError> {
        tokio.sleep_ms(100).await();
        self.db.get(&id)
            .cloned()
            .ok_or(ApiError.NotFound)
    }
}

// === Main Application Logic ===
def async fn main() {
    MockApiClient.new()
        .let { |client|
            [1, 3, 2] // List of IDs to fetch
                .iter()
                .for_each_concurrent { |id|
                    client.fetch_user(id)
                        .await()
                        .match()
                            .case(Ok({name, ..}), |{name, ..}| {
                                f"Success! Found user: {name}" |> print()
                            })
                            .case(Err(ApiError.NotFound), || {
                                f"Error: User {id} not found." |> print()
                            })
                            .run()
                }
                .await()
        }
}
```
 
