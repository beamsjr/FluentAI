# FluentAI Examples

This directory contains example programs demonstrating FluentAI language features and the new Continuum UI framework.

## 🚀 Running Examples

### Prerequisites
1. Build the FluentAI CLI:
   ```bash
   cargo build --release -p fluentai-cli
   ```

2. The CLI binary will be at: `./target/release/fluentai`

### Running FluentAI Examples

#### Currently Runnable Examples:

1. **runnable_demo.flc** - Demonstrates current FluentAI features
   ```bash
   ./target/release/fluentai run examples/runnable_demo.flc
   ```
   
   Or use the helper script:
   ```bash
   ./run_example.sh runnable_demo
   ```

   This example shows:
   - Object-oriented programming with closures
   - Functional programming and method chaining
   - Pattern matching
   - Error handling
   - Collections and transformations
   - String formatting (f-strings)

2. **Legacy .ai examples** - Original syntax examples
   ```bash
   cargo run -p fluentai-cli -- run examples/hello.ai
   cargo run -p fluentai-cli -- run examples/arithmetic.ai
   cargo run -p fluentai-cli -- run examples/lists.ai
   ```

#### Continuum UI Examples (Not Yet Runnable):

These examples demonstrate the Continuum UI syntax but cannot run until Phase 3 (compiler lowering) is implemented:

1. **continuum_showcase.flc** - Full task management application with:
   - Multiple views (Dashboard, Tasks, Analytics)
   - Theme switching (light/dark)
   - 3D analytics visualization
   - Reactive state management

2. **continuum_simple_demo.flc** - Simple counter with:
   - Basic state reactivity
   - Conditional rendering
   - 2D and 3D UI elements

3. **continuum_compilation_demo.flc** - Shows how Continuum compiles to FluentAI:
   - Declarative → Imperative transformation
   - State management implementation
   - Dom effect integration

## 📝 FluentAI Syntax (FLC)

### Basic Program Structure
```flc
// Define functions
private function greet(name: string) {
    $(f"Hello, {name}!").print();
}

// Main entry point
private function main() {
    greet("World");
}

// Run the program
main()
```

### Object-Oriented Programming
```flc
public function Counter(initial: int) {
    let state = {"count": initial};
    
    let self = {
        "get": () => state.count,
        "increment": () => {
            state.count = state.count + 1;
            self
        }
    };
    
    return self;
}
```

### Pattern Matching
```flc
match value {
    0 => "zero",
    1..10 => "small",
    n if n > 10 => "large",
    _ => "other"
}
```

## 🎨 Continuum UI Syntax (Future)

### Reactive State
```flc
public state_field count: int = 0
public state_field theme: string = "light"
```

### Declarative UI
```flc
public surface app {
    background: theme == "light" ? "#fff" : "#000",
    
    element button {
        content: "Click me",
        on_click: disturb count(count + 1)
    }
    
    element display {
        content: f"Count: {count}",
        
        when count > 0 {
            color: "#28a745"
        }
        when count < 0 {
            color: "#dc3545"
        }
    }
}
```

### 3D UI
```flc
public space visualization {
    element data_cube {
        type: cube,
        size: 2.0,
        rotation: animated,
        color: data > threshold ? "#28a745" : "#dc3545"
    }
}
```

## 🛠️ Current Implementation Status

### ✅ Working Features:
- FLC syntax parsing
- Object-oriented patterns via closures
- Pattern matching
- Method chaining
- String interpolation (f-strings)
- Collections (lists, maps)
- Error handling (try/catch)
- Basic async/promises
- Lexer support for Continuum UI keywords
- AST nodes for UI constructs

### 🚧 In Progress:
- Continuum UI compiler (Phase 3)
- Dom effect runtime
- Full async/await support
- Module imports/exports

### ❌ Not Yet Implemented:
- Continuum UI rendering
- Reactive state runtime
- 3D rendering backend
- Hot reloading

## 🔍 Debugging

Add the `--debug` flag for verbose output:
```bash
./target/release/fluentai run examples/runnable_demo.flc --debug
```

## 📚 Documentation

- **continuum_features_showcase.md** - Complete overview of Continuum UI features
- **Legacy README content below** - Information about .ai file examples

---

## Legacy .ai Examples

The following examples use the original Lisp-like syntax (`.ai` files):

### Working Examples:
- `hello.ai` - Simple arithmetic
- `arithmetic.ai` - Basic operations
- `let_binding.ai` - Variable bindings
- `lambda.ai` - Function definitions
- `lists.ai` - List operations
- `match_example.ai` - Pattern matching
- `effects.ai` - Effect system demo

### Run legacy examples:
```bash
cargo run -p fluentai-cli -- run examples/hello.ai
```

## Contributing

When adding new examples:
1. Use `.flc` extension for new FLC syntax
2. Use `.ai` extension for legacy Lisp syntax
3. Include header comments explaining the example
4. Update this README
5. Test that the example runs (if applicable)