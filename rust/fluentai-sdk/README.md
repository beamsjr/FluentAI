# FluentAI SDK

High-level SDK for embedding FluentAI into Rust applications.

## Features

- **Simple API**: Easy-to-use interface for evaluating FluentAI code
- **Session Management**: Stateful sessions with globals and module loading
- **Host Functions**: Register custom Rust functions callable from FluentAI
- **Value Conversion**: Seamless conversion between Rust and FluentAI values
- **Security**: Sandboxed execution with configurable permissions
- **Performance**: JIT compilation support for hot code paths

## Quick Start

```rust
use fluentai_sdk::prelude::*;

fn main() -> Result<()> {
    // Initialize the SDK
    fluentai_sdk::init()?;
    
    // Simple evaluation
    let result = eval("(+ 1 2 3)")?;
    println!("Result: {:?}", result);
    
    // Create a session
    let mut session = new_session()?;
    session.set_global("x", Value::Number(42.0));
    let result = session.eval("(* x 2)")?;
    println!("Result: {:?}", result);
    
    Ok(())
}
```

## Creating Sessions

### Basic Session

```rust
let mut session = Session::new(SessionOptions::default())?;
```

### Development Session

```rust
let mut session = Session::new(SessionOptions::development())?;
```

### Production Session

```rust
let mut session = Session::new(SessionOptions::production())?;
```

### Sandboxed Session

```rust
let mut session = Session::new(SessionOptions::sandboxed())?;
```

### Custom Configuration

```rust
let mut session = FluentAIBuilder::new()
    .debug(true)
    .timeout(5000)  // 5 seconds
    .memory_limit(16 * 1024 * 1024)  // 16MB
    .add_module_path("./modules")
    .build()?;
```

## Host Functions

Register custom Rust functions that can be called from FluentAI:

```rust
let sqrt = HostFunction::new("sqrt", 1, |args| {
    match &args[0] {
        Value::Number(n) => Ok(Value::Number(n.sqrt())),
        _ => Err(RuntimeError::host("sqrt expects a number")),
    }
});

let mut session = FluentAIBuilder::new()
    .add_function(sqrt)
    .build()?;

let result = session.eval("(sqrt 16)")?;  // Returns 4.0
```

## Value Conversion

Convert between Rust and FluentAI values:

```rust
// Rust to FluentAI
session.set_global("numbers", vec![1, 2, 3].into_value()?);

// FluentAI to Rust
let result = session.eval("(+ 1 2)")?;
let sum: f64 = result.from_value()?;
```

## Scripts

Load and execute FluentAI scripts:

```rust
// From file
let result = session.run_file("script.fl")?;

// From string
let script = Script::new("my-script", "(define x 42) x");
let result = session.run_script(&script)?;
```

## Security

Configure security settings for sandboxed execution:

```rust
let session = FluentAIBuilder::sandboxed()
    .timeout(1000)  // 1 second timeout
    .allow_fs(false)
    .allow_network(false)
    .allow_process(false)
    .build()?;
```

## Examples

See the `examples/` directory for more detailed examples:

- `sdk_demo.rs` - Comprehensive SDK demonstration
- More examples coming soon...

## License

MIT