# FluentAI Parser

High-performance S-expression parser for FluentAI with support for deeply nested structures.

## Features

- **Zero-copy parsing** where possible
- **Arena allocation** for AST nodes
- **Optimized lexer** using logos
- **Minimal allocations**
- **Depth tracking** to prevent stack overflow
- **Iterative parsing** for extremely deep nesting
- **Threaded parsing** with configurable stack size

## Parser Types

### Standard Recursive Parser

The default parser uses recursive descent and is suitable for most use cases:

```rust
use fluentai_parser::parse;

let ast = parse("(+ 1 2 3)")?;
```

Features:
- Fast and efficient for typical code
- Supports all language features
- Has configurable depth limit (default: 1000)

### Iterative Parser

For extremely deeply nested structures, use the iterative parser:

```rust
use fluentai_parser::parse_iterative;

// This would overflow the stack with recursive parsing
let deeply_nested = generate_deeply_nested_expr(10000);
let ast = parse_iterative(&deeply_nested)?;
```

Features:
- Uses explicit stack on heap instead of call stack
- Can handle arbitrarily deep nesting (limited by memory)
- Slightly slower for normal code
- Currently limited to basic expressions (no special forms)

### Threaded Parser

For environments with specific stack requirements:

```rust
use fluentai_parser::{parse_with_stack_size, ThreadedParserConfig};

// Parse with custom stack size (e.g., for deeply recursive code)
let ast = parse_with_stack_size(source, 16 * 1024 * 1024)?; // 16MB stack

// Full configuration
let config = ThreadedParserConfig::default()
    .with_stack_size(8 * 1024 * 1024)
    .with_max_depth(5000)
    .with_thread_name("my-parser".to_string());
let ast = parse_threaded(source.to_string(), config)?;
```

Features:
- Runs parser in separate thread with custom stack size
- Useful for environments with limited main thread stack
- Supports all language features
- Slight overhead from thread creation

## When to Use Which Parser

### Use the Standard Parser when:
- Parsing normal code with reasonable nesting (< 1000 levels)
- You need full language support including special forms
- Performance is critical for typical code
- Running in normal environments

### Use the Iterative Parser when:
- Parsing machine-generated code with extreme nesting
- Stack size is absolutely minimal
- You only need basic expression parsing
- Testing parser robustness

### Use the Threaded Parser when:
- Need deeper recursion than default stack allows
- Running in environment with limited thread stack
- Need full language support with deep nesting
- Can afford slight thread creation overhead

## Depth Limits

Both parsers support configurable depth limits:

```rust
// Recursive parser with custom limit
let ast = parse_with_depth_limit(source, 500)?;

// Iterative parser with custom limit
let ast = parse_iterative_with_depth(source, 20000)?;
```

## Performance

For typical code, the recursive parser is faster due to lower overhead.
For deeply nested code, the iterative parser is more robust.

Example benchmarks (200 levels deep):
- Recursive parser: ~900µs
- Iterative parser: ~1ms

## Thread Safety

The iterative parser's stack usage is predictable and doesn't depend on the system's thread stack size, making it suitable for use in threads with limited stack space.