# FluentAi Examples

Welcome to the FluentAi examples directory! This contains demonstrations and tutorials to help you learn and use FluentAi.

## Getting Started

If you're new to FluentAi, start with these examples in order:

1. **Basic Language Features**
   - `hello.cl` - Simple hello world
   - `fibonacci.cl` - Basic recursion
   - `list_operations.cl` - Working with lists

2. **Pattern Matching**
   - `simple_pattern_test.cl` - Basic pattern matching
   - `pattern_matching_demo.cl` - Advanced patterns
   - `minimal_pattern_test.cl` - Pattern matching edge cases

3. **Module System**
   - `simple_module_test.cl` - Basic module usage
   - `module_demo.cl` - Advanced module features

## Advanced Features

### Effects System
- `effects_demo.cl` - Introduction to effects
- `advanced_effects.cl` - Complex effect handling
- `logger_demo.cl` - Practical effect example

### Contracts & Verification
- `contracts_demo.cl` - Basic contracts
- `contract_verification_demo.cl` - Contract verification
- `contract_proof_demo.cl` - Mathematical proofs

### Async Programming
- `async_await_demo.cl` - Async/await basics
- `concurrency_demo.cl` - Concurrent programming

### AI-First Features
- `ai_first_demo.cl` - AI integration features
- `ml_optimization_demo.cl` - ML-guided optimization
- `network_effects_demo.cl` - Network effect analysis

## Complete Applications

### ClaudeScope
A comprehensive network security analyzer demonstrating real-world FluentAi usage:
- `claudescope/` - Full application with documentation

### Web Applications
- `counter_app.cl` - Simple counter application
- `todo_app_components.cl` - Todo list with components
- Various `counter_*.html` files - Different UI implementations

## Running Examples

```bash
# Run a simple example
python python/scripts/run_claudelang.py examples/hello.cl

# Run with the REPL
python python/scripts/run_claudelang.py --repl

# Run a specific example
python python/scripts/run_claudelang.py examples/fibonacci.cl
```

## UI Components

FluentAi includes experimental UI component support:
- `ui_demo.cl` - UI component basics
- `simple_button.cl` - Button component
- `component_demo.cl` - Component composition
- HTML files demonstrate compiled output

## Note for Developers

If you're looking for integration tests for the Rust implementation, see `/rust/examples/README.md`.