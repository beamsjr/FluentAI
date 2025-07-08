# FluentAi Examples

Welcome to the FluentAi examples directory! This contains demonstrations and tutorials to help you learn and use FluentAi.

## Getting Started

If you're new to FluentAi, start with these examples in order:

1. **Basic Language Features**
   - `hello.ai` - Simple hello world
   - `fibonacci.ai` - Basic recursion
   - `list_operations.ai` - Working with lists

2. **Pattern Matching**
   - `simple_pattern_test.ai` - Basic pattern matching
   - `pattern_matching_demo.ai` - Advanced patterns
   - `minimal_pattern_test.ai` - Pattern matching edge cases

3. **Module System**
   - `simple_module_test.ai` - Basic module usage
   - `module_demo.ai` - Advanced module features

## Advanced Features

### Effects System
- `effects_demo.ai` - Introduction to effects
- `advanced_effects.ai` - Complex effect handling
- `logger_demo.ai` - Practical effect example

### Contracts & Verification
- `contracts_demo.ai` - Basic contracts
- `contract_verification_demo.ai` - Contract verification
- `contract_proof_demo.ai` - Mathematical proofs

### Async Programming
- `async_await_demo.ai` - Async/await basics
- `concurrency_demo.ai` - Concurrent programming

### AI-First Features
- `ai_first_demo.ai` - AI integration features
- `ml_optimization_demo.ai` - ML-guided optimization
- `network_effects_demo.ai` - Network effect analysis

## Complete Applications

### ClaudeScope
A comprehensive network security analyzer demonstrating real-world FluentAi usage:
- `claudescope/` - Full application with documentation

### Web Applications
- `counter_app.ai` - Simple counter application
- `todo_app_components.ai` - Todo list with components
- Various `counter_*.html` files - Different UI implementations

## Running Examples

```bash
# Run a simple example
python python/scripts/run_fluentai.py examples/hello.ai

# Run with the REPL
python python/scripts/run_fluentai.py --repl

# Run a specific example
python python/scripts/run_fluentai.py examples/fibonacci.ai
```

## UI Components

FluentAi includes experimental UI component support:
- `ui_demo.ai` - UI component basics
- `simple_button.ai` - Button component
- `component_demo.ai` - Component composition
- HTML files demonstrate compiled output

## Note for Developers

If you're looking for integration tests for the Rust implementation, see `/rust/examples/README.md`.