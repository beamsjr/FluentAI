# FluentAI VM Changelog

All notable changes to the FluentAI VM will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- Fixed tail call optimization bug where the VM was incorrectly handling stack ordering during `TailCall` instruction execution. The VM now correctly pops the function first before popping arguments, matching the compiler's code generation and the regular `Call` instruction behavior.

### Added
- Comprehensive documentation for tail call optimization in `docs/tail_call_optimization.md`
- Example file demonstrating various tail-recursive algorithms in `examples/tail_recursion_examples.cl`
- Debug support for tail call execution with `FLUENTAI_DEBUG_TAILCALL` environment variable

### Changed
- Updated error handling test to accept "Stack overflow" error message variant

## [0.1.0] - Initial Release

### Added
- Stack-based virtual machine with bytecode execution
- Comprehensive instruction set including arithmetic, logic, control flow, and function operations
- Tail call optimization for recursive functions
- Garbage collection with concurrent GC support
- Security features including capability-based security and resource limits
- Module system with import/export support
- Effects system integration
- Debug mode with breakpoints and step-by-step execution
- Memory pooling and optimization features
- SIMD support for vector operations
- Comprehensive test suite with high coverage