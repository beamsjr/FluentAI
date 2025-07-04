# Python Interface for FluentAi

This directory contains the Python interface layer for FluentAi (formerly ClaudeLang), which provides access to the Rust implementation through PyO3 bindings.

## Directory Structure

```
python/
├── fluentai/          # Python package (currently empty, for future Python modules)
├── scripts/           # Command-line interface scripts
│   ├── run_claudelang.py  # Main CLI runner for executing FluentAi programs
│   └── run_example.py     # Simple example demonstrating basic FluentAi features
└── tests/             # Test files
    ├── run_tests.py           # Main test runner
    ├── test_rust_bindings.py  # Tests for Rust-Python integration
    ├── test_ui_parser.py      # UI component parsing tests
    ├── test_ui_compile.py     # UI component compilation tests
    └── compile_counter.py     # Example UI compilation script
```

## How It Works

FluentAi uses a hybrid Python-Rust architecture:

1. **Core Implementation**: The language parser, compiler, and runtime are implemented in Rust for performance
2. **Python Bindings**: PyO3 creates Python bindings for the Rust code, making it accessible from Python
3. **Python Interface**: These Python scripts provide the user-facing CLI and testing infrastructure

## Key Files

### Scripts

- **`run_claudelang.py`**: The main entry point for running FluentAi programs
  - Handles command-line arguments
  - Supports file execution, REPL mode, and testing
  - Calls into the Rust implementation via `claudelang_rust` module

- **`run_example.py`**: A simple example script showing basic FluentAi usage
  - Demonstrates parsing and executing FluentAi code
  - Good starting point for understanding the API

### Tests

- **`run_tests.py`**: Orchestrates all test suites
  - Runs Rust binding tests
  - Executes UI component tests (if applicable)
  - Provides unified test reporting

- **`test_rust_bindings.py`**: Verifies the Rust-Python bridge is working
  - Tests basic parsing functionality
  - Ensures PyO3 bindings are correctly installed

- **`test_ui_parser.py`** & **`test_ui_compile.py`**: UI component functionality
  - Tests parsing of UI component syntax
  - Validates JavaScript compilation output
  - (May be deprecated depending on current FluentAi features)

## Setup and Usage

1. **Build the Rust extensions** (from project root):
   ```bash
   pip install setuptools-rust
   python setup_rust.py build_ext --inplace
   ```

2. **Run FluentAi programs**:
   ```bash
   python python/scripts/run_claudelang.py examples/hello.fl
   ```

3. **Run tests**:
   ```bash
   python python/tests/run_tests.py
   ```

## Dependencies

- Python 3.9+
- setuptools-rust (for building)
- PyO3 (handled by Rust build)

## Note on Naming

These files still reference "ClaudeLang" in various places. They need to be updated to use "FluentAi" as part of the ongoing renaming effort.