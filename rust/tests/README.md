# FluentAi Integration Tests

This directory contains integration tests that verify the interaction between different components of the FluentAi system.

## Test Coverage

### Module System Tests

- **Module Loading**: Tests that modules can be loaded from disk
- **Import/Export**: Verifies import and export mechanisms work correctly
- **Qualified Names**: Tests module.variable syntax
- **Module Caching**: Verifies modules are properly cached
- **Circular Dependencies**: Tests detection of circular module dependencies
- **Search Paths**: Tests multiple module search directories

### Package Manager Tests

- **Manifest Parsing**: Tests reading and writing claude.json files
- **Dependency Resolution**: Verifies version constraint solving
- **Lock Files**: Tests reproducible dependency installation
- **Registry Operations**: Tests package publishing and retrieval

### VM Integration Tests

- **Module Execution**: Tests that modules can be executed in the VM
- **Effect Handling**: Tests modules that use effects
- **Cross-Module Calls**: Tests calling functions from imported modules

## Running Tests

Run all integration tests:

```bash
cargo test --test integration_test
```

Run specific test:

```bash
cargo test --test integration_test test_module_loading_and_execution
```

Run with output:

```bash
cargo test --test integration_test -- --nocapture
```

## Test Structure

Each test follows this pattern:

1. **Setup**: Create temporary directories and files
2. **Execute**: Run the test scenario
3. **Verify**: Check the results
4. **Cleanup**: Automatic via TempDir

Example:

```rust
#[test]
fn test_module_feature() {
    // Setup
    let temp_dir = TempDir::new().unwrap();
    fs::write(temp_dir.path().join("module.cl"), MODULE_CODE).unwrap();
    
    // Execute
    let result = load_and_run_module("module");
    
    // Verify
    assert_eq!(result, expected_value);
    
    // Cleanup happens automatically
}
```

## Test Scenarios

### Basic Module Loading

Tests that a simple module can be loaded and its exports accessed.

### Module Dependencies

Tests that modules can import and use other modules.

### Package Installation

Tests that packages can be installed from a registry.

### Error Handling

Tests that appropriate errors are returned for:
- Missing modules
- Missing exports
- Circular dependencies
- Version conflicts

## Adding New Tests

1. Add test function to `integration_test.rs`
2. Use descriptive test names
3. Include comments explaining what is being tested
4. Clean up any created resources
5. Verify both success and failure cases

## Debugging Tests

Use environment variables for debugging:

```bash
# Enable debug logging
RUST_LOG=debug cargo test --test integration_test

# Keep temporary files
RUST_TEST_NOCAPTURE=1 cargo test --test integration_test
```

## Performance Tests

Some tests verify performance characteristics:

- Module caching reduces load time
- Dependency resolution completes in reasonable time
- Large module graphs can be handled

## Known Issues

- Some tests may fail on Windows due to path handling
- Network tests require internet connection
- Registry tests may be slow due to HTTP operations