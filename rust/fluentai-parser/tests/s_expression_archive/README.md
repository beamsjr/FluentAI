# S-Expression Test Archive

This directory contains the original s-expression parser tests that were archived when FluentAI transitioned from s-expression syntax to FLC (Fluent Lambda Chain) syntax.

## Why These Tests Are Archived

1. **Syntax Migration**: FluentAI has migrated from s-expression syntax to FLC syntax
2. **Parser Removal**: The s-expression parser has been completely removed from the codebase
3. **Reference Value**: These tests document features that were supported in s-expressions and may need to be implemented in FLC

## Test Categories

- **async_parsing_tests.rs**: Async/await/spawn functionality
- **begin_parsing_tests.rs**: Multiple top-level expressions
- **channel_operations_tests.rs**: Concurrent channel operations
- **complex_pattern_parsing_tests.rs**: Advanced pattern matching (or, as, guard, range, view patterns)
- **contract_parsing_tests.rs**: Function contracts (requires/ensures/invariant)
- **error_tests.rs**: Comprehensive error handling and reporting
- **handler_parsing_tests.rs**: Effect handler syntax
- **iot_demo_validation.rs** & **iot_syntax_test.rs**: IoT-specific constructs
- **module_system_tests.rs**: Module imports/exports
- **qualified_symbol_tests.rs**: Module-qualified symbols

## Migration Status

See `/Users/joe/repos/claudelang/rust/fluentai-parser/FLC_PARSER_TODO.md` for the current implementation status of these features in the FLC parser.

## Note

These tests should not be run as part of the normal test suite since they use s-expression syntax which is no longer supported.