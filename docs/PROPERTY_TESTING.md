# Property-Based Testing for ClaudeLang

This directory contains property-based tests using the Hypothesis library. Property-based testing helps ensure that ClaudeLang behaves correctly for a wide range of inputs by automatically generating test cases.

## Overview

Property-based tests verify invariants and properties that should hold for all valid inputs, rather than testing specific examples. This helps catch edge cases that might be missed by traditional example-based tests.

## Test Categories

### 1. Parser Properties (`TestParserProperties`)
- **Parse Safety**: Parser should not crash on any valid expression
- **Literal Roundtrip**: Parsing and evaluating literals preserves their values
- **String Escaping**: String literals handle escape sequences correctly

### 2. Interpreter Properties (`TestInterpreterProperties`)
- **Mathematical Properties**: Addition is commutative and associative
- **Identity Function**: `(lambda (x) x)` returns its input unchanged
- **Control Flow**: If expressions correctly branch based on conditions
- **List Operations**: List construction and destructuring work correctly

### 3. Effect Properties (`TestEffectProperties`)
- **Determinism**: Pure functions always return the same result
- **Effect Isolation**: Effects don't leak between evaluations
- **IO Effects**: Basic IO operations return expected values

### 4. Pattern Matching Properties (`TestPatternMatchingProperties`)
- **Variable Binding**: Pattern variables bind to matched values
- **List Patterns**: List destructuring works correctly
- **Wildcard Matching**: Wildcard pattern `_` matches any value

### 5. ADT Properties (`TestADTProperties`)
- **Constructor Preservation**: ADT constructors preserve their arguments
- **Pattern Extraction**: Pattern matching extracts values correctly
- **Exhaustive Matching**: All constructors can be matched

### 6. Type Annotation Properties (`TestTypeAnnotationProperties`)
- **Value Preservation**: Type ascription doesn't change values
- **Crash Safety**: Type ascription handles any expression gracefully

## Running the Tests

### Install Dependencies
```bash
pip install hypothesis
```

### Run All Property Tests
```bash
python -m unittest tests.test_properties -v
```

### Run with Different Profiles
```bash
# Quick testing during development
HYPOTHESIS_PROFILE=dev python -m unittest tests.test_properties

# Thorough testing for CI
HYPOTHESIS_PROFILE=ci python -m unittest tests.test_properties

# Debugging failures
HYPOTHESIS_PROFILE=debug python -m unittest tests.test_properties
```

## Hypothesis Profiles

- **debug**: 5 examples, no deadline (for debugging)
- **dev**: 10 examples, 2s deadline (for development)
- **ci**: 50 examples, 5s deadline (for continuous integration)

## Writing New Property Tests

1. Import necessary modules:
```python
from hypothesis import given, strategies as st
from hypothesis.strategies import composite
```

2. Define custom strategies for generating test data:
```python
@composite
def my_strategy(draw):
    # Generate test data
    return draw(st.integers())
```

3. Write property tests:
```python
@given(st.integers())
def test_my_property(self, value):
    # Test that property holds
    result = self.interpreter.eval(f"(my-function {value})")
    self.assertEqual(result.data, expected_value)
```

## Benefits

1. **Edge Case Discovery**: Automatically finds inputs that break assumptions
2. **Regression Prevention**: Properties ensure behavior remains consistent
3. **Documentation**: Properties serve as executable specifications
4. **Confidence**: Thousands of test cases generated automatically

## Known Limitations

Some tests may fail due to:
- Parser limitations with certain string escape sequences
- Floating point precision in extreme cases
- Syntax variations not yet implemented

These are tracked and will be addressed in future updates.