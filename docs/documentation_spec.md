# FluentAI Documentation Specification

This document defines the documentation format for FluentAI, designed to be machine-readable first while maintaining human accessibility.

## Documentation Structure

All FluentAI documentation follows a structured JSON format with human-readable projections.

### Function Documentation Format

```json
{
  "type": "function",
  "name": "function_name",
  "signature": {
    "inputs": [
      {"name": "param1", "type": "Type1", "description": "Parameter description"}
    ],
    "output": {"type": "ReturnType", "description": "Return value description"},
    "effects": ["PURE", "IO", "STATE"],
    "temporal_constraints": "optional timing constraints"
  },
  "semantics": {
    "preconditions": ["condition1", "condition2"],
    "postconditions": ["condition1", "condition2"],
    "invariants": ["invariant1"],
    "formal_spec": "optional formal specification"
  },
  "examples": [
    {
      "description": "Example description",
      "input": {"param1": "value1"},
      "output": "expected_output",
      "effects_produced": [],
      "execution_trace": []
    }
  ],
  "performance": {
    "time_complexity": "O(n)",
    "space_complexity": "O(1)",
    "parallelizable": true
  },
  "related": ["related_function1", "related_concept1"],
  "implementation_notes": "Optional implementation details"
}
```

### Type Documentation Format

```json
{
  "type": "type_definition",
  "name": "TypeName",
  "kind": "record|variant|alias",
  "definition": {
    "fields": [
      {"name": "field1", "type": "Type1", "required": true}
    ]
  },
  "constraints": ["constraint1", "constraint2"],
  "examples": [
    {
      "description": "Valid instance",
      "value": {"field1": "value1"},
      "valid": true
    }
  ],
  "operations": ["operation1", "operation2"],
  "laws": [
    {
      "name": "Associativity",
      "statement": "op(a, op(b, c)) = op(op(a, b), c)",
      "proof": "optional proof or reference"
    }
  ]
}
```

### Module Documentation Format

```json
{
  "type": "module",
  "name": "ModuleName",
  "purpose": "Module purpose description",
  "exports": [
    {"name": "export1", "kind": "function|type|value"}
  ],
  "dependencies": ["module1", "module2"],
  "effects_used": ["IO", "STATE"],
  "guarantees": [
    "All functions are total",
    "No runtime exceptions"
  ],
  "usage_examples": []
}
```

## Documentation Principles

### 1. Machine-Readable First

- All documentation stored as structured data (JSON)
- Consistent schema across all documentation types
- Formal specifications included where applicable
- Examples include expected outputs and traces

### 2. Executable Examples

Every example must be executable with verifiable outputs:

```json
{
  "example": {
    "code": "(+ 2 3)",
    "expected_output": "5",
    "expected_type": "Int",
    "expected_effects": ["PURE"],
    "test_id": "example_add_001"
  }
}
```

### 3. Progressive Disclosure

Documentation organized in layers:

1. **Summary**: One-line description
2. **Core**: Essential information for basic usage
3. **Details**: Complete specification
4. **Advanced**: Implementation details, optimizations

### 4. Cross-References

All related concepts linked:

```json
{
  "see_also": [
    {"type": "function", "name": "map", "reason": "Similar operation"},
    {"type": "concept", "name": "Functor", "reason": "Theoretical foundation"}
  ]
}
```

### 5. Formal Properties

Where applicable, include formal properties:

```json
{
  "properties": [
    {
      "name": "Commutativity",
      "formula": "f(a, b) = f(b, a)",
      "holds": true,
      "proof_reference": "theorem_123"
    }
  ]
}
```

## Documentation Generation

### From Code to Documentation

FluentAI code can generate documentation:

```clojure
(define-function add
  :type (-> Int Int Int)
  :effects #{:pure}
  :doc {:summary "Adds two integers"
        :examples [{:input [2 3] :output 5}]}
  [a b]
  (+ a b))
```

Generates:

```json
{
  "type": "function",
  "name": "add",
  "signature": {
    "inputs": [
      {"name": "a", "type": "Int"},
      {"name": "b", "type": "Int"}
    ],
    "output": {"type": "Int"},
    "effects": ["PURE"]
  },
  "summary": "Adds two integers",
  "examples": [
    {"input": {"a": 2, "b": 3}, "output": 5}
  ]
}
```

### Documentation Validation

All documentation must pass validation:

1. **Schema Validation**: Conforms to JSON schema
2. **Example Validation**: Examples execute correctly
3. **Type Validation**: Types exist and are correct
4. **Cross-Reference Validation**: All references resolve
5. **Completeness Validation**: Required fields present

## AI-Specific Features

### 1. Pattern Matching

Documentation includes patterns for AI to recognize:

```json
{
  "patterns": [
    {
      "name": "Error Handling",
      "template": "(handle-error <expr> <handler>)",
      "when_to_use": "When operation might fail",
      "example": "(handle-error (/ x y) (const 0))"
    }
  ]
}
```

### 2. Synthesis Hints

Hints for code synthesis:

```json
{
  "synthesis_hints": {
    "common_usage": ["Used for collection processing", "Often combined with filter"],
    "parameter_constraints": ["List must be non-empty"],
    "typical_context": ["Inside map operations", "Data transformation pipelines"]
  }
}
```

### 3. Learning Examples

Categorized examples for training:

```json
{
  "learning_examples": [
    {
      "category": "basic",
      "difficulty": 1,
      "concepts": ["arithmetic", "pure_functions"],
      "example": "(+ 1 2)"
    },
    {
      "category": "intermediate",
      "difficulty": 3,
      "concepts": ["higher_order", "composition"],
      "example": "(map (compose inc double) [1 2 3])"
    }
  ]
}
```

## Human Projection

While stored as JSON, documentation can be projected to readable formats:

### Markdown Projection

```markdown
# Function: add

**Type**: `Int -> Int -> Int`  
**Effects**: Pure

Adds two integers.

## Examples

```clojure
(add 2 3)  ; => 5
```

## See Also
- [subtract](#subtract) - Inverse operation
```

### Interactive Projection

Documentation can be queried interactively:

```
> doc add
add : Int -> Int -> Int [PURE]
Adds two integers

> doc add --examples
(add 2 3) => 5
(add -1 1) => 0

> doc add --formal
∀ a, b ∈ ℤ: add(a, b) = a + b
Commutative: add(a, b) = add(b, a)
Associative: add(a, add(b, c)) = add(add(a, b), c)
```

## Documentation as Code

Documentation is part of the language:

```clojure
(documentation add
  :tested true
  :verified true
  :last-updated "2024-01-20"
  :maintainer "system"
  :stability "stable")
```

This ensures documentation stays synchronized with code and can be programmatically updated and verified.