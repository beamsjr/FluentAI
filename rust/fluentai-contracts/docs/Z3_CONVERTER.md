# Z3 Converter Documentation

The Z3 converter translates FluentAI contract expressions into Z3 SMT formulas for static verification.

## Supported Operators

### Arithmetic Operations

| Operator | Description | Z3 Mapping | Example |
|----------|-------------|------------|---------|
| `+` | Addition | Integer/Real addition | `(+ x y)` |
| `-` | Subtraction | Integer/Real subtraction | `(- x y)` |
| `*` | Multiplication | Integer/Real multiplication | `(* x y)` |
| `/` | Division | Integer/Real division | `(/ x y)` |
| `mod`, `modulo`, `%` | Modulo | Integer remainder | `(mod x y)` |
| `abs` | Absolute value | Conditional expression | `(abs x)` |
| `min` | Minimum | Conditional expression | `(min x y)` |
| `max` | Maximum | Conditional expression | `(max x y)` |

### Comparison Operations

| Operator | Description | Z3 Mapping | Example |
|----------|-------------|------------|---------|
| `=`, `==`, `eq?` | Equality | Equality | `(= x y)` |
| `!=`, `<>`, `not=` | Inequality | Negated equality | `(!= x y)` |
| `<` | Less than | Less than | `(< x y)` |
| `<=` | Less than or equal | Less than or equal | `(<= x y)` |
| `>` | Greater than | Greater than | `(> x y)` |
| `>=` | Greater than or equal | Greater than or equal | `(>= x y)` |

### Logical Operations

| Operator | Description | Z3 Mapping | Example |
|----------|-------------|------------|---------|
| `and` | Logical AND | Boolean conjunction | `(and p q)` |
| `or` | Logical OR | Boolean disjunction | `(or p q)` |
| `not` | Logical NOT | Boolean negation | `(not p)` |
| `xor` | Exclusive OR | Boolean XOR | `(xor p q)` |
| `implies`, `=>` | Implication | Boolean implication | `(implies p q)` |

### Predicates

| Predicate | Description | Z3 Mapping | Example |
|-----------|-------------|------------|---------|
| `zero?` | Test if zero | `(= x 0)` | `(zero? x)` |
| `positive?` | Test if positive | `(> x 0)` | `(positive? x)` |
| `negative?` | Test if negative | `(< x 0)` | `(negative? x)` |
| `even?` | Test if even | `(= (mod x 2) 0)` | `(even? x)` |
| `odd?` | Test if odd | `(not (= (mod x 2) 0))` | `(odd? x)` |

### List/Array Operations (Simplified)

| Operator | Description | Current Implementation | Future Enhancement |
|----------|-------------|----------------------|-------------------|
| `length`, `list-length` | List length | Symbolic integer | Array theory |
| `nth`, `list-ref` | Element access | Symbolic value | Array select |
| `member?`, `member` | Membership test | Symbolic boolean | Quantifiers |
| `null?`, `empty?` | Empty test | Length = 0 | Direct check |

### Contract-Specific Functions

| Function | Description | Implementation Status |
|----------|-------------|--------------------|
| `old` | Pre-state value | Placeholder (needs state tracking) |

## Usage Example

```rust
use z3::{Config, Context};
use fluentai_contracts::z3_converter::{Z3Converter, Z3Sort, Z3Expr};

// Create Z3 context
let config = Config::new();
let context = Context::new(&config);

// Create converter with AST graph
let mut converter = Z3Converter::new(&context, &graph);

// Declare variables
converter.declare_var("x", Z3Sort::Int);
converter.declare_var("y", Z3Sort::Int);

// Convert contract expression
match converter.convert_node(contract_expr) {
    Ok(Z3Expr::Bool(formula)) => {
        // Use formula in solver
    }
    _ => // Handle error
}
```

## Supported Z3 Sorts

- `Bool`: Boolean values
- `Int`: Integer values
- `Real`: Real numbers
- `String`: String values (basic support)
- Arrays: Planned for future implementation

## Implementation Details

### Arithmetic Operations

- Division follows Z3's integer division semantics
- Modulo uses Z3's `rem` operation
- `abs(x)` is implemented as `ite(x >= 0, x, -x)`
- `min(x,y)` is implemented as `ite(x <= y, x, y)`
- `max(x,y)` is implemented as `ite(x >= y, x, y)`

### Type Requirements

Most operations require specific types:
- Arithmetic operations: Integer or Real arguments
- Comparison operations: Matching numeric types
- Logical operations: Boolean arguments
- Predicates: Integer arguments (for numeric predicates)

### Error Handling

The converter returns `ContractError::InvalidExpression` for:
- Unknown operators
- Type mismatches
- Wrong number of arguments
- Unsupported node types

## Future Enhancements

1. **Full Array Theory Support**
   - Proper array sorts
   - Array select/store operations
   - Array equality

2. **Quantifier Support**
   - Universal quantification (`forall`)
   - Existential quantification (`exists`)
   - Bounded quantifiers

3. **String Theory**
   - String operations
   - Regular expressions
   - String constraints

4. **State Tracking**
   - Proper `old()` implementation
   - Ghost variables
   - Frame conditions

5. **Advanced Numeric Operations**
   - Bit-vector operations
   - Non-linear arithmetic
   - Floating-point reasoning

## Performance Considerations

- The converter creates fresh Z3 expressions for each conversion
- Variable declarations are cached in a HashMap
- Complex expressions may create large Z3 terms
- Consider using incremental solving for multiple queries

## Debugging Tips

1. Enable Z3 logging to see generated formulas
2. Use `Z3Expr::as_bool()` or `as_int()` to extract typed expressions
3. Check solver satisfiability incrementally
4. Use `get_model()` to understand counterexamples