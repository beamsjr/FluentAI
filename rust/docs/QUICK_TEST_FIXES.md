# Quick Test Fixes - Actionable Items

This document provides immediate actions to enable more tests in the FluentAI test suite.

## 1. Simple Function Additions (< 30 min)

### Add to `fluentai-stdlib/src/math.rs`:

```rust
// Integer division - returns quotient only
fn quotient(args: &[Value]) -> Result<Value> {
    ensure_arity("quotient", args, 2, 2)?;
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(anyhow!("Division by zero"))
            } else {
                Ok(Value::Int(a / b))
            }
        }
        _ => Err(anyhow!("quotient expects two integers"))
    }
}

// Integer remainder
fn remainder(args: &[Value]) -> Result<Value> {
    ensure_arity("remainder", args, 2, 2)?;
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            if *b == 0 {
                Err(anyhow!("Division by zero"))
            } else {
                Ok(Value::Int(a % b))
            }
        }
        _ => Err(anyhow!("remainder expects two integers"))
    }
}

// Don't forget to register them:
// registry.register("quotient", quotient, 2, Some(2), true, "Integer division quotient");
// registry.register("remainder", remainder, 2, Some(2), true, "Integer division remainder");
```

### Add to `fluentai-stdlib/src/core.rs`:

```rust
// List constructor function
fn list_fn(args: &[Value]) -> Result<Value> {
    Ok(Value::List(args.to_vec()))
}

// Register it:
// registry.register("list", list_fn, 0, None, true, "Create a list from arguments");
```

## 2. Enable Ignored Tests (< 10 min)

### In `fluentai-vm/tests/iot_demo_test.rs`:

Remove all `#[ignore = "Stdlib functions not properly loaded"]` attributes and run:
```bash
cargo test -p fluentai-vm iot_demo_test -- --nocapture
```

Expected results:
- Tests might actually pass now that stdlib integration works
- If they fail, the error will show exactly which function is missing

### In `fluentai-stdlib/tests/math_tests.rs`:

Uncomment these tests after adding quotient/remainder:
```rust
// Lines ~183-202
#[test]
fn test_quotient_remainder() {
    let stdlib = init_stdlib();
    
    let quotient = stdlib.get("quotient").unwrap();
    assert_eq!(
        quotient.call(&[Value::Int(17), Value::Int(5)]).unwrap(),
        Value::Int(3)
    );
    
    let remainder = stdlib.get("remainder").unwrap();
    assert_eq!(
        remainder.call(&[Value::Int(17), Value::Int(5)]).unwrap(),
        Value::Int(2)
    );
}
```

## 3. String Test Verification (< 5 min)

### In `fluentai-stdlib/tests/string_comprehensive_tests.rs`:

Check if these tests actually work (the functions ARE implemented):
- Line 112: `test_string_repeat` - string-repeat exists in strings_extended.rs
- Line 136: `test_string_reverse` - string-reverse exists
- Line 227: `test_string_format` - string-format exists

Just remove the comments and run the tests!

## 4. Test What's Already Working

Run these to verify the analysis:
```bash
# Verify string functions work
cargo test -p fluentai-stdlib string_comprehensive

# Verify VM stdlib integration
cargo test -p fluentai-vm stdlib_integration

# Check if IoT tests work now
cargo test -p fluentai-vm iot_demo
```

## 5. Symbol Type (Requires More Work)

This is the biggest missing piece. It requires:
1. Add `Symbol(String)` variant to `fluentai_vm::bytecode::Value`
2. Update parser to recognize 'symbol syntax
3. Add symbol->string function
4. Update all match statements that handle Value

## Summary

**Can be done in < 1 hour:**
- Add quotient and remainder functions
- Add list constructor function  
- Enable and run IoT demo tests
- Uncomment string tests that should work

**Requires design decisions:**
- Symbol type implementation
- Parser validation for special forms

**Already working (just needs verification):**
- VM-stdlib integration
- String manipulation functions
- Higher-order functions (map, filter, fold)