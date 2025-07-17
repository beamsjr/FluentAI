# Double Negation Parsing Issue

## Problem
The expression `-(-5)` is not parsed correctly as double negation. Instead of producing `neg(neg(5))`, it produces `neg(-5)`.

## Root Cause
The issue is in the lexer (`fluentai-parser/src/flc_lexer.rs`), not the parser. The lexer's regex patterns for Integer and Float literals include an optional minus sign:

```rust
#[regex(r"-?[0-9]+", priority = 5, callback = |lex| lex.slice().parse::<i64>().ok())]
Integer(i64),

#[regex(r"-?[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 5, callback = |lex| lex.slice().parse::<f64>().ok())]
Float(f64),
```

This means that `-5` is tokenized as a single `Integer(-5)` token rather than as two tokens: `Minus` followed by `Integer(5)`.

## Why This Causes Problems
When parsing `-(-5)`:
1. The outer `-` is correctly parsed as a unary minus operator
2. Inside the parentheses, `-5` is lexed as `Integer(-5)` 
3. The result is `neg(Integer(-5))` instead of `neg(neg(Integer(5)))`

## Solution
The lexer should not include the minus sign in number literals. The minus should always be a separate token that the parser handles as a unary operator. The regex patterns should be changed to:

```rust
#[regex(r"[0-9]+", priority = 5, callback = |lex| lex.slice().parse::<i64>().ok())]
Integer(i64),

#[regex(r"[0-9]+\.[0-9]+([eE][+-]?[0-9]+)?", priority = 5, callback = |lex| lex.slice().parse::<f64>().ok())]
Float(f64),
```

This way:
- `-5` would be lexed as `[Minus, Integer(5)]`
- The parser's unary expression handling would correctly create `neg(5)`
- Double negation `-(-5)` would work as expected

## Test Case
A test was added in `/Users/joe/repos/claudelang/rust/fluentai-parser/tests/test_double_negation.rs` that confirms this issue. The test shows that `-(-5)` is currently parsed as:

```
Application:
  Variable: neg
  Literal: Integer(-5)
```

Instead of the expected:

```
Application:
  Variable: neg
  Application:
    Variable: neg
    Literal: Integer(5)
```

## Additional Consideration
The existing test case `"(-2) ** 2"` (in the exponentiation tests) currently passes because it expects the result to be `(-2)^2 = 4`. However, this might need to be reconsidered after fixing the lexer, as `(-2)` would then be parsed as `neg(2)` rather than `Integer(-2)`.