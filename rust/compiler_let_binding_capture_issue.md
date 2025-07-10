# Compiler Issue: Let Bindings and Variable Capture in Closures

## Summary

The FluentAI compiler is not correctly handling variable capture for closures that reference let-bound variables. This causes spawned tasks to fail when they try to access variables from their enclosing scope.

## Problem Description

When a lambda expression references a variable bound by a `let` expression, the compiler should:
1. Recognize the variable as a free variable that needs to be captured
2. Generate a closure (using `MakeClosure`) instead of a simple function (using `MakeFunc`)
3. Properly compile the let binding to create and store the variable

Currently, the compiler is:
- Generating `MakeFunc` for lambdas that reference outer variables (should be `MakeClosure`)
- Treating let-bound variables as globals instead of locals
- Not generating the proper bytecode for let binding initialization

## Reproduction

The issue can be reproduced with the following test case from `fluentai-vm/tests/spawn_integration_test.rs`:

```rust
#[test]
#[ignore = "Compiler issue: let bindings not generating proper code for channel creation and variable capture"]
fn test_spawn_with_channel() {
    // Create a graph for: (let ((ch (chan))) (spawn (lambda () (send! ch 42))) (receive! ch))
    let mut graph = Graph::new();

    // Create channel: (chan)
    let channel = graph.add_node(Node::Channel).unwrap();

    // Create lambda: (lambda () (send! ch 42))
    let ch_var = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let value = graph.add_node(Node::Literal(Literal::Integer(42))).unwrap();
    let send = graph.add_node(Node::Send { channel: ch_var, value }).unwrap();
    let lambda = graph.add_node(Node::Lambda {
        params: vec![],
        body: send,
    }).unwrap();

    // Create spawn: (spawn lambda)
    let spawn = graph.add_node(Node::Spawn { expr: lambda }).unwrap();

    // Create receive: (receive! ch)
    let ch_var2 = graph.add_node(Node::Variable { name: "ch".to_string() }).unwrap();
    let receive = graph.add_node(Node::Receive { channel: ch_var2 }).unwrap();

    // Create sequence: spawn then receive
    let sequence = graph.add_node(Node::List(vec![spawn, receive])).unwrap();

    // Create let binding: (let ((ch channel)) sequence)
    let let_node = graph.add_node(Node::Let {
        bindings: vec![("ch".to_string(), channel)],
        body: sequence,
    }).unwrap();

    // ... rest of test
}
```

## Current Bytecode Output

The compiler currently generates:

```
Chunk 0: Some("main")
  0: Instruction { opcode: MakeFunc, arg: 1 }     // Should be MakeClosure
  1: Instruction { opcode: Spawn, arg: 0 }
  2: Instruction { opcode: LoadGlobal, arg: 0 }   // Should be Load (local)
  3: Instruction { opcode: Receive, arg: 0 }
  4: Instruction { opcode: MakeList, arg: 2 }
  5: Instruction { opcode: Halt, arg: 0 }
  Constants: [String("ch")]

Chunk 1: Some("lambda")
  0: Instruction { opcode: LoadGlobal, arg: 0 }   // Should be LoadCaptured
  1: Instruction { opcode: PushIntSmall, arg: 42 }
  2: Instruction { opcode: Send, arg: 0 }
  3: Instruction { opcode: Return, arg: 0 }
  Constants: [String("ch")]
```

## Expected Bytecode

The compiler should generate something like:

```
Chunk 0: Some("main")
  0: Instruction { opcode: Channel, arg: 0 }      // Create channel
  1: Instruction { opcode: Store, arg: 0 }        // Store as local "ch"
  2: Instruction { opcode: Load, arg: 0 }         // Load "ch" for capture
  3: Instruction { opcode: MakeClosure, arg: ... } // Create closure capturing "ch"
  4: Instruction { opcode: Spawn, arg: 0 }
  5: Instruction { opcode: Load, arg: 0 }         // Load local "ch"
  6: Instruction { opcode: Receive, arg: 0 }
  7: Instruction { opcode: MakeList, arg: 2 }
  8: Instruction { opcode: PopN, arg: 1 }         // Clean up let binding
  9: Instruction { opcode: Halt, arg: 0 }

Chunk 1: Some("lambda")
  0: Instruction { opcode: LoadCaptured, arg: 0 } // Load captured "ch"
  1: Instruction { opcode: PushIntSmall, arg: 42 }
  2: Instruction { opcode: Send, arg: 0 }
  3: Instruction { opcode: Return, arg: 0 }
```

## Root Cause Analysis

The issue appears to be in the compiler's handling of:

1. **Let binding compilation**: The compiler is not generating code to evaluate and store let bindings
2. **Variable resolution**: Let-bound variables are being treated as globals instead of locals
3. **Free variable detection**: The lambda's free variables are not being properly identified
4. **Closure generation**: Even when free variables exist, `MakeFunc` is used instead of `MakeClosure`

## Impact

This issue prevents:
- Spawned tasks from accessing variables from their enclosing scope
- Proper lexical scoping in concurrent code
- Channel-based communication patterns where channels are created in let bindings

## Workaround

Currently, the only workaround is to use global variables or pass all required values as parameters to spawned functions.

## Related Code

The relevant compiler code is likely in:
- `fluentai-vm/src/compiler.rs` - Main compiler implementation
- `compile_let()` - Let binding compilation
- `compile_lambda()` - Lambda compilation and free variable detection
- `find_free_variables()` - Free variable analysis
- `compile_variable()` - Variable resolution

## Test Status

- `test_spawn_with_lambda` ✅ - Works because it doesn't use let bindings
- `test_multiple_spawns` ✅ - Works because it doesn't use let bindings
- `test_spawn_with_channel` ❌ - Fails due to this issue

## Priority

High - This is a fundamental issue that breaks lexical scoping in concurrent code, which is essential for the actor model implementation.

## Labels

- bug
- compiler
- closures
- let-bindings
- actor-model