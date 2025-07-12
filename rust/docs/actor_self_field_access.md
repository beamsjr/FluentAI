# Actor Self Field Access Implementation Plan

## Current State

The FLC actor syntax allows defining actors with state fields:

```flc
private actor Counter {
    count: int = 0;
    
    private handle Message(msg: any) {
        receive {
            "increment" => count + 1,  // Should access the 'count' field
            "get" => count,
            _ => count
        }
    }
}
```

However, the parser currently doesn't support accessing state fields within handlers.

## Problem

1. The parser creates handlers as regular lambda functions
2. Field names (like `count`) are parsed as free variables
3. There's no mechanism to bind these to the actor's state

## Solution Approach

### Option 1: Transform at Parse Time
Modify the parser to:
1. Track when we're inside an actor handler
2. Transform field references to state parameter access
3. Generate: `(state, msg) => state.count + 1` or similar

### Option 2: Compile-Time Transformation
1. Keep the AST as-is
2. During compilation of actor handlers, transform variable references that match state fields

### Option 3: Runtime State Binding
1. Pass state as a record/map to handlers
2. Use field access operations at runtime

## Recommended Approach

Option 1 is most straightforward but requires parser context awareness.

## Implementation Steps

1. **Parser Changes**:
   - Add actor context tracking to parser state
   - When parsing an actor definition, record state field names
   - When parsing handler bodies, transform matching variable references

2. **AST Changes**:
   - Consider adding a FieldAccess node type
   - Or transform to appropriate existing nodes

3. **Compiler Changes**:
   - Ensure actor handlers receive state as first parameter
   - State could be a record, tuple, or single value

4. **Runtime Changes**:
   - Actor message handling needs to pass current state to handler
   - Handler return value becomes new state

## Current Workaround

Until this is implemented, actors must use explicit state parameter:

```flc
private actor Counter {
    count: int = 0;
    
    private handle Message(msg: any) {
        receive {
            "increment" => {
                // Can't access 'count' directly
                // Would need: (state, msg) => state + 1
            }
        }
    }
}
```

## Related Issues

- #86: Complete FLC Actor Syntax Implementation
- Multiple handler methods also need this feature
- Typed message handlers would benefit from similar approach