# Closure Capture Implementation Plan

## Current State

Lambda functions work but cannot access variables from outer scopes:
- `compile_variable` only looks in current scope
- `MakeFunc` creates functions with empty environments
- No mechanism to capture free variables

## Implementation Strategy

### Step 1: Identify Free Variables
During lambda compilation, track which variables are:
- **Bound**: Parameters or locally defined
- **Free**: Used but not defined in the lambda

### Step 2: Capture Values
When emitting `MakeFunc`:
- Push captured values onto stack
- Store count of captured values in instruction

### Step 3: Function Creation
Modify `MakeFunc` to:
- Pop N captured values from stack
- Store them in function's environment

### Step 4: Variable Resolution
Modify `compile_variable` to:
- Check locals first
- Check captured variables (in environment)
- Fall back to globals

### Step 5: Runtime Support
- Add environment to call frames
- Load captured values when function is called
- Make them accessible via special opcodes

## Code Changes Required

### 1. Compiler Changes
```rust
// Track captured variables during compilation
struct CapturedVar {
    name: String,
    outer_scope_index: usize,
    capture_index: usize,
}

// Modified compile_lambda
fn compile_lambda(&mut self, ...) {
    // 1. Identify free variables
    let free_vars = self.find_free_variables(body, params);
    
    // 2. Emit code to push captured values
    for var in &free_vars {
        self.compile_load_from_outer_scope(var);
    }
    
    // 3. Create function with captures
    self.emit(Instruction::with_arg(
        Opcode::MakeClosure, 
        (chunk_id << 16) | (free_vars.len() as u32)
    ));
}
```

### 2. New Opcodes
- `MakeClosure` - Like MakeFunc but captures N values
- `LoadCaptured` - Load from captured environment

### 3. VM Changes
```rust
// MakeClosure implementation
MakeClosure => {
    let packed = instruction.arg;
    let chunk_id = (packed >> 16) as usize;
    let capture_count = (packed & 0xFFFF) as usize;
    
    let mut env = Vec::with_capacity(capture_count);
    for _ in 0..capture_count {
        env.push(self.pop()?);
    }
    env.reverse();
    
    self.push(Value::Function { chunk_id, env })?;
}
```

## Testing Plan

1. Simple capture: `(let ((x 10)) (lambda () x))`
2. Multiple captures: `(let ((x 1) (y 2)) (lambda () (+ x y)))`
3. Nested lambdas: `(lambda (x) (lambda (y) (+ x y)))`
4. Captured mutable state (future with set!)

## Benefits

- Enables functional programming patterns
- Allows creation of stateful functions
- Makes lambdas truly useful
- Foundation for advanced features (generators, coroutines)