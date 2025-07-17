# AST Node Definitions and Transformation Guide

## Core Types

### NodeId
- Defined in `fluentai-core/src/ast.rs`
- Type: `struct NodeId(pub NonZeroU32)`
- Represents a unique identifier for nodes in the AST graph
- NodeId(0) is reserved as invalid/null

### Node Enum
The complete `Node` enum is defined in `fluentai-core/src/ast.rs` with the following variants:
- `Literal(Literal)` - Numbers, strings, booleans, nil
- `Variable { name: String }` - Variable references
- `Lambda { params: Vec<String>, body: NodeId }` - Anonymous functions
- `Let { bindings: Vec<(String, NodeId)>, body: NodeId }` - Local bindings
- `Letrec { bindings: Vec<(String, NodeId)>, body: NodeId }` - Recursive bindings
- `If { condition: NodeId, then_branch: NodeId, else_branch: NodeId }` - Conditionals
- `Application { function: NodeId, args: Vec<NodeId> }` - Function calls
- `Effect { effect_type: EffectType, operation: String, args: Vec<NodeId> }` - Effects
- `Handler { handlers: Vec<(EffectType, Option<String>, NodeId)>, body: NodeId }` - Effect handlers
- `List(Vec<NodeId>)` - Lists
- `Map(Vec<(NodeId, NodeId)>)` - Maps/dictionaries
- `Match { expr: NodeId, branches: Vec<(Pattern, NodeId)> }` - Pattern matching
- `Module { name: String, exports: Vec<String>, body: NodeId }` - Modules
- `Import { module_path: String, import_list: Vec<ImportItem>, import_all: bool }` - Imports
- `Export { export_list: Vec<ExportItem> }` - Exports
- `QualifiedVariable { module_name: String, variable_name: String }` - Module::variable
- `Define { name: String, value: NodeId }` - Top-level definitions
- `Assignment { target: NodeId, value: NodeId }` - Assignments
- `Begin { exprs: Vec<NodeId> }` - Sequential evaluation
- `Async { body: NodeId }` - Async blocks
- `Await { expr: NodeId }` - Await expressions
- `Spawn { expr: NodeId }` - Spawn concurrent tasks
- `Channel { capacity: Option<NodeId> }` - Channel creation
- `Send { channel: NodeId, value: NodeId }` - Send to channel
- `Receive { channel: NodeId }` - Receive from channel
- `TrySend { channel: NodeId, value: NodeId }` - Non-blocking send
- `TryReceive { channel: NodeId }` - Non-blocking receive
- `Select { arms: Vec<(NodeId, NodeId)> }` - Select from channels
- And more...

### Graph Structure
```rust
pub struct Graph {
    pub nodes: AstHashMap<NodeId, Node>,
    pub root_id: Option<NodeId>,
    next_id: u32,
    pub metadata: AstHashMap<NodeId, NodeMetadata>,
    pub graph_metadata: AstHashMap<String, String>,
}
```

Key methods:
- `Graph::new()` - Create new graph
- `graph.add_node(node: Node) -> Result<NodeId>` - Add node and get its ID
- `graph.get_node(id: NodeId) -> Option<&Node>` - Get node reference
- `graph.get_node_mut(id: NodeId) -> Option<&mut Node>` - Get mutable node reference

## Relationship between chunk_id and NodeId

**Important**: `chunk_id` is NOT a `NodeId`. It's a `usize` that represents:
- In `Value::Function { chunk_id: usize, env: Vec<Value> }` - A bytecode chunk ID
- Generated during compilation when the compiler creates bytecode chunks for lambda expressions
- Used by the VM to execute functions

The compiler (in `fluentai-vm/src/compiler.rs`) generates chunk_ids when compiling Lambda nodes:
```rust
// In compile_lambda:
let chunk_id = self.bytecode.add_chunk(lambda_chunk);
```

## AST Transformation Patterns

### 1. Node Cloning
All Node variants implement `Clone` (via `#[derive(Clone)]`), so you can:
```rust
let cloned_node = node.clone();
```

### 2. Node Copying with Substitution (from inline.rs)
```rust
fn copy_with_substitution(
    graph: &Graph,
    node_id: NodeId,
    substitutions: &FxHashMap<String, NodeId>,
    node_mapping: &mut FxHashMap<NodeId, NodeId>,
    optimized: &mut Graph,
) -> Result<NodeId>
```

### 3. Creating New Nodes
```rust
let new_node = Node::Variable { name: "x".to_string() };
let node_id = graph.add_node(new_node)?;
```

### 4. Updating Node References
When transforming AST, remember to update all NodeId references:
```rust
match node {
    Node::Application { function, args } => {
        *function = node_map.get(function).copied().unwrap_or(*function);
        for arg in args {
            *arg = node_map.get(arg).copied().unwrap_or(*arg);
        }
    }
    // ... handle other node types
}
```

## Existing Optimization Passes for Reference

### 1. Function Inlining (`fluentai-optimizer/src/passes/inline.rs`)
- Performs beta reduction by substituting arguments
- Handles variable capture and free variables
- Creates node mappings during transformation

### 2. Function Specialization (`fluentai-optimizer/src/passes/function_specialization.rs`)
- Creates specialized versions of functions for constant arguments
- Analyzes call patterns throughout the program
- Generates new function names like `func_spec_0_5` for specialized versions
- Uses substitution to replace parameters with constants

## Key Considerations for AST Transformations

1. **Node Mapping**: Always maintain a mapping from old NodeIds to new NodeIds
2. **Variable Capture**: Be careful about free variables when transforming lambdas
3. **Recursive Traversal**: Most transformations need to recursively visit all child nodes
4. **Graph Consistency**: Ensure all NodeId references point to valid nodes
5. **Root Updates**: Don't forget to update `graph.root_id` after transformation

## Common Pitfalls (from CLAUDE.md)

Watch out for catch-all patterns in match statements:
```rust
match node {
    Node::Application { .. } => { /* handle */ }
    _ => {} // This catches everything else!
    Node::Channel { .. } => { /* This will NEVER be reached! */ }
}
```

Always add new cases BEFORE the catch-all pattern.