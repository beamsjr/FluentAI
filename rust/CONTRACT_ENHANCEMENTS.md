# FluentAI Contract System Enhancements

This document describes the four major enhancements made to the FluentAI contract system:

1. **Contract Inheritance** - Ability to extend contracts in derived types
2. **Contract Composition** - Combine multiple contracts with logical operators
3. **Temporal Contracts** - Express properties over time
4. **Contract Debugging** - Better error messages showing which part of contract failed

## 1. Contract Inheritance

Contract inheritance allows you to create hierarchies of contracts, enabling code reuse and ensuring behavioral subtyping following the Liskov Substitution Principle (LSP).

### Features

- **Standard Inheritance**: Derived contracts must satisfy base contracts
- **Refinement**: Strengthen postconditions while maintaining base guarantees
- **Weakening**: Relax constraints for more general implementations
- **Interface Implementation**: Ensure contracts meet interface requirements

### Example: Standard Inheritance

```rust
use fluentai_contracts::{*, inheritance::*};

// Base contract for a sorting function
let base_sort = Contract {
    function_name: "sort".to_string(),
    preconditions: vec![
        condition("array != null"),
        condition("array.length > 0"),
    ],
    postconditions: vec![
        condition("is_sorted(result)"),
        condition("same_elements(array, result)"),
    ],
    ..Default::default()
};

// Derived contract for stable sort
let stable_sort = Contract {
    function_name: "stable_sort".to_string(),
    preconditions: vec![
        condition("array != null"),
        condition("array.length > 0"),
    ],
    postconditions: vec![
        condition("is_sorted(result)"),
        condition("same_elements(array, result)"),
        condition("preserves_relative_order(array, result)"), // Additional guarantee
    ],
    ..Default::default()
};

// Set up inheritance
let mut hierarchy = ContractHierarchy::new();
hierarchy.add_contract(base_sort);
hierarchy.add_contract(stable_sort);
hierarchy.add_inheritance(
    "sort".to_string(),
    "stable_sort".to_string(),
    InheritanceType::Refinement
)?;
```

### Contract Interfaces

Define interfaces that multiple contracts can implement:

```rust
let sortable_interface = ContractInterface {
    name: "Sortable".to_string(),
    required_preconditions: vec![
        condition("input != null"),
    ],
    required_postconditions: vec![
        condition("is_sorted(result)"),
    ],
    required_invariants: vec![],
    optional_conditions: vec![
        condition("is_stable(result)"),
    ],
};

hierarchy.add_interface(sortable_interface);
```

## 2. Contract Composition

Contract composition allows you to combine multiple contracts using logical operators, creating more complex specifications from simpler ones.

### Composition Types

1. **Conjunction (AND)**: All contracts must hold
2. **Disjunction (OR)**: At least one contract must hold
3. **Sequential**: Contracts applied in sequence
4. **Exclusive OR (XOR)**: Exactly one contract must hold
5. **Implication**: If first holds, then second must hold

### Example: XOR Composition

```rust
// Contract for processing type A data
let process_type_a = Contract {
    function_name: "process_type_a".to_string(),
    preconditions: vec![condition("is_type_a(input)")],
    postconditions: vec![condition("result.type == 'A'")],
    ..Default::default()
};

// Contract for processing type B data
let process_type_b = Contract {
    function_name: "process_type_b".to_string(),
    preconditions: vec![condition("is_type_b(input)")],
    postconditions: vec![condition("result.type == 'B'")],
    ..Default::default()
};

// Compose with XOR - exactly one type processor should be used
let xor_contract = hierarchy.compose_contracts(
    &["process_type_a".to_string(), "process_type_b".to_string()],
    CompositionType::ExclusiveOr
)?;
```

### Example: Implication Composition

```rust
// If validation passes, processing must succeed
let validation_contract = Contract {
    function_name: "validate".to_string(),
    postconditions: vec![condition("is_valid(result)")],
    ..Default::default()
};

let processing_contract = Contract {
    function_name: "process".to_string(),
    preconditions: vec![condition("is_valid(input)")],
    postconditions: vec![condition("processed(result)")],
    ..Default::default()
};

// Create implication: validation => processing
let impl_contract = hierarchy.compose_contracts(
    &["validate".to_string(), "process".to_string()],
    CompositionType::Implication
)?;
```

## 3. Temporal Contracts

Temporal contracts allow you to specify properties that must hold over time, using Linear Temporal Logic (LTL) operators.

### Temporal Operators

- **Always (□)**: Property holds in all future states
- **Eventually (◇)**: Property holds in some future state
- **Next (○)**: Property holds in the next state
- **Until (U)**: First property holds until second becomes true
- **Weak Until (W)**: Like Until but second may never hold
- **Release (R)**: Dual of Until
- **Since (S)**: Past-time operator
- **Previously (P)**: Past-time version of Next

### Example: Response Property

```rust
use fluentai_contracts::{temporal::*, temporal_dsl::*};

// "Always, if a request is made, eventually a response occurs"
let request = atom(condition("request_sent"));
let response = atom(condition("response_received"));

let response_property = always(implies(
    request,
    eventually(response)
));

let contract = TemporalContractBuilder::new("response_property".to_string())
    .formula(response_property)
    .bound(100) // Check within 100 steps
    .build()?;
```

### Example: Mutual Exclusion

```rust
// "At most one process in critical section at a time"
let p1_critical = atom(condition("process1_in_critical"));
let p2_critical = atom(condition("process2_in_critical"));

let mutex = always(not(and(vec![p1_critical, p2_critical])));

let contract = TemporalContractBuilder::new("mutual_exclusion".to_string())
    .formula(mutex)
    .safety(mutex) // This is a safety property
    .build()?;
```

### Bounded Model Checking

Temporal properties can be verified using bounded model checking:

```rust
let mut bmc = BoundedModelChecker::new(100); // Max bound of 100
let initial_state = BMCState {
    index: 0,
    assignments: HashMap::new(),
    propositions: HashSet::new(),
};

let result = bmc.check_contract(&temporal_contract, &initial_state)?;
if !result.verified {
    println!("Counterexample found at depth {}", result.bound);
}
```

## 4. Enhanced Contract Debugging

The enhanced debugging system provides visual diagrams and detailed error messages when contracts fail.

### Features

- **Visual Diagrams**: ASCII-art visualization of contract failures
- **Evaluation Traces**: Step-by-step breakdown of condition evaluation
- **Variable Snapshots**: All variable values at failure point
- **Smart Suggestions**: Context-aware fix suggestions
- **Interactive REPL**: Debug contract violations interactively

### Example: Debug Output

```
╔══════════════════════════════════════╗
║      CONTRACT FAILURE DIAGRAM        ║
╚══════════════════════════════════════╝

Failed Condition:
└─ (x > 0)
   └─ Message: x must be positive

Evaluation Trace:
├─ ✗ (x > 0)
   └─ Result: false
   └─ x = -5

Variable Values at Failure:
  x = -5
  y = 10
  @old_x = 0

Suggestions:
  • Check input validation before calling this function
  • Ensure x is at least 0 before calling
```

### Using the Debugger

```rust
use fluentai_contracts::debugging::*;

let debugger = ContractDebugger::default();
let debug_info = debugger.debug_violation(&violation, &context);

// Print visual diagram
println!("{}", debug_info.visual_diagram);

// Get suggestions
for suggestion in &debug_info.suggestions {
    println!("Suggestion: {}", suggestion);
}
```

### Interactive REPL

```rust
let mut repl = ContractDebugRepl::new();
repl.add_violation(violation, &context);

// Available commands:
// - list: Show all violations
// - show <idx>: Show detailed violation with diagram
// - trace <idx>: Show evaluation trace
// - values <idx>: Show variable values
// - suggest <idx>: Show fix suggestions
```

## State Machine Contracts

As an additional feature, the system now supports contracts for finite state machines:

### Example: Traffic Light FSM

```rust
use fluentai_contracts::state_machine::*;

let machine = StateMachineBuilder::new("traffic_light".to_string())
    .state(State {
        id: "red".to_string(),
        name: "Red Light".to_string(),
        is_initial: true,
        invariants: vec![condition("timer >= 0")],
        ..Default::default()
    })
    .state(State {
        id: "green".to_string(),
        name: "Green Light".to_string(),
        invariants: vec![condition("cars_waiting == 0")],
        ..Default::default()
    })
    .transition(Transition {
        from: "red".to_string(),
        to: "green".to_string(),
        event: "timer_expired".to_string(),
        guard: Some(condition("timer == 0")),
        actions: vec![TransitionAction::Assign("timer".to_string(), json!(30))],
        postconditions: vec![condition("light_changed")],
    })?
    .build();

// Define safety properties
let contract = StateMachineContract {
    name: "traffic_safety".to_string(),
    safety_properties: vec![
        SafetyProperty {
            name: "no_simultaneous_green".to_string(),
            forbidden_predicates: vec![
                condition("north_green && east_green"),
            ],
            ..Default::default()
        }
    ],
    deadlock_free: true,
    deterministic: true,
    ..Default::default()
};
```

## Integration Example

Here's how to use multiple contract features together:

```rust
// 1. Define base contracts with temporal properties
let base_protocol = Contract {
    function_name: "protocol_step".to_string(),
    preconditions: vec![condition("initialized")],
    postconditions: vec![condition("step_completed")],
    ..Default::default()
};

// 2. Create temporal formula for liveness
let liveness = always(implies(
    atom(condition("request_pending")),
    eventually(atom(condition("request_handled")))
));

// 3. Set up inheritance for specialized protocols
let secure_protocol = Contract {
    function_name: "secure_protocol_step".to_string(),
    preconditions: vec![
        condition("initialized"),
        condition("authenticated"),
    ],
    postconditions: vec![
        condition("step_completed"),
        condition("audit_logged"),
    ],
    ..Default::default()
};

// 4. Compose contracts for complex scenarios
let composed = hierarchy.compose_contracts(
    &["protocol_step".to_string(), "secure_protocol_step".to_string()],
    CompositionType::Implication
)?;

// 5. Debug any failures with enhanced debugging
if let Err(violation) = verify_contract(&composed) {
    let debug_info = debugger.debug_violation(&violation, &context);
    println!("Failure: {}", debug_info.visual_diagram);
}
```

## Best Practices

1. **Use inheritance** for behavioral subtyping and code reuse
2. **Use composition** to build complex contracts from simple ones
3. **Use temporal contracts** for concurrent and reactive systems
4. **Enable debugging** during development for better error messages
5. **Combine features** as needed for your specific use case

## Performance Considerations

- Contract inheritance verification: O(n) where n is inheritance depth
- XOR/Implication composition: O(1) additional overhead
- Temporal verification: Exponential in formula size (use bounded model checking)
- Debugging overhead: ~10-20% when enabled, negligible when disabled
- State machine verification: O(states × transitions)

## Future Work

1. **Temporal syntax in parser** - Direct support for temporal operators in FluentAI syntax
2. **Probabilistic contracts** - Support for stochastic properties
3. **Distributed contracts** - Contracts across multiple nodes
4. **Contract synthesis** - Automatically generate contracts from examples
5. **Contract repair** - Suggest fixes when contracts are violated