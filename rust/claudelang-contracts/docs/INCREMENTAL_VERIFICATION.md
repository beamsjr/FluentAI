# Incremental Verification

This document explains the incremental verification system that tracks dependencies and only re-verifies contracts affected by changes.

## Overview

Incremental verification significantly improves performance in large codebases by:
- Tracking dependencies between functions and contracts
- Detecting which functions have changed
- Re-verifying only affected contracts
- Caching verification results for unchanged contracts

## Architecture

### Dependency Tracking

The system maintains several dependency graphs:

1. **Function Dependencies** (`function_deps`)
   - Maps each function to the functions it calls
   - Example: `factorial -> [factorial, multiply]`

2. **Reverse Dependencies** (`reverse_deps`)
   - Maps each function to functions that call it
   - Example: `multiply -> [factorial, double]`

3. **Contract Dependencies** (`contract_deps`)
   - Maps each contract to functions it references
   - Example: `factorial_contract -> [factorial, >=, *]`

4. **Function-Contract Mapping** (`function_contracts`)
   - Maps each function to its contracts
   - Example: `factorial -> [factorial_contract]`

### Change Detection

Functions are marked as changed when:
- Their implementation is modified (detected via hash comparison)
- A function they depend on changes (transitive)

### Incremental Algorithm

```
1. Build initial dependency graphs
2. Compute hashes for all function bodies
3. On change:
   a. Compare new hash with cached hash
   b. If different, mark function as changed
   c. Transitively mark dependent functions
   d. Identify affected contracts
   e. Re-verify only affected contracts
   f. Update cache with new results
```

## Usage

### Basic Setup

```rust
use claudelang_contracts::{IncrementalVerifier, StaticVerifier};

// Create incremental verifier
let mut inc_verifier = IncrementalVerifier::new(&graph);

// Build dependencies from contracts
inc_verifier.build_dependencies(&contracts)?;

// Initial verification
let mut static_verifier = StaticVerifier::new(&graph);
let results = inc_verifier.verify_incremental(&contracts, &mut static_verifier)?;
```

### Handling Changes

```rust
// When a function changes
inc_verifier.mark_function_changed("my_function");
inc_verifier.update_function_hash("my_function", new_body_node);

// Get affected contracts
let contracts_to_verify = inc_verifier.get_contracts_to_verify();

// Re-verify incrementally
let results = inc_verifier.verify_incremental(&contracts, &mut static_verifier)?;
```

### Dependency Analysis

```rust
use claudelang_contracts::DependencyAnalyzer;

// Analyze function calls
let mut analyzer = DependencyAnalyzer::new(&graph);
let called_functions = analyzer.analyze_function_calls(function_body);
```

## Example

Consider this dependency scenario:

```clojure
;; Base function
(define (add x y)
  (+ x y))

;; Dependent function
(define (double n)
  (add n n))

;; Another dependent
(define (quadruple n)
  (double (double n)))
```

When `add` changes:
1. `add` is marked as changed
2. `double` is transitively marked (calls `add`)
3. `quadruple` is transitively marked (calls `double`)
4. All contracts for these functions are re-verified
5. Contracts for unrelated functions use cached results

## Performance Benefits

### Without Incremental Verification
- 100 functions, 200 contracts
- Change 1 function
- Must re-verify all 200 contracts
- Time: O(n) where n = total contracts

### With Incremental Verification
- Same scenario
- Only re-verify contracts of changed function + dependents
- Typically 5-20 contracts
- Time: O(d) where d = affected contracts << n

## Statistics

The system provides statistics via `IncrementalStats`:

```rust
let stats = inc_verifier.get_stats();
println!("Total functions: {}", stats.total_functions);
println!("Changed functions: {}", stats.changed_functions);
println!("Cached results: {}", stats.cached_results);
```

## Implementation Details

### Hash Computation

Function hashes are computed from:
- AST structure
- Literal values
- Variable names
- Operation types

This ensures any semantic change triggers re-verification.

### Cache Management

The verification cache uses:
- Contract name as key
- Full `VerificationResult` as value
- LRU eviction when memory constrained (future enhancement)

### Transitive Dependencies

The system handles:
- Direct calls: `f` calls `g`
- Indirect calls: `f` calls `g` calls `h`
- Mutual recursion: `f` calls `g`, `g` calls `f`
- Higher-order functions (future enhancement)

## Best Practices

1. **Granular Functions**: Smaller functions lead to more precise dependency tracking
2. **Pure Functions**: Pure functions are easier to cache and verify
3. **Clear Contracts**: Well-defined contracts make dependency analysis more accurate
4. **Regular Commits**: Frequent small changes benefit more from incremental verification

## Limitations

Current limitations include:
- No support for dynamic function calls
- Limited higher-order function analysis
- No cross-module dependency tracking (future)
- Hash computation is simplified

## Future Enhancements

1. **Parallel Incremental Verification**: Verify independent contracts in parallel
2. **Persistent Cache**: Save cache between sessions
3. **Fine-grained Dependencies**: Track parameter-level dependencies
4. **Smart Scheduling**: Prioritize critical contracts
5. **Incremental Proof Generation**: Update proofs incrementally

## Debugging

Enable verbose logging to debug dependency issues:

```rust
// Future API
inc_verifier.set_verbose(true);
inc_verifier.dump_dependencies("deps.dot"); // GraphViz output
```

## Integration with CI/CD

Incremental verification is ideal for:
- Pre-commit hooks (verify only changed code)
- Pull request validation (verify PR changes)
- Continuous integration (cache between builds)

Example CI configuration:
```yaml
- name: Incremental Contract Verification
  run: |
    # Load cache from previous run
    cargo run --example verify_incremental -- \
      --cache-file .contract-cache \
      --changed-files ${{ steps.changes.outputs.files }}
```