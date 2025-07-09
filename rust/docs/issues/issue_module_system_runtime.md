# Complete Module System Runtime Implementation

## Overview

The module system has full parsing and loading infrastructure but cannot export/import values at runtime. This severely limits code organization and reusability.

## Current State

### What Works ✅
- Module syntax parses correctly
- Module loading from filesystem works
- Module file resolution works

### What's Broken ❌
- No global binding mechanism for exports
- Cannot access exported values from other modules
- Qualified access (module.function) doesn't work
- Import aliases not implemented

## Implementation Tasks

### 1. Global Export Registry
- [ ] Create global export table in VM
- [ ] Implement export binding during module evaluation
- [ ] Support re-exports from other modules
- [ ] Handle circular dependencies gracefully

### 2. Import Resolution
- [ ] Resolve imports to actual values at runtime
- [ ] Support selective imports: `(import "math" (sin cos))`
- [ ] Support wildcard imports: `(import "math" *)`
- [ ] Support qualified-only imports: `(import "math" ())`

### 3. Qualified Access
- [ ] Parse and compile module.variable syntax
- [ ] Resolve qualified names to module exports
- [ ] Support nested module access: `math.trig.sin`
- [ ] Error on accessing non-exported members

### 4. Import Aliases
- [ ] Implement alias syntax: `(import "math" (sin as sine))`
- [ ] Support module aliases: `(import "math" :as m)`
- [ ] Handle alias conflicts

## Example Usage

```lisp
;; math.ai
(module math [pi square cube]
  (define pi 3.14159)
  (define square (lambda (x) (* x x)))
  (define cube (lambda (x) (* x x x)))
  (define internal-helper (lambda () "not exported")))

;; main.ai
(import "math" (pi square))
(import "math" ())  ; Qualified access only

(print pi)                    ; => 3.14159
(print (square 5))           ; => 25
(print math.cube)            ; => #<procedure>
(print (math.cube 3))        ; => 27
(print math.internal-helper) ; => Error: 'internal-helper' not exported

;; With aliases
(import "math" (square as sq))
(import "geometry" :as geo)

(print (sq 4))              ; => 16
(print (geo.area-circle 5)) ; => 78.53975
```

## Technical Design

### Export Table Structure
```rust
struct ModuleExports {
    module_name: String,
    exports: HashMap<String, Value>,
    re_exports: HashMap<String, String>, // name -> source_module
}

struct GlobalModuleRegistry {
    modules: HashMap<String, ModuleExports>,
    loading: HashSet<String>, // For circular dependency detection
}
```

### Compilation Changes
- Add new opcodes: `LoadModule`, `GetExport`, `GetQualified`
- Extend symbol table to track module imports
- Add module resolution phase before execution

## Test Requirements

- Unit tests for each import/export variation
- Integration tests with multiple modules
- Circular dependency detection tests
- Performance tests for large module graphs
- Error case coverage

## Priority

**High** - This is fundamental for building real applications

## Labels

- enhancement
- module-system
- high-priority