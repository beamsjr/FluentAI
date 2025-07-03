# ClaudeLang Module System

The ClaudeLang module system provides a way to organize code into reusable, encapsulated units with controlled visibility.

## Table of Contents
- [Overview](#overview)
- [Module Declaration](#module-declaration)
- [Imports](#imports)
- [Exports](#exports)
- [Module Resolution](#module-resolution)
- [Best Practices](#best-practices)

## Overview

Modules in ClaudeLang allow you to:
- Organize code into logical units
- Control visibility of functions and values
- Avoid naming conflicts
- Create reusable libraries
- Manage dependencies

## Module Declaration

### Basic Syntax

```clojure
(module module-name (export symbol1 symbol2 ...)
  body-expression)
```

### Example

```clojure
(module math (export add multiply square pi)
  (let ((pi 3.14159)
        (add (lambda (a b) (+ a b)))
        (multiply (lambda (a b) (* a b)))
        (square (lambda (x) (* x x)))
        (internal-helper (lambda (x) (* x 2))))  ; Not exported
    nil))
```

## Imports

### Import Specific Symbols

```clojure
(import "module-path" (symbol1 symbol2))
```

### Import with Aliases

```clojure
(import "math" (add as plus multiply as times))
```

### Import All Exports

```clojure
(import "math" *)
```

### Qualified Access

Even without importing specific symbols, you can use qualified names:

```clojure
(import "math" ())  ; Import nothing, but make module available
(math.add 2 3)      ; Use qualified name
```

## Exports

Only exported symbols are accessible from outside the module:

```clojure
(module secure-module (export public-api)
  (let ((secret-key "private-data")
        (public-api (lambda (data)
          (process data secret-key)))
        (process (lambda (data key)
          ; Implementation
          )))
    nil))
```

## Module Resolution

### Search Paths

Modules are searched in the following order:
1. Current directory
2. Directories specified in module search paths
3. Standard library locations

### File Naming

- Module files use the `.cl` extension
- Module `foo` is looked for as:
  - `foo.cl` (file)
  - `foo/module.cl` (directory with module file)

### Example Directory Structure

```
project/
├── main.cl
├── lib/
│   ├── math.cl
│   └── utils.cl
└── modules/
    ├── database/
    │   └── module.cl
    └── network.cl
```

## Module Loading

### Caching

Modules are cached after first load:
- Subsequent imports return the cached module
- Circular dependencies are detected and prevented
- Cache can be configured with size limits

### Configuration

```rust
use claudelang_modules::{ModuleLoader, ModuleConfig};

let config = ModuleConfig {
    search_paths: vec![
        PathBuf::from("./lib"),
        PathBuf::from("./modules"),
    ],
    enable_cache: true,
    max_cache_size: 100,
    allow_circular: false,
};

let loader = ModuleLoader::new(config);
```

## Best Practices

### 1. Module Organization

Group related functionality:

```clojure
; Good: Cohesive module
(module string-utils (export trim split join capitalize)
  ; String manipulation functions
  )

; Avoid: Grab-bag of unrelated functions
(module misc (export random-stuff other-thing unrelated)
  ; Mixed concerns
  )
```

### 2. Minimal Exports

Export only what's necessary:

```clojure
(module api (export create-user get-user)
  (let ((validate-email (lambda (email) ...))     ; Internal
        (hash-password (lambda (pass) ...))       ; Internal
        (create-user (lambda (name email pass)
          ; Uses internal functions
          ))
        (get-user (lambda (id) ...)))
    nil))
```

### 3. Avoid Circular Dependencies

Structure modules hierarchically:

```
core/     (no dependencies)
utils/    (depends on core)
domain/   (depends on core, utils)
app/      (depends on all above)
```

### 4. Namespace Conventions

Use descriptive module names:

```clojure
; Good
(module company.product.auth ...)
(module math.linear-algebra ...)

; Avoid
(module util ...)
(module stuff ...)
```

## Examples

### Library Module

```clojure
(module collections (export list-map list-filter list-reduce)
  (let ((list-map (lambda (f xs)
          (if (null? xs)
              nil
              (cons (f (car xs)) (list-map f (cdr xs))))))
        
        (list-filter (lambda (pred xs)
          (if (null? xs)
              nil
              (if (pred (car xs))
                  (cons (car xs) (list-filter pred (cdr xs)))
                  (list-filter pred (cdr xs))))))
        
        (list-reduce (lambda (f init xs)
          (if (null? xs)
              init
              (list-reduce f (f init (car xs)) (cdr xs))))))
    nil))
```

### Application Module

```clojure
(import "collections" (list-map list-filter))
(import "math" *)

(module app (export main process-data)
  (let ((process-data (lambda (data)
          (list-map math.square
            (list-filter (lambda (x) (> x 0)) data))))
        
        (main (lambda ()
          (let ((result (process-data [1 -2 3 -4 5])))
            (print "Processed:" result)))))
    nil))
```

### Effect Module

```clojure
(module io (export read-file write-file with-file)
  (let ((read-file (lambda (path)
          (effect io:read path)))
        
        (write-file (lambda (path content)
          (effect io:write path content)))
        
        (with-file (lambda (path mode f)
          (let ((handle (effect io:open path mode)))
            (handle-finally
              (f handle)
              (effect io:close handle))))))
    nil))
```

## Integration with VM

The module system is integrated with the ClaudeLang VM through:

1. **Module Opcodes**: Special bytecode instructions for module operations
2. **Module Loader**: Handles finding and loading modules
3. **Module Environment**: Manages module-level bindings and imports

### VM Module Operations

- `LoadModule`: Load a module by name
- `ImportBinding`: Import specific binding from module
- `LoadQualified`: Load qualified variable (module.name)
- `BeginModule`: Start module definition
- `EndModule`: Complete module definition
- `ExportBinding`: Export a binding from module

## Error Handling

Common module errors:

```clojure
; ModuleNotFound
(import "non-existent" ())  ; Error: Module not found

; ExportNotFound
(import "math" (undefined-function))  ; Error: Export not found

; CircularDependency
; a.cl imports b.cl, b.cl imports a.cl  ; Error: Circular dependency
```

## Performance Considerations

1. **Lazy Loading**: Modules are loaded only when imported
2. **Caching**: Loaded modules are cached to avoid re-parsing
3. **Compile-Time Resolution**: Import/export checking happens at compile time
4. **Minimal Runtime Overhead**: Module boundaries have negligible runtime cost

## Future Enhancements

Planned features:
- Module versioning
- Dynamic module loading
- Module hot-reloading for development
- Private module members
- Module interfaces/signatures