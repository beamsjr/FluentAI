# FluentAI Module System

FluentAI provides a comprehensive module system for organizing code into reusable components with support for imports, exports, and top-level definitions.

## Module Definition

Modules are defined using the `module` form with an export list and body:

```lisp
(module module-name [export1 export2 ...]
  ; Module body - can contain multiple expressions
  (define export1 value1)
  (define export2 value2)
  ; Additional definitions and expressions
  ...)
```

The module body can contain:
- Top-level definitions using `define`
- Import statements
- Export statements
- Any other expressions

### Top-Level Definitions

Use `define` for top-level bindings within modules:

```lisp
; Simple value definition
(define pi 3.14159)

; Function definition (simple syntax)
(define square (lambda (x) (* x x)))

; Function definition (nested syntax)
(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))
```

## Importing Modules

Use the `import` form to bring module exports into scope:

```lisp
; Import specific functions
(import "path/to/module" (func1 func2))

; Import all exports
(import "path/to/module" *)

; Import with empty list for qualified access only
(import "path/to/module" ())

; Relative imports
(import "./sibling-module" (helper))
(import "../parent/module" (util))

; Import with renaming (planned feature)
(import "path/to/module" ((original-name as new-name)))
```

### Qualified Access

When you import a module, you can access its exports using qualified names:

```lisp
(import "math" ())  ; Import for qualified access only
(define area (lambda (r) (* math.pi r r)))

; Or combine with specific imports
(import "math" (sin cos))
(define calculate (lambda (x) (+ (sin x) (* math.e (cos x)))))
```

## Module Search Paths

The module system searches for modules in these locations:
1. Current directory
2. `modules/` subdirectory
3. Standard library modules directory

## Example Modules

### Math Utilities Module

```lisp
; modules/math_utils.ai
(module math-utils [square cube factorial pi golden-ratio]
  ; Constants
  (define pi 3.14159265359)
  (define golden-ratio 1.618033988749)
  
  ; Function definitions
  (define (square x) (* x x))
  
  (define (cube x) (* x x x))
  
  (define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1))))))
```

### String Utilities Module

```lisp
; modules/string_utils.ai
(module string-utils [trim split join capitalize]
  (import "stdlib/strings" *)
  
  (define (trim s)
    (string-trim s " \t\n"))
  
  (define (split s delim)
    (string-split s delim))
  
  (define (join strings delim)
    (string-join strings delim))
  
  (define (capitalize s)
    (if (string-empty? s)
        s
        (string-append (string-upper (string-slice s 0 1))
                       (string-slice s 1)))))
```

### Using the Modules

```lisp
; main.ai
(import "modules/math_utils" (square cube))
(import "modules/string_utils" *)

; Direct usage
(print "square(5) =" (square 5))
(print "cube(3) =" (cube 3))

; Using string utilities
(define text "  hello world  ")
(print "Trimmed:" (trim text))
(print "Capitalized:" (capitalize (trim text)))

; Qualified access
(import "modules/math_utils" ())
(print "Area of circle:" (* math-utils.pi (square 5)))
```

## Module Isolation

Each module executes in its own environment:
- Module-local definitions don't pollute the global namespace
- Modules can only access explicitly imported values
- Circular dependencies are detected and prevented

## Best Practices

1. **One module per file**: Keep modules focused and cohesive
2. **Clear exports**: Only export the public interface
3. **Avoid side effects**: Module loading should be pure
4. **Document exports**: Add comments explaining exported functions

## Implementation Notes

The module system is implemented with:
- **Lazy loading**: Modules are loaded on first import
- **Caching**: Loaded modules are cached to avoid re-parsing
- **Static resolution**: Imports are resolved at parse time when possible

## Advanced Features

### Export Statement

You can also use the `export` statement to dynamically export values:

```lisp
(module dynamic-exports []
  (define helper1 (lambda (x) (* x 2)))
  (define helper2 (lambda (x) (+ x 1)))
  
  ; Conditionally export based on configuration
  (if (debug-mode?)
      (export helper1 helper2 debug-tools)
      (export helper1 helper2)))
```

### Module with Multiple Body Expressions

Modules can contain multiple top-level expressions that are evaluated in order:

```lisp
(module logger [log-info log-error set-level]
  ; Initialize module state
  (define log-level 'INFO)
  
  ; Define logging functions
  (define (set-level level)
    (set! log-level level))
  
  (define (should-log? level)
    (case level
      ('ERROR #t)
      ('INFO (member? log-level '(INFO DEBUG)))
      ('DEBUG (eq? log-level 'DEBUG))
      (else #f)))
  
  (define (log-info message data)
    (when (should-log? 'INFO)
      (print "[INFO]" message data)))
  
  (define (log-error message data)
    (when (should-log? 'ERROR)
      (print "[ERROR]" message data)))
  
  ; Module initialization
  (log-info "Logger initialized" {:level log-level}))
```

## Future Enhancements

Planned improvements to the module system:
- Import renaming with `as` syntax (partially implemented)
- Nested module namespaces
- Module versioning
- Package management integration
- Circular dependency resolution
- Module hot-reloading for development