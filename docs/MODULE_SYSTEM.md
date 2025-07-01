# ClaudeLang Module System

ClaudeLang provides a module system for organizing code into reusable components.

## Module Definition

Modules are defined using the `module` form:

```lisp
(module module-name (export name1 name2 ...)
  ; Module body
  (let ((name1 value1))
    (let ((name2 value2))
      ; Return exported values as tuple
      (tuple name1 name2 ...))))
```

## Importing Modules

Use the `import` form to bring module exports into scope:

```lisp
; Import specific functions
(import "path/to/module" (func1 func2))

; Import all exports
(import "path/to/module" *)

; Import with renaming (planned feature)
(import "path/to/module" ((original-name as new-name)))
```

## Module Search Paths

The module system searches for modules in these locations:
1. Current directory
2. `modules/` subdirectory
3. Standard library modules directory

## Example Modules

### Math Utilities Module

```lisp
; modules/math_utils.cl
(module math-utils (export square cube factorial-memo)
  
  (let ((square (lambda (x) (* x x))))
    (let ((cube (lambda (x) (* x x x))))
      (let ((factorial-memo 
              (memoize 
                (lambda (n)
                  (if (<= n 1)
                      1
                      (* n (factorial-memo (- n 1))))))))
        (tuple square cube factorial-memo)))))
```

### Using the Module

```lisp
; main.cl
(import "modules/math_utils" (square cube))

(io:print "square(5) =" (square 5))
(io:print "cube(3) =" (cube 3))
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

## Future Enhancements

Planned improvements to the module system:
- Import renaming with `as` syntax
- Nested module namespaces
- Module versioning
- Package management integration