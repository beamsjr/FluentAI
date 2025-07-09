# Integrate UI Framework with Parser

## Overview

The UI compiler can generate React, Vue, and Web Components, but the UI syntax is not recognized by the parser. This prevents using the UI framework despite the backend being implemented.

## Current State

### What Works ✅
- UI compiler can transform AST to multiple targets
- Code generators for React, Vue, Web Components, Vanilla JS work
- Component system design is complete

### What's Broken ❌
- Parser doesn't recognize UI syntax (`ui:element`, `ui:text`, etc.)
- No connection between parser and UI compiler
- No working examples or tests
- No documentation for UI features

## Implementation Tasks

### 1. Parser Extensions
- [ ] Add UI syntax tokens to lexer
- [ ] Implement UI expression parsing rules
- [ ] Support component definition syntax
- [ ] Parse event handlers and props
- [ ] Handle JSX-like children syntax

### 2. AST Extensions
- [ ] Add UI-specific node types to AST
- [ ] Support component references
- [ ] Handle prop spreading
- [ ] Support conditional rendering
- [ ] Add fragment support

### 3. Compiler Integration
- [ ] Connect parser output to UI compiler
- [ ] Add UI compilation phase to build pipeline
- [ ] Support mixed UI/logic code
- [ ] Handle component imports

### 4. Runtime Support
- [ ] Implement UI effect handlers
- [ ] Add DOM manipulation effects
- [ ] Support event handling
- [ ] Implement reactive state updates

## Syntax Design

```lisp
;; Component definition
(define-component "Button" {:text :string :onClick :function}
  (lambda (props)
    (ui:element "button" 
      {:className "btn"
       :onClick (get props :onClick)}
      (ui:text (get props :text)))))

;; Using components
(define-component "App" {}
  (lambda (_)
    (let ((count (reactive-state 0)))
      (ui:element "div" {:className "app"}
        (ui:element "h1" {} 
          (ui:text "Counter App"))
        (ui:element "p" {}
          (ui:text (str "Count: " (reactive-get count))))
        (Button {:text "Increment"
                 :onClick (lambda () 
                           (reactive-update! count inc))})))))

;; Conditional rendering
(ui:if (> count 10)
  (ui:element "p" {} (ui:text "Count is high!"))
  (ui:element "p" {} (ui:text "Keep clicking!")))

;; Lists
(ui:for (item items)
  (ui:element "li" {:key (get item :id)}
    (ui:text (get item :name))))

;; Fragments
(ui:fragment
  (ui:element "h1" {} (ui:text "Title"))
  (ui:element "p" {} (ui:text "Content")))
```

## Parser Grammar Updates

```
ui_expr := 
    | "ui:element" string props children
    | "ui:text" expr
    | "ui:if" expr ui_expr ui_expr?
    | "ui:for" binding collection ui_expr
    | "ui:fragment" ui_expr*
    | component_ref props

props := "{" (keyword expr)* "}"
children := ui_expr*
component_ref := identifier
```

## Integration Points

1. **Parser**: Recognize UI syntax and generate appropriate AST nodes
2. **Compiler**: Transform UI AST to target framework code
3. **VM**: Execute UI effects for dynamic updates
4. **Optimizer**: Optimize UI trees (remove unnecessary re-renders)

## Test Requirements

- Parser tests for all UI syntax variations
- Compiler tests for each target framework
- Integration tests with reactive state
- Performance tests for large component trees
- Visual regression tests

## Examples Needed

1. **TodoMVC** - Complete todo application
2. **Counter** - Simple state management
3. **Form Handling** - Input validation and submission
4. **Data Table** - Lists and pagination
5. **Modal/Dialog** - Component composition
6. **Router** - Client-side routing

## Priority

**High** - UI support is a key differentiator for FluentAI

## Labels

- enhancement
- ui-framework
- parser
- high-priority