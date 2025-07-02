# ClaudeLang UI System

ClaudeLang provides a powerful, reactive UI system for building web applications. It combines functional programming principles with modern UI patterns to create efficient, maintainable interfaces.

## Overview

The UI system consists of several key components:

- **Virtual DOM**: Efficient diffing and rendering
- **Reactive State**: Automatic UI updates on state changes
- **Component System**: Reusable UI components with lifecycle hooks
- **Effect Integration**: Seamless integration with ClaudeLang's effect system
- **Learning Optimization**: AI-powered performance optimizations

## Core Concepts

### Virtual DOM

ClaudeLang uses a virtual DOM to efficiently update the UI:

```lisp
; Create virtual DOM nodes
(dom:h "div" {:class "container"}
  [(dom:h "h1" {} [(dom:text "Hello, World!")])
   (dom:h "p" {} [(dom:text "Welcome to ClaudeLang UI")])])
```

### Reactive State

State that automatically triggers UI updates:

```lisp
; Create reactive reference
(let ((count (effect reactive:ref 0)))
  ; Get value
  (effect reactive:get count)
  
  ; Set value
  (effect reactive:set count 42)
  
  ; Update with function
  (effect reactive:update count (lambda (n) (+ n 1))))
```

### Computed Values

Derived state that updates automatically:

```lisp
(let ((items (effect reactive:ref [1 2 3]))
      (sum (effect reactive:computed
             (lambda ()
               (reduce + (effect reactive:get items))))))
  ; sum automatically updates when items change
  (effect reactive:get sum))
```

### Watchers

React to state changes:

```lisp
(effect reactive:watch [count]
  (lambda (values)
    (print "Count changed to:" (get values count))))
```

## Components

### Functional Components

Simple components as functions:

```lisp
(define Greeting
  (lambda (props)
    (dom:h "div" {}
      [(dom:h "h1" {} [(dom:text (concat "Hello, " (get props :name)))])])))

; Usage
(Greeting {:name "Alice"})
```

### Class Components

Components with state and lifecycle:

```lisp
(define Counter
  (ui:component "Counter"
    {:initial (prop :number :default 0)}
    {
      :state (lambda (self)
               {:count (get (props self) :initial)})
      
      :render (lambda (self)
                (dom:h "div" {}
                  [(dom:h "span" {} [(dom:text (get (state self) :count))])
                   (dom:h "button"
                     {:onClick (lambda ()
                                (set-state self {:count (+ (get (state self) :count) 1)}))}
                     [(dom:text "+")])]))
      
      :mounted (lambda (self)
                (print "Counter mounted"))
    }))
```

## UI Primitives

### Creating Elements

```lisp
; Basic element
(dom:h "div" {:id "app"} [])

; With children
(dom:h "ul" {}
  [(dom:h "li" {} [(dom:text "Item 1")])
   (dom:h "li" {} [(dom:text "Item 2")])])

; Text nodes
(dom:text "Hello, World!")

; Fragments
(dom:fragment
  [(dom:h "h1" {} [(dom:text "Title")])
   (dom:h "p" {} [(dom:text "Content")])])
```

### Conditional Rendering

```lisp
; if/else
(ui:if show-details
  (dom:h "div" {} [(dom:text "Details...")])
  (dom:h "div" {} [(dom:text "Summary")]))

; when (renders nothing if false)
(ui:when is-logged-in
  (dom:h "div" {} [(dom:text "Welcome!")]))
```

### List Rendering

```lisp
(ui:for items
  (lambda (item index)
    (dom:h "li" {:key (get item :id)}
      [(dom:text (get item :name))])))
```

### Event Handling

```lisp
(dom:h "button"
  {:onClick (lambda (event)
             (print "Clicked!"))}
  [(dom:text "Click me")])

; Multiple events
(dom:h "input"
  {:onInput (lambda (e) (handle-input e))
   :onFocus (lambda (e) (handle-focus e))
   :onBlur (lambda (e) (handle-blur e))})
```

### Styling

```lisp
; Inline styles
(dom:h "div"
  {:style (ui:style {:color "red"
                     :fontSize "16px"
                     :marginTop "10px"})}
  [(dom:text "Styled text")])

; Dynamic classes
(dom:h "div"
  {:class (ui:class "base-class"
                    {"active" is-active
                     "disabled" is-disabled})}
  [(dom:text "Dynamic classes")])
```

## Advanced Features

### Portals

Render content outside the component hierarchy:

```lisp
(ui:portal
  (dom:h "div" {:class "modal"}
    [(dom:text "Modal content")])
  "#modal-root")
```

### Suspense

Handle async content with loading states:

```lisp
(ui:suspense
  (fetch-data)  ; Returns a promise
  (dom:h "div" {} [(dom:text "Loading...")]))  ; Fallback
```

### Two-Way Binding

```lisp
(let ((value (effect reactive:ref "")))
  (dom:h "input"
    {:value (effect reactive:get value)
     :onInput (lambda (e)
               (effect reactive:set value
                 (get (get e :target) :value)))}))
```

## Performance Optimization

### Batch Updates

```lisp
(effect reactive:batch
  (lambda ()
    ; Multiple state updates are batched
    (effect reactive:set state1 value1)
    (effect reactive:set state2 value2)
    (effect reactive:set state3 value3)))
```

### Memoization

```lisp
(let ((expensive-computed
       (effect reactive:computed
         (memoize
           (lambda ()
             ; Expensive computation
             (complex-calculation (effect reactive:get data)))))))
  ; Result is cached until dependencies change
  (effect reactive:get expensive-computed))
```

### Keys for Lists

Always use keys for efficient list updates:

```lisp
(ui:for items
  (lambda (item)
    (dom:h "li" {:key (get item :id)}  ; Important!
      [(dom:text (get item :name))])))
```

## Learning Optimization

ClaudeLang can learn from user interactions to optimize UI performance:

```lisp
; Enable learning for a component
(optimize:learn-from-usage MyComponent)

; The system will:
; - Track interaction patterns
; - Optimize rendering strategies
; - Pre-fetch likely needed data
; - Adjust update batching based on usage
```

## Integration with Effects

The UI system integrates seamlessly with ClaudeLang's effect system:

```lisp
(define DataTable
  (lambda ()
    (let ((data (effect reactive:ref [])))
      ; Fetch data with network effect
      (effect network:fetch "https://api.example.com/data"
        {:on-success (lambda (response)
                      (effect reactive:set data response))})
      
      ; Render
      (lambda ()
        (dom:h "table" {}
          (ui:for (effect reactive:get data)
            (lambda (row)
              (dom:h "tr" {:key (get row :id)}
                [(dom:h "td" {} [(dom:text (get row :name))])
                 (dom:h "td" {} [(dom:text (get row :value))])]))))))))
```

## Best Practices

1. **Use Functional Components** when possible for simplicity
2. **Minimize State** - derive as much as possible with computed values
3. **Batch Updates** to reduce re-renders
4. **Use Keys** for all list items
5. **Memoize Expensive Computations**
6. **Clean Up Effects** in unmount lifecycle hooks
7. **Prefer Computed Over Watchers** when deriving state

## Example: Complete Todo App

See `examples/ui_demo.cl` for a complete todo application demonstrating:

- Component composition
- Reactive state management
- Event handling
- Computed values
- List rendering with keys
- Conditional rendering
- Style management

## Future Enhancements

The UI system is designed to support:

- **Server-Side Rendering** (SSR)
- **Progressive Enhancement**
- **Web Components** compilation
- **React/Vue Integration**
- **Native Mobile** targets
- **Accessibility** optimizations
- **Performance Profiling** tools