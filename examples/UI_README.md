# Running FluentAI UI Examples

This guide explains how to run FluentAI UI applications.

## Quick Start - Demo HTML

The easiest way to see FluentAI UI in action is to open the pre-built demo:

```bash
# Open in your default browser
open examples/ui_demo.html

# Or use Python's built-in server
cd examples
python3 -m http.server 8000
# Then visit http://localhost:8000/ui_demo.html
```

This demonstrates a fully functional todo application built with FluentAI's UI system.

## Compiling FluentAI UI Code

To compile your own FluentAI UI files to JavaScript:

```bash
# Compile to vanilla JavaScript
python scripts/compile_ui.py examples/ui_demo.ai output.js

# Compile to React
python scripts/compile_ui.py examples/ui_demo.ai output.js --format=react

# Compile to Vue
python scripts/compile_ui.py examples/ui_demo.ai output.js --format=vue
```

## Running in Development

### Option 1: Using the FluentAI REPL

```bash
# Start the REPL
python -m fluentai repl

# Load and run UI code
> (load "examples/ui_demo.ai")
> (main)
```

### Option 2: Direct Execution

```bash
# Run a FluentAI UI file
python -m fluentai run examples/ui_demo.ai
```

## Creating Your Own UI App

1. **Create a new .ai file:**

```lisp
; my-app.ai
(module my-app
  (export [create-app]))

(define create-app
  (lambda ()
    (let ((message (effect reactive:ref "Hello, World!")))
      (lambda ()
        (dom:h "div" {}
          [(dom:h "h1" {} [(dom:text (effect reactive:get message))])
           (dom:h "button"
             {:onClick (lambda ()
                        (effect reactive:set message "Clicked!"))}
             [(dom:text "Click me")])])))))

(define main
  (lambda ()
    (let ((app (create-app)))
      (effect dom:render (app) "#app"))))
```

2. **Create an HTML file:**

```html
<!DOCTYPE html>
<html>
<head>
    <title>My FluentAI App</title>
</head>
<body>
    <div id="app"></div>
    <script src="fluentai-runtime.js"></script>
    <script src="my-app.js"></script>
</body>
</html>
```

3. **Compile and run:**

```bash
python scripts/compile_ui.py my-app.ai my-app.js
open my-app.html
```

## Using with Build Tools

### Webpack

```javascript
// webpack.config.js
module.exports = {
  module: {
    rules: [
      {
        test: /\.ai$/,
        use: 'fluentai-loader'
      }
    ]
  }
};
```

### Vite

```javascript
// vite.config.js
import fluentai from 'vite-plugin-fluentai';

export default {
  plugins: [fluentai()]
};
```

## Browser DevTools Integration

FluentAI UI apps work with browser DevTools:

- **React DevTools**: When compiled with `--format=react`
- **Vue DevTools**: When compiled with `--format=vue`
- **Redux DevTools**: For state debugging with FluentAI's reactive system

## Performance Tips

1. **Use production builds:**
   ```bash
   python scripts/compile_ui.py app.ai app.js --minify
   ```

2. **Enable code splitting:**
   ```lisp
   (import-lazy "./heavy-component.ai")
   ```

3. **Memoize expensive computations:**
   ```lisp
   (let ((result (effect reactive:computed
                   (memoize expensive-function))))
     ...)
   ```

## Debugging

1. **Enable source maps:**
   ```bash
   python scripts/compile_ui.py app.ai app.js --source-maps
   ```

2. **Use browser console:**
   ```lisp
   (effect io:print "Debug:" value)  ; Logs to console
   ```

3. **React/Vue DevTools** for component inspection

## Example Apps

- `ui_demo.ai` - Todo application with reactive state
- `ui_demo.html` - Pre-built HTML demo
- More examples coming soon!

## Next Steps

- Read the [UI Documentation](../docs/UI.md)
- Explore the [Component API](../docs/COMPONENTS.md)
- Learn about [Reactive State](../docs/REACTIVE.md)
- See [Performance Optimization](../docs/UI_PERFORMANCE.md)