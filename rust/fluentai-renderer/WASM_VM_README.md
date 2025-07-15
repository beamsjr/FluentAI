# FluentAI VM WebAssembly Integration

This document describes how to use the FluentAI VM in web browsers through WebAssembly.

## Overview

The FluentAI VM has been successfully compiled to WebAssembly, allowing you to run FluentAI code directly in web browsers. This integration provides:

- Full FluentAI language parsing and compilation
- VM execution in the browser
- Basic rendering capabilities through Canvas API
- JavaScript interoperability

## Building the WASM Module

To build the FluentAI VM for WebAssembly:

```bash
cd fluentai-renderer
./build_vm_wasm.sh
```

This creates a `pkg-vm/` directory containing:
- `fluentai_renderer.js` - JavaScript bindings
- `fluentai_renderer_bg.wasm` - WebAssembly module
- `fluentai_renderer.d.ts` - TypeScript definitions
- Demo HTML files

## Usage

### Basic Setup

```html
<!DOCTYPE html>
<html>
<head>
    <title>FluentAI VM Demo</title>
</head>
<body>
    <canvas id="myCanvas" width="800" height="600"></canvas>
    
    <script type="module">
        import init, { create_fluentai_vm } from './fluentai_renderer.js';
        
        async function main() {
            // Initialize WASM module
            await init();
            
            // Create VM instance with canvas
            const canvas = document.getElementById('myCanvas');
            const vm = await create_fluentai_vm(canvas);
            
            // Run FluentAI code
            const code = `
                private function main() {
                    let x = 10;
                    let y = 20;
                    x + y
                }
                main()
            `;
            
            try {
                const result = await vm.run(code);
                console.log('Result:', result);
            } catch (error) {
                console.error('Error:', error);
            }
        }
        
        main();
    </script>
</body>
</html>
```

### API Reference

#### `create_fluentai_vm(canvas: HTMLCanvasElement): FluentAIVM`
Creates a new VM instance with a canvas for rendering.

#### `vm.run(source: string): any`
Compiles and executes FluentAI source code, returning the result.

#### `vm.compile(source: string): void`
Compiles FluentAI source code without executing it.

#### `vm.execute(): any`
Executes previously compiled code.

#### `vm.call_function(name: string, args: any): any`
Calls a global function by name with the given arguments.

#### `vm.set_global(name: string, value: any): void`
Sets a global variable in the VM.

#### `vm.get_global(name: string): any`
Gets a global variable from the VM.

#### `test_compile(source: string): string`
Tests compilation without creating a VM instance.

## Supported Features

### Currently Supported:
- ✅ Core language features (variables, functions, lambdas)
- ✅ Collections (lists, maps)
- ✅ Control flow (if/else, pattern matching)
- ✅ Basic arithmetic and logic operations
- ✅ Function calls and recursion
- ✅ JSON-compatible data structures

### Not Yet Supported in WASM:
- ❌ Effects system (IO, Network, etc.)
- ❌ Standard library functions
- ❌ Async/await operations
- ❌ Module imports
- ❌ Native rendering (uses Canvas 2D instead)

## Examples

### Basic Math
```javascript
const result = await vm.run(`
    let a = 10;
    let b = 20;
    a + b
`);
// Result: 30
```

### Functions
```javascript
const result = await vm.run(`
    private function factorial(n: int) -> int {
        if (n <= 1) { 1 }
        else { n * factorial(n - 1) }
    }
    factorial(5)
`);
// Result: 120
```

### Collections
```javascript
const result = await vm.run(`
    let numbers = [1, 2, 3, 4, 5];
    let doubled = numbers.map(x => x * 2);
    let sum = numbers.reduce(0, (acc, x) => acc + x);
    {
        "doubled": doubled,
        "sum": sum
    }
`);
// Result: { doubled: [2, 4, 6, 8, 10], sum: 15 }
```

## Running the Demos

1. Navigate to the build output:
   ```bash
   cd pkg-vm
   ```

2. Start the demo server:
   ```bash
   python3 serve_demo.py
   ```

3. Open in browser:
   - Main demo: http://localhost:8000/fluentai_vm_demo.html
   - Simple test: http://localhost:8000/simple_test.html

## Limitations

1. **No Effects System**: The WASM build excludes the effects system to avoid dependencies on tokio and other native-only libraries.

2. **Limited Standard Library**: Most stdlib functions are not available in WASM.

3. **Performance**: While functional, the WASM version may be slower than native execution.

4. **Memory Management**: WASM has memory limitations compared to native execution.

## Future Improvements

- [ ] Implement browser-specific effects (DOM manipulation, fetch API)
- [ ] Add WebGL rendering support
- [ ] Optimize compilation and execution performance
- [ ] Support for more standard library functions
- [ ] Better error messages and debugging support

## Troubleshooting

### Module not found errors
Ensure you're serving the files through a web server (not file://) due to CORS restrictions.

### Initialization errors
Check the browser console for detailed error messages. Modern browsers with WebAssembly support are required.

### Performance issues
For better performance, use the release build (which is the default) and avoid creating large data structures.