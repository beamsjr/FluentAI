# FluentAI WASM VM Technical Guide

## Architecture Overview

The FluentAI WASM VM integration consists of several key components that work together to enable FluentAI code execution in web browsers:

### 1. Core WASM Bindings (`fluentai_vm_wasm.rs`)

The main entry point exposes three key functions to JavaScript:

```rust
#[wasm_bindgen]
pub struct FluentAIVM {
    vm: VM,
    compiler: Compiler,
    canvas: Option<HtmlCanvasElement>,
}

#[wasm_bindgen]
impl FluentAIVM {
    pub fn run(&mut self, source: &str) -> Result<JsValue, JsValue>
    pub fn compile(&mut self, source: &str) -> Result<(), JsValue>
    pub fn execute(&mut self) -> Result<JsValue, JsValue>
}
```

### 2. Value Conversion Layer

FluentAI values are automatically converted to/from JavaScript values:

| FluentAI Type | JavaScript Type | Notes |
|---------------|-----------------|-------|
| `Value::Nil` | `null` | |
| `Value::Boolean` | `boolean` | |
| `Value::Integer` | `number` | |
| `Value::Float` | `number` | |
| `Value::String` | `string` | |
| `Value::List` | `Array` | Recursive conversion |
| `Value::Object` | `Object` | HashMap → JS Object |
| `Value::Function` | `"[Function]"` | Not directly callable from JS |

### 3. Canvas Integration

The VM can render directly to an HTML5 Canvas element:

```javascript
const canvas = document.getElementById('myCanvas');
const vm = await create_fluentai_vm(canvas);
```

Rendering commands in FluentAI code return objects that are interpreted by the JavaScript side:

```fluentai
// Returns a render command object
{
    type: "circle",
    x: 100,
    y: 100,
    radius: 50,
    color: "#FF0000"
}
```

## Building from Source

### Prerequisites

1. Install Rust and wasm-pack:
```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
```

2. Install Node.js for serving demos (optional)

### Build Steps

1. Navigate to the renderer directory:
```bash
cd rust/fluentai-renderer
```

2. Run the build script:
```bash
./build_vm_wasm.sh
```

This will:
- Compile the Rust code to WASM
- Generate JavaScript bindings
- Create TypeScript definitions
- Output everything to `pkg-vm/`

### Build Options

For a debug build (larger but with better error messages):
```bash
wasm-pack build --dev --target web --out-dir pkg-vm \
    --out-name fluentai_renderer \
    --features "vm-integration"
```

For a release build (optimized for size and speed):
```bash
wasm-pack build --target web --out-dir pkg-vm \
    --out-name fluentai_renderer \
    --features "vm-integration"
```

## API Reference

### JavaScript API

#### `init()`
Initializes the WASM module. Must be called before any other functions.

```javascript
import init from './fluentai_renderer.js';
await init();
```

#### `create_fluentai_vm(canvas?: HTMLCanvasElement): FluentAIVM`
Creates a new VM instance, optionally with a canvas for rendering.

```javascript
const vm = await create_fluentai_vm(document.getElementById('canvas'));
```

#### `FluentAIVM.run(source: string): any`
Compiles and executes FluentAI source code, returning the result.

```javascript
const result = await vm.run(`
    let x = 10;
    let y = 20;
    x + y
`);
console.log(result); // 30
```

#### `FluentAIVM.compile(source: string): void`
Compiles source code without executing it. Useful for syntax checking.

```javascript
try {
    await vm.compile(sourceCode);
    console.log("Code compiled successfully!");
} catch (error) {
    console.error("Compilation error:", error);
}
```

#### `FluentAIVM.execute(): any`
Executes previously compiled code.

```javascript
await vm.compile(sourceCode);
const result = await vm.execute();
```

#### `test_compile(source: string): string`
Tests compilation without creating a VM instance.

```javascript
const result = await test_compile("let x = 10; x + 5");
console.log(result); // "Compilation successful!"
```

## FluentAI Language Support in WASM

### Supported Features

✅ **Core Language**
- Variables and constants
- Functions and lambdas
- Control flow (if/else, match)
- Loops (while, for)
- Collections (lists, maps)
- String interpolation
- Pattern matching

✅ **Data Types**
- Primitives (int, float, bool, string)
- Collections (List, Map, Set)
- Objects (key-value pairs)
- Functions as values

✅ **Operations**
- Arithmetic operations
- Logical operations
- Collection operations (map, filter, reduce)
- String operations

### Currently Unsupported

❌ **Advanced Features**
- Module imports
- Async/await
- Effects system
- Standard library functions
- File I/O
- Network operations

## Performance Considerations

### Memory Management

The WASM module has a linear memory model with:
- Initial memory: 1MB
- Maximum memory: 16MB (configurable)
- Garbage collection: Handled by Rust's reference counting

### Optimization Tips

1. **Minimize Value Conversions**
   ```fluentai
   // Bad: Many small returns
   for i in range(100) {
       render_circle(i, i, 5)
   }
   
   // Good: Batch operations
   let shapes = range(100).map(i => {
       {type: "circle", x: i, y: i, radius: 5}
   });
   shapes
   ```

2. **Reuse Compiled Code**
   ```javascript
   // Compile once, execute multiple times
   await vm.compile(animationCode);
   
   function animate() {
       const result = vm.execute();
       renderShapes(result);
       requestAnimationFrame(animate);
   }
   ```

3. **Avoid Large Data Structures**
   Keep collections under 10,000 elements for optimal performance.

## Debugging

### Enable Console Logging

The WASM module includes console logging for debugging:

```javascript
// In your HTML
<script>
  // Enable debug logging
  window.FLUENTAI_DEBUG = true;
</script>
```

### Common Issues

1. **"Module not found" errors**
   - Ensure you're serving files via HTTP (not file://)
   - Check that all paths are relative to the HTML file

2. **"Out of memory" errors**
   - Reduce data structure sizes
   - Clear unused variables
   - Increase WASM memory limit

3. **Performance issues**
   - Use the release build
   - Batch rendering operations
   - Profile with browser DevTools

## Examples

### Animation Loop
```javascript
const animationCode = `
    let time = 0;
    let particles = create_particles(100);
    
    private function animate() {
        particles = update_particles(particles, time);
        time = time + 1;
        render_particles(particles)
    }
    
    animate()
`;

await vm.compile(animationCode);

function draw() {
    const shapes = vm.execute();
    renderToCanvas(shapes);
    requestAnimationFrame(draw);
}
draw();
```

### Interactive Application
```javascript
let mouseX = 0, mouseY = 0;

canvas.addEventListener('mousemove', (e) => {
    mouseX = e.offsetX;
    mouseY = e.offsetY;
});

const interactiveCode = `
    private function draw(mx: float, my: float) {
        [
            {type: "circle", x: mx, y: my, radius: 20, color: "#FF0000"}
        ]
    }
`;

await vm.compile(interactiveCode + '\ndraw');

function render() {
    vm.set_global("mouse_x", mouseX);
    vm.set_global("mouse_y", mouseY);
    const shapes = vm.call_function("draw", [mouseX, mouseY]);
    renderToCanvas(shapes);
    requestAnimationFrame(render);
}
```

## Security Considerations

The WASM module runs in a sandboxed environment with:
- No filesystem access
- No network access (unless through browser APIs)
- No ability to execute arbitrary system calls
- Memory isolation from other WASM modules

However, be aware that:
- User-provided code can create infinite loops
- Large computations can freeze the browser tab
- Memory usage is limited but not prevented

## Future Enhancements

### Planned Features

1. **WebWorker Support**
   - Run VM in a worker thread
   - Non-blocking execution
   - Better performance for long-running code

2. **Streaming Compilation**
   - Compile large programs incrementally
   - Faster initial load times

3. **Source Maps**
   - Map runtime errors to source locations
   - Better debugging experience

4. **WASM SIMD**
   - Use SIMD instructions for math operations
   - Significant performance improvements

## Contributing

To contribute to the WASM module:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Run the build and verify it works
6. Submit a pull request

### Development Workflow

```bash
# Make changes to Rust code
edit src/fluentai_vm_wasm.rs

# Build and test
./build_vm_wasm.sh
cd pkg-vm
python3 -m http.server 8000

# Open browser to test
open http://localhost:8000/impressive_demo.html
```

## Resources

- [FluentAI Language Reference](../docs/LANGUAGE_REFERENCE.md)
- [WebAssembly MDN Documentation](https://developer.mozilla.org/en-US/docs/WebAssembly)
- [wasm-bindgen Book](https://rustwasm.github.io/wasm-bindgen/)