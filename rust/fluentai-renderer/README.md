# FluentAI Renderer

High-performance 3D rendering engine for Continuum UI with WebAssembly support.

## Overview

The FluentAI Renderer provides:
- 🎨 **3D Rendering**: WebGPU-based rendering for desktop
- 🌐 **WebAssembly Support**: Run FluentAI code directly in browsers
- 🎯 **AR Capabilities**: Augmented reality features for web
- ⚡ **Real-time Performance**: Optimized for smooth animations
- 🔧 **VM Integration**: Full FluentAI language execution in WASM

## Quick Start

### Running the WebAssembly Demo

1. Build the WASM module:
```bash
./build_vm_wasm.sh
```

2. Start the demo server:
```bash
cd pkg-vm
python3 -m http.server 8000
```

3. Open in browser:
```
http://localhost:8000/impressive_demo.html
```

For more details, see the [WASM Quick Start Guide](./WASM_QUICK_START.md).

## Documentation

- 📚 [WASM Quick Start Guide](./WASM_QUICK_START.md) - Get started in 5 minutes
- 🔧 [WASM Technical Guide](./WASM_VM_TECHNICAL_GUIDE.md) - Detailed technical documentation
- 📖 [WASM VM README](./WASM_VM_README.md) - API reference and examples
- 📋 [Integration Plan](../FLUENTAI_WASM_INTEGRATION_PLAN.md) - Roadmap and future features

## Features

### WebAssembly Module

The renderer includes a complete FluentAI VM compiled to WebAssembly:

- ✅ Full language parsing and compilation
- ✅ Real-time code execution
- ✅ Canvas-based rendering
- ✅ Interactive demos with live coding
- ✅ Performance monitoring

### Native Rendering (Desktop)

- WebGPU-based 3D rendering
- Text rendering with font support
- Reactive layout system
- Physics integration with Rapier2D
- Component-based UI system

### Web Features

- WebGL fallback for broader compatibility
- AR support via WebXR
- Responsive canvas rendering
- Touch and gesture support

## Building from Source

### Prerequisites

- Rust 1.70+
- wasm-pack (for WebAssembly builds)
- Python 3 (for serving demos)

### Native Build

```bash
cargo build --release
```

### WebAssembly Build

```bash
./build_vm_wasm.sh
```

This creates the WASM module in `pkg-vm/` with:
- `fluentai_renderer.js` - JavaScript bindings
- `fluentai_renderer_bg.wasm` - WebAssembly module  
- `impressive_demo.html` - Interactive demo
- `minimal_test.html` - Test suite

## Examples

### Basic FluentAI in Browser

```javascript
import init, { create_fluentai_vm } from './fluentai_renderer.js';

await init();
const vm = await create_fluentai_vm();

const result = await vm.run(`
    let x = 10;
    let y = 20;
    x + y
`);
console.log(result); // 30
```

### Canvas Rendering

```javascript
const canvas = document.getElementById('myCanvas');
const vm = await create_fluentai_vm(canvas);

await vm.run(`
    let shapes = [
        {type: "circle", x: 100, y: 100, radius: 50, color: "red"},
        {type: "rect", x: 200, y: 50, width: 100, height: 100, color: "blue"}
    ];
    shapes
`);
```

## Architecture

```
fluentai-renderer/
├── src/
│   ├── lib.rs                 # Main library entry
│   ├── renderer.rs            # Native WebGPU renderer
│   ├── webgl_renderer.rs      # WebGL fallback
│   ├── fluentai_vm_wasm.rs   # WASM VM bindings
│   ├── ar/                    # AR features
│   ├── components/            # UI components
│   └── ...
├── pkg-vm/                    # WASM build output
│   ├── fluentai_renderer.js
│   ├── fluentai_renderer_bg.wasm
│   └── impressive_demo.html
└── examples/                  # Example applications
```

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Submit a pull request

## License

See the main FluentAI repository for license information.