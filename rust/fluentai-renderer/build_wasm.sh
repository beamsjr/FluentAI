#!/bin/bash

# Build the WASM module
echo "Building FluentAI Renderer for WebAssembly..."

# Install wasm-pack if not already installed
if ! command -v wasm-pack &> /dev/null; then
    echo "Installing wasm-pack..."
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

# Build the WASM module
wasm-pack build --target web --out-dir www/pkg

echo "Build complete! Output in www/pkg/"
echo "To run the demo:"
echo "  1. cd www"
echo "  2. python3 -m http.server 8000"
echo "  3. Open http://localhost:8000 in your browser"