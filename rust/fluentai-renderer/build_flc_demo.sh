#!/bin/bash

# Build script for AR Living Cards FLC demo

echo "Building AR Living Cards Demo with FluentAI Continuum..."
echo "=================================================="

# Check if we're in the right directory
if [ ! -f "Cargo.toml" ]; then
    echo "Error: Must run from fluentai-renderer directory"
    exit 1
fi

# Step 1: Compile the FluentAI runtime to WASM
echo "1. Building FluentAI runtime..."
cd ../
cargo build --target wasm32-unknown-unknown --release

# Step 2: Generate WASM bindings
echo "2. Generating WASM bindings..."
wasm-bindgen target/wasm32-unknown-unknown/release/fluentai_vm.wasm \
    --out-dir fluentai-renderer/examples/wasm \
    --target web \
    --no-typescript

# Step 3: Compile Continuum UI modules
echo "3. Building Continuum UI modules..."
cd fluentai-renderer
cargo build --target wasm32-unknown-unknown --release --features wasm

# Step 4: Package everything
echo "4. Packaging demo..."
mkdir -p examples/dist
cp examples/ar_living_cards.flc examples/dist/
cp examples/ar_living_cards_demo.html examples/dist/index.html

# Step 5: Create a simple server
cat > examples/dist/serve.py << 'EOF'
#!/usr/bin/env python3
import http.server
import socketserver
import os

PORT = 8080

class CORSHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Access-Control-Allow-Origin', '*')
        self.send_header('Access-Control-Allow-Methods', 'GET, POST, OPTIONS')
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        super().end_headers()

    def do_OPTIONS(self):
        self.send_response(200)
        self.end_headers()

    def guess_type(self, path):
        mimetype = super().guess_type(path)
        if path.endswith('.wasm'):
            return 'application/wasm'
        elif path.endswith('.flc'):
            return 'text/plain'
        return mimetype

os.chdir(os.path.dirname(os.path.abspath(__file__)))

with socketserver.TCPServer(("", PORT), CORSHTTPRequestHandler) as httpd:
    print(f"FluentAI Continuum Demo Server")
    print(f"==============================")
    print(f"Server running at: http://localhost:{PORT}")
    print(f"")
    print(f"The AR Living Cards demo showcases:")
    print(f"  • FluentAI Language (FLC) for logic")
    print(f"  • Continuum UI for 3D rendering") 
    print(f"  • Physics simulation for cards")
    print(f"  • Reactive state management")
    print(f"  • AR spatial anchoring")
    print(f"  • Gesture recognition")
    print(f"")
    print(f"Press Ctrl+C to stop the server")
    httpd.serve_forever()
EOF

chmod +x examples/dist/serve.py

echo ""
echo "Build complete!"
echo ""
echo "To run the demo:"
echo "  cd examples/dist"
echo "  ./serve.py"
echo ""
echo "Then open http://localhost:8080 in your browser"
echo ""
echo "Note: This demo uses FluentAI Language (FLC) compiled to WASM"
echo "      with Continuum UI for the visual rendering."