#!/bin/bash
# Build script for FluentAI 3D Demo

echo "🔨 Building FluentAI 3D WASM Demo..."

# Ensure we have the wasm32 target
rustup target add wasm32-unknown-unknown

# Install wasm-pack if not already installed
if ! command -v wasm-pack &> /dev/null; then
    echo "📦 Installing wasm-pack..."
    curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
fi

# Build the library for WASM
echo "📦 Building WASM module..."
wasm-pack build --target web --out-dir examples/pkg --no-typescript

echo "✅ Build complete!"
echo ""
echo "To run the demo:"
echo "  1. cd examples"
echo "  2. python3 -m http.server 8080"
echo "  3. Open http://localhost:8080/3d_demo_real.html"

# Copy the HTML file to ensure paths are correct
echo "📄 Setting up demo files..."
cp examples/devtools_showcase.html examples/index.html

# Create a simple JavaScript wrapper
cat > examples/devtools_demo.js << 'EOF'
import init, { DevToolsShowcase } from './pkg/fluentai_renderer.js';

let showcase = null;
let animationId = null;

export async function initDevTools() {
    await init();
    
    showcase = new DevToolsShowcase('render-canvas');
    showcase.init_demo();
    
    // Set up keyboard event handling
    document.addEventListener('keydown', (e) => {
        showcase.handle_key(e.key, e.ctrlKey, e.shiftKey, e.altKey);
    });
    
    // Start render loop
    function render(timestamp) {
        showcase.render(timestamp);
        animationId = requestAnimationFrame(render);
    }
    render(0);
    
    return showcase;
}

export function addParticles(count = 50) {
    if (showcase) {
        showcase.add_particles(count);
    }
}

export function clearParticles() {
    if (showcase) {
        showcase.clear_particles();
    }
}

export function toggleAnimation() {
    if (showcase) {
        showcase.toggle_animation();
    }
}

export function getStats() {
    if (showcase) {
        return showcase.get_stats();
    }
    return null;
}

// Auto-initialize when module loads
window.addEventListener('DOMContentLoaded', () => {
    initDevTools().then(() => {
        console.log('DevTools showcase initialized!');
    }).catch(err => {
        console.error('Failed to initialize:', err);
    });
});
EOF

# Create a simple Python server script
cat > examples/serve_devtools.py << 'EOF'
#!/usr/bin/env python3
import http.server
import socketserver
import os

PORT = 8080
DIRECTORY = "."

class CORSHTTPRequestHandler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header('Cross-Origin-Embedder-Policy', 'require-corp')
        self.send_header('Cross-Origin-Opener-Policy', 'same-origin')
        self.send_header('Access-Control-Allow-Origin', '*')
        super().end_headers()

    def do_GET(self):
        # Set correct MIME types
        if self.path.endswith('.wasm'):
            self.send_response(200)
            self.send_header('Content-Type', 'application/wasm')
            self.end_headers()
            with open(self.path[1:], 'rb') as f:
                self.wfile.write(f.read())
        else:
            super().do_GET()

os.chdir(DIRECTORY)

with socketserver.TCPServer(("", PORT), CORSHTTPRequestHandler) as httpd:
    print(f"🚀 DevTools showcase server running at http://localhost:{PORT}/")
    print(f"📂 Serving files from: {os.getcwd()}")
    print("\nOpen http://localhost:8080/devtools_showcase.html in your browser")
    print("Press Ctrl+C to stop the server")
    httpd.serve_forever()
EOF

chmod +x examples/serve_devtools.py

echo "✅ Build complete!"
echo ""
echo "To run the demo:"
echo "  cd examples"
echo "  ./serve_devtools.py"
echo ""
echo "Then open http://localhost:8080/devtools_showcase.html"