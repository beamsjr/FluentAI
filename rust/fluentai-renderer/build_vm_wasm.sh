#!/bin/bash

# Build script for FluentAI VM WASM bindings

set -e

echo "Building FluentAI VM WASM module..."

# Build the WASM module
echo "Compiling to WASM..."
wasm-pack build --target web --out-dir pkg-vm \
    --out-name fluentai_renderer \
    --features "vm-integration" 

# Copy the demo files
echo "Copying demo files..."
cp examples/fluentai_vm_demo.html pkg-vm/
cp serve_demo.py pkg-vm/

# Create a simple index page
cat > pkg-vm/index.html << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>FluentAI WASM Demos</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            max-width: 800px;
            margin: 50px auto;
            padding: 20px;
        }
        h1 { color: #333; }
        .demo-list {
            list-style: none;
            padding: 0;
        }
        .demo-list li {
            margin: 15px 0;
            padding: 15px;
            background: #f5f5f5;
            border-radius: 8px;
        }
        .demo-list a {
            color: #007AFF;
            text-decoration: none;
            font-weight: 500;
        }
        .demo-list a:hover {
            text-decoration: underline;
        }
        .description {
            color: #666;
            margin-top: 5px;
            font-size: 14px;
        }
    </style>
</head>
<body>
    <h1>FluentAI WebAssembly Demos</h1>
    <p>Choose a demo to explore FluentAI's capabilities in the browser:</p>
    
    <ul class="demo-list">
        <li>
            <a href="fluentai_vm_demo.html">FluentAI VM Demo</a>
            <div class="description">
                Interactive demo of the FluentAI virtual machine running in WebAssembly.
                Write and execute FluentAI code directly in your browser!
            </div>
        </li>
        <li>
            <a href="ar_demo.html">AR Living Cards Demo</a>
            <div class="description">
                Augmented Reality task management dashboard with physics-enabled cards.
            </div>
        </li>
        <li>
            <a href="ar_living_cards_demo.html">AR Living Cards (Advanced)</a>
            <div class="description">
                Full-featured AR dashboard with gesture recognition and spatial anchors.
            </div>
        </li>
    </ul>
    
    <h2>Getting Started</h2>
    <p>These demos require a modern web browser with WebAssembly support.</p>
    <p>For the AR demos, you'll need a device with camera access or use mouse/touch input.</p>
</body>
</html>
EOF

echo "Build complete! Output in pkg-vm/"
echo ""
echo "To run the demos:"
echo "  cd pkg-vm"
echo "  python3 serve_demo.py"
echo ""
echo "Then open http://localhost:8000 in your browser"