# FluentAI WASM Quick Start Guide

Get FluentAI running in your browser in under 5 minutes!

## Option 1: Use Pre-built Demo (Fastest)

1. Navigate to the pre-built demo:
```bash
cd /Users/joe/repos/claudelang/rust/fluentai-renderer/pkg-vm
```

2. Start a local server:
```bash
python3 -m http.server 8000
```

3. Open in browser:
```
http://localhost:8000/impressive_demo.html
```

That's it! You now have a fully functional FluentAI environment running in your browser.

## Option 2: Create Your Own Page

### Step 1: Copy the Required Files

Create a new directory and copy these files:
```bash
mkdir my-fluentai-app
cd my-fluentai-app

# Copy WASM files
cp path/to/pkg-vm/fluentai_renderer.js .
cp path/to/pkg-vm/fluentai_renderer_bg.wasm .
```

### Step 2: Create an HTML File

Create `index.html`:

```html
<!DOCTYPE html>
<html>
<head>
    <title>My FluentAI App</title>
    <style>
        body {
            font-family: Arial, sans-serif;
            margin: 20px;
            background: #f0f0f0;
        }
        #canvas {
            border: 2px solid #333;
            background: white;
            display: block;
            margin: 20px 0;
        }
        #code {
            width: 100%;
            height: 200px;
            font-family: monospace;
            font-size: 14px;
        }
        button {
            padding: 10px 20px;
            font-size: 16px;
            margin: 5px;
            cursor: pointer;
        }
        #output {
            margin-top: 20px;
            padding: 10px;
            background: #e0e0e0;
            border-radius: 5px;
            min-height: 50px;
        }
    </style>
</head>
<body>
    <h1>My FluentAI App</h1>
    
    <canvas id="canvas" width="800" height="400"></canvas>
    
    <textarea id="code">
// FluentAI Code - Edit me!
let colors = ["#FF6B6B", "#4ECDC4", "#45B7D1", "#FFA07A", "#98D8C8"];
let circles = [];

// Create random circles
let i = 0;
while (i < 20) {
    circles.push({
        type: "circle",
        x: Math.random() * 800,
        y: Math.random() * 400,
        radius: Math.random() * 30 + 10,
        color: colors[i % colors.len()]
    });
    i = i + 1;
}

// Add a title
circles.push({
    type: "text",
    x: 400,
    y: 50,
    text: "Welcome to FluentAI!",
    size: 32,
    color: "#333"
});

// Return the shapes to render
circles

// Math helper
let Math = {
    random: || 0.5
};
    </textarea>
    
    <div>
        <button onclick="runCode()">▶️ Run</button>
        <button onclick="clearCanvas()">🧹 Clear</button>
    </div>
    
    <div id="output"></div>
    
    <script type="module">
        import init, { create_fluentai_vm, test_compile } from './fluentai_renderer.js';
        
        let vm = null;
        const canvas = document.getElementById('canvas');
        const ctx = canvas.getContext('2d');
        const codeEditor = document.getElementById('code');
        const output = document.getElementById('output');
        
        // Initialize FluentAI
        async function initialize() {
            try {
                output.innerHTML = 'Initializing FluentAI...';
                await init();
                vm = await create_fluentai_vm(canvas);
                output.innerHTML = '✅ FluentAI Ready! Click Run to execute code.';
                
                // Run initial code
                runCode();
            } catch (error) {
                output.innerHTML = `❌ Error: ${error}`;
            }
        }
        
        // Run FluentAI code
        window.runCode = async function() {
            if (!vm) {
                output.innerHTML = '⚠️ VM not initialized yet...';
                return;
            }
            
            try {
                const code = codeEditor.value;
                const result = await vm.run(code);
                
                // Clear canvas
                ctx.clearRect(0, 0, canvas.width, canvas.height);
                
                // Render result
                if (Array.isArray(result)) {
                    renderShapes(result);
                    output.innerHTML = `✅ Rendered ${result.length} shapes`;
                } else {
                    output.innerHTML = `✅ Result: ${JSON.stringify(result)}`;
                }
            } catch (error) {
                output.innerHTML = `❌ Error: ${error}`;
            }
        }
        
        // Clear canvas
        window.clearCanvas = function() {
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            output.innerHTML = 'Canvas cleared';
        }
        
        // Render shapes to canvas
        function renderShapes(shapes) {
            shapes.forEach(shape => {
                ctx.save();
                
                switch(shape.type) {
                    case 'circle':
                        ctx.fillStyle = shape.color || '#000';
                        ctx.beginPath();
                        ctx.arc(shape.x, shape.y, shape.radius, 0, Math.PI * 2);
                        ctx.fill();
                        break;
                        
                    case 'rect':
                        ctx.fillStyle = shape.color || '#000';
                        ctx.fillRect(shape.x, shape.y, shape.width, shape.height);
                        break;
                        
                    case 'text':
                        ctx.fillStyle = shape.color || '#000';
                        ctx.font = `${shape.size || 16}px Arial`;
                        ctx.textAlign = shape.align || 'center';
                        ctx.fillText(shape.text, shape.x, shape.y);
                        break;
                        
                    case 'line':
                        ctx.strokeStyle = shape.color || '#000';
                        ctx.lineWidth = shape.width || 1;
                        ctx.beginPath();
                        ctx.moveTo(shape.x1, shape.y1);
                        ctx.lineTo(shape.x2, shape.y2);
                        ctx.stroke();
                        break;
                }
                
                ctx.restore();
            });
        }
        
        // Start the app
        initialize();
    </script>
</body>
</html>
```

### Step 3: Run Your App

1. Start a server in your app directory:
```bash
python3 -m http.server 8000
```

2. Open in browser:
```
http://localhost:8000
```

## Quick Examples

### 1. Animated Sine Wave
```fluentai
let points = [];
let x = 0;

while (x < 800) {
    let y = 200 + sin(x * 0.02) * 100;
    points.push({
        type: "circle",
        x: x,
        y: y,
        radius: 3,
        color: "blue"
    });
    x = x + 5;
}

points

// Simple sine approximation
private function sin(x: float) -> float {
    x - (x * x * x) / 6.0
}
```

### 2. Interactive Grid
```fluentai
let grid = [];
let size = 40;
let cols = 20;
let rows = 10;

let row = 0;
while (row < rows) {
    let col = 0;
    while (col < cols) {
        grid.push({
            type: "rect",
            x: col * size,
            y: row * size,
            width: size - 2,
            height: size - 2,
            color: if ((row + col) % 2 == 0) { "#4ECDC4" } else { "#FF6B6B" }
        });
        col = col + 1;
    }
    row = row + 1;
}

grid
```

### 3. Data Visualization
```fluentai
let data = [30, 50, 80, 40, 60, 90, 70];
let bars = [];
let barWidth = 100;
let maxHeight = 300;

let i = 0;
while (i < data.len()) {
    let height = (data[i] / 100.0) * maxHeight;
    bars.push({
        type: "rect",
        x: i * (barWidth + 10) + 50,
        y: 350 - height,
        width: barWidth,
        height: height,
        color: "#4ECDC4"
    });
    
    // Add value label
    bars.push({
        type: "text",
        x: i * (barWidth + 10) + 50 + barWidth / 2,
        y: 340 - height,
        text: data[i].to_string(),
        size: 16,
        color: "#333"
    });
    
    i = i + 1;
}

bars
```

## Next Steps

1. **Explore the Language**: Check out the [FluentAI Language Reference](../docs/LANGUAGE_REFERENCE.md)
2. **Try the Demos**: Load `impressive_demo.html` for more examples
3. **Build Something**: Create your own visualizations and share them!

## Troubleshooting

### Nothing appears when I run code
- Check the browser console for errors
- Make sure your shapes have valid coordinates
- Verify that you're returning an array of shape objects

### "VM not initialized" error
- Wait for the initialization to complete
- Check that the WASM files are loading correctly

### Performance issues
- Reduce the number of shapes
- Avoid creating very large arrays
- Use simpler calculations in loops

## Resources

- [Full Technical Guide](./WASM_VM_TECHNICAL_GUIDE.md)
- [API Reference](./WASM_VM_README.md)
- [Example Demos](./pkg-vm/impressive_demo.html)