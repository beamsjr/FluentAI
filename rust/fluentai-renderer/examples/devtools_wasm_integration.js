// This file shows how to properly integrate the WASM-built DevTools showcase
// with the HTML demo, using our actual FluentAI rendering system

import init, { DevToolsShowcase } from './pkg/fluentai_renderer.js';

let showcase = null;
let animationId = null;

export async function initDevTools() {
    try {
        // Initialize the WASM module
        await init();
        
        // Create the DevTools showcase using our actual renderer
        showcase = new DevToolsShowcase('render-canvas');
        showcase.init_demo();
        
        // Set up keyboard event handling to pass to WASM
        document.addEventListener('keydown', (e) => {
            showcase.handle_key(e.key, e.ctrlKey, e.shiftKey, e.altKey);
        });
        
        // Connect UI controls to WASM methods
        connectControls();
        
        // Start the render loop
        function render(timestamp) {
            showcase.render(timestamp);
            updateStatsFromWASM();
            animationId = requestAnimationFrame(render);
        }
        render(0);
        
        console.log('FluentAI DevTools WASM initialized successfully!');
        return showcase;
        
    } catch (error) {
        console.error('Failed to initialize WASM:', error);
        throw error;
    }
}

function connectControls() {
    // Particle count slider
    const countSlider = document.getElementById('particleCount');
    if (countSlider) {
        countSlider.addEventListener('input', function() {
            const count = parseInt(this.value);
            // Clear and regenerate with new count
            showcase.clear_particles();
            for (let i = 0; i < count; i++) {
                showcase.add_particles(1);
            }
            document.getElementById('particleCountValue').textContent = count;
        });
    }
    
    // Regenerate button
    window.regenerateParticles = function() {
        const count = parseInt(document.getElementById('particleCount').value);
        showcase.clear_particles();
        showcase.add_particles(count);
    };
    
    // Clear button
    window.clearParticles = function() {
        showcase.clear_particles();
    };
    
    // Toggle animation
    window.toggleAnimation = function() {
        showcase.toggle_animation();
    };
    
    // Tool activation dropdown
    window.activateTool = function(tool) {
        // Simulate keyboard shortcuts through WASM
        switch(tool) {
            case 'devtools':
                showcase.handle_key('F12', false, false, false);
                break;
            case 'inspector':
                showcase.handle_key('I', true, true, false);
                break;
            case 'profiler':
                showcase.handle_key('P', true, true, false);
                break;
            case 'hotreload':
                showcase.handle_key('r', true, false, false);
                break;
            case 'debug':
                showcase.handle_key('d', false, false, false);
                break;
        }
        document.getElementById('toolsDropdown').value = '';
    };
}

function updateStatsFromWASM() {
    if (!showcase) return;
    
    // Get stats from WASM
    const stats = showcase.get_stats();
    
    // Update DOM with real values from our renderer
    document.getElementById('fps').textContent = Math.round(stats.fps);
    document.getElementById('drawCalls').textContent = stats.drawCalls;
    document.getElementById('memory').textContent = stats.memory.toFixed(1) + ' MB';
    document.getElementById('components').textContent = stats.componentCount;
}

// Replace the mock startDemo with real WASM initialization
window.startDemo = async function() {
    document.getElementById('welcome').style.opacity = '0';
    
    // Show loading
    document.getElementById('loading').style.display = 'block';
    
    try {
        // Initialize WASM
        await initDevTools();
        
        // Hide loading and welcome
        setTimeout(() => {
            document.getElementById('loading').style.display = 'none';
            document.getElementById('welcome').style.display = 'none';
            document.getElementById('controlPanel').style.display = 'block';
            document.getElementById('componentPanel').style.display = 'block';
            
            // The canvas is now controlled by our WASM renderer!
        }, 500);
        
    } catch (error) {
        document.getElementById('loading').style.display = 'none';
        showError('Failed to initialize FluentAI renderer: ' + error.message);
    }
};

// Auto-start when DOM is ready
if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', () => {
        // Wait for user to click start
    });
} else {
    // DOM already loaded
}