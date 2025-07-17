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
