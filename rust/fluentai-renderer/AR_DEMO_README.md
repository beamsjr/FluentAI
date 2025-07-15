# AR Living Cards Demo

An interactive AR dashboard demonstrating physics-enabled task cards in a Kanban board layout.

## Quick Start (HTML Mock)

The easiest way to see the demo is to open the HTML mock version:

1. Open `examples/ar_demo.html` in a web browser
2. The demo shows a simulated AR Kanban board with draggable cards

### Features in the HTML Mock:
- **Three columns**: Todo (red), In Progress (orange), Done (green)
- **Interactive cards**: Click and drag to move cards between columns
- **Debug mode**: Press 'D' to toggle physics visualization
- **Controls**: Add new cards, reset positions

## Full WASM Build (Advanced)

To build the full Rust/WASM version:

### Prerequisites
- Rust toolchain with `wasm32-unknown-unknown` target
- wasm-pack (`cargo install wasm-pack`)
- Python 3 (for local server)

### Build Steps

```bash
# 1. Add WASM target if not already installed
rustup target add wasm32-unknown-unknown

# 2. Make the build script executable and run it
chmod +x build_wasm.sh
./build_wasm.sh

# 3. Start the local server
./serve.py

# 4. Open http://localhost:8080/examples/ar_demo.html
```

## Demo Features

### Physics-Enabled Cards
- Cards have realistic physics simulation
- Drag to move cards smoothly
- Flick gesture support for throwing cards
- Automatic snapping to columns when released nearby

### Visual Debug Mode
Press 'D' to toggle debug visualization:
- **Green circles**: Physics body centers
- **Yellow boxes**: Collision boundaries
- **Magenta arrows**: Force/velocity vectors
- **Colored zones**: Column drop areas

### Gesture Support
- **Tap**: Select a card
- **Double Tap**: Open card details (future feature)
- **Long Press**: Context menu (future feature)
- **Drag**: Move cards around
- **Flick**: Throw cards with momentum
- **Pinch**: Scale the dashboard (future feature)

## Architecture

The demo consists of several modular components:

1. **AR Session** (`ar/ar_session.rs`): Manages the WebXR session and coordinates all subsystems
2. **Living Cards** (`ar/living_cards.rs`): Physics-enabled card system with Kanban logic
3. **Gesture Recognition** (`ar/gesture_recognition.rs`): Detects various touch gestures
4. **Spatial Anchors** (`ar/spatial_anchor.rs`): Anchors UI elements in AR space
5. **Debug Overlay** (`ar/debug_overlay.rs`): Visual debugging tools

## Extending the Demo

### Adding New Card Types
Modify `CardStatus` enum in `living_cards.rs` to add new columns or states.

### Custom Physics
Adjust physics parameters in `LivingCardsSystem::new()`:
- `physics_timestep`: Physics update frequency
- Card properties like `tap_threshold`, `swipe_threshold`, etc.

### New Gestures
Add gesture types to the `Gesture` enum and implement detection in `GestureRecognizer`.

## Troubleshooting

### WASM Build Issues
- Ensure all dependencies in `Cargo.toml` have WASM support
- Check that `wasm-bindgen` versions match between dependencies
- Use `--dev` flag with wasm-pack for faster builds during development

### Performance
- The debug overlay can impact performance; disable it for smoother experience
- Reduce physics update frequency if needed by adjusting `physics_timestep`

## Future Enhancements

- Real WebXR integration for true AR experience
- Persistence and cloud sync
- Multi-user collaboration
- Voice commands
- More card visualization options
- Integration with task management APIs