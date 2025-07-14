# Continuum Language Design

## Overview

Continuum extends FluentAI with declarative UI constructs that compile down to Dom effects. The language emphasizes physics-based layouts and reactive state management.

## Core Constructs

### 1. Surface

A `surface` is a 2D or 3D rendering context. It's the top-level container for UI elements.

```flc
surface my_app {
    // Surface properties
    size: (800, 600)
    background: "#f0f0f0"
    
    // Child elements
    element title {
        // ...
    }
}
```

### 2. Space

A `space` is a 3D container that can hold elements in 3D coordinates.

```flc
space ar_scene {
    // Space properties
    anchor: world.floor
    size: (2, 2, 2)  // meters
    
    // 3D elements
    element globe {
        content: model("earth.glb")
        position: (0, 1, 0)
        scale: 0.5
    }
}
```

### 3. Element

An `element` is a visual component that can be 2D or 3D.

```flc
element button {
    // Content
    content: "Click me"
    
    // Layout properties (Phase 2)
    position: (100, 100)
    size: (100, 40)
    
    // Styling
    color: "#ffffff"
    background: "#007bff"
    radius: 4
    
    // Interactions
    on_click: disturb menu_state
}
```

### 4. State Fields (Phase 3)

State fields define reactive state that drives UI updates.

```flc
state_field menu_open: bool = false

element menu {
    // Conditional visibility
    when menu_open == true {
        visible: true
        opacity: 1.0
    }
    when menu_open == false {
        visible: false
        opacity: 0.0
    }
}
```

### 5. Layout Properties (Phase 3)

Physics-based layout properties:

```flc
element item {
    // Attraction to other elements
    attraction_to: parent.center
    attraction_strength: 0.8
    
    // Repulsion from siblings
    repulsion_from: siblings
    repulsion_distance: 50
    
    // Constraints
    constrain_to: parent.bounds
    
    // Springs and joints
    spring_to: anchor_point
    spring_stiffness: 0.5
}
```

## Compilation Strategy

The Continuum syntax will be compiled to FluentAI code that uses Dom effects:

```flc
// Continuum source
surface app {
    element hello {
        content: "Hello, World!"
        position: (100, 100)
        color: "#000000"
    }
}

// Compiles to:
private function __continuum_app() {
    let elements = [
        {
            "type": "text",
            "content": "Hello, World!",
            "position": [100, 100],
            "color": "#000000"
        }
    ];
    
    perform Dom.render(elements);
}

__continuum_app();
```

## Type System Integration

Continuum elements will have types:

```flc
// Element type definition
type Button = element {
    content: string,
    on_click: () -> void,
    disabled?: bool
}

// Usage
element submit_button: Button {
    content: "Submit"
    on_click: () => handle_submit()
    disabled: form_invalid
}
```

## Module System

Continuum components can be exported and imported:

```flc
// components.flc
module components;

public surface button_component(text: string, handler: () -> void) {
    element button {
        content: text
        on_click: handler
        // styling...
    }
}

// main.flc
use components::button_component;

surface app {
    button_component("Click me", () => $(""Clicked!").print())
}
```