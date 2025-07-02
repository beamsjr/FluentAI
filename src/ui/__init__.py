"""
ClaudeLang UI Module

This module provides web UI capabilities for ClaudeLang, including:
- Virtual DOM implementation
- Component system with lifecycle hooks
- Reactive state management
- UI primitives for building interfaces
"""

from .components import (
    Component,
    FunctionalComponent,
    create_component,
    register_component,
    get_component,
    h,
    fragment,
    text,
    Button,
    Input,
    PropDefinition,
    LifecyclePhase
)

from .primitives import (
    UI_PRIMITIVES,
    register_ui_primitives
)

__all__ = [
    # Component system
    'Component',
    'FunctionalComponent',
    'create_component',
    'register_component',
    'get_component',
    'PropDefinition',
    'LifecyclePhase',
    
    # VNode helpers
    'h',
    'fragment',
    'text',
    
    # Example components
    'Button',
    'Input',
    
    # Primitives
    'UI_PRIMITIVES',
    'register_ui_primitives'
]