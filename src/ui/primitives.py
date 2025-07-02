"""
UI Primitives for ClaudeLang

This module registers UI-related primitive operations that can be used
directly in ClaudeLang code to build interactive web applications.
"""

from typing import Dict, Callable, Any, List
from ..core.ast import Function, EffectType, TypeAnnotation
from ..effects.dom_handler import VNode


class UIPrimitiveRegistry:
    """Registry for UI primitive operations"""
    
    def __init__(self):
        self.primitives: Dict[str, Function] = {}
        self.implementations: Dict[str, Callable] = {}
        self._register_ui_primitives()
    
    def register(self, name: str, func: Function, implementation: Callable):
        """Register a UI primitive"""
        self.primitives[name] = func
        self.implementations[name] = implementation
    
    def _register_ui_primitives(self):
        """Register all UI primitives"""
        
        # DOM creation primitives
        self.register(
            "dom:h",
            Function(
                name="dom:h",
                arity=3,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("String"),  # tag
                        TypeAnnotation("Dict"),    # props
                        TypeAnnotation("List"),    # children
                        TypeAnnotation("VNode")    # return
                    ],
                    effects={EffectType.DOM}
                )
            ),
            lambda tag, props, children: VNode(tag=tag, props=props or {}, children=children or [])
        )
        
        self.register(
            "dom:text",
            Function(
                name="dom:text",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Any"),
                        TypeAnnotation("String")
                    ]
                )
            ),
            lambda content: str(content)
        )
        
        self.register(
            "dom:fragment",
            Function(
                name="dom:fragment",
                arity=1,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List"),
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            lambda children: VNode(tag='fragment', children=children or [])
        )
        
        # Component primitives
        self.register(
            "ui:component",
            Function(
                name="ui:component",
                arity=2,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("String"),   # component name
                        TypeAnnotation("Dict"),     # props
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            self._create_component_vnode
        )
        
        # Reactive primitives
        self.register(
            "ui:ref",
            Function(
                name="ui:ref",
                arity=1,
                effects={EffectType.STATE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Any"),
                        TypeAnnotation("Ref")
                    ]
                )
            ),
            lambda initial: {"type": "ref", "value": initial}
        )
        
        self.register(
            "ui:computed",
            Function(
                name="ui:computed",
                arity=1,
                effects={EffectType.STATE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Function"),
                        TypeAnnotation("Computed")
                    ]
                )
            ),
            lambda fn: {"type": "computed", "fn": fn}
        )
        
        self.register(
            "ui:watch",
            Function(
                name="ui:watch",
                arity=2,
                effects={EffectType.STATE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List"),     # dependencies
                        TypeAnnotation("Function"), # callback
                        TypeAnnotation("Watcher")
                    ]
                )
            ),
            lambda deps, fn: {"type": "watcher", "deps": deps, "fn": fn}
        )
        
        # Event handling
        self.register(
            "ui:on",
            Function(
                name="ui:on",
                arity=2,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("String"),    # event name
                        TypeAnnotation("Function"),  # handler
                        TypeAnnotation("EventHandler")
                    ]
                )
            ),
            lambda event, handler: {"type": "event", "name": event, "handler": handler}
        )
        
        # Style primitives
        self.register(
            "ui:style",
            Function(
                name="ui:style",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Dict"),
                        TypeAnnotation("Style")
                    ]
                )
            ),
            self._create_style_object
        )
        
        self.register(
            "ui:class",
            Function(
                name="ui:class",
                arity=-1,  # Variadic
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[],  # Dynamic
                    effects={EffectType.PURE}
                )
            ),
            self._create_class_string
        )
        
        # Conditional rendering
        self.register(
            "ui:if",
            Function(
                name="ui:if",
                arity=3,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Bool"),
                        TypeAnnotation("VNode"),
                        TypeAnnotation("VNode"),
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            lambda cond, then_vnode, else_vnode: then_vnode if cond else else_vnode
        )
        
        self.register(
            "ui:when",
            Function(
                name="ui:when",
                arity=2,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Bool"),
                        TypeAnnotation("VNode"),
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            lambda cond, vnode: vnode if cond else VNode(tag='fragment')
        )
        
        # List rendering
        self.register(
            "ui:for",
            Function(
                name="ui:for",
                arity=2,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List"),
                        TypeAnnotation("Function"),
                        TypeAnnotation("List")
                    ]
                )
            ),
            lambda items, render_fn: [render_fn(item, idx) for idx, item in enumerate(items)]
        )
        
        # Portal rendering
        self.register(
            "ui:portal",
            Function(
                name="ui:portal",
                arity=2,
                effects={EffectType.DOM},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("VNode"),
                        TypeAnnotation("String"),  # target selector
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            lambda vnode, target: VNode(
                tag='portal',
                props={'target': target},
                children=[vnode]
            )
        )
        
        # Suspense/loading states
        self.register(
            "ui:suspense",
            Function(
                name="ui:suspense",
                arity=2,
                effects={EffectType.DOM, EffectType.ASYNC},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Promise"),
                        TypeAnnotation("VNode"),  # fallback
                        TypeAnnotation("VNode")
                    ]
                )
            ),
            lambda promise, fallback: VNode(
                tag='suspense',
                props={'promise': promise},
                children=[fallback]
            )
        )
    
    def _create_component_vnode(self, name: str, props: Dict[str, Any]) -> VNode:
        """Create a VNode for a component"""
        return VNode(
            tag=f'component:{name}',
            props=props
        )
    
    def _create_style_object(self, styles: Dict[str, Any]) -> str:
        """Convert style dict to CSS string"""
        css_parts = []
        for key, value in styles.items():
            # Convert camelCase to kebab-case
            css_key = ''.join(['-' + c.lower() if c.isupper() else c for c in key]).lstrip('-')
            css_parts.append(f"{css_key}: {value}")
        return '; '.join(css_parts)
    
    def _create_class_string(self, *args) -> str:
        """Create class string from various inputs"""
        classes = []
        
        for arg in args:
            if isinstance(arg, str):
                classes.append(arg)
            elif isinstance(arg, list):
                classes.extend(arg)
            elif isinstance(arg, dict):
                # Object form: {className: condition}
                for cls, condition in arg.items():
                    if condition:
                        classes.append(cls)
        
        return ' '.join(filter(None, classes))


# Global UI primitive registry
UI_PRIMITIVES = UIPrimitiveRegistry()


def register_ui_primitives(primitive_registry):
    """Register UI primitives with the main primitive registry"""
    ui_registry = UI_PRIMITIVES
    
    for name, func in ui_registry.primitives.items():
        primitive_registry.register(
            name,
            func,
            ui_registry.implementations[name]
        )