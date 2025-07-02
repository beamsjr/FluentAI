"""
UI and DOM Primitives for ClaudeLang

This module defines all UI-related primitive functions that are available
in ClaudeLang for building user interfaces.
"""

from typing import Any, Dict, List, Optional, Callable, TYPE_CHECKING
from .ast import Function, EffectType, TypeAnnotation

if TYPE_CHECKING:
    from .primitives import PrimitiveRegistry


class UIPrimitives:
    """Registry of UI-specific primitive functions"""
    
    def __init__(self):
        self.registry = {}
        self._register_dom_primitives()
        self._register_ui_primitives()
        self._register_component_primitives()
        self._register_prop_primitives()
    
    def _register_dom_primitives(self):
        """Register DOM manipulation primitives"""
        
        # dom:h - Create virtual DOM nodes
        self.registry["dom:h"] = Function(
            name="dom:h",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),      # tag
                    TypeAnnotation("Map"),         # props
                    TypeAnnotation("List"),        # children
                    TypeAnnotation("VNode")        # return
                ]
            )
        )
        
        # dom:text - Create text nodes
        self.registry["dom:text"] = Function(
            name="dom:text",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("VNode")
                ]
            )
        )
        
        # dom:fragment - Create fragment nodes
        self.registry["dom:fragment"] = Function(
            name="dom:fragment",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List"),
                    TypeAnnotation("VNode")
                ]
            )
        )
        
        # dom:portal - Render to different DOM location
        self.registry["dom:portal"] = Function(
            name="dom:portal",
            arity=2,
            effects={EffectType.DOM},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("VNode"),
                    TypeAnnotation("String"),  # selector
                    TypeAnnotation("VNode")
                ]
            )
        )
    
    def _register_ui_primitives(self):
        """Register UI helper primitives"""
        
        # ui:ref - Create reactive reference
        self.registry["ui:ref"] = Function(
            name="ui:ref",
            arity=1,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Ref", [TypeAnnotation("a")])
                ]
            )
        )
        
        # ui:computed - Create computed value
        self.registry["ui:computed"] = Function(
            name="ui:computed",
            arity=1,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("ComputedRef")
                ]
            )
        )
        
        # ui:watch - Watch reactive values
        self.registry["ui:watch"] = Function(
            name="ui:watch",
            arity=-1,  # Variadic: deps array + callback + options
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List"),
                    TypeAnnotation("Function"),
                    TypeAnnotation("Unsubscribe")
                ]
            )
        )
        
        # ui:class - Dynamic class binding
        self.registry["ui:class"] = Function(
            name="ui:class",
            arity=-1,  # Variadic
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("...Any"),
                    TypeAnnotation("String")
                ]
            )
        )
        
        # ui:style - Dynamic style binding
        self.registry["ui:style"] = Function(
            name="ui:style",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Map"),
                    TypeAnnotation("String")
                ]
            )
        )
        
        # ui:if - Conditional rendering
        self.registry["ui:if"] = Function(
            name="ui:if",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Bool"),
                    TypeAnnotation("VNode"),
                    TypeAnnotation("VNode"),
                    TypeAnnotation("VNode")
                ]
            )
        )
        
        # ui:when - Conditional display (no else)
        self.registry["ui:when"] = Function(
            name="ui:when",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Bool"),
                    TypeAnnotation("VNode"),
                    TypeAnnotation("VNode")
                ]
            )
        )
        
        # ui:for - List rendering
        self.registry["ui:for"] = Function(
            name="ui:for",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Function", [
                        TypeAnnotation("a"),
                        TypeAnnotation("VNode")
                    ]),
                    TypeAnnotation("List", [TypeAnnotation("VNode")])
                ]
            )
        )
        
        # ui:key - Set key for list items
        self.registry["ui:key"] = Function(
            name="ui:key",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("VNode"),
                    TypeAnnotation("String"),
                    TypeAnnotation("VNode")
                ]
            )
        )
    
    def _register_component_primitives(self):
        """Register component-related primitives"""
        
        # ui:component - Define a component
        self.registry["ui:component"] = Function(
            name="ui:component",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),      # name
                    TypeAnnotation("Map"),         # props definition
                    TypeAnnotation("Function"),    # render function
                    TypeAnnotation("Component")
                ]
            )
        )
        
        # ui:create - Create component instance (for use in render functions)
        self.registry["ui:create"] = Function(
            name="ui:create",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Component"),
                    TypeAnnotation("Map"),       # props
                    TypeAnnotation("VNode")
                ]
            )
        )
        
        # ui:mount - Mount component
        self.registry["ui:mount"] = Function(
            name="ui:mount",
            arity=3,
            effects={EffectType.DOM},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Component"),
                    TypeAnnotation("Map"),       # props
                    TypeAnnotation("String"),    # selector
                    TypeAnnotation("ComponentInstance")
                ]
            )
        )
        
        # ui:unmount - Unmount component
        self.registry["ui:unmount"] = Function(
            name="ui:unmount",
            arity=1,
            effects={EffectType.DOM},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("ComponentInstance"),
                    TypeAnnotation("Unit")
                ]
            )
        )
        
        # ui:update - Update component props
        self.registry["ui:update"] = Function(
            name="ui:update",
            arity=2,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("ComponentInstance"),
                    TypeAnnotation("Map"),
                    TypeAnnotation("Unit")
                ]
            )
        )
        
        # ui:lifecycle - Component lifecycle hooks
        self.registry["ui:lifecycle"] = Function(
            name="ui:lifecycle",
            arity=2,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),    # hook name
                    TypeAnnotation("Function"),  # handler
                    TypeAnnotation("Unit")
                ]
            )
        )
    
    def _register_prop_primitives(self):
        """Register prop definition primitives"""
        
        # prop - Define a component prop
        self.registry["prop"] = Function(
            name="prop",
            arity=-1,  # Variadic: type + options
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Keyword"),     # type
                    TypeAnnotation("...Any"),      # options
                    TypeAnnotation("PropDefinition")
                ]
            )
        )
        
        # ui:validate-props - Validate props against definitions
        self.registry["ui:validate-props"] = Function(
            name="ui:validate-props",
            arity=2,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Map"),  # prop definitions
                    TypeAnnotation("Map"),  # actual props
                    TypeAnnotation("Result", [
                        TypeAnnotation("Map"),
                        TypeAnnotation("ValidationError")
                    ])
                ]
            )
        )
        
        # ui:default-props - Apply default prop values
        self.registry["ui:default-props"] = Function(
            name="ui:default-props",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Map"),  # prop definitions
                    TypeAnnotation("Map"),  # actual props
                    TypeAnnotation("Map")   # props with defaults
                ]
            )
        )
    
    def register_all(self, primitive_registry: 'PrimitiveRegistry'):
        """Register all UI primitives with the main primitive registry"""
        for name, func in self.registry.items():
            # For now, we'll register the function definition only
            # The actual implementation will be in the runtime
            primitive_registry.register(name, func, lambda *args: None)
    
    def get_all_names(self) -> List[str]:
        """Get all UI primitive names"""
        return list(self.registry.keys())


# Create singleton instance
UI_PRIMITIVES = UIPrimitives()