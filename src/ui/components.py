"""
Component System for ClaudeLang UI

This module provides a component abstraction with lifecycle hooks,
props validation, and efficient rendering through the virtual DOM.
"""

from typing import Dict, Any, List, Optional, Callable, Union, Type
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import uuid
from enum import Enum, auto

from ..core.ast import EffectType
from ..effects.dom_handler import VNode


class LifecyclePhase(Enum):
    """Component lifecycle phases"""
    CREATED = auto()
    BEFORE_MOUNT = auto()
    MOUNTED = auto()
    BEFORE_UPDATE = auto()
    UPDATED = auto()
    BEFORE_UNMOUNT = auto()
    UNMOUNTED = auto()


@dataclass
class PropDefinition:
    """Definition for a component prop"""
    name: str
    type: Optional[Type] = None
    required: bool = False
    default: Any = None
    validator: Optional[Callable[[Any], bool]] = None


@dataclass
class ComponentState:
    """Internal state for a component instance"""
    id: str
    name: str
    props: Dict[str, Any]
    state: Dict[str, Any]
    refs: Dict[str, Any]
    children: List['Component']
    phase: LifecyclePhase
    vnode: Optional[VNode] = None
    parent: Optional['Component'] = None
    context: Dict[str, Any] = field(default_factory=dict)


class Component(ABC):
    """Base component class"""
    
    def __init__(self, props: Optional[Dict[str, Any]] = None, effect_context=None):
        self._id = str(uuid.uuid4())
        self._props = props or {}
        self._state = {}
        self._refs = {}
        self._children = []
        self._phase = LifecyclePhase.CREATED
        self._vnode = None
        self._parent = None
        self._context = {}
        self._watchers = []
        self._effect_context = effect_context
        
        # Validate props
        self._validate_props()
        
        # Call created lifecycle hook
        self.created()
    
    @classmethod
    def get_prop_definitions(cls) -> List[PropDefinition]:
        """Get prop definitions for this component"""
        return getattr(cls, '_prop_definitions', [])
    
    def _validate_props(self):
        """Validate component props"""
        for prop_def in self.get_prop_definitions():
            if prop_def.required and prop_def.name not in self._props:
                raise ValueError(f"Required prop '{prop_def.name}' not provided")
            
            if prop_def.name in self._props:
                value = self._props[prop_def.name]
                
                # Type validation
                if prop_def.type and not isinstance(value, prop_def.type):
                    raise TypeError(
                        f"Prop '{prop_def.name}' must be of type {prop_def.type}"
                    )
                
                # Custom validation
                if prop_def.validator and not prop_def.validator(value):
                    raise ValueError(
                        f"Prop '{prop_def.name}' failed validation"
                    )
            elif prop_def.default is not None:
                # Set default value
                self._props[prop_def.name] = prop_def.default
    
    @property
    def props(self) -> Dict[str, Any]:
        """Get component props"""
        return self._props.copy()
    
    @property
    def state(self) -> Dict[str, Any]:
        """Get component state"""
        return self._state.copy()
    
    def set_state(self, updates: Dict[str, Any]):
        """Update component state and trigger re-render"""
        old_state = self._state.copy()
        self._state.update(updates)
        
        if old_state != self._state:
            self._phase = LifecyclePhase.BEFORE_UPDATE
            self.before_update(old_state, self._state)
            
            # Re-render
            self._vnode = self.render()
            
            self._phase = LifecyclePhase.UPDATED
            self.updated(old_state, self._state)
    
    def get_ref(self, ref_name: str) -> Any:
        """Get a ref value"""
        return self._refs.get(ref_name)
    
    def set_ref(self, ref_name: str, value: Any):
        """Set a ref value"""
        self._refs[ref_name] = value
    
    # Lifecycle hooks (can be overridden)
    
    def created(self):
        """Called when component is created"""
        pass
    
    def before_mount(self):
        """Called before component is mounted"""
        pass
    
    def mounted(self):
        """Called after component is mounted"""
        pass
    
    def before_update(self, old_state: Dict[str, Any], new_state: Dict[str, Any]):
        """Called before component updates"""
        pass
    
    def updated(self, old_state: Dict[str, Any], new_state: Dict[str, Any]):
        """Called after component updates"""
        pass
    
    def before_unmount(self):
        """Called before component is unmounted"""
        pass
    
    def unmounted(self):
        """Called after component is unmounted"""
        pass
    
    @abstractmethod
    def render(self) -> VNode:
        """Render the component to a virtual DOM node"""
        pass
    
    def mount(self):
        """Mount the component"""
        self._phase = LifecyclePhase.BEFORE_MOUNT
        self.before_mount()
        
        # Initial render
        self._vnode = self.render()
        
        self._phase = LifecyclePhase.MOUNTED
        self.mounted()
    
    def unmount(self):
        """Unmount the component"""
        self._phase = LifecyclePhase.BEFORE_UNMOUNT
        self.before_unmount()
        
        # Clean up watchers
        for watcher_id in self._watchers:
            # Call reactive:unwatch through effect system
            if hasattr(self, '_effect_context'):
                self._effect_context.perform(EffectType.STATE, 'reactive:unwatch', watcher_id)
        
        # Unmount children
        for child in self._children:
            child.unmount()
        
        self._phase = LifecyclePhase.UNMOUNTED
        self.unmounted()
    
    def watch(self, deps: List[str], callback: Callable, options: Optional[Dict[str, Any]] = None):
        """Watch reactive dependencies"""
        # Integrate with reactive system through effect context
        if hasattr(self, '_effect_context'):
            watcher_id = self._effect_context.perform(
                EffectType.STATE, 
                'reactive:watch', 
                callback, 
                deps,
                options or {}
            )
            self._watchers.append(watcher_id)
            return watcher_id
        else:
            # Fallback if no effect context
            watcher_id = f"watcher_{self._id}_{len(self._watchers)}"
            self._watchers.append(watcher_id)
            return watcher_id
    
    def emit(self, event: str, data: Any = None):
        """Emit an event to parent"""
        if self._parent and hasattr(self._parent, '_handle_child_event'):
            self._parent._handle_child_event(self, event, data)
    
    def set_reactive_state(self, key: str, value: Any):
        """Set reactive state that triggers watchers"""
        if self._effect_context:
            # Create reactive reference if it doesn't exist
            ref_id = f"{self._id}:{key}"
            self._effect_context.perform(EffectType.STATE, 'reactive:set', ref_id, value)
        else:
            # Fallback to regular state
            self._state[key] = value
            self.update()
    
    def get_reactive_state(self, key: str) -> Any:
        """Get reactive state value"""
        if self._effect_context:
            ref_id = f"{self._id}:{key}"
            return self._effect_context.perform(EffectType.STATE, 'reactive:get', ref_id)
        else:
            return self._state.get(key)
    
    def _handle_child_event(self, child: 'Component', event: str, data: Any):
        """Handle event from child component"""
        # Can be overridden to handle child events
        pass


class FunctionalComponent:
    """Functional component wrapper"""
    
    def __init__(self, render_fn: Callable[[Dict[str, Any]], VNode], 
                 prop_definitions: Optional[List[PropDefinition]] = None):
        self.render_fn = render_fn
        self.prop_definitions = prop_definitions or []
    
    def __call__(self, props: Optional[Dict[str, Any]] = None) -> VNode:
        """Render the functional component"""
        props = props or {}
        
        # Validate props
        for prop_def in self.prop_definitions:
            if prop_def.required and prop_def.name not in props:
                raise ValueError(f"Required prop '{prop_def.name}' not provided")
            
            if prop_def.name in props:
                value = props[prop_def.name]
                
                if prop_def.type and not isinstance(value, prop_def.type):
                    raise TypeError(
                        f"Prop '{prop_def.name}' must be of type {prop_def.type}"
                    )
                
                if prop_def.validator and not prop_def.validator(value):
                    raise ValueError(
                        f"Prop '{prop_def.name}' failed validation"
                    )
            elif prop_def.default is not None:
                props[prop_def.name] = prop_def.default
        
        return self.render_fn(props)


def create_component(name: str, 
                    render_fn: Callable,
                    lifecycle_hooks: Optional[Dict[str, Callable]] = None,
                    prop_definitions: Optional[List[PropDefinition]] = None) -> Type[Component]:
    """Create a component class dynamically"""
    
    class DynamicComponent(Component):
        _prop_definitions = prop_definitions or []
        
        def render(self) -> VNode:
            return render_fn(self)
    
    # Add lifecycle hooks
    if lifecycle_hooks:
        for hook_name, hook_fn in lifecycle_hooks.items():
            if hasattr(DynamicComponent, hook_name):
                setattr(DynamicComponent, hook_name, hook_fn)
    
    # Set name
    DynamicComponent.__name__ = name
    
    return DynamicComponent


# Helper functions for creating VNodes

def h(tag: str, props: Optional[Dict[str, Any]] = None, 
      children: Optional[List[Union[VNode, str]]] = None) -> VNode:
    """Create a VNode (similar to React.createElement)"""
    return VNode(
        tag=tag,
        props=props or {},
        children=children or []
    )


def fragment(children: List[Union[VNode, str]]) -> VNode:
    """Create a fragment VNode"""
    return VNode(
        tag='fragment',
        props={},
        children=children
    )


def text(content: str) -> str:
    """Create a text node"""
    return str(content)


# Component registry

_component_registry: Dict[str, Type[Component]] = {}


def register_component(name: str, component: Type[Component]):
    """Register a component globally"""
    _component_registry[name] = component


def get_component(name: str) -> Optional[Type[Component]]:
    """Get a registered component"""
    return _component_registry.get(name)


# Example components

class Button(Component):
    """Example button component"""
    
    _prop_definitions = [
        PropDefinition('label', str, required=True),
        PropDefinition('onClick', Callable, required=False),
        PropDefinition('disabled', bool, default=False),
        PropDefinition('variant', str, default='primary',
                      validator=lambda v: v in ['primary', 'secondary', 'danger'])
    ]
    
    def render(self) -> VNode:
        return h('button', {
            'class': f'btn btn-{self.props["variant"]}',
            'disabled': self.props.get('disabled', False),
            'onClick': self.props.get('onClick')
        }, [text(self.props['label'])])


class Input(Component):
    """Example input component with two-way binding"""
    
    _prop_definitions = [
        PropDefinition('value', str, default=''),
        PropDefinition('onChange', Callable, required=False),
        PropDefinition('placeholder', str, default=''),
        PropDefinition('type', str, default='text')
    ]
    
    def __init__(self, props: Optional[Dict[str, Any]] = None):
        super().__init__(props)
        self._state = {'value': props.get('value', '')}
    
    def render(self) -> VNode:
        return h('input', {
            'type': self.props.get('type', 'text'),
            'value': self.state['value'],
            'placeholder': self.props.get('placeholder', ''),
            'onInput': lambda e: self._handle_input(e)
        })
    
    def _handle_input(self, event):
        """Handle input event"""
        new_value = event.get('target', {}).get('value', '')
        self.set_state({'value': new_value})
        
        # Call onChange prop if provided
        on_change = self.props.get('onChange')
        if on_change:
            on_change(new_value)


# Register example components
register_component('Button', Button)
register_component('Input', Input)