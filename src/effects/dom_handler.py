"""
DOM Effect Handler for ClaudeLang

This module implements DOM manipulation effects for web UI integration.
It provides a virtual DOM abstraction that can be compiled to efficient
DOM operations or used with existing web frameworks.
"""

from typing import Dict, Any, List, Optional, Callable, Union, Tuple
from dataclasses import dataclass, field
from abc import ABC, abstractmethod
import json
import weakref
from collections import defaultdict

from ..core.ast import EffectType
from .handlers import EffectHandler, EffectRequest, EffectResult


@dataclass
class VNode:
    """Virtual DOM node representation"""
    tag: str
    props: Dict[str, Any] = field(default_factory=dict)
    children: List[Union['VNode', str]] = field(default_factory=list)
    key: Optional[str] = None
    ref: Optional[str] = None
    
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary for serialization"""
        return {
            'tag': self.tag,
            'props': self.props,
            'children': [
                child.to_dict() if isinstance(child, VNode) else child
                for child in self.children
            ],
            'key': self.key,
            'ref': self.ref
        }


@dataclass
class DOMPatch:
    """Represents a DOM update operation"""
    type: str  # 'create', 'update', 'remove', 'move'
    target: Optional[str] = None  # Element ID or selector
    data: Dict[str, Any] = field(default_factory=dict)


class DOMHandler(EffectHandler):
    """Handler for DOM manipulation effects"""
    
    def __init__(self, 
                 root_selector: str = '#app',
                 batch_updates: bool = True,
                 enable_virtual_dom: bool = True):
        super().__init__()
        self.root_selector = root_selector
        self.batch_updates = batch_updates
        self.enable_virtual_dom = enable_virtual_dom
        
        # Virtual DOM state
        self.vdom_tree: Optional[VNode] = None
        self.dom_refs: Dict[str, Any] = {}  # ref -> element mapping
        
        # Event listeners
        self.event_listeners: Dict[str, List[Callable]] = defaultdict(list)
        self.listener_cleanup: Dict[str, Callable] = {}
        
        # Update batching
        self.pending_patches: List[DOMPatch] = []
        self.update_scheduled = False
        
        # Component state
        self.component_state: Dict[str, Any] = {}
        self.component_refs: Dict[str, weakref.ref] = {}
    
    def can_handle(self, effect_type: EffectType, operation: str) -> bool:
        return effect_type == EffectType.DOM
    
    def handle(self, request: EffectRequest) -> EffectResult:
        op = request.operation
        args = request.arguments
        
        if op == "create-element":
            # Create virtual element
            tag = args[0]
            props = args[1] if len(args) > 1 else {}
            children = args[2] if len(args) > 2 else []
            
            vnode = VNode(tag=tag, props=props, children=children)
            element_id = f"vnode_{id(vnode)}"
            self.dom_refs[element_id] = vnode
            
            return EffectResult(value=element_id)
        
        elif op == "set-attribute":
            # Set element attribute
            element_id = args[0]
            attr_name = args[1]
            attr_value = args[2]
            
            if element_id in self.dom_refs:
                element = self.dom_refs[element_id]
                if isinstance(element, VNode):
                    element.props[attr_name] = attr_value
                    self._schedule_update()
            
            return EffectResult(value=None)
        
        elif op == "set-text":
            # Set element text content
            element_id = args[0]
            text = args[1]
            
            if element_id in self.dom_refs:
                element = self.dom_refs[element_id]
                if isinstance(element, VNode):
                    element.children = [str(text)]
                    self._schedule_update()
            
            return EffectResult(value=None)
        
        elif op == "append-child":
            # Append child to element
            parent_id = args[0]
            child_id = args[1]
            
            if parent_id in self.dom_refs and child_id in self.dom_refs:
                parent = self.dom_refs[parent_id]
                child = self.dom_refs[child_id]
                if isinstance(parent, VNode):
                    parent.children.append(child)
                    self._schedule_update()
            
            return EffectResult(value=None)
        
        elif op == "remove-child":
            # Remove child from element
            parent_id = args[0]
            child_id = args[1]
            
            if parent_id in self.dom_refs:
                parent = self.dom_refs[parent_id]
                if isinstance(parent, VNode) and child_id in self.dom_refs:
                    child = self.dom_refs[child_id]
                    if child in parent.children:
                        parent.children.remove(child)
                        self._schedule_update()
            
            return EffectResult(value=None)
        
        elif op == "query-selector":
            # Query element by selector
            selector = args[0]
            # In a real implementation, this would query actual DOM
            # For now, return a mock element ID
            element_id = f"query_{selector}"
            return EffectResult(value=element_id)
        
        elif op == "add-listener":
            # Add event listener
            element_id = args[0]
            event_type = args[1]
            handler = args[2]
            
            key = f"{element_id}:{event_type}"
            self.event_listeners[key].append(handler)
            
            # In a real implementation, this would attach to actual DOM
            return EffectResult(value=key)
        
        elif op == "remove-listener":
            # Remove event listener
            listener_key = args[0]
            
            if listener_key in self.event_listeners:
                del self.event_listeners[listener_key]
                if listener_key in self.listener_cleanup:
                    self.listener_cleanup[listener_key]()
                    del self.listener_cleanup[listener_key]
            
            return EffectResult(value=None)
        
        elif op == "render":
            # Render virtual DOM tree
            vnode = args[0] if args else None
            
            if isinstance(vnode, str) and vnode in self.dom_refs:
                vnode = self.dom_refs[vnode]
            
            if isinstance(vnode, VNode):
                self.vdom_tree = vnode
                patches = self._diff_vdom(None, vnode)
                self._apply_patches(patches)
            
            return EffectResult(value=None)
        
        elif op == "batch-updates":
            # Execute function with batched updates
            update_fn = args[0]
            
            was_batching = self.batch_updates
            self.batch_updates = True
            
            try:
                result = update_fn()
                self._flush_updates()
                return EffectResult(value=result)
            finally:
                self.batch_updates = was_batching
        
        elif op == "create-ref":
            # Create a reference object
            ref_id = f"ref_{len(self.dom_refs)}"
            self.dom_refs[ref_id] = None
            return EffectResult(value=ref_id)
        
        elif op == "get-ref":
            # Get referenced element
            ref_id = args[0]
            return EffectResult(value=self.dom_refs.get(ref_id))
        
        elif op == "set-ref":
            # Set referenced element
            ref_id = args[0]
            element = args[1]
            self.dom_refs[ref_id] = element
            return EffectResult(value=None)
        
        elif op == "mount-component":
            # Mount a component
            component_fn = args[0]
            props = args[1] if len(args) > 1 else {}
            container_id = args[2] if len(args) > 2 else self.root_selector
            
            # Create component instance
            component_id = f"component_{len(self.component_state)}"
            self.component_state[component_id] = {
                'fn': component_fn,
                'props': props,
                'state': {},
                'mounted': True
            }
            
            # Render component
            vnode = component_fn(props)
            if isinstance(vnode, VNode):
                self.dom_refs[component_id] = vnode
                return EffectResult(value=component_id)
            
            return EffectResult(value=None)
        
        elif op == "unmount-component":
            # Unmount a component
            component_id = args[0]
            
            if component_id in self.component_state:
                self.component_state[component_id]['mounted'] = False
                # Cleanup listeners, refs, etc.
                # In real implementation, would trigger cleanup
            
            return EffectResult(value=None)
        
        elif op == "update-component":
            # Update component props
            component_id = args[0]
            new_props = args[1]
            
            if component_id in self.component_state:
                component = self.component_state[component_id]
                old_props = component['props']
                component['props'] = new_props
                
                # Re-render if props changed
                if old_props != new_props:
                    vnode = component['fn'](new_props)
                    if isinstance(vnode, VNode):
                        self.dom_refs[component_id] = vnode
                        self._schedule_update()
            
            return EffectResult(value=None)
        
        else:
            raise ValueError(f"Unknown DOM operation: {op}")
    
    def _schedule_update(self):
        """Schedule a DOM update on the next tick"""
        if self.batch_updates and not self.update_scheduled:
            self.update_scheduled = True
            # In real implementation, would use requestAnimationFrame
            # For now, just set a flag
    
    def _flush_updates(self):
        """Flush all pending DOM updates"""
        if self.pending_patches:
            self._apply_patches(self.pending_patches)
            self.pending_patches.clear()
        self.update_scheduled = False
    
    def _diff_vdom(self, old_vnode: Optional[VNode], new_vnode: Optional[VNode]) -> List[DOMPatch]:
        """Diff two virtual DOM trees and return patches"""
        patches = []
        
        if old_vnode is None and new_vnode is not None:
            # Create new element
            patches.append(DOMPatch(
                type='create',
                data={'vnode': new_vnode.to_dict()}
            ))
        elif old_vnode is not None and new_vnode is None:
            # Remove element
            patches.append(DOMPatch(
                type='remove',
                target=old_vnode.key or str(id(old_vnode))
            ))
        elif old_vnode is not None and new_vnode is not None:
            # Update element
            if old_vnode.tag != new_vnode.tag:
                # Different tags, replace entire element
                patches.append(DOMPatch(
                    type='remove',
                    target=old_vnode.key or str(id(old_vnode))
                ))
                patches.append(DOMPatch(
                    type='create',
                    data={'vnode': new_vnode.to_dict()}
                ))
            else:
                # Same tag, diff props and children
                if old_vnode.props != new_vnode.props:
                    patches.append(DOMPatch(
                        type='update',
                        target=old_vnode.key or str(id(old_vnode)),
                        data={'props': new_vnode.props}
                    ))
                
                # Diff children (simplified)
                # In real implementation, would use key-based reconciliation
                for i, (old_child, new_child) in enumerate(
                    zip(old_vnode.children, new_vnode.children)
                ):
                    if isinstance(old_child, VNode) and isinstance(new_child, VNode):
                        patches.extend(self._diff_vdom(old_child, new_child))
                    elif old_child != new_child:
                        patches.append(DOMPatch(
                            type='update',
                            target=f"{old_vnode.key or id(old_vnode)}_child_{i}",
                            data={'text': str(new_child)}
                        ))
        
        return patches
    
    def _apply_patches(self, patches: List[DOMPatch]):
        """Apply patches to the DOM (or accumulate them)"""
        if self.batch_updates:
            self.pending_patches.extend(patches)
        else:
            # In real implementation, would apply to actual DOM
            # For now, just store them
            pass
    
    def get_vdom_tree(self) -> Optional[Dict[str, Any]]:
        """Get current virtual DOM tree as dict"""
        if self.vdom_tree:
            return self.vdom_tree.to_dict()
        return None
    
    def cleanup(self):
        """Clean up resources"""
        # Remove all event listeners
        for key in list(self.listener_cleanup.keys()):
            self.listener_cleanup[key]()
        
        self.event_listeners.clear()
        self.listener_cleanup.clear()
        self.dom_refs.clear()
        self.component_state.clear()
        self.pending_patches.clear()