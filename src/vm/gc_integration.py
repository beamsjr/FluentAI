"""
GC Integration for ClaudeLang VM

Provides integration between the VM and the improved garbage collector.
"""

from typing import Any, Dict, Optional, List
from dataclasses import dataclass, field

from .gc import ObjectType, GCObject, RefCountedObject, GCValue, GCEnvironment, GCClosure, GCList
from .gc_improved import GenerationalGC, GenerationalObject, Generation


class VMValue(GenerationalObject):
    """VM value with generational GC support"""
    def __init__(self, data: Any, type_info: Optional[Any] = None,
                 provenance: Optional[List[str]] = None,
                 effects_triggered: Optional[List[Any]] = None):
        super().__init__()
        self.data = data
        self.type_info = type_info
        self.provenance = provenance or []
        self.effects_triggered = effects_triggered or []
        self.gc_type = ObjectType.VALUE
        self._id = id(self)  # Use object id for hashing
    
    def __hash__(self):
        return self._id
    
    def __eq__(self, other):
        return self is other
    
    def get_references(self) -> List[GCObject]:
        """Get references for GC traversal"""
        refs = []
        
        # Check if data contains GC objects
        if isinstance(self.data, GCObject):
            refs.append(self.data)
        elif isinstance(self.data, (list, tuple)):
            for item in self.data:
                if isinstance(item, GCObject):
                    refs.append(item)
        elif isinstance(self.data, dict):
            for value in self.data.values():
                if isinstance(value, GCObject):
                    refs.append(value)
        
        return refs


class VMEnvironment(GenerationalObject):
    """VM environment with generational GC support"""
    def __init__(self, parent: Optional['VMEnvironment'] = None):
        super().__init__()
        self.bindings: Dict[str, VMValue] = {}
        self.parent = parent
        self.gc_type = ObjectType.ENVIRONMENT
        self._id = id(self)  # Use object id for hashing
        if self.parent:
            self.parent.incref()
    
    def __hash__(self):
        return self._id
    
    def __eq__(self, other):
        return self is other
    
    def get_references(self) -> List[GCObject]:
        """Get all referenced objects"""
        refs = []
        if self.parent:
            refs.append(self.parent)
        refs.extend(self.bindings.values())
        return refs
    
    def bind(self, name: str, value: VMValue, gc: Optional[GenerationalGC] = None):
        """Bind with write barrier"""
        old_value = self.bindings.get(name)
        if old_value:
            old_value.decref()
        
        value.incref()
        self.bindings[name] = value
        
        # Trigger write barrier
        if gc:
            gc.write_barrier(self, name, value)
    
    def lookup(self, name: str) -> Optional[VMValue]:
        """Lookup a binding"""
        if name in self.bindings:
            return self.bindings[name]
        elif self.parent:
            return self.parent.lookup(name)
        return None
    
    def extend(self) -> 'VMEnvironment':
        """Create child environment"""
        return VMEnvironment(parent=self)


class VMClosure(GenerationalObject):
    """VM closure with generational GC support"""
    def __init__(self, params: List[str], body_id: str, 
                 captured_env: VMEnvironment, name: str = "<anonymous>"):
        super().__init__()
        self.params = params
        self.body_id = body_id
        self.captured_env = captured_env
        self.name = name
        self.gc_type = ObjectType.CLOSURE
        self._id = id(self)  # Use object id for hashing
        self.captured_env.incref()
    
    def __hash__(self):
        return self._id
    
    def __eq__(self, other):
        return self is other
    
    def get_references(self) -> List[GCObject]:
        """Closure references its environment"""
        return [self.captured_env]


class VMList(GenerationalObject):
    """VM list with generational GC support"""
    def __init__(self, elements: Optional[List[VMValue]] = None):
        super().__init__()
        self.elements = elements or []
        self.gc_type = ObjectType.LIST
        self._id = id(self)  # Use object id for hashing
        for elem in self.elements:
            elem.incref()
    
    def __hash__(self):
        return self._id
    
    def __eq__(self, other):
        return self is other
    
    def get_references(self) -> List[GCObject]:
        """List references all elements"""
        return self.elements
    
    def append(self, value: VMValue, gc: Optional[GenerationalGC] = None):
        """Append with write barrier"""
        value.incref()
        self.elements.append(value)
        
        if gc:
            gc.write_barrier(self, f"element_{len(self.elements)-1}", value)
    
    def set_element(self, index: int, value: VMValue, gc: Optional[GenerationalGC] = None):
        """Set element with write barrier"""
        if 0 <= index < len(self.elements):
            old_value = self.elements[index]
            old_value.decref()
            
            value.incref()
            self.elements[index] = value
            
            if gc:
                gc.write_barrier(self, f"element_{index}", value)


class VMGarbageCollector:
    """
    High-level GC interface for the VM.
    
    Provides convenient methods for VM operations while
    maintaining GC invariants.
    """
    
    def __init__(self, **kwargs):
        self.gc = GenerationalGC(**kwargs)
        self.gc.start_concurrent_gc()
    
    def allocate_value(self, data: Any, type_info: Any = None) -> VMValue:
        """Allocate a new value"""
        value = VMValue(data=data, type_info=type_info)
        self.gc.allocate(value)
        return value
    
    def allocate_environment(self, parent: Optional[VMEnvironment] = None) -> VMEnvironment:
        """Allocate a new environment"""
        env = VMEnvironment(parent=parent)
        self.gc.allocate(env)
        return env
    
    def allocate_closure(self, params: List[str], body_id: str, 
                        env: VMEnvironment, name: str = "<anonymous>") -> VMClosure:
        """Allocate a new closure"""
        closure = VMClosure(params=params, body_id=body_id, 
                           captured_env=env, name=name)
        self.gc.allocate(closure)
        return closure
    
    def allocate_list(self, elements: Optional[List[VMValue]] = None) -> VMList:
        """Allocate a new list"""
        list_obj = VMList(elements=elements or [])
        self.gc.allocate(list_obj)
        return list_obj
    
    def add_root(self, obj: GCObject):
        """Add a GC root"""
        self.gc.add_root(obj)
    
    def remove_root(self, obj: GCObject):
        """Remove a GC root"""
        self.gc.remove_root(obj)
    
    def collect(self):
        """Force a collection"""
        self.gc.collect_young()
    
    def collect_full(self):
        """Force a full collection"""
        self.gc.collect_old()
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get GC statistics"""
        return self.gc.get_statistics()
    
    def shutdown(self):
        """Shutdown GC (stop concurrent thread)"""
        self.gc.stop_concurrent_gc()
    
    def enable_write_barriers(self):
        """Enable write barriers"""
        self.gc.write_barrier_enabled = True
    
    def disable_write_barriers(self):
        """Disable write barriers (for bulk operations)"""
        self.gc.write_barrier_enabled = False
    
    def with_disabled_barriers(self):
        """Context manager for operations without write barriers"""
        class BarrierContext:
            def __init__(self, gc):
                self.gc = gc
                self.was_enabled = gc.write_barrier_enabled
            
            def __enter__(self):
                self.gc.write_barrier_enabled = False
                return self
            
            def __exit__(self, *args):
                self.gc.write_barrier_enabled = self.was_enabled
        
        return BarrierContext(self.gc)