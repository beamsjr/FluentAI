"""
Garbage Collection for ClaudeLang VM

Implements reference counting with cycle detection for managing memory
in the ClaudeLang runtime.
"""

from typing import Any, Dict, Set, List, Optional, Protocol, runtime_checkable
from dataclasses import dataclass, field
from enum import Enum, auto
import weakref
from collections import deque


class ObjectType(Enum):
    """Types of objects that need garbage collection"""
    VALUE = auto()
    CLOSURE = auto()
    ENVIRONMENT = auto()
    LIST = auto()
    DICT = auto()
    SET = auto()
    TUPLE = auto()
    MODULE = auto()


@runtime_checkable
class GCObject(Protocol):
    """Protocol for objects that can be garbage collected"""
    def get_type(self) -> ObjectType:
        """Get the object type"""
        ...
    
    def get_references(self) -> List['GCObject']:
        """Get all objects this object references"""
        ...
    
    def mark(self) -> None:
        """Mark object as reachable during GC"""
        ...
    
    def is_marked(self) -> bool:
        """Check if object is marked"""
        ...
    
    def clear_mark(self) -> None:
        """Clear the mark"""
        ...


@dataclass
class RefCountedObject:
    """Base class for reference counted objects"""
    ref_count: int = field(default=0, init=False)
    marked: bool = field(default=False, init=False)
    gc_type: ObjectType = field(default=ObjectType.VALUE, init=False)
    
    def incref(self) -> None:
        """Increment reference count"""
        self.ref_count += 1
    
    def decref(self) -> int:
        """Decrement reference count and return new count"""
        self.ref_count -= 1
        return self.ref_count
    
    def mark(self) -> None:
        self.marked = True
    
    def is_marked(self) -> bool:
        return self.marked
    
    def clear_mark(self) -> None:
        self.marked = False
    
    def get_type(self) -> ObjectType:
        return self.gc_type


@dataclass
class GCValue(RefCountedObject):
    """Garbage collected value object"""
    data: Any
    type_info: Optional[Any] = None
    provenance: List[str] = field(default_factory=list)
    effects_triggered: List[Any] = field(default_factory=list)
    gc_type: ObjectType = field(default=ObjectType.VALUE, init=False)
    
    def __post_init__(self):
        self.gc_type = ObjectType.VALUE
    
    def get_references(self) -> List[GCObject]:
        """Values may reference other GC objects in their data"""
        refs = []
        if isinstance(self.data, GCObject):
            refs.append(self.data)
        elif isinstance(self.data, (list, tuple)):
            refs.extend(obj for obj in self.data if isinstance(obj, GCObject))
        elif isinstance(self.data, dict):
            for v in self.data.values():
                if isinstance(v, GCObject):
                    refs.append(v)
        return refs


@dataclass
class GCEnvironment(RefCountedObject):
    """Garbage collected environment"""
    bindings: Dict[str, 'GCValue'] = field(default_factory=dict)
    parent: Optional['GCEnvironment'] = None
    gc_type: ObjectType = field(default=ObjectType.ENVIRONMENT, init=False)
    
    def __post_init__(self):
        self.gc_type = ObjectType.ENVIRONMENT
        if self.parent:
            self.parent.incref()
    
    def get_references(self) -> List[GCObject]:
        """Environment references its parent and all bound values"""
        refs = []
        if self.parent:
            refs.append(self.parent)
        refs.extend(self.bindings.values())
        return refs
    
    def bind(self, name: str, value: GCValue) -> None:
        """Bind a value, managing reference counts"""
        if name in self.bindings:
            self.bindings[name].decref()
        value.incref()
        self.bindings[name] = value
    
    def extend(self) -> 'GCEnvironment':
        """Create child environment"""
        return GCEnvironment(parent=self)


@dataclass
class GCClosure(RefCountedObject):
    """Garbage collected closure"""
    params: List[str]
    body_id: str
    captured_env: GCEnvironment
    name: str = "<anonymous>"
    gc_type: ObjectType = field(default=ObjectType.CLOSURE, init=False)
    
    def __post_init__(self):
        self.gc_type = ObjectType.CLOSURE
        self.captured_env.incref()
    
    def get_references(self) -> List[GCObject]:
        """Closure references its captured environment"""
        return [self.captured_env]


@dataclass
class GCList(RefCountedObject):
    """Garbage collected list"""
    elements: List[GCValue] = field(default_factory=list)
    gc_type: ObjectType = field(default=ObjectType.LIST, init=False)
    
    def __post_init__(self):
        self.gc_type = ObjectType.LIST
        for elem in self.elements:
            elem.incref()
    
    def get_references(self) -> List[GCObject]:
        """List references all its elements"""
        return self.elements
    
    def append(self, value: GCValue) -> None:
        """Append value, managing reference count"""
        value.incref()
        self.elements.append(value)
    
    def cons(self, head: GCValue) -> 'GCList':
        """Create new list with head prepended"""
        new_list = GCList(elements=[head] + self.elements)
        return new_list


class GarbageCollector:
    """
    Garbage collector using reference counting with cycle detection.
    
    Uses a hybrid approach:
    1. Reference counting for immediate collection of acyclic objects
    2. Mark-and-sweep for detecting and collecting cycles
    """
    
    def __init__(self, cycle_threshold: int = 100):
        self.objects: List[GCObject] = []
        self.roots: List[GCObject] = []
        self.allocations_since_collection = 0
        self.cycle_threshold = cycle_threshold
        self.stats = {
            'collections': 0,
            'objects_collected': 0,
            'cycles_detected': 0,
            'total_allocations': 0
        }
    
    def allocate(self, obj: GCObject) -> GCObject:
        """Register a new object with the GC"""
        self.objects.append(obj)
        self.allocations_since_collection += 1
        self.stats['total_allocations'] += 1
        
        # Note: We don't run collection here anymore to avoid collecting
        # objects that are being allocated but not yet rooted.
        # Collection should be triggered separately or when adding roots.
        
        return obj
    
    def check_collection_needed(self) -> None:
        """Check if collection should run and run it if needed"""
        if self.allocations_since_collection >= self.cycle_threshold:
            self.collect_cycles()
    
    def add_root(self, obj: GCObject) -> None:
        """Add a root object (won't be collected)"""
        if isinstance(obj, RefCountedObject):
            obj.incref()
        # Check if already a root using identity
        is_root = any(r is obj for r in self.roots)
        if not is_root:
            self.roots.append(obj)
        
        # Safe point to check for collection
        self.check_collection_needed()
    
    def remove_root(self, obj: GCObject) -> None:
        """Remove a root object"""
        # Remove root using identity comparison
        for i, r in enumerate(self.roots):
            if r is obj:
                del self.roots[i]
                if isinstance(obj, RefCountedObject):
                    if obj.decref() == 0:
                        self.collect_object(obj)
                break
    
    def collect_object(self, obj: GCObject) -> None:
        """Collect a single object when its refcount reaches 0"""
        # Remove object using identity comparison to avoid __eq__ recursion
        for i, o in enumerate(self.objects):
            if o is obj:
                del self.objects[i]
                self.stats['objects_collected'] += 1
                
                # Decrement references to other objects
                for ref in obj.get_references():
                    if isinstance(ref, RefCountedObject):
                        # Check if ref is in objects using identity
                        ref_in_objects = any(o is ref for o in self.objects)
                        if ref_in_objects and ref.decref() == 0:
                            self.collect_object(ref)
                break
    
    def collect_cycles(self) -> None:
        """Run mark-and-sweep to collect cycles"""
        self.stats['collections'] += 1
        self.allocations_since_collection = 0
        
        # Clear all marks
        for obj in self.objects:
            obj.clear_mark()
        
        # Mark phase: mark all reachable objects from roots
        to_mark = deque(self.roots)
        while to_mark:
            obj = to_mark.popleft()
            # Check if obj is in objects using identity
            obj_in_objects = any(o is obj for o in self.objects)
            if obj_in_objects and not obj.is_marked():
                obj.mark()
                for ref in obj.get_references():
                    # Check if ref is in objects using identity
                    ref_in_objects = any(o is ref for o in self.objects)
                    if ref_in_objects:
                        to_mark.append(ref)
        
        # Sweep phase: collect unmarked objects
        to_collect = []
        for obj in self.objects[:]:  # Copy list to avoid modification during iteration
            if not obj.is_marked() and isinstance(obj, RefCountedObject):
                # This is part of a cycle or unreachable
                to_collect.append(obj)
        
        # Collect unreachable objects
        for obj in to_collect:
            # Check if still in objects using identity
            obj_still_there = any(o is obj for o in self.objects)
            if obj_still_there:  # Double-check it hasn't been collected
                self.stats['cycles_detected'] += 1
                self.collect_object(obj)
    
    def collect_all(self) -> None:
        """Force full garbage collection"""
        self.collect_cycles()
    
    def get_stats(self) -> Dict[str, int]:
        """Get GC statistics"""
        return {
            **self.stats,
            'live_objects': len(self.objects),
            'root_objects': len(self.roots)
        }


# Global GC instance
_gc = GarbageCollector()


def reset_gc():
    """Reset the global GC instance (for testing)"""
    global _gc
    _gc = GarbageCollector()


def gc_allocate(obj: GCObject) -> GCObject:
    """Allocate object with GC tracking"""
    return _gc.allocate(obj)


def gc_add_root(obj: GCObject) -> None:
    """Add GC root"""
    _gc.add_root(obj)


def gc_remove_root(obj: GCObject) -> None:
    """Remove GC root"""
    _gc.remove_root(obj)


def gc_collect() -> None:
    """Force garbage collection"""
    _gc.collect_all()


def gc_stats() -> Dict[str, int]:
    """Get GC statistics"""
    return _gc.get_stats()


def gc_enable_debug(enable: bool = True) -> None:
    """Enable/disable GC debug logging"""
    # TODO: Add debug logging
    pass