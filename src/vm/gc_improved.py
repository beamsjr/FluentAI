"""
Improved Garbage Collection for ClaudeLang VM

Implements a generational garbage collector with:
- Generational collection (young, old generations)
- Concurrent marking
- Write barriers
- Better performance characteristics
"""

from typing import Any, Dict, Set, List, Optional, Protocol, runtime_checkable, Deque
from dataclasses import dataclass, field
from enum import Enum, auto
import weakref
from collections import deque
import threading
import time

from .gc import ObjectType, GCObject, RefCountedObject


class Generation(Enum):
    """Garbage collection generations"""
    YOUNG = 0     # Frequently collected
    OLD = 1       # Less frequently collected
    PERMANENT = 2 # Never collected (system objects)


@dataclass
class GenerationalObject(RefCountedObject):
    """Object with generation tracking"""
    generation: Generation = field(default=Generation.YOUNG)
    age: int = field(default=0)  # Number of collections survived
    remembered: bool = field(default=False)  # In remembered set


class GenerationalGC:
    """
    Generational garbage collector with concurrent collection.
    
    Features:
    - Young generation (nursery) for short-lived objects
    - Old generation for long-lived objects
    - Remembered set for old->young references
    - Concurrent marking to reduce pause times
    - Write barriers to track mutations
    """
    
    def __init__(self, 
                 young_size: int = 1000,
                 young_threshold: int = 100,
                 old_threshold: int = 10,
                 promotion_age: int = 3):
        # Generation spaces
        self.young_space: List[GCObject] = []
        self.old_space: List[GCObject] = []
        self.permanent_space: List[GCObject] = []
        
        # Collection thresholds
        self.young_size = young_size
        self.young_threshold = young_threshold  # Trigger young GC
        self.old_threshold = old_threshold      # Trigger old GC after N young GCs
        self.promotion_age = promotion_age      # Promote after surviving N collections
        
        # Remembered set: old objects that reference young objects
        self.remembered_set: Set[GCObject] = set()
        
        # Roots
        self.roots: List[GCObject] = []
        
        # Statistics
        self.young_collections = 0
        self.old_collections = 0
        self.allocations_since_young_gc = 0
        self.young_gcs_since_old_gc = 0
        
        self.stats = {
            'young_collections': 0,
            'old_collections': 0,
            'objects_promoted': 0,
            'objects_collected': 0,
            'total_pause_time': 0.0,
            'max_pause_time': 0.0
        }
        
        # Concurrent collection
        self.gc_thread = None
        self.gc_lock = threading.RLock()
        self.should_stop = False
        self.pending_collection = threading.Event()
        
        # Write barrier tracking
        self.write_barrier_enabled = True
        self.mutation_log: Deque[Tuple[GCObject, str, GCObject]] = deque(maxlen=1000)
    
    def start_concurrent_gc(self):
        """Start concurrent GC thread"""
        if not self.gc_thread:
            self.gc_thread = threading.Thread(target=self._gc_worker, daemon=True)
            self.gc_thread.start()
    
    def stop_concurrent_gc(self):
        """Stop concurrent GC thread"""
        self.should_stop = True
        self.pending_collection.set()
        if self.gc_thread:
            self.gc_thread.join()
    
    def _gc_worker(self):
        """Background GC worker thread"""
        while not self.should_stop:
            self.pending_collection.wait()
            if self.should_stop:
                break
            
            self.pending_collection.clear()
            self._perform_concurrent_collection()
    
    def allocate(self, obj: GCObject) -> GCObject:
        """Allocate object in young generation"""
        with self.gc_lock:
            if isinstance(obj, GenerationalObject):
                obj.generation = Generation.YOUNG
                obj.age = 0
            
            self.young_space.append(obj)
            self.allocations_since_young_gc += 1
            
            # Check if young generation collection needed
            if (self.allocations_since_young_gc >= self.young_threshold or
                len(self.young_space) >= self.young_size):
                self.collect_young()
            
            return obj
    
    def add_root(self, obj: GCObject):
        """Add root object"""
        with self.gc_lock:
            if obj not in self.roots:
                self.roots.append(obj)
                if isinstance(obj, RefCountedObject):
                    obj.incref()
    
    def remove_root(self, obj: GCObject):
        """Remove root object"""
        with self.gc_lock:
            if obj in self.roots:
                self.roots.remove(obj)
                if isinstance(obj, RefCountedObject):
                    obj.decref()
    
    def write_barrier(self, container: GCObject, field: str, value: GCObject):
        """
        Write barrier to track old->young references.
        Called when container.field = value
        """
        if not self.write_barrier_enabled:
            return
        
        with self.gc_lock:
            # Track mutation for concurrent GC
            self.mutation_log.append((container, field, value))
            
            # If old object references young object, add to remembered set
            if (isinstance(container, GenerationalObject) and 
                isinstance(value, GenerationalObject)):
                if (container.generation == Generation.OLD and 
                    value.generation == Generation.YOUNG):
                    self.remembered_set.add(container)
                    container.remembered = True
    
    def collect_young(self):
        """Collect young generation (minor GC)"""
        start_time = time.time()
        
        with self.gc_lock:
            self.stats['young_collections'] += 1
            self.allocations_since_young_gc = 0
            self.young_gcs_since_old_gc += 1
            
            # Mark phase: start from roots and remembered set
            marked = set()
            worklist = []
            
            # Track all young objects we find (including those referenced from old)
            all_young_marked = set()
            
            # Add all roots - they might reference young objects
            worklist.extend(self.roots)
            
            # Add remembered set (old objects referencing young)
            worklist.extend(self.remembered_set)
            
            # Mark reachable objects
            while worklist:
                obj = worklist.pop()
                
                # If this is an old object, we only care about its young references
                if not self._is_young(obj):
                    # Mark young objects referenced by old objects
                    if hasattr(obj, 'get_references'):
                        for ref in obj.get_references():
                            if self._is_young(ref) and ref not in marked:
                                worklist.append(ref)
                                all_young_marked.add(ref)  # Track it
                    continue
                
                # This is a young object - mark it
                if obj in marked:
                    continue
                
                marked.add(obj)
                all_young_marked.add(obj)  # Track it
                
                # Mark young references
                if hasattr(obj, 'get_references'):
                    for ref in obj.get_references():
                        if self._is_young(ref) and ref not in marked:
                            worklist.append(ref)
            
            # Sweep phase: collect unmarked young objects
            survivors = []
            collected = 0
            
            # Process objects in young_space
            for obj in self.young_space:
                if obj in marked:
                    # Survivor - check if should be promoted
                    if isinstance(obj, GenerationalObject):
                        old_age = obj.age
                        obj.age += 1
                        if obj.age >= self.promotion_age:
                            self._promote_to_old(obj)
                            self.stats['objects_promoted'] += 1
                        else:
                            survivors.append(obj)
                    else:
                        survivors.append(obj)
                else:
                    # Collect object
                    collected += 1
                    self._finalize_object(obj)
            
            # Also process young objects that were marked but not in young_space
            # (can happen when objects are only reachable from old generation)
            for obj in all_young_marked:
                if obj not in self.young_space and self._is_young(obj):
                    if isinstance(obj, GenerationalObject):
                        obj.age += 1
                        if obj.age >= self.promotion_age:
                            self._promote_to_old(obj)
                            self.stats['objects_promoted'] += 1
                        else:
                            survivors.append(obj)
                    else:
                        survivors.append(obj)
            
            self.young_space = survivors
            self.stats['objects_collected'] += collected
            
            # Clear remembered set entries for collected objects
            self.remembered_set = {obj for obj in self.remembered_set 
                                   if self._is_alive(obj)}
            
            # Check if old generation collection needed
            if self.young_gcs_since_old_gc >= self.old_threshold:
                self.collect_old()
        
        # Record timing
        pause_time = time.time() - start_time
        self.stats['total_pause_time'] += pause_time
        self.stats['max_pause_time'] = max(self.stats['max_pause_time'], pause_time)
    
    def collect_old(self):
        """Collect old generation (major GC)"""
        start_time = time.time()
        
        with self.gc_lock:
            self.stats['old_collections'] += 1
            self.young_gcs_since_old_gc = 0
            
            # First do a young collection
            self.collect_young()
            
            # Mark phase for old generation
            marked = set()
            worklist = list(self.roots)
            
            # Mark reachable objects in both generations
            while worklist:
                obj = worklist.pop()
                if obj in marked:
                    continue
                
                marked.add(obj)
                
                if hasattr(obj, 'get_references'):
                    for ref in obj.get_references():
                        if ref not in marked:
                            worklist.append(ref)
            
            # Sweep old generation
            survivors = []
            collected = 0
            
            for obj in self.old_space:
                if obj in marked:
                    survivors.append(obj)
                else:
                    collected += 1
                    self._finalize_object(obj)
            
            self.old_space = survivors
            self.stats['objects_collected'] += collected
        
        pause_time = time.time() - start_time
        self.stats['total_pause_time'] += pause_time
        self.stats['max_pause_time'] = max(self.stats['max_pause_time'], pause_time)
    
    def _perform_concurrent_collection(self):
        """Perform concurrent collection (experimental)"""
        # Concurrent marking for old generation
        # This is a simplified version - production would need more sophistication
        
        with self.gc_lock:
            # Snapshot roots
            roots_snapshot = list(self.roots)
        
        # Mark concurrently (without lock)
        marked = set()
        worklist = roots_snapshot
        
        while worklist:
            obj = worklist.pop()
            if obj in marked:
                continue
            
            marked.add(obj)
            
            if hasattr(obj, 'get_references'):
                for ref in obj.get_references():
                    if ref not in marked:
                        worklist.append(ref)
        
        # Process mutations that happened during marking
        with self.gc_lock:
            for container, field, value in self.mutation_log:
                if container in marked and value not in marked:
                    # Re-mark from this object
                    worklist = [value]
                    while worklist:
                        obj = worklist.pop()
                        if obj not in marked:
                            marked.add(obj)
                            if hasattr(obj, 'get_references'):
                                worklist.extend(obj.get_references())
            
            # Clear mutation log
            self.mutation_log.clear()
            
            # Sweep phase (needs lock)
            # ... (similar to collect_old sweep phase)
    
    def _is_young(self, obj: GCObject) -> bool:
        """Check if object is in young generation"""
        if isinstance(obj, GenerationalObject):
            return obj.generation == Generation.YOUNG
        return obj in self.young_space
    
    def _is_alive(self, obj: GCObject) -> bool:
        """Check if object is still alive"""
        return (obj in self.young_space or 
                obj in self.old_space or 
                obj in self.permanent_space)
    
    def _promote_to_old(self, obj: GenerationalObject):
        """Promote object from young to old generation"""
        obj.generation = Generation.OLD
        if obj in self.young_space:
            self.young_space.remove(obj)
        self.old_space.append(obj)
        
        # Check if promoted object references young objects
        if hasattr(obj, 'get_references'):
            for ref in obj.get_references():
                if self._is_young(ref):
                    self.remembered_set.add(obj)
                    obj.remembered = True
                    break
    
    def _finalize_object(self, obj: GCObject):
        """Finalize object before collection"""
        # Call destructor if available
        if hasattr(obj, '__del__'):
            try:
                obj.__del__()
            except:
                pass  # Ignore errors in destructors
        
        # Decrement reference counts of referenced objects
        if hasattr(obj, 'get_references'):
            for ref in obj.get_references():
                if isinstance(ref, RefCountedObject):
                    ref.decref()
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get GC statistics"""
        with self.gc_lock:
            return {
                **self.stats,
                'young_size': len(self.young_space),
                'old_size': len(self.old_space),
                'permanent_size': len(self.permanent_space),
                'remembered_set_size': len(self.remembered_set),
                'avg_pause_time': (self.stats['total_pause_time'] / 
                                  max(1, self.stats['young_collections'] + 
                                      self.stats['old_collections']))
            }