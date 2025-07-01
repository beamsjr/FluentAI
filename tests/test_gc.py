"""
Tests for ClaudeLang Garbage Collector
"""

import unittest
from src.vm.gc import (
    GarbageCollector, GCValue, GCEnvironment, GCClosure, GCList,
    ObjectType, gc_allocate, gc_add_root, gc_remove_root, gc_collect, gc_stats, reset_gc
)


class TestGarbageCollector(unittest.TestCase):
    
    def setUp(self):
        """Set up test GC"""
        self.gc = GarbageCollector(cycle_threshold=5)
    
    def test_reference_counting(self):
        """Test basic reference counting"""
        # Create a value
        value = GCValue(data=42)
        self.gc.allocate(value)
        self.assertEqual(value.ref_count, 0)
        
        # Add as root
        self.gc.add_root(value)
        self.assertEqual(value.ref_count, 1)
        
        # Remove root should collect
        self.gc.remove_root(value)
        self.assertEqual(value.ref_count, 0)
        self.assertNotIn(value, self.gc.objects)
    
    def test_environment_references(self):
        """Test environment reference management"""
        # Create parent environment
        parent = GCEnvironment()
        self.gc.allocate(parent)
        self.gc.add_root(parent)
        
        # Create child environment
        child = parent.extend()
        self.gc.allocate(child)
        self.assertEqual(parent.ref_count, 2)  # root + child
        
        # Create value and bind it
        value = GCValue(data="hello")
        self.gc.allocate(value)
        child.bind("x", value)
        self.assertEqual(value.ref_count, 1)
        
        # Remove child from roots
        self.gc.add_root(child)
        self.gc.remove_root(parent)
        self.assertEqual(parent.ref_count, 1)  # just child now
        
        # Parent should still be alive through child
        self.assertIn(parent, self.gc.objects)
    
    def test_closure_environment_cycle(self):
        """Test cycle detection with closure capturing environment"""
        # Create environment
        env = GCEnvironment()
        self.gc.allocate(env)
        
        # Create closure that captures environment
        closure = GCClosure(
            params=["x"],
            body_id="body123",
            captured_env=env
        )
        self.gc.allocate(closure)
        
        # Bind closure in its own environment (creates cycle)
        closure_value = GCValue(data=closure)
        self.gc.allocate(closure_value)
        env.bind("self", closure_value)
        
        # Add as root
        self.gc.add_root(env)
        
        # All should be reachable
        self.gc.collect_cycles()
        self.assertIn(env, self.gc.objects)
        self.assertIn(closure, self.gc.objects)
        self.assertIn(closure_value, self.gc.objects)
        
        # Remove root - cycle should be detected and collected
        self.gc.remove_root(env)
        self.gc.collect_cycles()
        
        self.assertNotIn(env, self.gc.objects)
        self.assertNotIn(closure, self.gc.objects)
        self.assertNotIn(closure_value, self.gc.objects)
    
    def test_list_operations(self):
        """Test list memory management"""
        # Create list with values
        v1 = GCValue(data=1)
        v2 = GCValue(data=2)
        v3 = GCValue(data=3)
        
        self.gc.allocate(v1)
        self.gc.allocate(v2)
        self.gc.allocate(v3)
        
        lst = GCList(elements=[v1, v2])
        self.gc.allocate(lst)
        self.gc.add_root(lst)
        
        # Values should have refcount from list
        self.assertEqual(v1.ref_count, 1)
        self.assertEqual(v2.ref_count, 1)
        self.assertEqual(v3.ref_count, 0)
        
        # Append increases refcount
        lst.append(v3)
        self.assertEqual(v3.ref_count, 1)
        
        # Cons creates new list
        new_list = lst.cons(v1)  # cons v1 to create new list [v1, v1, v2, v3]
        self.gc.allocate(new_list)
        
        # After cons, v1 appears twice in new_list and once in lst
        self.assertEqual(v1.ref_count, 3)  # 1 from lst + 2 from new_list
        self.assertEqual(v2.ref_count, 2)  # 1 from lst + 1 from new_list
        self.assertEqual(v3.ref_count, 2)  # 1 from lst + 1 from new_list
        
        # Remove root list
        self.gc.remove_root(lst)
        
        # Original list and unreferenced values should be collected
        self.assertNotIn(lst, self.gc.objects)
        # v1, v2, v3 still referenced by new_list
    
    def test_cycle_threshold(self):
        """Test automatic cycle collection on threshold"""
        # Set low threshold
        gc = GarbageCollector(cycle_threshold=3)
        
        # Create some cycles
        for i in range(5):
            env = GCEnvironment()
            gc.allocate(env)
            closure = GCClosure(["x"], f"body{i}", env)
            gc.allocate(closure)
            value = GCValue(data=closure)
            gc.allocate(value)
            env.bind("closure", value)
            # Add one as root to trigger collection check
            if i == 2:
                gc.add_root(env)
                gc.remove_root(env)  # Remove immediately to create garbage
        
        # Should have triggered collection when we added/removed root
        self.assertGreater(gc.stats['collections'], 0)
    
    def test_complex_references(self):
        """Test complex reference patterns"""
        # Create interconnected objects
        env1 = GCEnvironment()
        env2 = GCEnvironment()
        self.gc.allocate(env1)
        self.gc.allocate(env2)
        
        # Create values that reference each other
        list1 = GCList()
        list2 = GCList()
        self.gc.allocate(list1)
        self.gc.allocate(list2)
        
        v1 = GCValue(data=list2)
        v2 = GCValue(data=list1)
        self.gc.allocate(v1)
        self.gc.allocate(v2)
        
        list1.append(v1)
        list2.append(v2)
        
        env1.bind("list", v1)
        env2.bind("list", v2)
        
        # Add one root
        self.gc.add_root(env1)
        
        # Everything except env2 should be reachable from env1
        self.gc.collect_cycles()
        # env2 was never connected to env1's reference graph, so it should be collected
        self.assertIn(env1, self.gc.objects)
        self.assertIn(v1, self.gc.objects)
        self.assertIn(list2, self.gc.objects)
        self.assertIn(v2, self.gc.objects)
        self.assertIn(list1, self.gc.objects)
        self.assertNotIn(env2, self.gc.objects)
        
        # Remove root - all should be collected
        self.gc.remove_root(env1)
        self.gc.collect_cycles()
        self.assertEqual(len(self.gc.objects), 0)
    
    def test_gc_stats(self):
        """Test GC statistics tracking"""
        # Create some objects
        for i in range(10):
            value = GCValue(data=i)
            self.gc.allocate(value)
            if i % 2 == 0:
                self.gc.add_root(value)
        
        # Force collection
        self.gc.collect_all()
        
        stats = self.gc.get_stats()
        self.assertEqual(stats['total_allocations'], 10)
        self.assertEqual(stats['root_objects'], 5)
        # After collection, non-root objects should be gone
        # We created 10 objects, added 5 as roots (i=0,2,4,6,8)
        # So 5 non-root objects should be collected
        self.assertGreaterEqual(stats['objects_collected'], 5)
        self.assertGreater(stats['collections'], 0)
    
    def test_value_with_nested_references(self):
        """Test values containing other GC objects"""
        # Create nested structure
        inner_list = GCList()
        self.gc.allocate(inner_list)
        
        inner_value = GCValue(data="inner")
        self.gc.allocate(inner_value)
        inner_list.append(inner_value)
        
        # Create value containing the list
        outer_value = GCValue(data=inner_list)
        self.gc.allocate(outer_value)
        self.gc.add_root(outer_value)
        
        # Inner objects should be reachable
        self.gc.collect_cycles()
        self.assertIn(inner_list, self.gc.objects)
        self.assertIn(inner_value, self.gc.objects)
        
        # Remove root
        self.gc.remove_root(outer_value)
        self.gc.collect_cycles()
        
        # All should be collected
        self.assertEqual(len(self.gc.objects), 0)
    
    def test_module_style_roots(self):
        """Test module-like persistent roots"""
        # Create module environment
        module_env = GCEnvironment()
        self.gc.allocate(module_env)
        self.gc.add_root(module_env)
        
        # Add some module-level bindings
        for i in range(5):
            value = GCValue(data=f"export_{i}")
            self.gc.allocate(value)
            module_env.bind(f"export_{i}", value)
        
        # Create temporary computation
        temp_env = module_env.extend()
        self.gc.allocate(temp_env)
        
        temp_value = GCValue(data="temporary")
        self.gc.allocate(temp_value)
        temp_env.bind("temp", temp_value)
        
        # Collect - temporaries should go, module should stay
        self.gc.collect_cycles()
        
        self.assertIn(module_env, self.gc.objects)
        self.assertNotIn(temp_env, self.gc.objects)
        self.assertNotIn(temp_value, self.gc.objects)
        
        # Module values should still be there
        self.assertEqual(len(module_env.bindings), 5)


class TestGCIntegration(unittest.TestCase):
    """Test GC with VM integration patterns"""
    
    def setUp(self):
        """Reset global GC for each test"""
        reset_gc()
    
    def test_function_call_pattern(self):
        """Test typical function call memory pattern"""
        # Global environment
        global_env = GCEnvironment()
        gc_allocate(global_env)
        gc_add_root(global_env)
        
        # Define a function
        func_closure = GCClosure(
            params=["x", "y"],
            body_id="add_body",
            captured_env=global_env
        )
        gc_allocate(func_closure)
        
        func_value = GCValue(data=func_closure)
        gc_allocate(func_value)
        global_env.bind("add", func_value)
        
        # Simulate function calls
        for i in range(10):
            # Create call environment
            call_env = global_env.extend()
            gc_allocate(call_env)
            
            # Bind arguments
            arg1 = GCValue(data=i)
            arg2 = GCValue(data=i + 1)
            gc_allocate(arg1)
            gc_allocate(arg2)
            
            call_env.bind("x", arg1)
            call_env.bind("y", arg2)
            
            # Result
            result = GCValue(data=i + (i + 1))
            gc_allocate(result)
            
            # After call, environment should be collectable
            gc_collect()
        
        # Only global environment and function should remain
        stats = gc_stats()
        self.assertEqual(stats['root_objects'], 1)  # global_env
        # Should have collected all temporary call environments
    
    def test_recursive_data_structure(self):
        """Test recursive data structure handling"""
        # Create a cyclic list structure
        list1 = GCList()
        list2 = GCList()
        list3 = GCList()
        
        gc_allocate(list1)
        gc_allocate(list2)
        gc_allocate(list3)
        
        # Create values pointing to lists
        v1 = GCValue(data=list2)
        v2 = GCValue(data=list3)
        v3 = GCValue(data=list1)  # Creates cycle
        
        gc_allocate(v1)
        gc_allocate(v2)
        gc_allocate(v3)
        
        list1.append(v1)
        list2.append(v2)
        list3.append(v3)
        
        # Add one as root
        gc_add_root(list1)
        
        # All should be reachable despite cycle
        gc_collect()
        stats = gc_stats()
        self.assertEqual(stats['live_objects'], 6)
        
        # Remove root - cycle should be detected
        gc_remove_root(list1)
        gc_collect()
        
        stats = gc_stats()
        self.assertEqual(stats['live_objects'], 0)
        self.assertGreater(stats['cycles_detected'], 0)


if __name__ == '__main__':
    unittest.main()