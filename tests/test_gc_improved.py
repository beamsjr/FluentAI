"""
Tests for improved garbage collector
"""

import unittest
import time
import gc as python_gc

from src.vm.gc_integration import (
    VMGarbageCollector, VMValue, VMEnvironment, VMClosure, VMList
)


class TestGenerationalGC(unittest.TestCase):
    """Test generational garbage collector"""
    
    def setUp(self):
        # Disable Python's GC to avoid interference
        python_gc.disable()
        
        # Create GC with small thresholds for testing
        self.gc = VMGarbageCollector(
            young_size=10,
            young_threshold=5,
            old_threshold=3,
            promotion_age=2
        )
    
    def tearDown(self):
        self.gc.shutdown()
        python_gc.enable()
    
    def test_basic_allocation(self):
        """Test basic object allocation"""
        # Allocate some values
        v1 = self.gc.allocate_value(42)
        v2 = self.gc.allocate_value("hello")
        v3 = self.gc.allocate_value([1, 2, 3])
        
        self.assertEqual(v1.data, 42)
        self.assertEqual(v2.data, "hello")
        self.assertEqual(v3.data, [1, 2, 3])
        
        # Check statistics
        stats = self.gc.get_statistics()
        self.assertEqual(stats['young_size'], 3)
    
    def test_young_generation_collection(self):
        """Test young generation collection"""
        # Allocate and root some objects
        root_env = self.gc.allocate_environment()
        self.gc.add_root(root_env)
        
        # Get initial collection count
        initial_stats = self.gc.get_statistics()
        initial_collections = initial_stats['young_collections']
        
        # Allocate values and bind some
        for i in range(10):
            value = self.gc.allocate_value(i)
            if i % 2 == 0:
                root_env.bind(f"v{i}", value, self.gc.gc)
        
        # Force young collection
        self.gc.collect()
        
        # Check that rooted objects survived
        self.assertIsNotNone(root_env.lookup("v0"))
        self.assertIsNotNone(root_env.lookup("v2"))
        
        # Check statistics
        stats = self.gc.get_statistics()
        self.assertGreater(stats['objects_collected'], 0)
        self.assertGreater(stats['young_collections'], initial_collections)
    
    def test_object_promotion(self):
        """Test promotion to old generation"""
        root = self.gc.allocate_environment()
        self.gc.add_root(root)
        
        # Create object that will survive multiple collections
        survivor = self.gc.allocate_value("survivor")
        root.bind("survivor", survivor, self.gc.gc)
        
        initial_stats = self.gc.get_statistics()
        
        # Force multiple young collections
        for i in range(3):
            # Allocate garbage
            for j in range(5):
                self.gc.allocate_value(f"garbage_{i}_{j}")
            self.gc.collect()
        
        # Check promotion happened
        stats = self.gc.get_statistics()
        self.assertGreater(stats['objects_promoted'], 0)
        self.assertEqual(survivor.generation.value, 1)  # OLD generation
    
    def test_write_barriers(self):
        """Test write barrier functionality"""
        # Create old generation object
        old_env = self.gc.allocate_environment()
        self.gc.add_root(old_env)
        
        # Force it to old generation
        for _ in range(3):
            self.gc.collect()
        
        # Create young object
        young_value = self.gc.allocate_value("young")
        
        # Write barrier should add old_env to remembered set
        old_env.bind("young_ref", young_value, self.gc.gc)
        
        self.assertIn(old_env, self.gc.gc.remembered_set)
    
    def test_circular_references(self):
        """Test handling of circular references"""
        # Create circular reference
        env1 = self.gc.allocate_environment()
        env2 = self.gc.allocate_environment()
        
        v1 = self.gc.allocate_value(env2)
        v2 = self.gc.allocate_value(env1)
        
        env1.bind("ref", v1, self.gc.gc)
        env2.bind("ref", v2, self.gc.gc)
        
        # Root one of them
        self.gc.add_root(env1)
        
        # Collect - should keep both alive due to root
        self.gc.collect_full()
        
        self.assertIsNotNone(env1.lookup("ref"))
        
        # Remove root
        self.gc.remove_root(env1)
        
        # Collect - should collect both
        self.gc.collect_full()
        
        stats = self.gc.get_statistics()
        self.assertGreater(stats['objects_collected'], 0)
    
    def test_list_operations(self):
        """Test list with GC"""
        # Create list
        lst = self.gc.allocate_list()
        self.gc.add_root(lst)
        
        # Add elements
        for i in range(5):
            value = self.gc.allocate_value(i)
            lst.append(value, self.gc.gc)
        
        # Collect
        self.gc.collect()
        
        # All elements should survive
        self.assertEqual(len(lst.elements), 5)
        
        # Replace element
        new_value = self.gc.allocate_value(999)
        lst.set_element(2, new_value, self.gc.gc)
        
        # Old value should be collectable
        self.gc.collect()
        
        self.assertEqual(lst.elements[2].data, 999)
    
    def test_closure_capture(self):
        """Test closure environment capture"""
        # Create environment with bindings
        env = self.gc.allocate_environment()
        self.gc.add_root(env)
        
        x = self.gc.allocate_value(10)
        y = self.gc.allocate_value(20)
        env.bind("x", x, self.gc.gc)
        env.bind("y", y, self.gc.gc)
        
        # Create closure capturing environment
        closure = self.gc.allocate_closure(
            ["a", "b"], "body_id", env, "test_fn"
        )
        self.gc.add_root(closure)
        
        # Remove env root - should still be alive via closure
        self.gc.remove_root(env)
        self.gc.collect_full()
        
        # Environment should still be accessible
        self.assertEqual(closure.captured_env.lookup("x").data, 10)
        self.assertEqual(closure.captured_env.lookup("y").data, 20)
    
    def test_gc_performance(self):
        """Test GC pause times"""
        # Allocate many objects
        root = self.gc.allocate_environment()
        self.gc.add_root(root)
        
        # Time allocations and collections
        start = time.time()
        
        for i in range(100):
            value = self.gc.allocate_value(f"value_{i}")
            if i % 10 == 0:
                root.bind(f"v{i}", value, self.gc.gc)
        
        alloc_time = time.time() - start
        
        # Force collections
        start = time.time()
        self.gc.collect()
        young_gc_time = time.time() - start
        
        start = time.time()
        self.gc.collect_full()
        old_gc_time = time.time() - start
        
        # Check pause times are reasonable
        stats = self.gc.get_statistics()
        self.assertLess(stats['max_pause_time'], 0.01)  # 10ms max
        self.assertLess(stats['avg_pause_time'], 0.005)  # 5ms average
        
        print(f"\nGC Performance:")
        print(f"  Allocation time: {alloc_time*1000:.2f}ms")
        print(f"  Young GC time: {young_gc_time*1000:.2f}ms")
        print(f"  Old GC time: {old_gc_time*1000:.2f}ms")
        print(f"  Max pause: {stats['max_pause_time']*1000:.2f}ms")
        print(f"  Avg pause: {stats['avg_pause_time']*1000:.2f}ms")
    
    def test_disable_write_barriers(self):
        """Test disabling write barriers for bulk operations"""
        env = self.gc.allocate_environment()
        self.gc.add_root(env)
        
        # Bulk insert with barriers disabled
        with self.gc.with_disabled_barriers():
            for i in range(100):
                value = self.gc.allocate_value(i)
                env.bind(f"bulk_{i}", value)
        
        # Re-enable and test normal operation
        new_value = self.gc.allocate_value("after_bulk")
        env.bind("after", new_value, self.gc.gc)
        
        # Should work normally
        self.gc.collect()
        self.assertIsNotNone(env.lookup("after"))


if __name__ == "__main__":
    unittest.main()