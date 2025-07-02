"""
Tests for VM with improved generational GC
"""

import unittest
import gc as python_gc

from src.vm.vm_gc_improved import GenerationalGCVM, create_generational_gc_vm
from src.vm.bytecode import BytecodeChunk, Instruction, Opcode


class TestGenerationalGCVM(unittest.TestCase):
    """Test VM with generational GC"""
    
    def setUp(self):
        python_gc.disable()
        self.vm = create_generational_gc_vm(
            young_size=50,
            young_threshold=20,
            old_threshold=3,
            promotion_age=2
        )
    
    def tearDown(self):
        del self.vm
        python_gc.enable()
    
    def test_basic_arithmetic(self):
        """Test basic arithmetic operations"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        chunk.add_constant(10)
        chunk.add_constant(20)
        
        chunk.add_instruction(Opcode.PUSH, 0)  # Push 10
        chunk.add_instruction(Opcode.PUSH, 1)  # Push 20
        chunk.add_instruction(Opcode.ADD, 0)   # Add
        
        result = self.vm.execute(chunk)
        self.assertEqual(result, 30)
        
        # Check GC stats
        stats = self.vm.gc.get_statistics()
        self.assertGreater(stats['young_size'], 0)
    
    def test_list_operations(self):
        """Test list operations with GC"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        chunk.add_constant(1)
        chunk.add_constant(2)
        chunk.add_constant(3)
        
        # Build list [1, 2, 3]
        chunk.add_instruction(Opcode.PUSH, 0)  # Push 1
        chunk.add_instruction(Opcode.PUSH, 1)  # Push 2
        chunk.add_instruction(Opcode.PUSH, 2)  # Push 3
        chunk.add_instruction(Opcode.MAKE_LIST, 3)
        
        # Get first element
        chunk.add_instruction(Opcode.LIST_HEAD, 0)  # Get first element
        
        result = self.vm.execute(chunk)
        self.assertEqual(result, 1)
    
    def test_global_variables(self):
        """Test global variable storage and retrieval"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        chunk.add_constant("x")
        chunk.add_constant(42)
        
        # Store 42 in global x
        chunk.add_instruction(Opcode.PUSH, 1)  # Push 42
        chunk.add_instruction(Opcode.STORE_GLOBAL, 0)  # Store in x
        
        # Load global x
        chunk.add_instruction(Opcode.LOAD_GLOBAL, 0)  # Load x
        
        result = self.vm.execute(chunk)
        self.assertEqual(result, 42)
    
    def test_gc_collection(self):
        """Test that GC collections happen"""
        initial_stats = self.vm.gc.get_statistics()
        
        chunk = BytecodeChunk(instructions=[], constants=[])
        
        # Allocate many values to trigger GC
        for i in range(30):
            chunk.add_constant(i)
            chunk.add_instruction(Opcode.PUSH, i)
            chunk.add_instruction(Opcode.POP, 0)
        
        self.vm.execute(chunk)
        
        final_stats = self.vm.gc.get_statistics()
        
        # Should have triggered at least one young collection
        self.assertGreater(final_stats['young_collections'], 
                          initial_stats['young_collections'])
    
    def test_object_promotion(self):
        """Test that objects get promoted to old generation"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        chunk.add_constant("survivor")
        chunk.add_constant("long-lived value")
        
        # Store value that will survive multiple collections
        chunk.add_instruction(Opcode.PUSH, 1)
        chunk.add_instruction(Opcode.STORE_GLOBAL, 0)
        
        # Force multiple collections by allocating garbage
        for _ in range(5):
            # Create temporary values
            for i in range(25):
                chunk.add_constant(f"temp_{i}")
                chunk.add_instruction(Opcode.PUSH, len(chunk.constants) - 1)
                chunk.add_instruction(Opcode.POP, 0)
        
        # Load the survivor
        chunk.add_instruction(Opcode.LOAD_GLOBAL, 0)
        
        result = self.vm.execute(chunk)
        self.assertEqual(result, "long-lived value")
        
        # Check that objects were promoted
        stats = self.vm.gc.get_statistics()
        self.assertGreater(stats['objects_promoted'], 0)
        self.assertGreater(stats['old_size'], 0)
    
    def test_closure_creation(self):
        """Test closure creation with GC"""
        chunk = BytecodeChunk(instructions=[], constants=[])
        
        # Function info
        func_info = {
            'params': ['x', 'y'],
            'body_id': 'func_123',
            'name': 'test_func'
        }
        chunk.add_constant(func_info)
        
        # Create closure
        chunk.add_instruction(Opcode.MAKE_FUNC, 0)
        
        result = self.vm.execute(chunk)
        self.assertIsNotNone(result)
        self.assertEqual(result.params, ['x', 'y'])
        self.assertEqual(result.name, 'test_func')
    
    def test_memory_cleanup(self):
        """Test that memory is properly cleaned up"""
        # Create and destroy many objects
        for _ in range(10):
            chunk = BytecodeChunk(instructions=[], constants=[])
            
            # Create lists and values
            for i in range(10):
                chunk.add_constant(i)
                chunk.add_instruction(Opcode.PUSH, i)
            chunk.add_instruction(Opcode.MAKE_LIST, 10)
            chunk.add_instruction(Opcode.POP, 0)
            
            self.vm.execute(chunk)
        
        # Force full GC
        stats = self.vm.gc_collect_full()
        
        # Most objects should have been collected
        self.assertLess(stats['young_size'] + stats['old_size'], 10)


if __name__ == "__main__":
    unittest.main()