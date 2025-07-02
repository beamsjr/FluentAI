"""
Tests for ClaudeLang debugger
"""

import unittest
import threading
import time
from pathlib import Path

from src.debugger import Debugger, DebuggerState, Breakpoint
from src.debugger.debug_session import DebugSession
from src.debugger.inspector import Inspector, InspectionResult
from src.core.ast import Literal, Lambda, FunctionCall
from src.core.graph import Graph
from src.interpreter.interpreter import Environment

class Closure:
    """Mock closure for testing"""
    def __init__(self):
        self.params = []
        self.env = None


class TestDebugger(unittest.TestCase):
    """Test core debugger functionality"""
    
    def test_create_debugger(self):
        """Test debugger creation"""
        debugger = Debugger()
        assert debugger.state == DebuggerState.IDLE
        assert len(debugger.breakpoints) == 0
        assert len(debugger.call_stack) == 0
    
    def test_add_breakpoint(self):
        """Test adding breakpoints"""
        debugger = Debugger()
        
        # Add simple breakpoint
        bp = debugger.add_breakpoint("test.cl", 10)
        assert bp.id == 1
        assert bp.file == "test.cl"
        assert bp.line == 10
        assert bp.enabled
        assert bp.hit_count == 0
        
        # Add conditional breakpoint
        bp2 = debugger.add_breakpoint("test.cl", 20, condition="x > 5")
        assert bp2.id == 2
        assert bp2.condition == "x > 5"
        
        # Add log breakpoint
        bp3 = debugger.add_breakpoint("test.cl", 30, log_message="x = {x}")
        assert bp3.log_message == "x = {x}"
    
    def test_remove_breakpoint(self):
        """Test removing breakpoints"""
        debugger = Debugger()
        
        bp1 = debugger.add_breakpoint("test.cl", 10)
        bp2 = debugger.add_breakpoint("test.cl", 20)
        
        assert len(debugger.breakpoints) == 2
        
        # Remove first breakpoint
        assert debugger.remove_breakpoint(bp1.id)
        assert len(debugger.breakpoints) == 1
        assert bp1.id not in debugger.breakpoints
        
        # Try to remove non-existent
        assert not debugger.remove_breakpoint(999)
    
    def test_toggle_breakpoint(self):
        """Test toggling breakpoint state"""
        debugger = Debugger()
        
        bp = debugger.add_breakpoint("test.cl", 10)
        assert bp.enabled
        
        # Toggle off
        debugger.toggle_breakpoint(bp.id)
        assert not debugger.breakpoints[bp.id].enabled
        
        # Toggle on
        debugger.toggle_breakpoint(bp.id)
        assert debugger.breakpoints[bp.id].enabled
    
    def test_clear_breakpoints(self):
        """Test clearing breakpoints"""
        debugger = Debugger()
        
        # Add breakpoints in different files
        debugger.add_breakpoint("test1.cl", 10)
        debugger.add_breakpoint("test1.cl", 20)
        debugger.add_breakpoint("test2.cl", 30)
        
        # Clear specific file
        count = debugger.clear_breakpoints("test1.cl")
        assert count == 2
        assert len(debugger.breakpoints) == 1
        
        # Clear all
        count = debugger.clear_breakpoints()
        assert count == 1
        assert len(debugger.breakpoints) == 0
    
    def test_execution_control(self):
        """Test execution control methods"""
        debugger = Debugger()
        
        # Start in idle state
        assert debugger.state == DebuggerState.IDLE
        
        # Simulate paused state
        debugger.state = DebuggerState.PAUSED
        
        # Continue
        debugger.continue_execution()
        assert debugger.state == DebuggerState.RUNNING
        
        # Pause
        debugger.pause_execution()
        assert debugger.state == DebuggerState.PAUSED
        
        # Step operations
        debugger.step_over()
        assert debugger.state == DebuggerState.STEPPING_OVER
        
        debugger.state = DebuggerState.PAUSED
        debugger.step_into()
        assert debugger.state == DebuggerState.STEPPING_IN
        
        debugger.state = DebuggerState.PAUSED
        debugger.step_out()
        assert debugger.state == DebuggerState.STEPPING_OUT
    
    def test_watch_expressions(self):
        """Test watch expression management"""
        debugger = Debugger()
        
        # Add watches
        debugger.add_watch("x", "x + 1")
        debugger.add_watch("list_len", "(length my-list)")
        
        assert len(debugger.watches) == 2
        assert debugger.watches["x"] == "x + 1"
        
        # Remove watch
        assert debugger.remove_watch("x")
        assert len(debugger.watches) == 1
        assert "x" not in debugger.watches
        
        # Remove non-existent
        assert not debugger.remove_watch("xyz")
    
    def test_event_handlers(self):
        """Test event handler registration and emission"""
        debugger = Debugger()
        events = []
        
        def handler(event):
            events.append(event)
        
        # Register handler
        debugger.register_event_handler("paused", handler)
        debugger.register_event_handler("continued", handler)
        
        # Emit events
        debugger._emit_event("paused", {"reason": "breakpoint"})
        debugger._emit_event("continued", {})
        
        # Check events were received
        assert len(events) == 2
        assert events[0].type == "paused"
        assert events[0].data["reason"] == "breakpoint"
        assert events[1].type == "continued"
    
    def test_breakpoint_lookup(self):
        """Test breakpoint lookup by location"""
        debugger = Debugger()
        
        # Add multiple breakpoints at same location
        bp1 = debugger.add_breakpoint("test.cl", 10)
        bp2 = debugger.add_breakpoint("test.cl", 10, condition="x > 0")
        bp3 = debugger.add_breakpoint("test.cl", 20)
        
        # Check lookup
        key = ("test.cl", 10)
        assert key in debugger._breakpoint_lookup
        assert len(debugger._breakpoint_lookup[key]) == 2
        assert bp1 in debugger._breakpoint_lookup[key]
        assert bp2 in debugger._breakpoint_lookup[key]
    
    def test_stack_management(self):
        """Test call stack management"""
        debugger = Debugger()
        
        # Simulate function calls
        debugger.on_function_enter("main", [])
        assert len(debugger.call_stack) == 1
        assert debugger.call_stack[0].name == "main"
        assert debugger.step_depth == 1
        
        debugger.on_function_enter("helper", [42])
        assert len(debugger.call_stack) == 2
        assert debugger.call_stack[1].name == "helper"
        assert debugger.call_stack[1].locals["arguments"] == [42]
        assert debugger.step_depth == 2
        
        # Exit functions
        debugger.on_function_exit("helper", "result")
        assert len(debugger.call_stack) == 1
        assert debugger.step_depth == 1
        
        debugger.on_function_exit("main", None)
        assert len(debugger.call_stack) == 0
        assert debugger.step_depth == 0


class TestDebugSession(unittest.TestCase):
    """Test debug session management"""
    
    def test_create_session(self):
        """Test session creation"""
        session = DebugSession()
        assert session.debugger is not None
        assert not session.is_running
        assert session.current_file is None
    
    def test_session_config(self):
        """Test session configuration"""
        session = DebugSession()
        
        # Default config
        assert not session.config["stopOnEntry"]
        assert session.config["showReturnValue"]
        
        # Update config
        session.set_configuration({
            "stopOnEntry": True,
            "maxStringLength": 50
        })
        assert session.config["stopOnEntry"]
        assert session.config["maxStringLength"] == 50
    
    def test_source_loading(self):
        """Test source file loading and caching"""
        session = DebugSession()
        
        # Create test file
        test_file = Path("test_debug.cl")
        test_file.write_text("(def x 42)\n(print x)")
        
        try:
            # Load source
            source = session._load_source(str(test_file))
            assert source == "(def x 42)\n(print x)"
            
            # Check cache
            assert str(test_file) in session.source_cache
            lines = session.get_source_lines(str(test_file), 1, 2)
            assert lines == ["(def x 42)", "(print x)"]
            
            # Get subset of lines
            lines = session.get_source_lines(str(test_file), 2, 2)
            assert lines == ["(print x)"]
            
        finally:
            test_file.unlink()
    
    def test_value_formatting(self):
        """Test value formatting for display"""
        session = DebugSession()
        
        # Basic types
        assert session.format_value(None) == "nil"
        assert session.format_value(True) == "true"
        assert session.format_value(False) == "false"
        assert session.format_value(42) == "42"
        assert session.format_value(3.14) == "3.14"
        assert session.format_value("hello") == '"hello"'
        
        # Lists
        assert session.format_value([1, 2, 3]) == '[1, 2, 3]'
        assert session.format_value([]) == '[]'
        
        # Long string truncation
        session.config["maxStringLength"] = 10
        assert session.format_value("a" * 20) == '"aaaaaaaaaa..."'
        
        # Long array truncation
        session.config["maxArrayLength"] = 3
        assert session.format_value([1, 2, 3, 4, 5]) == '[1, 2, 3, ... (2 more)]'
        
        # Maps
        assert session.format_value({"a": 1, "b": 2}) in ['{a: 1, b: 2}', '{b: 2, a: 1}']
        
        # Large map truncation
        large_map = {f"k{i}": i for i in range(20)}
        formatted = session.format_value(large_map)
        assert "... (10 more)" in formatted
    
    def test_thread_info(self):
        """Test thread information"""
        session = DebugSession()
        
        threads = session.get_threads()
        assert len(threads) == 1
        assert threads[0]["id"] == 1
        assert threads[0]["name"] == "main"
    
    def test_loaded_sources(self):
        """Test loaded sources tracking"""
        session = DebugSession()
        
        # Initially empty
        sources = session.get_loaded_sources()
        assert len(sources) == 0
        
        # Add to cache
        session.source_cache["/path/to/test.cl"] = ["line1", "line2"]
        session.source_cache["/path/to/lib.cl"] = ["(def x 1)"]
        
        sources = session.get_loaded_sources()
        assert len(sources) == 2
        
        # Check format
        for source in sources:
            assert "name" in source
            assert "path" in source
            assert "sourceReference" in source
            assert source["sourceReference"] == 0


class TestInspector(unittest.TestCase):
    """Test value inspector"""
    
    def test_create_inspector(self):
        """Test inspector creation"""
        inspector = Inspector()
        assert inspector.max_depth == 5
        assert inspector.max_items == 100
    
    def test_inspect_primitives(self):
        """Test inspecting primitive values"""
        inspector = Inspector()
        
        # Nil
        result = inspector.inspect(None)
        assert result.type_name == "nil"
        assert result.repr == "nil"
        
        # Bool
        result = inspector.inspect(True)
        assert result.type_name == "bool"
        assert result.repr == "true"
        
        result = inspector.inspect(False)
        assert result.type_name == "bool"
        assert result.repr == "false"
        
        # Numbers
        result = inspector.inspect(42)
        assert result.type_name == "int"
        assert result.repr == "42"
        assert result.metadata["hex"] == "0x2a"
        assert result.metadata["binary"] == "0b101010"
        
        result = inspector.inspect(3.14)
        assert result.type_name == "float"
        assert result.repr == "3.14"
        
        # String
        result = inspector.inspect("hello")
        assert result.type_name == "string"
        assert result.repr == "'hello'"
        assert result.metadata["length"] == 5
        assert not result.metadata["truncated"]
        
        # Long string
        long_str = "x" * 200
        result = inspector.inspect(long_str)
        assert result.type_name == "string"
        assert "..." in result.repr
        assert result.metadata["truncated"]
    
    def test_inspect_collections(self):
        """Test inspecting collections"""
        inspector = Inspector()
        
        # List
        result = inspector.inspect([1, 2, 3])
        assert result.type_name == "list"
        assert result.repr == "[1, 2, 3]"
        assert result.metadata["length"] == 3
        assert result.children is not None
        assert len(result.children) == 3
        assert result.children[0] == ("[0]", 1)
        
        # Large list
        large_list = list(range(200))
        result = inspector.inspect(large_list)
        assert "... (195 more)" in result.repr
        assert result.metadata["truncated"]
        
        # Dict
        result = inspector.inspect({"a": 1, "b": 2})
        assert result.type_name == "map"
        assert result.metadata["size"] == 2
        assert result.children is not None
        assert len(result.children) == 2
    
    def test_inspect_closure(self):
        """Test inspecting closures"""
        inspector = Inspector()
        
        # Create a mock closure
        closure = Closure()
        closure.params = ["x", "y"]
        closure.env = Environment()
        
        result = inspector.inspect(closure)
        assert result.type_name == "closure"
        assert result.repr == "<closure(x, y)>"
        assert result.metadata["arity"] == 2
        assert result.children is not None
        assert len(result.children) == 2  # parameters and environment
    
    def test_inspect_ast_node(self):
        """Test inspecting AST nodes"""
        inspector = Inspector()
        
        # Create a literal node
        node = Literal()
        node.value = 42
        node.source_location = {"file": "test.cl", "line": 10, "column": 5}
        
        result = inspector.inspect(node)
        assert result.type_name == "AST.Literal"
        assert result.repr == "<Literal>"
        assert result.metadata["node_type"] == "Literal"
        assert result.metadata["source_location"] == node.source_location
    
    def test_circular_reference_handling(self):
        """Test handling of circular references"""
        inspector = Inspector()
        
        # Create circular structure
        obj = {"a": 1}
        obj["self"] = obj
        
        result = inspector.inspect(obj)
        assert result.type_name == "map"
        # Should not infinite loop
        assert result.children is not None
    
    def test_format_inspection(self):
        """Test formatting inspection results"""
        inspector = Inspector()
        
        # Simple value
        result = inspector.inspect(42)
        formatted = inspector.format_inspection(result)
        assert "int: 42" in formatted
        assert "hex: 0x2a" in formatted
        
        # Nested structure
        result = inspector.inspect({"name": "test", "values": [1, 2, 3]})
        formatted = inspector.format_inspection(result)
        assert "map:" in formatted
        assert "name:" in formatted
        assert "values:" in formatted
        assert "list: [1, 2, 3]" in formatted
    
    def test_object_info(self):
        """Test getting detailed object information"""
        inspector = Inspector()
        
        obj = {"test": 123}
        info = inspector.get_object_info(obj)
        
        assert info["type"] == "dict"
        assert info["id"] == id(obj)
        assert "repr" in info
        assert info["references"] >= 1
        assert isinstance(info["attributes"], list)
        assert info["gc_tracked"] is not None


class TestDebuggerIntegration(unittest.TestCase):
    """Test debugger integration with interpreter"""
    
    def test_breakpoint_hit(self):
        """Test hitting breakpoints during execution"""
        # This would require full integration with the interpreter
        # For now, we'll skip this test
        self.skipTest("Requires full interpreter integration")
    
    def test_step_execution(self):
        """Test step execution through code"""
        # This would require full integration with the interpreter
        # For now, we'll skip this test
        self.skipTest("Requires full interpreter integration")
    
    def test_debug_event_flow(self):
        """Test debug event flow"""
        debugger = Debugger()
        session = DebugSession(debugger)
        
        events = []
        
        def capture_event(event):
            events.append(event)
        
        # Register event handlers
        debugger.register_event_handler("paused", capture_event)
        debugger.register_event_handler("continued", capture_event)
        debugger.register_event_handler("terminated", capture_event)
        
        # Simulate debug session
        debugger.state = DebuggerState.PAUSED
        debugger._emit_event("paused", {"reason": "entry"})
        
        debugger.continue_execution()
        
        debugger._emit_event("terminated", {"exitCode": 0})
        
        # Check events
        assert len(events) >= 2
        assert any(e.type == "paused" for e in events)
        assert any(e.type == "continued" for e in events)


if __name__ == "__main__":
    unittest.main()