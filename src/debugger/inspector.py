"""
Runtime value inspector for ClaudeLang debugger

Provides detailed inspection of values and data structures.
"""

from typing import Any, Dict, List, Optional, Tuple, Set
from dataclasses import dataclass, field
import inspect
import gc
import sys

from ..core.ast import ASTNode, Lambda, Function
from ..interpreter.interpreter import Environment

# Define mock Closure if not available
try:
    from ..interpreter.evaluator import Closure
except ImportError:
    class Closure:
        """Mock closure for compatibility"""
        pass


@dataclass
class InspectionResult:
    """Result of inspecting a value"""
    value: Any
    type_name: str
    repr: str
    children: Optional[List[Tuple[str, Any]]] = field(default=None)
    metadata: Optional[Dict[str, Any]] = field(default=None)


class Inspector:
    """Inspector for runtime values"""
    
    def __init__(self, max_depth: int = 5, max_items: int = 100):
        self.max_depth = max_depth
        self.max_items = max_items
        self._inspected_ids: Set[int] = set()
    
    def inspect(self, value: Any, depth: int = 0) -> InspectionResult:
        """Inspect a value and return detailed information"""
        # Avoid infinite recursion
        obj_id = id(value)
        if obj_id in self._inspected_ids and depth > 0:
            return InspectionResult(
                value=value,
                type_name=type(value).__name__,
                repr="<circular reference>"
            )
        
        self._inspected_ids.add(obj_id)
        
        try:
            if value is None:
                return self._inspect_nil()
            elif isinstance(value, bool):
                return self._inspect_bool(value)
            elif isinstance(value, (int, float)):
                return self._inspect_number(value)
            elif isinstance(value, str):
                return self._inspect_string(value)
            elif isinstance(value, list):
                return self._inspect_list(value, depth)
            elif isinstance(value, dict):
                return self._inspect_dict(value, depth)
            elif isinstance(value, Closure):
                return self._inspect_closure(value)
            elif isinstance(value, Function):
                return self._inspect_function(value)
            elif isinstance(value, ASTNode):
                return self._inspect_ast_node(value, depth)
            elif isinstance(value, Environment):
                return self._inspect_environment(value, depth)
            else:
                return self._inspect_generic(value, depth)
        finally:
            self._inspected_ids.remove(obj_id)
    
    def _inspect_nil(self) -> InspectionResult:
        """Inspect nil value"""
        return InspectionResult(
            value=None,
            type_name="nil",
            repr="nil"
        )
    
    def _inspect_bool(self, value: bool) -> InspectionResult:
        """Inspect boolean value"""
        return InspectionResult(
            value=value,
            type_name="bool",
            repr="true" if value else "false"
        )
    
    def _inspect_number(self, value: float) -> InspectionResult:
        """Inspect numeric value"""
        type_name = "int" if isinstance(value, int) else "float"
        return InspectionResult(
            value=value,
            type_name=type_name,
            repr=str(value),
            metadata={
                "hex": hex(int(value)) if isinstance(value, int) else None,
                "binary": bin(int(value)) if isinstance(value, int) else None
            }
        )
    
    def _inspect_string(self, value: str) -> InspectionResult:
        """Inspect string value"""
        # Truncate long strings
        display = value
        if len(value) > 100:
            display = value[:100] + "..."
        
        return InspectionResult(
            value=value,
            type_name="string",
            repr=repr(display),
            metadata={
                "length": len(value),
                "truncated": len(value) > 100
            }
        )
    
    def _inspect_list(self, value: list, depth: int) -> InspectionResult:
        """Inspect list value"""
        children = []
        
        if depth < self.max_depth:
            for i, item in enumerate(value[:self.max_items]):
                children.append((f"[{i}]", item))
        
        repr_items = []
        for i, item in enumerate(value[:5]):  # Show first 5 items in repr
            if isinstance(item, str):
                repr_items.append(repr(item))
            else:
                repr_items.append(str(item))
        
        repr_str = "[" + ", ".join(repr_items)
        if len(value) > 5:
            repr_str += f", ... ({len(value) - 5} more)"
        repr_str += "]"
        
        return InspectionResult(
            value=value,
            type_name="list",
            repr=repr_str,
            children=children if children else None,
            metadata={
                "length": len(value),
                "truncated": len(value) > self.max_items
            }
        )
    
    def _inspect_dict(self, value: dict, depth: int) -> InspectionResult:
        """Inspect dictionary value"""
        children = []
        
        if depth < self.max_depth:
            for i, (k, v) in enumerate(list(value.items())[:self.max_items]):
                children.append((str(k), v))
        
        repr_items = []
        for i, (k, v) in enumerate(list(value.items())[:3]):  # Show first 3 items
            if isinstance(v, str):
                v_repr = repr(v)
            else:
                v_repr = str(v)
            repr_items.append(f"{k}: {v_repr}")
        
        repr_str = "{" + ", ".join(repr_items)
        if len(value) > 3:
            repr_str += f", ... ({len(value) - 3} more)"
        repr_str += "}"
        
        return InspectionResult(
            value=value,
            type_name="map",
            repr=repr_str,
            children=children if children else None,
            metadata={
                "size": len(value),
                "truncated": len(value) > self.max_items
            }
        )
    
    def _inspect_closure(self, value: Closure) -> InspectionResult:
        """Inspect closure value"""
        children = []
        
        # Add parameters
        if hasattr(value, 'params'):
            children.append(("parameters", value.params))
        
        # Add captured environment
        if hasattr(value, 'env'):
            children.append(("environment", value.env))
        
        param_str = ""
        if hasattr(value, 'params'):
            param_str = f"({', '.join(value.params)})"
        
        return InspectionResult(
            value=value,
            type_name="closure",
            repr=f"<closure{param_str}>",
            children=children if children else None,
            metadata={
                "arity": len(value.params) if hasattr(value, 'params') else 0
            }
        )
    
    def _inspect_function(self, value: Function) -> InspectionResult:
        """Inspect built-in function"""
        return InspectionResult(
            value=value,
            type_name="function",
            repr=f"<function {value.name}>",
            metadata={
                "name": value.name,
                "arity": value.arity,
                "effects": [e.name for e in value.effects] if value.effects else []
            }
        )
    
    def _inspect_ast_node(self, value: ASTNode, depth: int) -> InspectionResult:
        """Inspect AST node"""
        node_type = type(value).__name__
        children = []
        
        if depth < self.max_depth:
            # Get node attributes
            for attr_name in dir(value):
                if not attr_name.startswith('_'):
                    attr_value = getattr(value, attr_name, None)
                    if attr_value is not None and not callable(attr_value):
                        children.append((attr_name, attr_value))
        
        return InspectionResult(
            value=value,
            type_name=f"AST.{node_type}",
            repr=f"<{node_type}>",
            children=children if children else None,
            metadata={
                "node_type": node_type,
                "source_location": getattr(value, 'source_location', None)
            }
        )
    
    def _inspect_environment(self, value: Environment, depth: int) -> InspectionResult:
        """Inspect environment"""
        children = []
        
        if depth < self.max_depth and hasattr(value, 'bindings'):
            for name, val in list(value.bindings.items())[:self.max_items]:
                children.append((name, val))
        
        binding_count = len(value.bindings) if hasattr(value, 'bindings') else 0
        
        return InspectionResult(
            value=value,
            type_name="environment",
            repr=f"<environment: {binding_count} bindings>",
            children=children if children else None,
            metadata={
                "binding_count": binding_count,
                "has_parent": hasattr(value, 'parent') and value.parent is not None
            }
        )
    
    def _inspect_generic(self, value: Any, depth: int) -> InspectionResult:
        """Inspect generic Python object"""
        type_name = type(value).__name__
        children = []
        
        if depth < self.max_depth:
            # Try to get attributes
            try:
                for attr_name in dir(value):
                    if not attr_name.startswith('_'):
                        attr_value = getattr(value, attr_name, None)
                        if attr_value is not None and not callable(attr_value):
                            children.append((attr_name, attr_value))
            except:
                pass
        
        return InspectionResult(
            value=value,
            type_name=type_name,
            repr=repr(value)[:100],  # Truncate long reprs
            children=children[:self.max_items] if children else None
        )
    
    def format_inspection(self, result: InspectionResult, indent: int = 0) -> str:
        """Format inspection result as string"""
        lines = []
        prefix = "  " * indent
        
        # Main line
        lines.append(f"{prefix}{result.type_name}: {result.repr}")
        
        # Metadata
        if result.metadata:
            for key, value in result.metadata.items():
                if value is not None:
                    lines.append(f"{prefix}  {key}: {value}")
        
        # Children
        if result.children and indent < self.max_depth:
            for name, child_value in result.children[:10]:  # Limit display
                child_result = self.inspect(child_value, indent + 1)
                lines.append(f"{prefix}  {name}:")
                lines.append(self.format_inspection(child_result, indent + 2))
        
        return "\n".join(lines)
    
    def get_object_info(self, obj: Any) -> Dict[str, Any]:
        """Get detailed information about an object"""
        info = {
            "type": type(obj).__name__,
            "id": id(obj),
            "repr": repr(obj)[:200],
            "size": None,
            "references": None
        }
        
        # Try to get size
        try:
            info["size"] = sys.getsizeof(obj)
        except:
            pass
        
        # Get reference count
        info["references"] = sys.getrefcount(obj) - 1  # Subtract our reference
        
        # Get attributes
        attrs = []
        try:
            for name in dir(obj):
                if not name.startswith('_'):
                    attrs.append(name)
        except:
            pass
        info["attributes"] = attrs[:20]  # Limit
        
        # Check if it's tracked by GC
        info["gc_tracked"] = gc.is_tracked(obj)
        
        return info