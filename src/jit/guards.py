"""
Guard-based Type Specialization for ClaudeLang JIT

This module implements runtime guards that enable the JIT compiler
to generate specialized code based on observed type patterns.
"""

from typing import Any, Dict, List, Optional, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum, auto
import ctypes

from ..types.type_system import TypeAnnotation, PrimitiveType, ListType, FunctionType


class GuardType(Enum):
    """Types of guards"""
    TYPE_CHECK = auto()      # Check exact type
    RANGE_CHECK = auto()     # Check numeric range
    SHAPE_CHECK = auto()     # Check data structure shape
    MONOMORPHIC = auto()     # Check single receiver type
    POLYMORPHIC = auto()     # Check limited set of types
    NULL_CHECK = auto()      # Check for null/none
    LENGTH_CHECK = auto()    # Check collection length
    CONSTANT = auto()        # Check for specific constant value


@dataclass
class GuardCondition:
    """A single guard condition"""
    guard_type: GuardType
    parameter_index: int  # Which parameter/local this applies to
    expected_value: Any
    fallback_allowed: bool = True
    
    def check_value(self, value: Any) -> bool:
        """Check if value satisfies this guard"""
        if self.guard_type == GuardType.TYPE_CHECK:
            return type(value).__name__ == self.expected_value
        
        elif self.guard_type == GuardType.RANGE_CHECK:
            if not isinstance(value, (int, float)):
                return False
            min_val, max_val = self.expected_value
            return min_val <= value <= max_val
        
        elif self.guard_type == GuardType.SHAPE_CHECK:
            if not hasattr(value, '__len__'):
                return False
            return len(value) == self.expected_value
        
        elif self.guard_type == GuardType.CONSTANT:
            return value == self.expected_value
        
        elif self.guard_type == GuardType.NULL_CHECK:
            return (value is None) == self.expected_value
        
        elif self.guard_type == GuardType.LENGTH_CHECK:
            if not hasattr(value, '__len__'):
                return False
            min_len, max_len = self.expected_value
            return min_len <= len(value) <= max_len
        
        elif self.guard_type == GuardType.MONOMORPHIC:
            return type(value) == self.expected_value
        
        elif self.guard_type == GuardType.POLYMORPHIC:
            return type(value) in self.expected_value
        
        return False


@dataclass 
class SpecializedCode:
    """Specialized native code with guards"""
    code_buffer: bytes
    entry_point: int
    guards: List[GuardCondition]
    specialization_id: str
    hit_count: int = 0
    miss_count: int = 0
    
    def check_guards(self, args: Tuple[Any, ...], locals: Dict[str, Any] = None) -> bool:
        """Check all guards for this specialization"""
        for guard in self.guards:
            if guard.parameter_index < len(args):
                value = args[guard.parameter_index]
            elif locals and guard.parameter_index >= 1000:
                # Local variables use indices >= 1000
                local_idx = guard.parameter_index - 1000
                local_name = f"local_{local_idx}"
                if local_name not in locals:
                    return False
                value = locals[local_name]
            else:
                return False
            
            if not guard.check_value(value):
                self.miss_count += 1
                return False
        
        self.hit_count += 1
        return True
    
    def execute(self, *args) -> Any:
        """Execute specialized code"""
        # Create function type based on argument count
        arg_types = [ctypes.c_long] * len(args)
        func_type = ctypes.CFUNCTYPE(ctypes.c_long, *arg_types)
        
        # Cast buffer to function
        func = func_type(self.entry_point)
        
        # Convert args to ctypes
        c_args = []
        for arg in args:
            if isinstance(arg, int):
                c_args.append(arg)
            elif isinstance(arg, bool):
                c_args.append(1 if arg else 0)
            else:
                # For now, only support integers
                raise TypeError(f"Unsupported type for JIT: {type(arg)}")
        
        return func(*c_args)


class TypeProfiler:
    """Tracks type information during execution"""
    
    def __init__(self):
        self.parameter_types: Dict[int, Dict[type, int]] = {}
        self.return_types: Dict[type, int] = {}
        self.constant_values: Dict[int, Dict[Any, int]] = {}
        self.numeric_ranges: Dict[int, Tuple[float, float]] = {}
        self.collection_lengths: Dict[int, Tuple[int, int]] = {}
        
    def record_parameter(self, index: int, value: Any):
        """Record parameter type information"""
        # Track type frequency
        if index not in self.parameter_types:
            self.parameter_types[index] = {}
        
        value_type = type(value)
        self.parameter_types[index][value_type] = \
            self.parameter_types[index].get(value_type, 0) + 1
        
        # Track constant values (for small integers)
        if isinstance(value, int) and -10 <= value <= 100:
            if index not in self.constant_values:
                self.constant_values[index] = {}
            self.constant_values[index][value] = \
                self.constant_values[index].get(value, 0) + 1
        
        # Track numeric ranges
        if isinstance(value, (int, float)):
            if index not in self.numeric_ranges:
                self.numeric_ranges[index] = (value, value)
            else:
                min_val, max_val = self.numeric_ranges[index]
                self.numeric_ranges[index] = (min(min_val, value), max(max_val, value))
        
        # Track collection lengths
        if hasattr(value, '__len__'):
            length = len(value)
            if index not in self.collection_lengths:
                self.collection_lengths[index] = (length, length)
            else:
                min_len, max_len = self.collection_lengths[index]
                self.collection_lengths[index] = (min(min_len, length), max(max_len, length))
    
    def record_return(self, value: Any):
        """Record return type information"""
        value_type = type(value)
        self.return_types[value_type] = self.return_types.get(value_type, 0) + 1
    
    def get_guards(self, threshold: float = 0.95) -> List[GuardCondition]:
        """Generate guards based on profiled information"""
        guards = []
        
        # Generate type guards for monomorphic parameters
        for param_idx, type_counts in self.parameter_types.items():
            total = sum(type_counts.values())
            for param_type, count in type_counts.items():
                ratio = count / total
                if ratio >= threshold:
                    # Monomorphic - single type dominates
                    guards.append(GuardCondition(
                        guard_type=GuardType.TYPE_CHECK,
                        parameter_index=param_idx,
                        expected_value=param_type.__name__
                    ))
                elif ratio >= 0.8 and len(type_counts) <= 3:
                    # Polymorphic - small set of types
                    guards.append(GuardCondition(
                        guard_type=GuardType.POLYMORPHIC,
                        parameter_index=param_idx,
                        expected_value=set(type_counts.keys())
                    ))
        
        # Generate constant guards for frequently constant parameters
        for param_idx, value_counts in self.constant_values.items():
            total = sum(value_counts.values())
            for value, count in value_counts.items():
                if count / total >= threshold:
                    guards.append(GuardCondition(
                        guard_type=GuardType.CONSTANT,
                        parameter_index=param_idx,
                        expected_value=value
                    ))
        
        # Generate range guards for numeric parameters
        for param_idx, (min_val, max_val) in self.numeric_ranges.items():
            if param_idx in self.parameter_types:
                # Only if mostly numeric
                type_counts = self.parameter_types[param_idx]
                numeric_count = sum(count for t, count in type_counts.items() 
                                  if t in (int, float))
                total = sum(type_counts.values())
                if numeric_count / total >= threshold:
                    # Tight range can enable optimizations
                    if max_val - min_val < 1000:
                        guards.append(GuardCondition(
                            guard_type=GuardType.RANGE_CHECK,
                            parameter_index=param_idx,
                            expected_value=(min_val, max_val)
                        ))
        
        # Generate length guards for collections
        for param_idx, (min_len, max_len) in self.collection_lengths.items():
            if min_len == max_len:
                # Fixed length - can unroll loops
                guards.append(GuardCondition(
                    guard_type=GuardType.SHAPE_CHECK,
                    parameter_index=param_idx,
                    expected_value=min_len
                ))
            elif max_len - min_len < 10:
                # Small range - can still optimize
                guards.append(GuardCondition(
                    guard_type=GuardType.LENGTH_CHECK,
                    parameter_index=param_idx,
                    expected_value=(min_len, max_len)
                ))
        
        return guards


class SpecializationCache:
    """Cache for specialized code versions"""
    
    def __init__(self, max_specializations: int = 5):
        self.specializations: List[SpecializedCode] = []
        self.max_specializations = max_specializations
        
    def find_specialization(self, args: Tuple[Any, ...], 
                          locals: Dict[str, Any] = None) -> Optional[SpecializedCode]:
        """Find a matching specialization"""
        for spec in self.specializations:
            if spec.check_guards(args, locals):
                return spec
        return None
    
    def add_specialization(self, spec: SpecializedCode):
        """Add a new specialization"""
        if len(self.specializations) >= self.max_specializations:
            # Evict least successful specialization
            self.specializations.sort(
                key=lambda s: s.hit_count / (s.hit_count + s.miss_count + 1)
            )
            self.specializations.pop(0)
        
        self.specializations.append(spec)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get specialization statistics"""
        total_hits = sum(s.hit_count for s in self.specializations)
        total_misses = sum(s.miss_count for s in self.specializations)
        
        return {
            "specialization_count": len(self.specializations),
            "total_hits": total_hits,
            "total_misses": total_misses,
            "hit_rate": total_hits / (total_hits + total_misses + 1),
            "specializations": [
                {
                    "id": s.specialization_id,
                    "guards": len(s.guards),
                    "hits": s.hit_count,
                    "misses": s.miss_count,
                    "success_rate": s.hit_count / (s.hit_count + s.miss_count + 1)
                }
                for s in self.specializations
            ]
        }


def generate_guard_code(codegen, guards: List[GuardCondition], 
                       fail_label: str) -> None:
    """Generate x86-64 code for guard checks"""
    from .x86_64_codegen import Register, Condition
    
    for guard in guards:
        if guard.guard_type == GuardType.TYPE_CHECK:
            # For now, we can only check integer types efficiently
            # Real implementation would need type tags
            if guard.expected_value == "int":
                # Assume integers are tagged with low bit = 0
                codegen.mov_reg_reg(Register.RAX, Register.RDI)  # First arg
                codegen.and_imm_reg(Register.RAX, 1)
                codegen.jcc(Condition.NOT_EQUAL, fail_label)
        
        elif guard.guard_type == GuardType.RANGE_CHECK:
            min_val, max_val = guard.expected_value
            # Check min
            codegen.cmp_imm_reg(Register.RDI, int(min_val))
            codegen.jcc(Condition.LESS, fail_label)
            # Check max
            codegen.cmp_imm_reg(Register.RDI, int(max_val))
            codegen.jcc(Condition.GREATER, fail_label)
        
        elif guard.guard_type == GuardType.CONSTANT:
            codegen.cmp_imm_reg(Register.RDI, int(guard.expected_value))
            codegen.jcc(Condition.NOT_EQUAL, fail_label)
        
        # Other guard types would need more complex implementations