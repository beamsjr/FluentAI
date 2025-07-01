"""
ClaudeLang JIT Compiler Module

Provides Just-In-Time compilation for hot code paths to improve performance.
"""

from .jit_compiler import (
    JITCompiler,
    TraceJIT,
    ExecutionProfile,
    CompiledCode,
    get_jit_compiler,
    HotspotThreshold
)
from .guards import (
    GuardType,
    GuardCondition,
    SpecializedCode,
    TypeProfiler,
    SpecializationCache
)

__all__ = [
    'JITCompiler',
    'TraceJIT', 
    'ExecutionProfile',
    'CompiledCode',
    'get_jit_compiler',
    'HotspotThreshold',
    'GuardType',
    'GuardCondition',
    'SpecializedCode',
    'TypeProfiler',
    'SpecializationCache'
]