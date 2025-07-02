"""
ClaudeLang - A modern Lisp dialect with advanced features
"""

__version__ = "0.1.0"

# Try to import Rust extensions if available
try:
    from . import claudelang_rust
    
    # Re-export Rust functions at module level
    parse = claudelang_rust.parse
    evaluate = claudelang_rust.evaluate
    compile_to_bytecode = claudelang_rust.compile
    
    # Mark that Rust extensions are available
    HAS_RUST_EXTENSIONS = True
    
except ImportError:
    # Fall back to Python implementation
    HAS_RUST_EXTENSIONS = False
    
    # Import Python versions
    from .parser.sexpr_parser import parse_sexpr as parse
    from .interpreter.interpreter import evaluate
    compile_to_bytecode = None  # Not available in Python version

# Always available imports
from .core.ast import Graph, Node
from .core.primitives import *
from .errors.exceptions import ClaudeLangError

__all__ = [
    'parse',
    'evaluate', 
    'compile_to_bytecode',
    'Graph',
    'Node',
    'ClaudeLangError',
    'HAS_RUST_EXTENSIONS',
]