"""
Wrapper to make Rust extensions work with the existing claudelang module structure
"""

# Import the Rust module
try:
    from claudelang_rust import parse as rust_parse, evaluate as rust_evaluate, compile as rust_compile, benchmark_parser
    HAS_RUST_EXTENSIONS = True
except ImportError:
    HAS_RUST_EXTENSIONS = False
    rust_parse = None
    rust_evaluate = None
    rust_compile = None
    benchmark_parser = None

# Re-export at module level
parse = rust_parse
evaluate = rust_evaluate
compile_to_bytecode = rust_compile

__all__ = ['parse', 'evaluate', 'compile_to_bytecode', 'HAS_RUST_EXTENSIONS', 'benchmark_parser']