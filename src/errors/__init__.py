"""
ClaudeLang Error Handling System
"""

from .diagnostics import (
    DiagnosticEngine,
    Diagnostic, 
    ErrorSeverity,
    SourceLocation,
    type_mismatch_error,
    undefined_variable_error,
    pattern_match_error,
    syntax_error
)

__all__ = [
    'DiagnosticEngine',
    'Diagnostic',
    'ErrorSeverity', 
    'SourceLocation',
    'type_mismatch_error',
    'undefined_variable_error',
    'pattern_match_error',
    'syntax_error'
]