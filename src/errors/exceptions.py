"""
ClaudeLang Exception Classes

Custom exception classes with enhanced error information.
"""

from typing import Optional, Any
from .diagnostics import Diagnostic, SourceLocation


class ClaudeLangError(Exception):
    """Base exception for ClaudeLang errors"""
    
    def __init__(self, message: str, diagnostic: Optional[Diagnostic] = None):
        super().__init__(message)
        self.diagnostic = diagnostic


class SyntaxError(ClaudeLangError):
    """Syntax error in source code"""
    pass


class NameError(ClaudeLangError):
    """Undefined name error"""
    
    def __init__(self, name: str, location: Optional[SourceLocation] = None):
        from .diagnostics import undefined_variable_error
        diagnostic = undefined_variable_error(name, location)
        super().__init__(f"Undefined variable: {name}", diagnostic)
        self.name = name


class TypeError(ClaudeLangError):
    """Type mismatch error"""
    
    def __init__(self, expected: str, actual: str, location: Optional[SourceLocation] = None):
        from .diagnostics import type_mismatch_error
        diagnostic = type_mismatch_error(expected, actual, location)
        super().__init__(f"Type mismatch: expected {expected}, found {actual}", diagnostic)
        self.expected = expected
        self.actual = actual


class PatternMatchError(ClaudeLangError):
    """Pattern matching failure"""
    
    def __init__(self, value: Any, location: Optional[SourceLocation] = None):
        from .diagnostics import pattern_match_error
        diagnostic = pattern_match_error(value, location)
        super().__init__(f"No patterns matched value: {value}", diagnostic)
        self.value = value


class EffectError(ClaudeLangError):
    """Effect handling error"""
    
    def __init__(self, effect: str, message: str, location: Optional[SourceLocation] = None):
        super().__init__(f"Effect error [{effect}]: {message}")
        self.effect = effect


class ModuleError(ClaudeLangError):
    """Module system error"""
    
    def __init__(self, message: str, module_name: Optional[str] = None):
        super().__init__(message)
        self.module_name = module_name