"""
ClaudeLang Error Diagnostics System

This module provides enhanced error messages with context, suggestions, and
clear explanations to help users understand and fix issues.
"""

from dataclasses import dataclass
from typing import List, Optional, Dict, Any
from enum import Enum


class ErrorSeverity(Enum):
    """Severity levels for diagnostic messages"""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"
    HINT = "hint"


@dataclass
class SourceLocation:
    """Location in source code"""
    filename: Optional[str] = None
    line: int = 0
    column: int = 0
    length: int = 1


@dataclass
class Diagnostic:
    """A diagnostic message with context"""
    severity: ErrorSeverity
    code: str  # Error code like "E001"
    message: str
    location: Optional[SourceLocation] = None
    suggestion: Optional[str] = None
    notes: List[str] = None
    related_info: List['Diagnostic'] = None
    
    def __post_init__(self):
        if self.notes is None:
            self.notes = []
        if self.related_info is None:
            self.related_info = []


class DiagnosticEngine:
    """Engine for collecting and formatting diagnostic messages"""
    
    def __init__(self):
        self.diagnostics: List[Diagnostic] = []
        self.source_cache: Dict[str, List[str]] = {}  # filename -> lines
    
    def add_diagnostic(self, diagnostic: Diagnostic):
        """Add a diagnostic message"""
        self.diagnostics.append(diagnostic)
    
    def error(self, code: str, message: str, location: Optional[SourceLocation] = None, 
              suggestion: Optional[str] = None, notes: List[str] = None):
        """Add an error diagnostic"""
        self.add_diagnostic(Diagnostic(
            severity=ErrorSeverity.ERROR,
            code=code,
            message=message,
            location=location,
            suggestion=suggestion,
            notes=notes
        ))
    
    def warning(self, code: str, message: str, location: Optional[SourceLocation] = None,
                suggestion: Optional[str] = None, notes: List[str] = None):
        """Add a warning diagnostic"""
        self.add_diagnostic(Diagnostic(
            severity=ErrorSeverity.WARNING,
            code=code,
            message=message,
            location=location,
            suggestion=suggestion,
            notes=notes
        ))
    
    def format_diagnostic(self, diagnostic: Diagnostic) -> str:
        """Format a diagnostic for display"""
        lines = []
        
        # Header
        severity_color = {
            ErrorSeverity.ERROR: "\033[91m",    # Red
            ErrorSeverity.WARNING: "\033[93m",   # Yellow
            ErrorSeverity.INFO: "\033[94m",      # Blue
            ErrorSeverity.HINT: "\033[92m"       # Green
        }
        reset_color = "\033[0m"
        
        header = f"{severity_color[diagnostic.severity]}{diagnostic.severity.value}{reset_color}"
        header += f"[{diagnostic.code}]: {diagnostic.message}"
        
        if diagnostic.location and diagnostic.location.filename:
            header += f"\n  --> {diagnostic.location.filename}:{diagnostic.location.line}:{diagnostic.location.column}"
        
        lines.append(header)
        
        # Source context
        if diagnostic.location and diagnostic.location.filename:
            source_lines = self._get_source_lines(diagnostic.location.filename)
            if source_lines and 0 < diagnostic.location.line <= len(source_lines):
                line_num = diagnostic.location.line
                line = source_lines[line_num - 1]
                
                # Show line with error
                lines.append(f"   {line_num} | {line}")
                
                # Show caret pointing to error
                spaces = " " * (len(str(line_num)) + 3 + diagnostic.location.column - 1)
                carets = "^" * diagnostic.location.length
                lines.append(f"   {spaces}{severity_color[diagnostic.severity]}{carets}{reset_color}")
        
        # Suggestion
        if diagnostic.suggestion:
            lines.append(f"\n{severity_color[ErrorSeverity.HINT]}help{reset_color}: {diagnostic.suggestion}")
        
        # Notes
        for note in diagnostic.notes:
            lines.append(f"note: {note}")
        
        # Related info
        for related in diagnostic.related_info:
            lines.append("\nrelated:")
            lines.append(self.format_diagnostic(related))
        
        return "\n".join(lines)
    
    def _get_source_lines(self, filename: str) -> List[str]:
        """Get source lines from cache or file"""
        if filename not in self.source_cache:
            try:
                with open(filename, 'r') as f:
                    self.source_cache[filename] = f.readlines()
            except:
                return []
        return self.source_cache[filename]
    
    def has_errors(self) -> bool:
        """Check if there are any error diagnostics"""
        return any(d.severity == ErrorSeverity.ERROR for d in self.diagnostics)
    
    def print_all(self):
        """Print all diagnostics"""
        for diagnostic in self.diagnostics:
            print(self.format_diagnostic(diagnostic))
            print()


# Common error generators
def type_mismatch_error(expected: str, actual: str, location: Optional[SourceLocation] = None) -> Diagnostic:
    """Generate a type mismatch error"""
    return Diagnostic(
        severity=ErrorSeverity.ERROR,
        code="E001",
        message=f"Type mismatch: expected {expected}, found {actual}",
        location=location,
        suggestion=f"Consider converting the value to {expected} or changing the expected type"
    )


def undefined_variable_error(name: str, location: Optional[SourceLocation] = None,
                           similar_names: List[str] = None) -> Diagnostic:
    """Generate an undefined variable error"""
    diagnostic = Diagnostic(
        severity=ErrorSeverity.ERROR,
        code="E002",
        message=f"Undefined variable: {name}",
        location=location
    )
    
    if similar_names:
        diagnostic.suggestion = f"Did you mean: {', '.join(similar_names)}?"
    
    return diagnostic


def pattern_match_error(value: Any, location: Optional[SourceLocation] = None) -> Diagnostic:
    """Generate a pattern match failure error"""
    return Diagnostic(
        severity=ErrorSeverity.ERROR,
        code="E003",
        message=f"No patterns matched value: {value}",
        location=location,
        suggestion="Add a catch-all pattern (_) or ensure all cases are covered",
        notes=["Pattern matching must be exhaustive"]
    )


def syntax_error(message: str, location: Optional[SourceLocation] = None,
                expected: Optional[str] = None) -> Diagnostic:
    """Generate a syntax error"""
    diagnostic = Diagnostic(
        severity=ErrorSeverity.ERROR,
        code="E004",
        message=message,
        location=location
    )
    
    if expected:
        diagnostic.suggestion = f"Expected: {expected}"
    
    return diagnostic