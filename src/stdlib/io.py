"""
ClaudeLang Standard Library - IO Functions

File operations and IO primitives for ClaudeLang.
All IO operations explicitly use the IO effect.
"""

import os
import json
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_io_functions():
    """Register IO functions"""
    
    # File reading
    PRIMITIVES.register(
        "file-read",
        Function(
            name="file-read",
            arity=1,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("String"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path: _safe_file_read(path)
    )
    
    # File writing
    PRIMITIVES.register(
        "file-write",
        Function(
            name="file-write",
            arity=2,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Unit"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path, content: _safe_file_write(path, content)
    )
    
    # File append
    PRIMITIVES.register(
        "file-append",
        Function(
            name="file-append",
            arity=2,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Unit"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path, content: _safe_file_append(path, content)
    )
    
    # File exists
    PRIMITIVES.register(
        "file-exists?",
        Function(
            name="file-exists?",
            arity=1,
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda path: os.path.exists(path)
    )
    
    # Delete file
    PRIMITIVES.register(
        "file-delete",
        Function(
            name="file-delete",
            arity=1,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Unit"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path: _safe_file_delete(path)
    )
    
    # List directory
    PRIMITIVES.register(
        "dir-list",
        Function(
            name="dir-list",
            arity=1,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("List", [TypeAnnotation("String")]),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path: _safe_dir_list(path)
    )
    
    # Create directory
    PRIMITIVES.register(
        "dir-create",
        Function(
            name="dir-create",
            arity=1,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Unit"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path: _safe_dir_create(path)
    )
    
    # Get current directory
    PRIMITIVES.register(
        "current-directory",
        Function(
            name="current-directory",
            arity=0,
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda: os.getcwd()
    )
    
    # Read lines from file
    PRIMITIVES.register(
        "file-read-lines",
        Function(
            name="file-read-lines",
            arity=1,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("List", [TypeAnnotation("String")]),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path: _safe_file_read_lines(path)
    )
    
    # Write lines to file
    PRIMITIVES.register(
        "file-write-lines",
        Function(
            name="file-write-lines",
            arity=2,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("List", [TypeAnnotation("String")]),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Unit"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path, lines: _safe_file_write_lines(path, lines)
    )
    
    # Console input
    PRIMITIVES.register(
        "read-line",
        Function(
            name="read-line",
            arity=0,
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda: input()
    )
    
    # Console output (already handled by effect system, but adding for completeness)
    PRIMITIVES.register(
        "print-line",
        Function(
            name="print-line",
            arity=1,
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda s: print(s) or None
    )
    
    # JSON operations
    PRIMITIVES.register(
        "json-parse",
        Function(
            name="json-parse",
            arity=1,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Any"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda s: _safe_json_parse(s)
    )
    
    PRIMITIVES.register(
        "json-stringify",
        Function(
            name="json-stringify",
            arity=1,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Any"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("String"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda obj: _safe_json_stringify(obj)
    )


# Helper functions for safe IO operations

def _safe_file_read(path):
    """Safely read a file"""
    try:
        with open(path, 'r') as f:
            return f.read()
    except Exception as e:
        return {"error": f"Failed to read file: {str(e)}"}


def _safe_file_write(path, content):
    """Safely write to a file"""
    try:
        with open(path, 'w') as f:
            f.write(content)
        return None  # Unit
    except Exception as e:
        return {"error": f"Failed to write file: {str(e)}"}


def _safe_file_append(path, content):
    """Safely append to a file"""
    try:
        with open(path, 'a') as f:
            f.write(content)
        return None  # Unit
    except Exception as e:
        return {"error": f"Failed to append to file: {str(e)}"}


def _safe_file_delete(path):
    """Safely delete a file"""
    try:
        os.remove(path)
        return None  # Unit
    except Exception as e:
        return {"error": f"Failed to delete file: {str(e)}"}


def _safe_dir_list(path):
    """Safely list directory contents"""
    try:
        return os.listdir(path)
    except Exception as e:
        return {"error": f"Failed to list directory: {str(e)}"}


def _safe_dir_create(path):
    """Safely create a directory"""
    try:
        os.makedirs(path, exist_ok=True)
        return None  # Unit
    except Exception as e:
        return {"error": f"Failed to create directory: {str(e)}"}


def _safe_file_read_lines(path):
    """Safely read lines from a file"""
    try:
        with open(path, 'r') as f:
            return f.read().splitlines()
    except Exception as e:
        return {"error": f"Failed to read lines: {str(e)}"}


def _safe_file_write_lines(path, lines):
    """Safely write lines to a file"""
    try:
        with open(path, 'w') as f:
            f.write('\n'.join(str(line) for line in lines))
        return None  # Unit
    except Exception as e:
        return {"error": f"Failed to write lines: {str(e)}"}


def _safe_json_parse(s):
    """Safely parse JSON"""
    try:
        return json.loads(s)
    except Exception as e:
        return {"error": f"Failed to parse JSON: {str(e)}"}


def _safe_json_stringify(obj):
    """Safely stringify to JSON"""
    try:
        return json.dumps(obj)
    except Exception as e:
        return {"error": f"Failed to stringify JSON: {str(e)}"}


# Initialize IO functions when module is imported
register_io_functions()