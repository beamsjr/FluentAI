"""
Effect Primitives for ClaudeLang

This module registers effect operations as language primitives.
"""

from typing import Any, List
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES
from .handlers import EffectRequest, EffectContext


# Global effect context (set by interpreter/VM)
_current_context: EffectContext = None


def set_effect_context(context: EffectContext):
    """Set the current effect context"""
    global _current_context
    _current_context = context


def get_effect_context() -> EffectContext:
    """Get the current effect context"""
    return _current_context


def register_effect_primitives():
    """Register all effect primitives"""
    
    # IO Effects
    PRIMITIVES.register(
        "io:print",
        Function(
            name="io:print",
            arity=-1,  # Variadic
            effects={EffectType.IO},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Any"),
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.IO}
            )
        ),
        lambda *args: _perform_effect(EffectType.IO, "print", *args)
    )
    
    PRIMITIVES.register(
        "io:read-line",
        Function(
            name="io:read-line",
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
        lambda: _perform_effect(EffectType.IO, "read-line")
    )
    
    PRIMITIVES.register(
        "io:open-file",
        Function(
            name="io:open-file",
            arity=2,
            effects={EffectType.IO, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("FileHandle")
                ],
                effects={EffectType.IO, EffectType.ERROR}
            )
        ),
        lambda path, mode: _perform_effect(EffectType.IO, "open-file", path, mode)
    )
    
    # State Effects
    PRIMITIVES.register(
        "state:get",
        Function(
            name="state:get",
            arity=1,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda key: _perform_effect(EffectType.STATE, "get", key)
    )
    
    PRIMITIVES.register(
        "state:set",
        Function(
            name="state:set",
            arity=2,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Any"),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda key, value: _perform_effect(EffectType.STATE, "set", key, value)
    )
    
    PRIMITIVES.register(
        "state:update",
        Function(
            name="state:update",
            arity=2,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Function", [TypeAnnotation("Any"), TypeAnnotation("Any")]),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda key, fn: _perform_effect(EffectType.STATE, "update", key, fn)
    )
    
    # Error Effects
    PRIMITIVES.register(
        "error:raise",
        Function(
            name="error:raise",
            arity=-1,  # Variadic (type, message, data)
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Error")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda *args: _perform_effect(EffectType.ERROR, "raise", *args)
    )
    
    PRIMITIVES.register(
        "error:catch",
        Function(
            name="error:catch",
            arity=2,
            effects={EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Function"),
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda error_type, handler: _perform_effect(EffectType.ERROR, "catch", error_type, handler)
    )
    
    # Time Effects
    PRIMITIVES.register(
        "time:now",
        Function(
            name="time:now",
            arity=0,
            effects={EffectType.TIME},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float")
                ],
                effects={EffectType.TIME}
            )
        ),
        lambda: _perform_effect(EffectType.TIME, "now")
    )
    
    PRIMITIVES.register(
        "time:sleep",
        Function(
            name="time:sleep",
            arity=1,
            effects={EffectType.TIME},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float"),
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.TIME}
            )
        ),
        lambda duration: _perform_effect(EffectType.TIME, "sleep", duration)
    )
    
    # Random Effects
    PRIMITIVES.register(
        "random:float",
        Function(
            name="random:float",
            arity=0,
            effects={EffectType.RANDOM},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float")
                ],
                effects={EffectType.RANDOM}
            )
        ),
        lambda: _perform_effect(EffectType.RANDOM, "random")
    )
    
    PRIMITIVES.register(
        "random:int",
        Function(
            name="random:int",
            arity=2,
            effects={EffectType.RANDOM},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.RANDOM}
            )
        ),
        lambda a, b: _perform_effect(EffectType.RANDOM, "randint", a, b)
    )
    
    PRIMITIVES.register(
        "random:choice",
        Function(
            name="random:choice",
            arity=1,
            effects={EffectType.RANDOM, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("a")
                ],
                effects={EffectType.RANDOM, EffectType.ERROR}
            )
        ),
        lambda choices: _perform_effect(EffectType.RANDOM, "choice", choices)
    )
    
    # Network Effects
    PRIMITIVES.register(
        "network:fetch",
        Function(
            name="network:fetch",
            arity=1,
            effects={EffectType.NETWORK, EffectType.ERROR},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Response")
                ],
                effects={EffectType.NETWORK, EffectType.ERROR}
            )
        ),
        lambda url: _perform_effect(EffectType.NETWORK, "fetch", url)
    )
    
    # Effect control primitives
    PRIMITIVES.register(
        "with-handler",
        Function(
            name="with-handler",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Handler"),
                    TypeAnnotation("Function"),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.PURE}
            )
        ),
        _with_handler
    )
    
    PRIMITIVES.register(
        "pure",
        Function(
            name="pure",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.PURE}
            )
        ),
        _run_pure
    )
    
    # Garbage Collection Effects
    PRIMITIVES.register(
        "gc:collect",
        Function(
            name="gc:collect",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-collect")
    )
    
    PRIMITIVES.register(
        "gc:stats",
        Function(
            name="gc:stats",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("String"), TypeAnnotation("Int")])
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-stats")
    )
    
    PRIMITIVES.register(
        "gc:set-threshold",
        Function(
            name="gc:set-threshold",
            arity=1,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda threshold: _perform_effect(EffectType.STATE, "gc-set-threshold", threshold)
    )
    
    PRIMITIVES.register(
        "gc:enable",
        Function(
            name="gc:enable",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-enable")
    )
    
    PRIMITIVES.register(
        "gc:disable",
        Function(
            name="gc:disable",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Unit")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-disable")
    )
    
    PRIMITIVES.register(
        "gc:is-enabled?",
        Function(
            name="gc:is-enabled?",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-is-enabled")
    )
    
    PRIMITIVES.register(
        "gc:get-threshold",
        Function(
            name="gc:get-threshold",
            arity=0,
            effects={EffectType.STATE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int")
                ],
                effects={EffectType.STATE}
            )
        ),
        lambda: _perform_effect(EffectType.STATE, "gc-get-threshold")
    )


def _perform_effect(effect_type: EffectType, operation: str, *args) -> Any:
    """Perform an effect using the current context"""
    context = get_effect_context()
    if context:
        return context.perform(effect_type, operation, *args)
    else:
        # Fallback behavior when no context
        if effect_type == EffectType.IO and operation == "print":
            print(*args)
            return None
        elif effect_type == EffectType.ERROR and operation == "raise":
            raise RuntimeError(f"Effect error: {args}")
        else:
            raise RuntimeError(f"No effect context for {effect_type}:{operation}")


def _with_handler(handler, thunk):
    """Run a thunk with a custom handler"""
    # This would need integration with the interpreter
    # For now, just run the thunk
    return thunk()


def _run_pure(thunk):
    """Run a thunk in a pure context (no effects allowed)"""
    # This would enforce purity
    return thunk()


# Register all effect primitives when module is imported
register_effect_primitives()