"""
ClaudeLang Standard Library - Core Functions

This module extends the primitive operations with higher-level functions.
"""

from typing import Dict, List, Any, Callable
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_core_functions():
    """Register core standard library functions"""
    
    # List operations
    PRIMITIVES.register(
        "length",
        Function(
            name="length",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: len(lst) if isinstance(lst, list) else 0
    )
    
    PRIMITIVES.register(
        "append",
        Function(
            name="append",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst1, lst2: lst1 + lst2
    )
    
    PRIMITIVES.register(
        "reverse",
        Function(
            name="reverse",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: list(reversed(lst))
    )
    
    PRIMITIVES.register(
        "nth",
        Function(
            name="nth",
            arity=2,
            effects={EffectType.ERROR},  # Can fail on out of bounds
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("a"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda lst, n: lst[n] if 0 <= n < len(lst) else {"error": f"Index {n} out of bounds"}
    )
    
    PRIMITIVES.register(
        "take",
        Function(
            name="take",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda n, lst: lst[:n]
    )
    
    PRIMITIVES.register(
        "drop",
        Function(
            name="drop",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda n, lst: lst[n:]
    )
    
    # Numeric operations
    PRIMITIVES.register(
        "abs",
        Function(
            name="abs",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: abs(x)
    )
    
    PRIMITIVES.register(
        "max",
        Function(
            name="max",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: max(x, y)
    )
    
    PRIMITIVES.register(
        "min",
        Function(
            name="min",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: min(x, y)
    )
    
    PRIMITIVES.register(
        "mod",
        Function(
            name="mod",
            arity=2,
            effects={EffectType.ERROR},  # Division by zero
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Int"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x, y: x % y if y != 0 else {"error": "Modulo by zero"}
    )
    
    # Boolean operations
    PRIMITIVES.register(
        "xor",
        Function(
            name="xor",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Bool"),
                    TypeAnnotation("Bool"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: x != y
    )
    
    # Comparison operations
    PRIMITIVES.register(
        ">",
        Function(
            name=">",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: x > y
    )
    
    PRIMITIVES.register(
        "<=",
        Function(
            name="<=",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: x <= y
    )
    
    PRIMITIVES.register(
        ">=",
        Function(
            name=">=",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: x >= y
    )
    
    PRIMITIVES.register(
        "!=",
        Function(
            name="!=",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: x != y
    )
    
    # Type predicates
    PRIMITIVES.register(
        "int?",
        Function(
            name="int?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: isinstance(x, int)
    )
    
    PRIMITIVES.register(
        "float?",
        Function(
            name="float?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: isinstance(x, float)
    )
    
    PRIMITIVES.register(
        "string?",
        Function(
            name="string?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: isinstance(x, str)
    )
    
    PRIMITIVES.register(
        "list?",
        Function(
            name="list?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: isinstance(x, list)
    )
    
    PRIMITIVES.register(
        "bool?",
        Function(
            name="bool?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: isinstance(x, bool)
    )
    
    # Higher-order functions (simplified implementations for REPL testing)
    # These work with Python lambdas, not ClaudeLang functions yet
    PRIMITIVES.register(
        "map",
        Function(
            name="map",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda func, lst: [func(x) for x in lst] if callable(func) else lst
    )
    
    PRIMITIVES.register(
        "filter",
        Function(
            name="filter",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: [x for x in lst if pred(x)] if callable(pred) else lst
    )
    
    PRIMITIVES.register(
        "fold",
        Function(
            name="fold",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("b"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("b")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda func, init, lst: _fold_impl(func, init, lst)
    )


def _fold_impl(func, init, lst):
    """Fold implementation"""
    acc = init
    for item in lst:
        if callable(func):
            acc = func(acc, item)
    return acc


# Initialize core functions when module is imported
register_core_functions()