"""
ClaudeLang Standard Library - Functional Programming Utilities

Higher-order functions and functional programming helpers.
"""

from typing import Any, Callable, List
from functools import reduce as py_reduce
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_functional_functions():
    """Register functional programming utilities"""
    
    # Function composition
    PRIMITIVES.register(
        "compose",
        Function(
            name="compose",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("c")]),
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("c")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, g: lambda x: f(g(x)) if callable(f) and callable(g) else None
    )
    
    PRIMITIVES.register(
        "pipe",
        Function(
            name="pipe",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("c")]),
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("c")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, g: lambda x: g(f(x)) if callable(f) and callable(g) else None
    )
    
    # Identity function
    PRIMITIVES.register(
        "identity",
        Function(
            name="identity",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("a")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: x
    )
    
    # Constant function
    PRIMITIVES.register(
        "const",
        Function(
            name="const",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: lambda _: x
    )
    
    # Flip arguments
    PRIMITIVES.register(
        "flip",
        Function(
            name="flip",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b"), TypeAnnotation("c")]),
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("a"), TypeAnnotation("c")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f: lambda x, y: f(y, x) if callable(f) else None
    )
    
    # Partial application
    PRIMITIVES.register(
        "partial",
        Function(
            name="partial",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("a"),
                    TypeAnnotation("Function")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, x: lambda *args: f(x, *args) if callable(f) else None
    )
    
    PRIMITIVES.register(
        "partial-right",
        Function(
            name="partial-right",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("a"),
                    TypeAnnotation("Function")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, x: lambda *args: f(*args, x) if callable(f) else None
    )
    
    # Curry and uncurry
    PRIMITIVES.register(
        "curry",
        Function(
            name="curry",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("Function")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f: lambda x: lambda y: f(x, y) if callable(f) else None
    )
    
    PRIMITIVES.register(
        "uncurry",
        Function(
            name="uncurry",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("Function")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f: lambda x, y: f(x)(y) if callable(f) else None
    )
    
    # Apply function
    PRIMITIVES.register(
        "apply",
        Function(
            name="apply",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("List", [TypeAnnotation("Any")]),
                    TypeAnnotation("Any")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, args: f(*args) if callable(f) and isinstance(args, list) else None
    )
    
    # Memoization
    _memo_cache = {}
    
    def memoize_impl(f):
        if not callable(f):
            return f
        
        def memoized(*args):
            key = (id(f), args)
            if key not in _memo_cache:
                _memo_cache[key] = f(*args)
            return _memo_cache[key]
        
        return memoized
    
    PRIMITIVES.register(
        "memoize",
        Function(
            name="memoize",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function"),
                    TypeAnnotation("Function")
                ],
                effects={EffectType.PURE}
            )
        ),
        memoize_impl
    )
    
    # List comprehension helpers
    PRIMITIVES.register(
        "map-indexed",
        Function(
            name="map-indexed",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("Int"), TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, lst: [f(i, x) for i, x in enumerate(lst)] if callable(f) and isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "filter-map",
        Function(
            name="filter-map",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Maybe", [TypeAnnotation("b")])]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, lst: [y for x in lst for y in ([f(x)] if f(x) is not None else [])] if callable(f) and isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "flat-map",
        Function(
            name="flat-map",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("List", [TypeAnnotation("b")])]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, lst: [y for x in lst for y in f(x)] if callable(f) and isinstance(lst, list) else []
    )
    
    # Fold variants
    PRIMITIVES.register(
        "fold-right",
        Function(
            name="fold-right",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b"), TypeAnnotation("b")]),
                    TypeAnnotation("b"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("b")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, init, lst: py_reduce(lambda acc, x: f(x, acc), reversed(lst), init) if callable(f) and isinstance(lst, list) else init
    )
    
    PRIMITIVES.register(
        "scan",
        Function(
            name="scan",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("b"), TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("b"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, init, lst: _scan_impl(f, init, lst)
    )
    
    # Predicates and combinators
    PRIMITIVES.register(
        "all?",
        Function(
            name="all?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: all(pred(x) for x in lst) if callable(pred) and isinstance(lst, list) else True
    )
    
    PRIMITIVES.register(
        "any?",
        Function(
            name="any?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: any(pred(x) for x in lst) if callable(pred) and isinstance(lst, list) else False
    )
    
    PRIMITIVES.register(
        "none?",
        Function(
            name="none?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: not any(pred(x) for x in lst) if callable(pred) and isinstance(lst, list) else True
    )
    
    # Function iteration
    PRIMITIVES.register(
        "iterate",
        Function(
            name="iterate",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("a")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda f, init, n: _iterate_impl(f, init, n)
    )
    
    PRIMITIVES.register(
        "repeat",
        Function(
            name="repeat",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, n: [x] * n if n >= 0 else []
    )
    
    PRIMITIVES.register(
        "replicate",
        Function(
            name="replicate",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Function", [TypeAnnotation("Int"), TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda n, f: [f(i) for i in range(n)] if callable(f) and n >= 0 else []
    )
    
    # Grouping and chunking
    PRIMITIVES.register(
        "group-by",
        Function(
            name="group-by",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Dict", [TypeAnnotation("b"), TypeAnnotation("List", [TypeAnnotation("a")])])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda key_fn, lst: _group_by_impl(key_fn, lst)
    )
    
    PRIMITIVES.register(
        "chunk",
        Function(
            name="chunk",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("List", [TypeAnnotation("a")])])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda n, lst: [lst[i:i+n] for i in range(0, len(lst), n)] if n > 0 and isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "sliding-window",
        Function(
            name="sliding-window",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("List", [TypeAnnotation("a")])])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda n, lst: [lst[i:i+n] for i in range(len(lst)-n+1)] if n > 0 and len(lst) >= n else []
    )


def _scan_impl(f, init, lst):
    """Scan implementation"""
    if not callable(f) or not isinstance(lst, list):
        return []
    
    result = [init]
    acc = init
    for x in lst:
        acc = f(acc, x)
        result.append(acc)
    return result


def _iterate_impl(f, init, n):
    """Iterate function n times"""
    if not callable(f) or n < 0:
        return []
    
    result = []
    current = init
    for _ in range(n):
        result.append(current)
        current = f(current)
    return result


def _group_by_impl(key_fn, lst):
    """Group elements by key function"""
    if not callable(key_fn) or not isinstance(lst, list):
        return {}
    
    groups = {}
    for item in lst:
        key = key_fn(item)
        if key not in groups:
            groups[key] = []
        groups[key].append(item)
    return groups


# Initialize functional functions when module is imported
register_functional_functions()