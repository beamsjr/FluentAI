"""
ClaudeLang Standard Library - Data Structures

Advanced data structure operations for ClaudeLang.
"""

from typing import Any, List, Dict, Optional, Tuple
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_data_functions():
    """Register data structure functions"""
    
    # Dictionary/Map operations (using Python dict internally)
    PRIMITIVES.register(
        "dict-new",
        Function(
            name="dict-new",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("Any"), TypeAnnotation("Any")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: {}
    )
    
    PRIMITIVES.register(
        "dict-get",
        Function(
            name="dict-get",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Maybe", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d, key: d.get(key) if isinstance(d, dict) else None
    )
    
    PRIMITIVES.register(
        "dict-get-or",
        Function(
            name="dict-get-or",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("b"),
                    TypeAnnotation("b")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d, key, default: d.get(key, default) if isinstance(d, dict) else default
    )
    
    PRIMITIVES.register(
        "dict-set",
        Function(
            name="dict-set",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("b"),
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d, key, value: {**d, key: value} if isinstance(d, dict) else {key: value}
    )
    
    PRIMITIVES.register(
        "dict-remove",
        Function(
            name="dict-remove",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d, key: {k: v for k, v in d.items() if k != key} if isinstance(d, dict) else {}
    )
    
    PRIMITIVES.register(
        "dict-has?",
        Function(
            name="dict-has?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d, key: key in d if isinstance(d, dict) else False
    )
    
    PRIMITIVES.register(
        "dict-keys",
        Function(
            name="dict-keys",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d: list(d.keys()) if isinstance(d, dict) else []
    )
    
    PRIMITIVES.register(
        "dict-values",
        Function(
            name="dict-values",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d: list(d.values()) if isinstance(d, dict) else []
    )
    
    PRIMITIVES.register(
        "dict-items",
        Function(
            name="dict-items",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("Tuple", [TypeAnnotation("a"), TypeAnnotation("b")])])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d: list(d.items()) if isinstance(d, dict) else []
    )
    
    PRIMITIVES.register(
        "dict-size",
        Function(
            name="dict-size",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d: len(d) if isinstance(d, dict) else 0
    )
    
    PRIMITIVES.register(
        "dict-merge",
        Function(
            name="dict-merge",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")]),
                    TypeAnnotation("Dict", [TypeAnnotation("a"), TypeAnnotation("b")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda d1, d2: {**d1, **d2} if isinstance(d1, dict) and isinstance(d2, dict) else {}
    )
    
    # Set operations
    PRIMITIVES.register(
        "set-new",
        Function(
            name="set-new",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("Any")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: set()
    )
    
    PRIMITIVES.register(
        "set-from-list",
        Function(
            name="set-from-list",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: set(lst) if isinstance(lst, list) else set()
    )
    
    PRIMITIVES.register(
        "set-add",
        Function(
            name="set-add",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, elem: s | {elem} if isinstance(s, set) else {elem}
    )
    
    PRIMITIVES.register(
        "set-remove",
        Function(
            name="set-remove",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, elem: s - {elem} if isinstance(s, set) else set()
    )
    
    PRIMITIVES.register(
        "set-has?",
        Function(
            name="set-has?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, elem: elem in s if isinstance(s, set) else False
    )
    
    PRIMITIVES.register(
        "set-union",
        Function(
            name="set-union",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s1, s2: s1 | s2 if isinstance(s1, set) and isinstance(s2, set) else set()
    )
    
    PRIMITIVES.register(
        "set-intersection",
        Function(
            name="set-intersection",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s1, s2: s1 & s2 if isinstance(s1, set) and isinstance(s2, set) else set()
    )
    
    PRIMITIVES.register(
        "set-difference",
        Function(
            name="set-difference",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Set", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s1, s2: s1 - s2 if isinstance(s1, set) and isinstance(s2, set) else set()
    )
    
    PRIMITIVES.register(
        "set-size",
        Function(
            name="set-size",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: len(s) if isinstance(s, set) else 0
    )
    
    PRIMITIVES.register(
        "set-to-list",
        Function(
            name="set-to-list",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Set", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: list(s) if isinstance(s, set) else []
    )
    
    # Advanced list operations
    PRIMITIVES.register(
        "list-slice",
        Function(
            name="list-slice",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst, start, end: lst[start:end] if isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "list-reverse",
        Function(
            name="list-reverse",
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
        lambda lst: list(reversed(lst)) if isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "list-sort",
        Function(
            name="list-sort",
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
        lambda lst: sorted(lst) if isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "list-flatten",
        Function(
            name="list-flatten",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("List", [TypeAnnotation("a")])]),
                    TypeAnnotation("List", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: [item for sublist in lst for item in sublist] if isinstance(lst, list) else []
    )
    
    PRIMITIVES.register(
        "list-zip",
        Function(
            name="list-zip",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("List", [TypeAnnotation("b")]),
                    TypeAnnotation("List", [TypeAnnotation("Tuple", [TypeAnnotation("a"), TypeAnnotation("b")])])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst1, lst2: list(zip(lst1, lst2)) if isinstance(lst1, list) and isinstance(lst2, list) else []
    )
    
    PRIMITIVES.register(
        "list-unzip",
        Function(
            name="list-unzip",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("Tuple", [TypeAnnotation("a"), TypeAnnotation("b")])]),
                    TypeAnnotation("Tuple", [
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("List", [TypeAnnotation("b")])
                    ])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pairs: tuple(map(list, zip(*pairs))) if pairs else ([], [])
    )
    
    PRIMITIVES.register(
        "list-partition",
        Function(
            name="list-partition",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Tuple", [
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("List", [TypeAnnotation("a")])
                    ])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: (
            [x for x in lst if pred(x)],
            [x for x in lst if not pred(x)]
        ) if callable(pred) and isinstance(lst, list) else ([], [])
    )
    
    PRIMITIVES.register(
        "list-find",
        Function(
            name="list-find",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Function", [TypeAnnotation("a"), TypeAnnotation("Bool")]),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Maybe", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda pred, lst: next((x for x in lst if pred(x)), None) if callable(pred) and isinstance(lst, list) else None
    )
    
    PRIMITIVES.register(
        "list-index-of",
        Function(
            name="list-index-of",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("a"),
                    TypeAnnotation("List", [TypeAnnotation("a")]),
                    TypeAnnotation("Maybe", [TypeAnnotation("Int")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda elem, lst: lst.index(elem) if isinstance(lst, list) and elem in lst else None
    )
    
    PRIMITIVES.register(
        "list-unique",
        Function(
            name="list-unique",
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
        lambda lst: list(dict.fromkeys(lst)) if isinstance(lst, list) else []
    )
    
    # Queue operations (using list as deque)
    PRIMITIVES.register(
        "queue-new",
        Function(
            name="queue-new",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Queue", [TypeAnnotation("Any")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: []
    )
    
    PRIMITIVES.register(
        "queue-enqueue",
        Function(
            name="queue-enqueue",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Queue", [TypeAnnotation("a")]),
                    TypeAnnotation("a"),
                    TypeAnnotation("Queue", [TypeAnnotation("a")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda q, elem: q + [elem] if isinstance(q, list) else [elem]
    )
    
    PRIMITIVES.register(
        "queue-dequeue",
        Function(
            name="queue-dequeue",
            arity=1,
            effects={EffectType.ERROR},  # Error on empty queue
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Queue", [TypeAnnotation("a")]),
                    TypeAnnotation("Tuple", [TypeAnnotation("a"), TypeAnnotation("Queue", [TypeAnnotation("a")])])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda q: (q[0], q[1:]) if q else {"error": "Cannot dequeue from empty queue"}
    )
    
    PRIMITIVES.register(
        "queue-peek",
        Function(
            name="queue-peek",
            arity=1,
            effects={EffectType.ERROR},  # Error on empty queue
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Queue", [TypeAnnotation("a")]),
                    TypeAnnotation("a")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda q: q[0] if q else {"error": "Cannot peek empty queue"}
    )
    
    PRIMITIVES.register(
        "queue-empty?",
        Function(
            name="queue-empty?",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Queue", [TypeAnnotation("a")]),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda q: len(q) == 0 if isinstance(q, list) else True
    )


# Initialize data functions when module is imported
register_data_functions()