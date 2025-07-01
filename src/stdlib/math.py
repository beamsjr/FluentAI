"""
ClaudeLang Standard Library - Math Functions

Advanced mathematical operations for ClaudeLang.
"""

import math
from typing import Union
from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_math_functions():
    """Register mathematical functions"""
    
    # Trigonometric functions
    PRIMITIVES.register(
        "sin",
        Function(
            name="sin",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.sin(x)
    )
    
    PRIMITIVES.register(
        "cos",
        Function(
            name="cos",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.cos(x)
    )
    
    PRIMITIVES.register(
        "tan",
        Function(
            name="tan",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.tan(x)
    )
    
    PRIMITIVES.register(
        "asin",
        Function(
            name="asin",
            arity=1,
            effects={EffectType.ERROR},  # Domain error possible
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.asin(x) if -1 <= x <= 1 else {"error": "Domain error: asin requires -1 <= x <= 1"}
    )
    
    PRIMITIVES.register(
        "acos",
        Function(
            name="acos",
            arity=1,
            effects={EffectType.ERROR},  # Domain error possible
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.acos(x) if -1 <= x <= 1 else {"error": "Domain error: acos requires -1 <= x <= 1"}
    )
    
    PRIMITIVES.register(
        "atan",
        Function(
            name="atan",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.atan(x)
    )
    
    PRIMITIVES.register(
        "atan2",
        Function(
            name="atan2",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda y, x: math.atan2(y, x)
    )
    
    # Exponential and logarithmic functions
    PRIMITIVES.register(
        "exp",
        Function(
            name="exp",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.exp(x)
    )
    
    PRIMITIVES.register(
        "log",
        Function(
            name="log",
            arity=1,
            effects={EffectType.ERROR},  # Domain error for x <= 0
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.log(x) if x > 0 else {"error": "Domain error: log requires x > 0"}
    )
    
    PRIMITIVES.register(
        "log10",
        Function(
            name="log10",
            arity=1,
            effects={EffectType.ERROR},  # Domain error for x <= 0
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.log10(x) if x > 0 else {"error": "Domain error: log10 requires x > 0"}
    )
    
    PRIMITIVES.register(
        "log2",
        Function(
            name="log2",
            arity=1,
            effects={EffectType.ERROR},  # Domain error for x <= 0
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.log2(x) if x > 0 else {"error": "Domain error: log2 requires x > 0"}
    )
    
    PRIMITIVES.register(
        "pow",
        Function(
            name="pow",
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
        lambda x, y: math.pow(x, y)
    )
    
    PRIMITIVES.register(
        "sqrt",
        Function(
            name="sqrt",
            arity=1,
            effects={EffectType.ERROR},  # Domain error for x < 0
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda x: math.sqrt(x) if x >= 0 else {"error": "Domain error: sqrt requires x >= 0"}
    )
    
    # Rounding functions
    PRIMITIVES.register(
        "ceil",
        Function(
            name="ceil",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.ceil(x)
    )
    
    PRIMITIVES.register(
        "floor",
        Function(
            name="floor",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.floor(x)
    )
    
    PRIMITIVES.register(
        "round",
        Function(
            name="round",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: round(x)
    )
    
    PRIMITIVES.register(
        "round-to",
        Function(
            name="round-to",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, places: round(x, places)
    )
    
    # Constants
    PRIMITIVES.register(
        "pi",
        Function(
            name="pi",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: math.pi
    )
    
    PRIMITIVES.register(
        "e",
        Function(
            name="e",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: math.e
    )
    
    PRIMITIVES.register(
        "tau",
        Function(
            name="tau",
            arity=0,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda: math.tau
    )
    
    # Other useful functions
    PRIMITIVES.register(
        "degrees",
        Function(
            name="degrees",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.degrees(x)
    )
    
    PRIMITIVES.register(
        "radians",
        Function(
            name="radians",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x: math.radians(x)
    )
    
    PRIMITIVES.register(
        "hypot",
        Function(
            name="hypot",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, y: math.hypot(x, y)
    )
    
    PRIMITIVES.register(
        "factorial",
        Function(
            name="factorial",
            arity=1,
            effects={EffectType.ERROR},  # Error for negative or non-integer
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda n: math.factorial(n) if n >= 0 and isinstance(n, int) else {"error": "factorial requires non-negative integer"}
    )
    
    PRIMITIVES.register(
        "gcd",
        Function(
            name="gcd",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda a, b: math.gcd(int(a), int(b))
    )
    
    PRIMITIVES.register(
        "lcm",
        Function(
            name="lcm",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda a, b: abs(a * b) // math.gcd(int(a), int(b)) if a and b else 0
    )
    
    # Statistical functions
    PRIMITIVES.register(
        "sum",
        Function(
            name="sum",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("Number")]),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: sum(lst) if isinstance(lst, list) else 0
    )
    
    PRIMITIVES.register(
        "product",
        Function(
            name="product",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("Number")]),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: math.prod(lst) if isinstance(lst, list) else 1
    )
    
    PRIMITIVES.register(
        "mean",
        Function(
            name="mean",
            arity=1,
            effects={EffectType.ERROR},  # Error for empty list
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("Number")]),
                    TypeAnnotation("Float")
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda lst: sum(lst) / len(lst) if lst else {"error": "mean of empty list"}
    )
    
    PRIMITIVES.register(
        "clamp",
        Function(
            name="clamp",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number"),
                    TypeAnnotation("Number")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda x, low, high: max(low, min(x, high))
    )


# Initialize math functions when module is imported
register_math_functions()