"""
ClaudeLang Standard Library - String Functions

String manipulation functions for ClaudeLang.
"""

from ..core.ast import Function, EffectType, TypeAnnotation
from ..core.primitives import PRIMITIVES


def register_string_functions():
    """Register string manipulation functions"""
    
    PRIMITIVES.register(
        "string-concat",
        Function(
            name="string-concat",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s1, s2: s1 + s2
    )
    
    PRIMITIVES.register(
        "string-length",
        Function(
            name="string-length",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Int")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: len(s)
    )
    
    PRIMITIVES.register(
        "substring",
        Function(
            name="substring",
            arity=3,
            effects={EffectType.ERROR},  # Index out of bounds
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("String"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda s, start, end: s[start:end] if 0 <= start <= end <= len(s) else {"error": "Invalid substring indices"}
    )
    
    PRIMITIVES.register(
        "string-ref",
        Function(
            name="string-ref",
            arity=2,
            effects={EffectType.ERROR},  # Index out of bounds
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Int"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("String"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda s, i: s[i] if 0 <= i < len(s) else {"error": f"Index {i} out of bounds"}
    )
    
    PRIMITIVES.register(
        "string-append",
        Function(
            name="string-append",
            arity=-1,  # Variadic
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[],  # Variadic
                effects={EffectType.PURE}
            )
        ),
        lambda *args: "".join(str(arg) for arg in args)
    )
    
    PRIMITIVES.register(
        "string-split",
        Function(
            name="string-split",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("List", [TypeAnnotation("String")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, delimiter: s.split(delimiter) if delimiter else list(s)
    )
    
    PRIMITIVES.register(
        "string-join",
        Function(
            name="string-join",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("List", [TypeAnnotation("String")]),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda delimiter, lst: delimiter.join(str(item) for item in lst)
    )
    
    PRIMITIVES.register(
        "string-trim",
        Function(
            name="string-trim",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: s.strip()
    )
    
    PRIMITIVES.register(
        "string-upcase",
        Function(
            name="string-upcase",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: s.upper()
    )
    
    PRIMITIVES.register(
        "string-downcase",
        Function(
            name="string-downcase",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: s.lower()
    )
    
    PRIMITIVES.register(
        "string-contains?",
        Function(
            name="string-contains?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, substring: substring in s
    )
    
    PRIMITIVES.register(
        "string-starts-with?",
        Function(
            name="string-starts-with?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, prefix: s.startswith(prefix)
    )
    
    PRIMITIVES.register(
        "string-ends-with?",
        Function(
            name="string-ends-with?",
            arity=2,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("Bool")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, suffix: s.endswith(suffix)
    )
    
    PRIMITIVES.register(
        "string-replace",
        Function(
            name="string-replace",
            arity=3,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("String"),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s, old, new: s.replace(old, new)
    )
    
    PRIMITIVES.register(
        "string->list",
        Function(
            name="string->list",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("List", [TypeAnnotation("String")])
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda s: list(s)
    )
    
    PRIMITIVES.register(
        "list->string",
        Function(
            name="list->string",
            arity=1,
            effects={EffectType.PURE},
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("List", [TypeAnnotation("String")]),
                    TypeAnnotation("String")
                ],
                effects={EffectType.PURE}
            )
        ),
        lambda lst: "".join(str(c) for c in lst)
    )
    
    PRIMITIVES.register(
        "char->int",
        Function(
            name="char->int",
            arity=1,
            effects={EffectType.ERROR},  # Not a single character
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("String"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("Int"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda c: ord(c) if len(c) == 1 else {"error": "Not a single character"}
    )
    
    PRIMITIVES.register(
        "int->char",
        Function(
            name="int->char",
            arity=1,
            effects={EffectType.ERROR},  # Invalid character code
            type_annotation=TypeAnnotation(
                name="Function",
                parameters=[
                    TypeAnnotation("Int"),
                    TypeAnnotation("Result", [
                        TypeAnnotation("String"),
                        TypeAnnotation("Error")
                    ])
                ],
                effects={EffectType.ERROR}
            )
        ),
        lambda n: chr(n) if 0 <= n <= 0x10FFFF else {"error": "Invalid character code"}
    )


# Initialize string functions when module is imported
register_string_functions()