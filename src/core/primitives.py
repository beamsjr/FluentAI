"""
ClaudeLang Primitive Operations

This module defines the built-in primitive operations available in ClaudeLang.
Each primitive is carefully designed to be unambiguous and have explicit effects.
"""

from typing import Dict, Callable, Any, List, Set
from .ast import Function, EffectType, TypeAnnotation


class PrimitiveRegistry:
    """Registry of built-in primitive operations"""
    
    def __init__(self):
        self.primitives: Dict[str, Function] = {}
        self.implementations: Dict[str, Callable] = {}
        self._register_core_primitives()
    
    def register(self, name: str, func: Function, implementation: Callable):
        """Register a primitive operation"""
        self.primitives[name] = func
        self.implementations[name] = implementation
    
    def get_function(self, name: str) -> Function:
        """Get a primitive function by name"""
        return self.primitives.get(name)
    
    def get_implementation(self, name: str) -> Callable:
        """Get the implementation of a primitive"""
        return self.implementations.get(name)
    
    def _register_core_primitives(self):
        """Register core primitive operations"""
        
        # Arithmetic operations
        self.register(
            "+",
            Function(
                name="+",
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
            lambda x, y: x + y
        )
        
        self.register(
            "-",
            Function(
                name="-",
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
            lambda x, y: x - y
        )
        
        self.register(
            "*",
            Function(
                name="*",
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
            lambda x, y: x * y
        )
        
        self.register(
            "/",
            Function(
                name="/",
                arity=2,
                effects={EffectType.ERROR},  # Division by zero possible
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Number"),
                        TypeAnnotation("Number"),
                        TypeAnnotation("Result", [
                            TypeAnnotation("Number"),
                            TypeAnnotation("Error")
                        ])
                    ],
                    effects={EffectType.ERROR}
                )
            ),
            lambda x, y: x / y if y != 0 else {"error": "Division by zero"}
        )
        
        # Comparison operations
        self.register(
            "==",
            Function(
                name="==",
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
            lambda x, y: x == y
        )
        
        self.register(
            "<",
            Function(
                name="<",
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
            lambda x, y: x < y
        )
        
        # Boolean operations
        self.register(
            "and",
            Function(
                name="and",
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
            lambda x, y: x and y
        )
        
        self.register(
            "or",
            Function(
                name="or",
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
            lambda x, y: x or y
        )
        
        self.register(
            "not",
            Function(
                name="not",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Bool"),
                        TypeAnnotation("Bool")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda x: not x
        )
        
        # List operations
        self.register(
            "cons",
            Function(
                name="cons",
                arity=2,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("a"),
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("List", [TypeAnnotation("a")])
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda head, tail: [head] + tail
        )
        
        self.register(
            "head",
            Function(
                name="head",
                arity=1,
                effects={EffectType.ERROR},  # Empty list error
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("Result", [
                            TypeAnnotation("a"),
                            TypeAnnotation("Error")
                        ])
                    ],
                    effects={EffectType.ERROR}
                )
            ),
            lambda lst: lst[0] if lst else {"error": "Empty list"}
        )
        
        self.register(
            "tail",
            Function(
                name="tail",
                arity=1,
                effects={EffectType.ERROR},  # Empty list error
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("Result", [
                            TypeAnnotation("List", [TypeAnnotation("a")]),
                            TypeAnnotation("Error")
                        ])
                    ],
                    effects={EffectType.ERROR}
                )
            ),
            lambda lst: lst[1:] if lst else {"error": "Empty list"}
        )
        
        self.register(
            "empty?",
            Function(
                name="empty?",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("List", [TypeAnnotation("a")]),
                        TypeAnnotation("Bool")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda lst: len(lst) == 0
        )
        
        # String operations
        self.register(
            "concat",
            Function(
                name="concat",
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
        
        # Type conversions
        self.register(
            "to-string",
            Function(
                name="to-string",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("a"),
                        TypeAnnotation("String")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda x: str(x)
        )
        
        self.register(
            "to-int",
            Function(
                name="to-int",
                arity=1,
                effects={EffectType.ERROR},  # Parse error possible
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
            lambda s: int(s) if s.isdigit() else {"error": f"Cannot parse '{s}' as integer"}
        )
        
        # Tuple operations
        self.register(
            "tuple",
            Function(
                name="tuple",
                arity=-1,  # Variadic
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[],  # Dynamic typing for variadic
                    effects={EffectType.PURE}
                )
            ),
            lambda *args: tuple(args)
        )
        
        self.register(
            "fst",
            Function(
                name="fst",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Tuple", [
                            TypeAnnotation("a"),
                            TypeAnnotation("b")
                        ]),
                        TypeAnnotation("a")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda t: t[0]
        )
        
        self.register(
            "snd",
            Function(
                name="snd",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Tuple", [
                            TypeAnnotation("a"),
                            TypeAnnotation("b")
                        ]),
                        TypeAnnotation("b")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda t: t[1]
        )
        
        # String conversion
        self.register(
            "to-string",
            Function(
                name="to-string",
                arity=1,
                effects={EffectType.PURE},
                type_annotation=TypeAnnotation(
                    name="Function",
                    parameters=[
                        TypeAnnotation("Any"),
                        TypeAnnotation("String")
                    ],
                    effects={EffectType.PURE}
                )
            ),
            lambda x: str(x)
        )
        
        # Memoization primitive
        def memoize_impl(func):
            """Create a memoized version of a function"""
            cache = {}
            def memoized(*args):
                # Convert args to a hashable key
                key = str(args)
                if key not in cache:
                    cache[key] = func(*args)
                return cache[key]
            return memoized
        
        self.register(
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


# Global primitive registry
PRIMITIVES = PrimitiveRegistry()