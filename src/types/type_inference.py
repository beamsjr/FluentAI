"""
ClaudeLang Type Inference for Optimization

This module implements a simpler type inference system focused on enabling
optimizations like type specialization and unboxing.
"""

from typing import Dict, Set, Optional, List, Union, Tuple
from dataclasses import dataclass, field
from enum import Enum, auto
from ..core.ast import *


class BasicType(Enum):
    """Basic types for optimization"""
    INT = auto()
    FLOAT = auto()
    BOOL = auto()
    STRING = auto()
    LIST = auto()
    FUNCTION = auto()
    ANY = auto()
    

@dataclass
class InferredType:
    """Type information for optimization"""
    base_type: BasicType
    is_constant: bool = False
    constant_value: Optional[Any] = None
    element_type: Optional['InferredType'] = None  # For lists
    param_types: Optional[List['InferredType']] = None  # For functions
    return_type: Optional['InferredType'] = None  # For functions
    
    def is_numeric(self) -> bool:
        """Check if type is numeric (int or float)"""
        return self.base_type in (BasicType.INT, BasicType.FLOAT)
    
    def __str__(self):
        if self.is_constant and self.constant_value is not None:
            return f"{self.base_type.name}({self.constant_value})"
        elif self.base_type == BasicType.LIST and self.element_type:
            return f"[{self.element_type}]"
        elif self.base_type == BasicType.FUNCTION:
            if self.param_types and self.return_type:
                params = ", ".join(str(p) for p in self.param_types)
                return f"({params}) -> {self.return_type}"
            return "Function"
        else:
            return self.base_type.name


class OptimizationTypeInferencer:
    """Type inference focused on optimization opportunities"""
    
    def __init__(self):
        self.node_types: Dict[str, InferredType] = {}
        self.builtin_signatures = self._init_builtin_signatures()
    
    def _init_builtin_signatures(self) -> Dict[str, InferredType]:
        """Initialize signatures for built-in functions"""
        INT = InferredType(BasicType.INT)
        FLOAT = InferredType(BasicType.FLOAT)
        BOOL = InferredType(BasicType.BOOL)
        STRING = InferredType(BasicType.STRING)
        ANY = InferredType(BasicType.ANY)
        
        def func_type(params: List[InferredType], ret: InferredType) -> InferredType:
            return InferredType(
                BasicType.FUNCTION,
                param_types=params,
                return_type=ret
            )
        
        return {
            # Integer arithmetic
            '+': func_type([INT, INT], INT),
            '-': func_type([INT, INT], INT),
            '*': func_type([INT, INT], INT),
            '/': func_type([INT, INT], INT),
            'mod': func_type([INT, INT], INT),
            
            # Float arithmetic
            '+.': func_type([FLOAT, FLOAT], FLOAT),
            '-.': func_type([FLOAT, FLOAT], FLOAT),
            '*.': func_type([FLOAT, FLOAT], FLOAT),
            '/.': func_type([FLOAT, FLOAT], FLOAT),
            
            # Comparison
            '<': func_type([INT, INT], BOOL),
            '>': func_type([INT, INT], BOOL),
            '<=': func_type([INT, INT], BOOL),
            '>=': func_type([INT, INT], BOOL),
            '==': func_type([ANY, ANY], BOOL),
            '!=': func_type([ANY, ANY], BOOL),
            
            # Boolean
            'and': func_type([BOOL, BOOL], BOOL),
            'or': func_type([BOOL, BOOL], BOOL),
            'not': func_type([BOOL], BOOL),
            
            # String
            'string-length': func_type([STRING], INT),
            'string-upcase': func_type([STRING], STRING),
            'string-downcase': func_type([STRING], STRING),
            'concat': func_type([STRING, STRING], STRING),
            
            # List operations
            'length': func_type([InferredType(BasicType.LIST, element_type=ANY)], INT),
            'empty?': func_type([InferredType(BasicType.LIST, element_type=ANY)], BOOL),
        }
    
    def infer_graph_types(self, graph: Graph) -> Dict[str, InferredType]:
        """Infer types for all nodes in a graph"""
        self.node_types = {}
        
        # Process nodes in topological order
        for node_id in graph.topological_sort():
            node = graph.get_node(node_id)
            if node:
                self._infer_node_type(node_id, node, graph)
        
        return self.node_types
    
    def _infer_node_type(self, node_id: str, node: ASTNode, graph: Graph) -> InferredType:
        """Infer type of a single node"""
        if node_id in self.node_types:
            return self.node_types[node_id]
        
        inferred = InferredType(BasicType.ANY)
        
        if isinstance(node, Literal):
            inferred = self._infer_literal(node)
        
        elif isinstance(node, Variable):
            inferred = self._infer_variable(node)
        
        elif isinstance(node, Function):
            inferred = self._infer_function(node)
        
        elif isinstance(node, Application):
            inferred = self._infer_application(node, graph)
        
        elif isinstance(node, If):
            inferred = self._infer_if(node, graph)
        
        elif isinstance(node, Let):
            inferred = self._infer_let(node, graph)
        
        elif isinstance(node, Lambda):
            inferred = self._infer_lambda(node, graph)
        
        self.node_types[node_id] = inferred
        return inferred
    
    def _infer_literal(self, node: Literal) -> InferredType:
        """Infer type of a literal"""
        value = node.value
        
        if isinstance(value, int):
            return InferredType(BasicType.INT, is_constant=True, constant_value=value)
        elif isinstance(value, float):
            return InferredType(BasicType.FLOAT, is_constant=True, constant_value=value)
        elif isinstance(value, bool):
            return InferredType(BasicType.BOOL, is_constant=True, constant_value=value)
        elif isinstance(value, str):
            return InferredType(BasicType.STRING, is_constant=True, constant_value=value)
        elif isinstance(value, list):
            # Infer element type from first element
            if value:
                first_elem = value[0]
                if isinstance(first_elem, int):
                    elem_type = InferredType(BasicType.INT)
                elif isinstance(first_elem, float):
                    elem_type = InferredType(BasicType.FLOAT)
                elif isinstance(first_elem, str):
                    elem_type = InferredType(BasicType.STRING)
                else:
                    elem_type = InferredType(BasicType.ANY)
            else:
                elem_type = InferredType(BasicType.ANY)
            
            return InferredType(
                BasicType.LIST,
                is_constant=True,
                constant_value=value,
                element_type=elem_type
            )
        else:
            return InferredType(BasicType.ANY)
    
    def _infer_variable(self, node: Variable) -> InferredType:
        """Infer type of a variable"""
        # Check if it's a built-in function
        if node.name in self.builtin_signatures:
            return self.builtin_signatures[node.name]
        
        # Otherwise, we don't know the type yet
        return InferredType(BasicType.ANY)
    
    def _infer_function(self, node: Function) -> InferredType:
        """Infer type of a function"""
        if node.name in self.builtin_signatures:
            return self.builtin_signatures[node.name]
        
        # Generic function type
        return InferredType(BasicType.FUNCTION)
    
    def _infer_application(self, node: Application, graph: Graph) -> InferredType:
        """Infer type of function application"""
        # Get function type
        func_node = graph.get_node(node.function_id)
        if not func_node:
            return InferredType(BasicType.ANY)
        
        func_type = self._infer_node_type(node.function_id, func_node, graph)
        
        # If it's a known function, return its return type
        if func_type.base_type == BasicType.FUNCTION and func_type.return_type:
            # Check if all arguments are constants
            all_constant = True
            arg_values = []
            
            for arg_id in node.argument_ids:
                arg_node = graph.get_node(arg_id)
                if arg_node:
                    arg_type = self._infer_node_type(arg_id, arg_node, graph)
                    if arg_type.is_constant:
                        arg_values.append(arg_type.constant_value)
                    else:
                        all_constant = False
                        break
            
            # If all arguments are constant, the result might be constant too
            result_type = func_type.return_type
            if all_constant and isinstance(func_node, (Variable, Function)):
                # Try to evaluate at compile time
                func_name = func_node.name
                if func_name in ['+', '-', '*', '/', 'mod']:
                    # Integer arithmetic
                    if len(arg_values) == 2 and all(isinstance(v, int) for v in arg_values):
                        try:
                            if func_name == '+':
                                value = arg_values[0] + arg_values[1]
                            elif func_name == '-':
                                value = arg_values[0] - arg_values[1]
                            elif func_name == '*':
                                value = arg_values[0] * arg_values[1]
                            elif func_name == '/':
                                value = arg_values[0] // arg_values[1]
                            elif func_name == 'mod':
                                value = arg_values[0] % arg_values[1]
                            
                            return InferredType(
                                BasicType.INT,
                                is_constant=True,
                                constant_value=value
                            )
                        except:
                            pass
                
                elif func_name in ['<', '>', '<=', '>=', '==', '!=']:
                    # Comparison
                    if len(arg_values) == 2:
                        try:
                            if func_name == '<':
                                value = arg_values[0] < arg_values[1]
                            elif func_name == '>':
                                value = arg_values[0] > arg_values[1]
                            elif func_name == '<=':
                                value = arg_values[0] <= arg_values[1]
                            elif func_name == '>=':
                                value = arg_values[0] >= arg_values[1]
                            elif func_name == '==':
                                value = arg_values[0] == arg_values[1]
                            elif func_name == '!=':
                                value = arg_values[0] != arg_values[1]
                            
                            return InferredType(
                                BasicType.BOOL,
                                is_constant=True,
                                constant_value=value
                            )
                        except:
                            pass
            
            return result_type
        
        return InferredType(BasicType.ANY)
    
    def _infer_if(self, node: If, graph: Graph) -> InferredType:
        """Infer type of conditional"""
        # Check if condition is constant
        cond_node = graph.get_node(node.condition_id)
        if cond_node:
            cond_type = self._infer_node_type(node.condition_id, cond_node, graph)
            
            if cond_type.is_constant:
                # Constant condition - type is the taken branch
                if cond_type.constant_value:
                    then_node = graph.get_node(node.then_id)
                    if then_node:
                        return self._infer_node_type(node.then_id, then_node, graph)
                else:
                    else_node = graph.get_node(node.else_id)
                    if else_node:
                        return self._infer_node_type(node.else_id, else_node, graph)
        
        # Otherwise, type is the union of both branches
        # For simplicity, we'll just say ANY unless both branches have same type
        then_node = graph.get_node(node.then_id)
        else_node = graph.get_node(node.else_id)
        
        if then_node and else_node:
            then_type = self._infer_node_type(node.then_id, then_node, graph)
            else_type = self._infer_node_type(node.else_id, else_node, graph)
            
            # If both branches have same base type, use that
            if then_type.base_type == else_type.base_type:
                # Constants only if both branches are same constant
                if (then_type.is_constant and else_type.is_constant and
                    then_type.constant_value == else_type.constant_value):
                    return then_type
                else:
                    return InferredType(then_type.base_type)
        
        return InferredType(BasicType.ANY)
    
    def _infer_let(self, node: Let, graph: Graph) -> InferredType:
        """Infer type of let expression"""
        # Just infer the body type
        body_node = graph.get_node(node.body_id)
        if body_node:
            return self._infer_node_type(node.body_id, body_node, graph)
        
        return InferredType(BasicType.ANY)
    
    def _infer_lambda(self, node: Lambda, graph: Graph) -> InferredType:
        """Infer type of lambda"""
        # For now, just return generic function type
        # A more sophisticated analysis would infer parameter and return types
        return InferredType(BasicType.FUNCTION)


def analyze_optimization_opportunities(graph: Graph) -> Dict[str, Any]:
    """Analyze a graph for optimization opportunities based on types"""
    inferencer = OptimizationTypeInferencer()
    types = inferencer.infer_graph_types(graph)
    
    opportunities = {
        'constant_nodes': [],
        'numeric_operations': [],
        'type_specializable': [],
        'unboxing_candidates': [],
    }
    
    for node_id, node_type in types.items():
        node = graph.get_node(node_id)
        
        # Constant propagation opportunities
        if node_type.is_constant:
            opportunities['constant_nodes'].append({
                'node_id': node_id,
                'type': str(node_type),
                'value': node_type.constant_value
            })
        
        # Numeric operations that can be specialized
        if isinstance(node, Application) and node_type.is_numeric():
            func_node = graph.get_node(node.function_id)
            if isinstance(func_node, (Variable, Function)):
                func_name = func_node.name
                if func_name in ['+', '-', '*', '/', 'mod']:
                    opportunities['numeric_operations'].append({
                        'node_id': node_id,
                        'operation': func_name,
                        'type': node_type.base_type.name
                    })
        
        # Functions that can be type-specialized
        if isinstance(node, Lambda):
            opportunities['type_specializable'].append({
                'node_id': node_id,
                'type': 'lambda'
            })
        
        # Values that can be unboxed
        if node_type.is_numeric() and not isinstance(node, Literal):
            opportunities['unboxing_candidates'].append({
                'node_id': node_id,
                'type': node_type.base_type.name
            })
    
    return opportunities


def demonstrate_type_inference():
    """Demonstrate type inference for optimization"""
    from ..parser import parse
    
    print("ClaudeLang Type Inference for Optimization")
    print("=" * 60)
    
    examples = [
        ("Constant arithmetic", "(+ 2 3)"),
        ("Mixed types", "(+ 2 (* 3 4))"),
        ("Conditional", "(if (> 10 5) 100 200)"),
        ("Variable arithmetic", "(let ((x 10)) (+ x 5))"),
        ("List operations", "(length [1 2 3 4 5])"),
        ("Complex expression", "(let ((f (lambda (x) (* x 2)))) (f 21))"),
    ]
    
    inferencer = OptimizationTypeInferencer()
    
    for name, code in examples:
        print(f"\n{name}: {code}")
        print("-" * 40)
        
        graph = parse(code)
        types = inferencer.infer_graph_types(graph)
        
        # Show inferred types
        for node_id, node in graph.nodes.items():
            if node_id in types:
                node_type = types[node_id]
                if isinstance(node, Literal):
                    print(f"  Literal {node.value}: {node_type}")
                elif isinstance(node, Variable):
                    print(f"  Variable '{node.name}': {node_type}")
                elif isinstance(node, Application):
                    print(f"  Application: {node_type}")
                elif isinstance(node, Lambda):
                    print(f"  Lambda: {node_type}")
        
        # Show optimization opportunities
        opportunities = analyze_optimization_opportunities(graph)
        
        if opportunities['constant_nodes']:
            print("\n  Constant propagation opportunities:")
            for opp in opportunities['constant_nodes']:
                print(f"    - Node {opp['node_id'][:8]}: {opp['value']}")
        
        if opportunities['numeric_operations']:
            print("\n  Type-specializable numeric operations:")
            for opp in opportunities['numeric_operations']:
                print(f"    - {opp['operation']} operation on {opp['type']}")


if __name__ == "__main__":
    demonstrate_type_inference()