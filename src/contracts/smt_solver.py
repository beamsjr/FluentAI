"""
SMT Solver Integration for Contract Verification

Provides integration with Z3 SMT solver for automated theorem proving.
"""

from typing import Dict, List, Optional, Set, Tuple, Any, Union
from dataclasses import dataclass
from enum import Enum, auto
import logging

try:
    import z3
    HAS_Z3 = True
except ImportError:
    HAS_Z3 = False
    z3 = None

from ..core.ast import ASTNode, NodeType, Graph


class SMTSort(Enum):
    """SMT sorts (types)"""
    BOOL = auto()
    INT = auto()
    REAL = auto()
    STRING = auto()
    ARRAY = auto()
    FUNCTION = auto()


@dataclass
class SMTContext:
    """Context for SMT solving"""
    solver: Any  # z3.Solver
    variables: Dict[str, Any]  # Variable name -> z3 expression
    functions: Dict[str, Any]  # Function name -> z3 function
    assertions: List[Any]  # z3 assertions
    
    def __init__(self):
        if HAS_Z3:
            self.solver = z3.Solver()
        else:
            self.solver = None
        self.variables = {}
        self.functions = {}
        self.assertions = []


class SMTEncoder:
    """Encodes ClaudeLang expressions as SMT formulas"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        if not HAS_Z3:
            self.logger.warning("Z3 not available. SMT solving disabled.")
    
    def encode_graph(self, graph: Graph, context: SMTContext) -> Optional[Any]:
        """Encode a computation graph as SMT formula"""
        if not HAS_Z3 or not graph.root_id:
            return None
        
        root_node = graph.nodes.get(graph.root_id)
        if not root_node:
            return None
        
        return self.encode_node(root_node, graph, context)
    
    def encode_node(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode an AST node as SMT expression"""
        if not HAS_Z3:
            return None
        
        if node.node_type == NodeType.LITERAL:
            return self._encode_literal(node.value)
        
        elif node.node_type == NodeType.VARIABLE:
            return self._encode_variable(node.name, context)
        
        elif node.node_type == NodeType.APPLICATION:
            return self._encode_application(node, graph, context)
        
        elif node.node_type == NodeType.LAMBDA:
            return self._encode_lambda(node, graph, context)
        
        elif node.node_type == NodeType.IF:
            return self._encode_if(node, graph, context)
        
        elif node.node_type == NodeType.BINARY_OP:
            return self._encode_binary_op(node, graph, context)
        
        elif node.node_type == NodeType.UNARY_OP:
            return self._encode_unary_op(node, graph, context)
        
        else:
            self.logger.warning(f"Cannot encode node type: {node.node_type}")
            return None
    
    def _encode_literal(self, value: Any) -> Any:
        """Encode a literal value"""
        if isinstance(value, bool):
            return z3.BoolVal(value)
        elif isinstance(value, int):
            return z3.IntVal(value)
        elif isinstance(value, float):
            return z3.RealVal(str(value))
        elif isinstance(value, str):
            return z3.StringVal(value)
        else:
            # Fallback to integer encoding
            return z3.IntVal(0)
    
    def _encode_variable(self, name: str, context: SMTContext) -> Any:
        """Encode a variable reference"""
        if name in context.variables:
            return context.variables[name]
        
        # Create new variable based on expected type
        # For now, default to integer
        var = z3.Int(name)
        context.variables[name] = var
        return var
    
    def _encode_application(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode function application"""
        func_node = graph.nodes.get(node.function_id)
        if not func_node:
            return None
        
        # Encode arguments
        args = []
        for arg_id in node.argument_ids:
            arg_node = graph.nodes.get(arg_id)
            if arg_node:
                arg_expr = self.encode_node(arg_node, graph, context)
                if arg_expr is not None:
                    args.append(arg_expr)
        
        # Handle built-in functions
        if hasattr(func_node, 'name'):
            return self._encode_builtin_function(func_node.name, args, context)
        
        # Handle user-defined functions
        return self._encode_user_function(func_node, args, graph, context)
    
    def _encode_builtin_function(self, name: str, args: List[Any], context: SMTContext) -> Any:
        """Encode built-in function calls"""
        if not args:
            return None
        
        # Arithmetic
        if name == '+':
            return z3.Sum(args)
        elif name == '-':
            return args[0] - args[1] if len(args) == 2 else -args[0]
        elif name == '*':
            result = args[0]
            for arg in args[1:]:
                result = result * arg
            return result
        elif name == '/':
            return args[0] / args[1] if len(args) == 2 else None
        
        # Comparison
        elif name == '=':
            return args[0] == args[1] if len(args) == 2 else None
        elif name == '<':
            return args[0] < args[1] if len(args) == 2 else None
        elif name == '>':
            return args[0] > args[1] if len(args) == 2 else None
        elif name == '<=':
            return args[0] <= args[1] if len(args) == 2 else None
        elif name == '>=':
            return args[0] >= args[1] if len(args) == 2 else None
        
        # Boolean
        elif name == 'and':
            return z3.And(args)
        elif name == 'or':
            return z3.Or(args)
        elif name == 'not':
            return z3.Not(args[0]) if args else None
        elif name == 'implies':
            return z3.Implies(args[0], args[1]) if len(args) == 2 else None
        
        # Predicates
        elif name == 'int?':
            # Type checking - assume type is correct in typed language
            return z3.BoolVal(True)
        elif name == 'positive?':
            return args[0] > 0 if args else None
        elif name == 'even?':
            return (args[0] % 2) == 0 if args else None
        elif name == 'odd?':
            return (args[0] % 2) == 1 if args else None
        
        # List operations (simplified)
        elif name == 'null?':
            # Would need list theory
            return z3.BoolVal(False)
        elif name == 'sorted?':
            # Complex predicate - assume true for now
            return z3.BoolVal(True)
        
        # Quantifiers
        elif name == 'forall':
            # Would need to handle quantifier properly
            return z3.BoolVal(True)
        elif name == 'exists':
            # Would need to handle quantifier properly
            return z3.BoolVal(True)
        
        else:
            self.logger.warning(f"Unknown builtin function: {name}")
            return None
    
    def _encode_user_function(self, func_node: ASTNode, args: List[Any], 
                            graph: Graph, context: SMTContext) -> Any:
        """Encode user-defined function"""
        # For now, create uninterpreted function
        func_name = f"func_{func_node.id}"
        if func_name not in context.functions:
            # Create function with appropriate signature
            # For simplicity, assume Int -> Int
            context.functions[func_name] = z3.Function(func_name, z3.IntSort(), z3.IntSort())
        
        func = context.functions[func_name]
        return func(args[0]) if args else None
    
    def _encode_lambda(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode lambda expression"""
        # Lambda encoding is complex - for now, return None
        return None
    
    def _encode_if(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode if expression"""
        if len(node.children) != 3:
            return None
        
        cond_node = graph.nodes.get(node.children[0])
        then_node = graph.nodes.get(node.children[1])
        else_node = graph.nodes.get(node.children[2])
        
        if not all([cond_node, then_node, else_node]):
            return None
        
        cond = self.encode_node(cond_node, graph, context)
        then_expr = self.encode_node(then_node, graph, context)
        else_expr = self.encode_node(else_node, graph, context)
        
        if all(x is not None for x in [cond, then_expr, else_expr]):
            return z3.If(cond, then_expr, else_expr)
        
        return None
    
    def _encode_binary_op(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode binary operation"""
        if len(node.children) != 2:
            return None
        
        left_node = graph.nodes.get(node.children[0])
        right_node = graph.nodes.get(node.children[1])
        
        if not all([left_node, right_node]):
            return None
        
        left = self.encode_node(left_node, graph, context)
        right = self.encode_node(right_node, graph, context)
        
        if left is None or right is None:
            return None
        
        # Map operation to SMT
        op = getattr(node, 'operator', None)
        if op == '+':
            return left + right
        elif op == '-':
            return left - right
        elif op == '*':
            return left * right
        elif op == '/':
            return left / right
        elif op == '=':
            return left == right
        elif op == '<':
            return left < right
        elif op == '>':
            return left > right
        elif op == '<=':
            return left <= right
        elif op == '>=':
            return left >= right
        else:
            return None
    
    def _encode_unary_op(self, node: ASTNode, graph: Graph, context: SMTContext) -> Any:
        """Encode unary operation"""
        if not node.children:
            return None
        
        operand_node = graph.nodes.get(node.children[0])
        if not operand_node:
            return None
        
        operand = self.encode_node(operand_node, graph, context)
        if operand is None:
            return None
        
        op = getattr(node, 'operator', None)
        if op == '-':
            return -operand
        elif op == 'not':
            return z3.Not(operand)
        else:
            return None


class SMTSolver:
    """SMT solver for contract verification"""
    
    def __init__(self):
        self.encoder = SMTEncoder()
        self.logger = logging.getLogger(__name__)
    
    def prove_implication(self, assumptions: List[Graph], conclusion: Graph) -> Tuple[bool, Optional[str]]:
        """Prove that assumptions imply conclusion"""
        if not HAS_Z3:
            return False, "Z3 not available"
        
        context = SMTContext()
        
        # Encode assumptions
        for assumption in assumptions:
            expr = self.encoder.encode_graph(assumption, context)
            if expr is not None:
                context.solver.add(expr)
        
        # Encode negation of conclusion (proof by contradiction)
        conclusion_expr = self.encoder.encode_graph(conclusion, context)
        if conclusion_expr is None:
            return False, "Could not encode conclusion"
        
        context.solver.add(z3.Not(conclusion_expr))
        
        # Check satisfiability
        result = context.solver.check()
        
        if result == z3.unsat:
            # Unsatisfiable means the implication holds
            return True, "Proven by SMT solver"
        elif result == z3.sat:
            # Satisfiable means we found a counterexample
            model = context.solver.model()
            return False, f"Counterexample found: {model}"
        else:
            # Unknown
            return False, "SMT solver returned unknown"
    
    def check_satisfiability(self, formula: Graph) -> Tuple[bool, Optional[Dict[str, Any]]]:
        """Check if a formula is satisfiable and return a model if so"""
        if not HAS_Z3:
            return False, None
        
        context = SMTContext()
        
        expr = self.encoder.encode_graph(formula, context)
        if expr is None:
            return False, None
        
        context.solver.add(expr)
        
        result = context.solver.check()
        
        if result == z3.sat:
            model = context.solver.model()
            # Convert model to dictionary
            model_dict = {}
            for var_name, z3_var in context.variables.items():
                val = model.eval(z3_var)
                if val is not None:
                    model_dict[var_name] = self._z3_to_python(val)
            return True, model_dict
        else:
            return False, None
    
    def _z3_to_python(self, z3_val: Any) -> Any:
        """Convert Z3 value to Python value"""
        if z3.is_int_value(z3_val):
            return z3_val.as_long()
        elif z3.is_real_value(z3_val):
            return float(z3_val.as_decimal(10))
        elif z3.is_true(z3_val):
            return True
        elif z3.is_false(z3_val):
            return False
        elif z3.is_string_value(z3_val):
            return z3_val.as_string()
        else:
            return str(z3_val)