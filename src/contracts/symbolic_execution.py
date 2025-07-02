"""
Symbolic Execution Engine for Contract Verification

Executes programs symbolically to verify contracts without concrete values.
"""

from typing import Dict, List, Optional, Set, Tuple, Any, Union
from dataclasses import dataclass, field
from enum import Enum, auto
import logging

from ..core.ast import ASTNode, NodeType, Graph, Function
from ..interpreter.interpreter import Environment


class SymbolicValueType(Enum):
    """Types of symbolic values"""
    CONCRETE = auto()      # Concrete value
    SYMBOLIC = auto()      # Purely symbolic
    CONSTRAINT = auto()    # Symbolic with constraints
    BOTTOM = auto()        # Unreachable/error state


@dataclass
class SymbolicValue:
    """Represents a symbolic or concrete value"""
    value_type: SymbolicValueType
    concrete_value: Optional[Any] = None
    symbol_name: Optional[str] = None
    constraints: List['SymbolicConstraint'] = field(default_factory=list)
    
    def is_concrete(self) -> bool:
        return self.value_type == SymbolicValueType.CONCRETE
    
    def is_symbolic(self) -> bool:
        return self.value_type == SymbolicValueType.SYMBOLIC
    
    def is_bottom(self) -> bool:
        return self.value_type == SymbolicValueType.BOTTOM
    
    def __str__(self):
        if self.is_concrete():
            return str(self.concrete_value)
        elif self.is_symbolic():
            return self.symbol_name or "?"
        elif self.is_bottom():
            return "⊥"
        else:
            return f"{self.symbol_name} where {self.constraints}"


@dataclass
class SymbolicConstraint:
    """A constraint on symbolic values"""
    constraint_type: str  # 'eq', 'lt', 'gt', 'type', etc.
    left: Union[SymbolicValue, str]
    right: Optional[Union[SymbolicValue, Any]] = None
    
    def __str__(self):
        if self.constraint_type == 'type':
            return f"{self.left} : {self.right}"
        elif self.constraint_type in ['eq', 'lt', 'gt', 'le', 'ge', 'ne']:
            op_map = {'eq': '=', 'lt': '<', 'gt': '>', 'le': '≤', 'ge': '≥', 'ne': '≠'}
            return f"{self.left} {op_map[self.constraint_type]} {self.right}"
        else:
            return f"{self.constraint_type}({self.left}, {self.right})"


@dataclass
class SymbolicState:
    """State during symbolic execution"""
    environment: Dict[str, SymbolicValue]
    path_condition: List[SymbolicConstraint]
    effects: List[Tuple[str, Any]]  # Effect type and symbolic args
    
    def __init__(self):
        self.environment = {}
        self.path_condition = []
        self.effects = []
    
    def bind(self, name: str, value: SymbolicValue):
        """Bind a name to a symbolic value"""
        self.environment[name] = value
    
    def lookup(self, name: str) -> Optional[SymbolicValue]:
        """Look up a symbolic value"""
        return self.environment.get(name)
    
    def add_constraint(self, constraint: SymbolicConstraint):
        """Add a path constraint"""
        self.path_condition.append(constraint)
    
    def fork(self) -> 'SymbolicState':
        """Create a copy of this state for path forking"""
        new_state = SymbolicState()
        new_state.environment = self.environment.copy()
        new_state.path_condition = self.path_condition.copy()
        new_state.effects = self.effects.copy()
        return new_state


class SymbolicExecutor:
    """Symbolic execution engine"""
    
    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.symbol_counter = 0
    
    def execute(self, graph: Graph, initial_state: Optional[SymbolicState] = None) -> List[SymbolicState]:
        """Symbolically execute a computation graph, returning all possible final states"""
        if not initial_state:
            initial_state = SymbolicState()
        
        if not graph.root_id or graph.root_id not in graph.nodes:
            return [initial_state]
        
        root_node = graph.nodes[graph.root_id]
        final_states = []
        result = self._execute_node(root_node, graph, initial_state, final_states)
        
        # If no states were collected (simple expression), return the current state
        if not final_states:
            final_states.append(initial_state)
        
        return final_states
    
    def _execute_node(self, node: ASTNode, graph: Graph, state: SymbolicState, 
                     final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute a single node symbolically"""
        
        if node.node_type == NodeType.LITERAL:
            return self._execute_literal(node, state)
        
        elif node.node_type == NodeType.VARIABLE:
            return self._execute_variable(node, state)
        
        elif node.node_type == NodeType.LAMBDA:
            return self._execute_lambda(node, graph, state)
        
        elif node.node_type == NodeType.APPLICATION:
            return self._execute_application(node, graph, state, final_states)
        
        elif node.node_type == NodeType.IF:
            return self._execute_if(node, graph, state, final_states)
        
        elif node.node_type == NodeType.LET:
            return self._execute_let(node, graph, state, final_states)
        
        elif node.node_type == NodeType.BINARY_OP:
            return self._execute_binary_op(node, graph, state, final_states)
        
        elif node.node_type == NodeType.UNARY_OP:
            return self._execute_unary_op(node, graph, state, final_states)
        
        else:
            # Unknown node type - return symbolic value
            return self._make_symbolic(f"unknown_{node.node_type}")
    
    def _execute_literal(self, node: ASTNode, state: SymbolicState) -> SymbolicValue:
        """Execute a literal node"""
        return SymbolicValue(
            value_type=SymbolicValueType.CONCRETE,
            concrete_value=node.value
        )
    
    def _execute_variable(self, node: ASTNode, state: SymbolicState) -> SymbolicValue:
        """Execute a variable reference"""
        value = state.lookup(node.name)
        if value:
            return value
        else:
            # Unbound variable - create symbolic value
            return self._make_symbolic(node.name)
    
    def _execute_lambda(self, node: ASTNode, graph: Graph, state: SymbolicState) -> SymbolicValue:
        """Execute a lambda - return symbolic function value"""
        return self._make_symbolic(f"lambda_{node.id}")
    
    def _execute_application(self, node: ASTNode, graph: Graph, state: SymbolicState,
                           final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute function application"""
        func_node = graph.nodes.get(node.function_id)
        if not func_node:
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        # Evaluate arguments
        args = []
        for arg_id in node.argument_ids:
            arg_node = graph.nodes.get(arg_id)
            if arg_node:
                arg_val = self._execute_node(arg_node, graph, state, final_states)
                args.append(arg_val)
        
        # Handle built-in functions
        if hasattr(func_node, 'name'):
            return self._execute_builtin(func_node.name, args, state)
        
        # Handle user functions symbolically
        return self._make_symbolic(f"app_{node.id}")
    
    def _execute_builtin(self, name: str, args: List[SymbolicValue], 
                        state: SymbolicState) -> SymbolicValue:
        """Execute built-in function"""
        
        # Arithmetic operations
        if name == '+' and all(arg.is_concrete() for arg in args):
            result = sum(arg.concrete_value for arg in args)
            return SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=result)
        elif name == '+':
            # Symbolic addition
            return self._make_symbolic_binop('+', args[0], args[1] if len(args) > 1 else None)
        
        elif name == '-' and all(arg.is_concrete() for arg in args):
            if len(args) == 1:
                return SymbolicValue(value_type=SymbolicValueType.CONCRETE, 
                                   concrete_value=-args[0].concrete_value)
            else:
                return SymbolicValue(value_type=SymbolicValueType.CONCRETE,
                                   concrete_value=args[0].concrete_value - args[1].concrete_value)
        elif name == '-':
            return self._make_symbolic_binop('-', args[0], args[1] if len(args) > 1 else None)
        
        # Comparisons
        elif name in ['=', '<', '>', '<=', '>=', '!=']:
            if len(args) == 2:
                if args[0].is_concrete() and args[1].is_concrete():
                    # Concrete comparison
                    left, right = args[0].concrete_value, args[1].concrete_value
                    if name == '=':
                        result = left == right
                    elif name == '<':
                        result = left < right
                    elif name == '>':
                        result = left > right
                    elif name == '<=':
                        result = left <= right
                    elif name == '>=':
                        result = left >= right
                    elif name == '!=':
                        result = left != right
                    return SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=result)
                else:
                    # Symbolic comparison - add constraint
                    constraint_map = {'=': 'eq', '<': 'lt', '>': 'gt', 
                                    '<=': 'le', '>=': 'ge', '!=': 'ne'}
                    constraint = SymbolicConstraint(
                        constraint_type=constraint_map[name],
                        left=args[0],
                        right=args[1]
                    )
                    # Return symbolic boolean
                    sym_result = self._make_symbolic(f"cmp_{self.symbol_counter}")
                    sym_result.constraints.append(constraint)
                    return sym_result
        
        # Type predicates
        elif name == 'int?' and args:
            if args[0].is_concrete():
                result = isinstance(args[0].concrete_value, int)
                return SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=result)
            else:
                # Add type constraint
                constraint = SymbolicConstraint(constraint_type='type', left=args[0], right='int')
                return self._make_symbolic_with_constraint(f"is_int_{args[0]}", constraint)
        
        # Effects
        elif name in ['print', 'read', 'write']:
            # Record effect
            state.effects.append((name, args))
            # Return unit value
            return SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=None)
        
        # Default: return symbolic value
        return self._make_symbolic(f"{name}_result")
    
    def _execute_if(self, node: ASTNode, graph: Graph, state: SymbolicState,
                   final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute if expression - may fork execution paths"""
        if len(node.children) != 3:
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        cond_node = graph.nodes.get(node.children[0])
        then_node = graph.nodes.get(node.children[1])
        else_node = graph.nodes.get(node.children[2])
        
        if not all([cond_node, then_node, else_node]):
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        # Evaluate condition
        cond_val = self._execute_node(cond_node, graph, state, final_states)
        
        if cond_val.is_concrete():
            # Concrete condition - take one branch
            if cond_val.concrete_value:
                return self._execute_node(then_node, graph, state, final_states)
            else:
                return self._execute_node(else_node, graph, state, final_states)
        else:
            # Symbolic condition - fork execution
            
            # Then branch
            then_state = state.fork()
            then_constraint = SymbolicConstraint(
                constraint_type='eq',
                left=cond_val,
                right=SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=True)
            )
            then_state.add_constraint(then_constraint)
            then_result = self._execute_node(then_node, graph, then_state, final_states)
            
            # Else branch
            else_state = state.fork()
            else_constraint = SymbolicConstraint(
                constraint_type='eq',
                left=cond_val,
                right=SymbolicValue(value_type=SymbolicValueType.CONCRETE, concrete_value=False)
            )
            else_state.add_constraint(else_constraint)
            else_result = self._execute_node(else_node, graph, else_state, final_states)
            
            # Return symbolic value representing the merge
            return self._make_symbolic(f"if_{node.id}")
    
    def _execute_let(self, node: ASTNode, graph: Graph, state: SymbolicState,
                    final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute let binding"""
        # Create new environment frame
        new_state = state.fork()
        
        # Process bindings
        for binding in node.bindings:
            name = binding['name']
            value_node = graph.nodes.get(binding['value_id'])
            if value_node:
                value = self._execute_node(value_node, graph, new_state, final_states)
                new_state.bind(name, value)
        
        # Execute body
        body_node = graph.nodes.get(node.body_id)
        if body_node:
            return self._execute_node(body_node, graph, new_state, final_states)
        
        return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
    
    def _execute_binary_op(self, node: ASTNode, graph: Graph, state: SymbolicState,
                          final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute binary operation"""
        if len(node.children) != 2:
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        left_node = graph.nodes.get(node.children[0])
        right_node = graph.nodes.get(node.children[1])
        
        if not all([left_node, right_node]):
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        left = self._execute_node(left_node, graph, state, final_states)
        right = self._execute_node(right_node, graph, state, final_states)
        
        op = getattr(node, 'operator', None)
        if op:
            return self._execute_builtin(op, [left, right], state)
        
        return self._make_symbolic(f"binop_{node.id}")
    
    def _execute_unary_op(self, node: ASTNode, graph: Graph, state: SymbolicState,
                         final_states: List[SymbolicState]) -> SymbolicValue:
        """Execute unary operation"""
        if not node.children:
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        operand_node = graph.nodes.get(node.children[0])
        if not operand_node:
            return SymbolicValue(value_type=SymbolicValueType.BOTTOM)
        
        operand = self._execute_node(operand_node, graph, state, final_states)
        
        op = getattr(node, 'operator', None)
        if op:
            return self._execute_builtin(op, [operand], state)
        
        return self._make_symbolic(f"unop_{node.id}")
    
    def _make_symbolic(self, name: str) -> SymbolicValue:
        """Create a new symbolic value"""
        self.symbol_counter += 1
        return SymbolicValue(
            value_type=SymbolicValueType.SYMBOLIC,
            symbol_name=f"{name}_{self.symbol_counter}"
        )
    
    def _make_symbolic_binop(self, op: str, left: SymbolicValue, 
                           right: Optional[SymbolicValue]) -> SymbolicValue:
        """Create symbolic value for binary operation"""
        if right:
            name = f"({left} {op} {right})"
        else:
            name = f"({op} {left})"
        return self._make_symbolic(name)
    
    def _make_symbolic_with_constraint(self, name: str, 
                                     constraint: SymbolicConstraint) -> SymbolicValue:
        """Create symbolic value with constraint"""
        sym_val = self._make_symbolic(name)
        sym_val.constraints.append(constraint)
        return sym_val