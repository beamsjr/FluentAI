"""
Runtime Contract Verification for ClaudeLang

This module implements runtime checking of function contracts,
including preconditions, postconditions, and invariants.
"""

from typing import Any, Dict, List, Optional, Tuple, Callable, TYPE_CHECKING

if TYPE_CHECKING:
    from ..interpreter.interpreter import Interpreter, Environment
from dataclasses import dataclass, field
from enum import Enum, auto

from ..core.ast import Contract, Graph, ASTNode, NodeType
from ..errors import DiagnosticEngine, SourceLocation


class ContractViolationType(Enum):
    """Types of contract violations"""
    PRECONDITION = auto()
    POSTCONDITION = auto()
    INVARIANT = auto()
    COMPLEXITY = auto()
    PURITY = auto()


@dataclass
class ContractViolation(Exception):
    """Exception raised when a contract is violated"""
    violation_type: ContractViolationType
    function_name: str
    condition: str
    message: str
    location: Optional[SourceLocation] = None
    
    def __str__(self):
        type_str = self.violation_type.name.replace('_', ' ').title()
        return f"{type_str} violation in {self.function_name}: {self.message}"


@dataclass
class ContractContext:
    """Context for contract verification"""
    function_name: str
    arguments: Dict[str, Any]
    old_values: Dict[str, Any] = field(default_factory=dict)
    result: Any = None
    effects_used: set = field(default_factory=set)
    call_depth: int = 0
    
    def capture_old_values(self, env: 'Environment', expressions: List[str]):
        """Capture 'old' values before function execution"""
        for expr in expressions:
            if expr.startswith("old(") and expr.endswith(")"):
                inner_expr = expr[4:-1]
                # This would need proper expression evaluation
                self.old_values[expr] = None  # Placeholder


class ContractVerifier:
    """Verifies contracts at runtime"""
    
    def __init__(self, interpreter: 'Interpreter'):
        self.interpreter = interpreter
        self.contracts: Dict[str, Contract] = {}
        self.enabled = True
        self.diagnostics = DiagnosticEngine()
        self.contract_stack: List[ContractContext] = []
    
    def register_contract(self, contract: Contract):
        """Register a contract for a function"""
        self.contracts[contract.function_name] = contract
    
    def enable(self):
        """Enable contract checking"""
        self.enabled = True
    
    def disable(self):
        """Disable contract checking (for performance)"""
        self.enabled = False
    
    def verify_function_call(self, function_name: str, args: List[Any], 
                           arg_names: List[str], env: 'Environment') -> ContractContext:
        """Verify preconditions before function call"""
        if not self.enabled or function_name not in self.contracts:
            return ContractContext(function_name, {})
        
        contract = self.contracts[function_name]
        
        # Create context with argument bindings
        arg_bindings = dict(zip(arg_names, args))
        context = ContractContext(function_name, arg_bindings)
        self.contract_stack.append(context)
        
        # Check preconditions
        for precond_id in contract.preconditions:
            if not self._evaluate_condition(precond_id, env, context):
                condition_str = self._get_condition_string(precond_id)
                raise ContractViolation(
                    violation_type=ContractViolationType.PRECONDITION,
                    function_name=function_name,
                    condition=condition_str,
                    message=f"Precondition failed: {condition_str}"
                )
        
        # Capture old values for postconditions
        context.capture_old_values(env, self._extract_old_expressions(contract))
        
        return context
    
    def verify_function_return(self, context: ContractContext, result: Any, 
                             env: 'Environment', effects: Optional[set] = None):
        """Verify postconditions after function return"""
        if not self.enabled or context.function_name not in self.contracts:
            return
        
        contract = self.contracts[context.function_name]
        context.result = result
        
        # Check purity constraint
        if contract.pure and effects:
            raise ContractViolation(
                violation_type=ContractViolationType.PURITY,
                function_name=context.function_name,
                condition="pure",
                message=f"Pure function used effects: {effects}"
            )
        
        # Check postconditions
        for postcond_id in contract.postconditions:
            if not self._evaluate_condition(postcond_id, env, context):
                condition_str = self._get_condition_string(postcond_id)
                raise ContractViolation(
                    violation_type=ContractViolationType.POSTCONDITION,
                    function_name=context.function_name,
                    condition=condition_str,
                    message=f"Postcondition failed: {condition_str}"
                )
        
        # Remove from stack
        if self.contract_stack and self.contract_stack[-1] == context:
            self.contract_stack.pop()
    
    def verify_invariant(self, context: ContractContext, env: 'Environment'):
        """Verify invariants during execution"""
        if not self.enabled or context.function_name not in self.contracts:
            return
        
        contract = self.contracts[context.function_name]
        
        for invariant_id in contract.invariants:
            if not self._evaluate_condition(invariant_id, env, context):
                condition_str = self._get_condition_string(invariant_id)
                raise ContractViolation(
                    violation_type=ContractViolationType.INVARIANT,
                    function_name=context.function_name,
                    condition=condition_str,
                    message=f"Invariant violated: {condition_str}"
                )
    
    def _evaluate_condition(self, condition_id: str, env: 'Environment', 
                          context: ContractContext) -> bool:
        """Evaluate a condition expression"""
        # Create augmented environment with contract variables
        contract_env = env.extend()
        
        # Add function arguments
        for name, value in context.arguments.items():
            contract_env.define(name, value)
        
        # Add special contract variables
        if context.result is not None:
            contract_env.define("result", context.result)
        
        # Add old values
        for name, value in context.old_values.items():
            contract_env.define(name, value)
        
        # Evaluate the condition
        try:
            result = self.interpreter.evaluate_node(condition_id, contract_env)
            return bool(result)
        except Exception as e:
            # If evaluation fails, consider it a violation
            return False
    
    def _get_condition_string(self, condition_id: str) -> str:
        """Get string representation of condition"""
        node = self.interpreter.graph.get_node(condition_id)
        if node:
            return self._ast_to_string(node)
        return f"<condition {condition_id}>"
    
    def _ast_to_string(self, node: ASTNode) -> str:
        """Convert AST node back to string representation"""
        # Simplified - real implementation would handle all node types
        if node.node_type == NodeType.LITERAL:
            return str(node.value)
        elif node.node_type == NodeType.VARIABLE:
            return node.name
        elif node.node_type == NodeType.FUNCTION:
            return node.name
        elif node.node_type == NodeType.APPLICATION:
            func = self._ast_to_string(self.interpreter.graph.get_node(node.function_id))
            args = [self._ast_to_string(self.interpreter.graph.get_node(arg_id)) 
                   for arg_id in node.argument_ids]
            return f"({func} {' '.join(args)})"
        else:
            return f"<{node.node_type.name}>"
    
    def _extract_old_expressions(self, contract: Contract) -> List[str]:
        """Extract expressions that use 'old' from postconditions"""
        old_exprs = []
        # This would parse postconditions to find old(...) expressions
        return old_exprs


@dataclass
class ContractChecker:
    """Decorator-style contract checker for functions"""
    verifier: ContractVerifier
    contract: Contract
    
    def __call__(self, func: Callable) -> Callable:
        """Wrap function with contract checking"""
        def wrapper(*args, **kwargs):
            # Create context
            arg_names = list(func.__code__.co_varnames[:func.__code__.co_argcount])
            # Create a dummy environment for contract checking
            from ..interpreter.interpreter import Environment
            context = self.verifier.verify_function_call(
                self.contract.function_name, args, arg_names, Environment()
            )
            
            try:
                # Execute function
                result = func(*args, **kwargs)
                
                # Verify postconditions
                from ..interpreter.interpreter import Environment
                self.verifier.verify_function_return(context, result, Environment())
                
                return result
            except Exception as e:
                # Clean up context on exception
                if self.verifier.contract_stack and self.verifier.contract_stack[-1] == context:
                    self.verifier.contract_stack.pop()
                raise
        
        return wrapper


class ContractMonitor:
    """Monitors contract violations and statistics"""
    
    def __init__(self):
        self.violations: List[ContractViolation] = []
        self.checks_performed = 0
        self.checks_passed = 0
        self.performance_impact: Dict[str, float] = {}
    
    def record_violation(self, violation: ContractViolation):
        """Record a contract violation"""
        self.violations.append(violation)
    
    def record_check(self, passed: bool, function: str, duration: float):
        """Record contract check statistics"""
        self.checks_performed += 1
        if passed:
            self.checks_passed += 1
        
        if function not in self.performance_impact:
            self.performance_impact[function] = 0.0
        self.performance_impact[function] += duration
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get contract checking statistics"""
        return {
            "total_checks": self.checks_performed,
            "passed_checks": self.checks_passed,
            "failed_checks": len(self.violations),
            "success_rate": self.checks_passed / max(1, self.checks_performed),
            "violations_by_type": self._count_violations_by_type(),
            "performance_impact": self.performance_impact,
            "total_overhead": sum(self.performance_impact.values())
        }
    
    def _count_violations_by_type(self) -> Dict[str, int]:
        """Count violations by type"""
        counts = {}
        for violation in self.violations:
            type_name = violation.violation_type.name
            counts[type_name] = counts.get(type_name, 0) + 1
        return counts


# Built-in contract predicates
def register_contract_predicates(env: 'Environment'):
    """Register built-in predicates for use in contracts"""
    
    # Type predicates
    env.define("int?", lambda x: isinstance(x, int))
    env.define("float?", lambda x: isinstance(x, float))
    env.define("number?", lambda x: isinstance(x, (int, float)))
    env.define("string?", lambda x: isinstance(x, str))
    env.define("list?", lambda x: isinstance(x, list))
    env.define("function?", lambda x: callable(x))
    env.define("boolean?", lambda x: isinstance(x, bool))
    
    # Numeric predicates
    env.define("positive?", lambda x: isinstance(x, (int, float)) and x > 0)
    env.define("negative?", lambda x: isinstance(x, (int, float)) and x < 0)
    env.define("even?", lambda x: isinstance(x, int) and x % 2 == 0)
    env.define("odd?", lambda x: isinstance(x, int) and x % 2 == 1)
    
    # List predicates
    env.define("empty?", lambda lst: isinstance(lst, list) and len(lst) == 0)
    env.define("sorted?", lambda lst: all(lst[i] <= lst[i+1] for i in range(len(lst)-1)))
    env.define("all", lambda pred, lst: all(pred(x) for x in lst))
    env.define("any", lambda pred, lst: any(pred(x) for x in lst))
    
    # Logic operators
    env.define("implies", lambda a, b: not a or b)
    env.define("iff", lambda a, b: a == b)
    
    # Quantifiers (simplified)
    env.define("forall", lambda var, domain, pred: all(pred(x) for x in domain))
    env.define("exists", lambda var, domain, pred: any(pred(x) for x in domain))