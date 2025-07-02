"""
ClaudeLang Linter

This module implements a linter for ClaudeLang that checks for:
- Code style violations
- Common mistakes and anti-patterns
- Potential bugs
- Performance issues
- Best practices
"""

from typing import List, Optional, Set, Dict, Any, Callable
from dataclasses import dataclass, field
from enum import Enum
from ..parser.sexpr_parser import parse
from ..core.ast import *
from ..errors.exceptions import ClaudeLangError


class LintLevel(Enum):
    """Severity levels for lint issues"""
    ERROR = "error"
    WARNING = "warning"
    INFO = "info"
    HINT = "hint"


@dataclass
class LintIssue:
    """A single lint issue"""
    rule_id: str
    level: LintLevel
    message: str
    line: int = 0
    column: int = 0
    end_line: Optional[int] = None
    end_column: Optional[int] = None
    suggestion: Optional[str] = None


@dataclass
class LintRule:
    """A lint rule definition"""
    id: str
    name: str
    description: str
    level: LintLevel
    check: Callable[[Graph, Dict[str, Any]], List[LintIssue]]
    enabled: bool = True
    config: Dict[str, Any] = field(default_factory=dict)


class ClaudeLangLinter:
    """Linter for ClaudeLang code"""
    
    def __init__(self, rules: Optional[List[LintRule]] = None):
        """Initialize linter with rules"""
        if rules is None:
            from .rules import get_default_rules
            rules = get_default_rules()
        
        self.rules = {rule.id: rule for rule in rules if rule.enabled}
        self.issues: List[LintIssue] = []
    
    def lint(self, source: str) -> List[LintIssue]:
        """Lint ClaudeLang source code"""
        self.issues = []
        
        # Parse the source
        try:
            ast = parse(source)
        except Exception as e:
            # Syntax errors are reported as lint errors
            self.issues.append(LintIssue(
                rule_id="syntax-error",
                level=LintLevel.ERROR,
                message=f"Syntax error: {str(e)}",
                line=getattr(e, 'line', 1),
                column=getattr(e, 'column', 0)
            ))
            return self.issues
        
        # Run each lint rule
        context = self._build_context(ast, source)
        
        for rule in self.rules.values():
            try:
                rule_issues = rule.check(ast, context)
                self.issues.extend(rule_issues)
            except Exception as e:
                # Rule errors shouldn't crash the linter
                self.issues.append(LintIssue(
                    rule_id=rule.id,
                    level=LintLevel.ERROR,
                    message=f"Lint rule '{rule.id}' failed: {str(e)}"
                ))
        
        # Sort issues by location
        self.issues.sort(key=lambda i: (i.line, i.column))
        
        return self.issues
    
    def _build_context(self, ast: Graph, source: str) -> Dict[str, Any]:
        """Build context for lint rules"""
        return {
            'source': source,
            'lines': source.split('\n'),
            'ast': ast,
            'defined_functions': self._collect_defined_functions(ast),
            'defined_variables': self._collect_defined_variables(ast),
            'imported_modules': self._collect_imported_modules(ast),
            'used_effects': self._collect_used_effects(ast)
        }
    
    def _collect_defined_functions(self, ast: Graph) -> Set[str]:
        """Collect all defined function names"""
        functions = set()
        
        for node in ast.nodes.values():
            if isinstance(node, Let):
                for binding in node.bindings:
                    # Check if binding value is a lambda
                    value_node = ast.nodes.get(binding['value_id'])
                    if isinstance(value_node, Lambda):
                        functions.add(binding['name'])
        
        return functions
    
    def _collect_defined_variables(self, ast: Graph) -> Set[str]:
        """Collect all defined variable names"""
        variables = set()
        
        for node in ast.nodes.values():
            if isinstance(node, Let):
                for binding in node.bindings:
                    variables.add(binding['name'])
            elif isinstance(node, Lambda):
                variables.update(node.parameter_names)
        
        return variables
    
    def _collect_imported_modules(self, ast: Graph) -> Set[str]:
        """Collect all imported module names"""
        modules = set()
        
        for node in ast.nodes.values():
            if isinstance(node, Import):
                modules.add(node.module_path)
        
        return modules
    
    def _collect_used_effects(self, ast: Graph) -> Set[str]:
        """Collect all used effect types"""
        effects = set()
        
        for node in ast.nodes.values():
            if isinstance(node, Effect):
                effects.add(node.effect_type.name)
        
        return effects
    
    def enable_rule(self, rule_id: str):
        """Enable a lint rule"""
        if rule_id in self.rules:
            self.rules[rule_id].enabled = True
    
    def disable_rule(self, rule_id: str):
        """Disable a lint rule"""
        if rule_id in self.rules:
            self.rules[rule_id].enabled = False
    
    def configure_rule(self, rule_id: str, config: Dict[str, Any]):
        """Configure a lint rule"""
        if rule_id in self.rules:
            self.rules[rule_id].config.update(config)


def lint_code(source: str, rules: Optional[List[LintRule]] = None) -> List[LintIssue]:
    """Lint ClaudeLang source code"""
    linter = ClaudeLangLinter(rules)
    return linter.lint(source)


def lint_file(filename: str, rules: Optional[List[LintRule]] = None) -> List[LintIssue]:
    """Lint a ClaudeLang file"""
    with open(filename, 'r') as f:
        source = f.read()
    
    return lint_code(source, rules)