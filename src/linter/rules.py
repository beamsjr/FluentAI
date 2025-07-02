"""
ClaudeLang Lint Rules

This module defines the default lint rules for ClaudeLang.
"""

from typing import List, Dict, Any, Set
from .linter import LintRule, LintLevel, LintIssue
from ..core.ast import *


def check_unused_variables(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for unused variables"""
    issues = []
    
    # Collect all variable definitions and their locations
    defined_vars: Dict[str, List[ASTNode]] = {}
    
    for node in ast.nodes.values():
        if isinstance(node, Let):
            for binding in node.bindings:
                name = binding['name']
                if name not in defined_vars:
                    defined_vars[name] = []
                defined_vars[name].append(node)
        elif isinstance(node, Lambda):
            for param in node.parameter_names:
                if param not in defined_vars:
                    defined_vars[param] = []
                defined_vars[param].append(node)
    
    # Collect all variable references
    used_vars = set()
    for node in ast.nodes.values():
        if isinstance(node, Variable):
            used_vars.add(node.name)
    
    # Find unused variables
    for var_name, def_nodes in defined_vars.items():
        if var_name not in used_vars and not var_name.startswith('_'):
            for def_node in def_nodes:
                loc = def_node.source_location or {}
                issues.append(LintIssue(
                    rule_id="unused-variable",
                    level=LintLevel.WARNING,
                    message=f"Variable '{var_name}' is defined but never used",
                    line=loc.get('line', 0),
                    column=loc.get('column', 0),
                    suggestion=f"Remove unused variable '{var_name}' or prefix with '_' to indicate intentional non-use"
                ))
    
    return issues


def check_shadowed_variables(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for shadowed variable names"""
    issues = []
    
    # Track variable scopes
    def check_scope(node_id: str, parent_vars: Set[str]):
        node = ast.nodes.get(node_id)
        if not node:
            return
        
        current_vars = parent_vars.copy()
        
        if isinstance(node, Let):
            for binding in node.bindings:
                name = binding['name']
                if name in parent_vars:
                    loc = node.source_location or {}
                    issues.append(LintIssue(
                        rule_id="shadowed-variable",
                        level=LintLevel.WARNING,
                        message=f"Variable '{name}' shadows an outer binding",
                        line=loc.get('line', 0),
                        column=loc.get('column', 0)
                    ))
                current_vars.add(name)
            
            # Check body with new scope
            check_scope(node.body_id, current_vars)
            
        elif isinstance(node, Lambda):
            for param in node.parameter_names:
                if param in parent_vars:
                    loc = node.source_location or {}
                    issues.append(LintIssue(
                        rule_id="shadowed-variable",
                        level=LintLevel.WARNING,
                        message=f"Parameter '{param}' shadows an outer binding",
                        line=loc.get('line', 0),
                        column=loc.get('column', 0)
                    ))
                current_vars.add(param)
            
            # Check body with new scope
            check_scope(node.body_id, current_vars)
            
        elif isinstance(node, Application):
            # Check function and arguments
            check_scope(node.function_id, parent_vars)
            for arg_id in node.argument_ids:
                check_scope(arg_id, parent_vars)
                
        elif isinstance(node, If):
            check_scope(node.condition_id, parent_vars)
            check_scope(node.then_id, parent_vars)
            check_scope(node.else_id, parent_vars)
            
        elif isinstance(node, Match):
            check_scope(node.expr_id, parent_vars)
            for branch in node.branches:
                # Pattern variables create new scope
                branch_vars = parent_vars.copy()
                pattern_vars = extract_pattern_vars(branch['pattern_id'], ast)
                branch_vars.update(pattern_vars)
                check_scope(branch['body_id'], branch_vars)
    
    # Start checking from root
    check_scope(ast.root_id, set())
    
    return issues


def extract_pattern_vars(pattern_id: str, ast: Graph) -> Set[str]:
    """Extract variable names from a pattern"""
    vars = set()
    pattern = ast.nodes.get(pattern_id)
    
    if isinstance(pattern, PatternVar):
        vars.add(pattern.name)
    elif isinstance(pattern, PatternConstructor):
        for sub_pattern_id in pattern.sub_patterns:
            vars.update(extract_pattern_vars(sub_pattern_id, ast))
    elif isinstance(pattern, PatternList):
        for elem_id in pattern.elements:
            vars.update(extract_pattern_vars(elem_id, ast))
        if pattern.rest_pattern:
            vars.update(extract_pattern_vars(pattern.rest_pattern, ast))
    
    return vars


def check_missing_else(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for if expressions without else branch (which would be an error)"""
    issues = []
    
    for node in ast.nodes.values():
        if isinstance(node, If):
            if not node.else_id:
                loc = node.source_location or {}
                issues.append(LintIssue(
                    rule_id="missing-else",
                    level=LintLevel.ERROR,
                    message="If expression must have an else branch",
                    line=loc.get('line', 0),
                    column=loc.get('column', 0),
                    suggestion="Add an else branch to the if expression"
                ))
    
    return issues


def check_empty_let(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for let expressions with no bindings"""
    issues = []
    
    for node in ast.nodes.values():
        if isinstance(node, Let) and not node.bindings:
            loc = node.source_location or {}
            issues.append(LintIssue(
                rule_id="empty-let",
                level=LintLevel.WARNING,
                message="Let expression has no bindings",
                line=loc.get('line', 0),
                column=loc.get('column', 0),
                suggestion="Remove empty let or add bindings"
            ))
    
    return issues


def check_redundant_lambda(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for redundant lambda expressions like (lambda (x) (f x))"""
    issues = []
    
    for node in ast.nodes.values():
        if isinstance(node, Lambda):
            body = ast.nodes.get(node.body_id)
            
            # Check if body is just a function application
            if isinstance(body, Application) and len(body.argument_ids) == len(node.parameter_names):
                # Check if arguments match parameters in order
                all_match = True
                for i, (param, arg_id) in enumerate(zip(node.parameter_names, body.argument_ids)):
                    arg_node = ast.nodes.get(arg_id)
                    if not isinstance(arg_node, Variable) or arg_node.name != param:
                        all_match = False
                        break
                
                if all_match:
                    func_node = ast.nodes.get(body.function_id)
                    if isinstance(func_node, (Variable, Function)):
                        loc = node.source_location or {}
                        func_name = func_node.name if hasattr(func_node, 'name') else 'function'
                        issues.append(LintIssue(
                            rule_id="redundant-lambda",
                            level=LintLevel.INFO,
                            message=f"Lambda can be simplified to just '{func_name}'",
                            line=loc.get('line', 0),
                            column=loc.get('column', 0),
                            suggestion=f"Replace (lambda ({' '.join(node.parameter_names)}) ({func_name} {' '.join(node.parameter_names)})) with {func_name}"
                        ))
    
    return issues


def check_unreachable_match_branches(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for unreachable match branches"""
    issues = []
    
    for node in ast.nodes.values():
        if isinstance(node, Match):
            has_wildcard = False
            wildcard_index = -1
            
            for i, branch in enumerate(node.branches):
                pattern = ast.nodes.get(branch['pattern_id'])
                
                if isinstance(pattern, PatternWildcard) or isinstance(pattern, PatternVar):
                    has_wildcard = True
                    wildcard_index = i
                    break
            
            # Check if there are branches after wildcard
            if has_wildcard and wildcard_index < len(node.branches) - 1:
                loc = node.source_location or {}
                issues.append(LintIssue(
                    rule_id="unreachable-match-branch",
                    level=LintLevel.WARNING,
                    message="Match branches after wildcard pattern are unreachable",
                    line=loc.get('line', 0),
                    column=loc.get('column', 0),
                    suggestion="Move wildcard pattern to the end or remove unreachable branches"
                ))
    
    return issues


def check_missing_type_annotations(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for functions without type annotations (info level)"""
    issues = []
    
    for node in ast.nodes.values():
        if isinstance(node, Lambda) and not node.type_annotation:
            # Check if this lambda is bound to a name
            for parent in ast.nodes.values():
                if isinstance(parent, Let):
                    for binding in parent.bindings:
                        if binding['value_id'] == node.id:
                            loc = node.source_location or {}
                            issues.append(LintIssue(
                                rule_id="missing-type-annotation",
                                level=LintLevel.INFO,
                                message=f"Function '{binding['name']}' lacks type annotation",
                                line=loc.get('line', 0),
                                column=loc.get('column', 0),
                                suggestion="Consider adding type annotation with (: function Type)"
                            ))
                            break
    
    return issues


def check_effect_usage(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for effects used without proper handlers"""
    issues = []
    used_effects = context.get('used_effects', set())
    
    # For now, just check if IO effects are used at top level
    if 'IO' in used_effects:
        has_io_handler = False
        for node in ast.nodes.values():
            if isinstance(node, Handler) and EffectType.IO in node.handles:
                has_io_handler = True
                break
        
        if not has_io_handler:
            # This is actually fine for top-level IO, but we can warn
            pass
    
    return issues


def check_long_functions(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check for overly long functions"""
    issues = []
    max_lines = 50  # Configurable
    
    lines = context.get('lines', [])
    
    for node in ast.nodes.values():
        if isinstance(node, Lambda):
            loc = node.source_location or {}
            start_line = loc.get('line', 0)
            
            # Try to estimate function length
            # This is approximate without proper source location tracking
            if start_line > 0:
                # Find the function in source
                depth = 0
                end_line = start_line
                
                for i in range(start_line - 1, len(lines)):
                    line = lines[i]
                    depth += line.count('(') - line.count(')')
                    if depth <= 0:
                        end_line = i + 1
                        break
                
                func_lines = end_line - start_line + 1
                if func_lines > max_lines:
                    issues.append(LintIssue(
                        rule_id="long-function",
                        level=LintLevel.INFO,
                        message=f"Function is {func_lines} lines long (max recommended: {max_lines})",
                        line=start_line,
                        column=loc.get('column', 0),
                        suggestion="Consider breaking this function into smaller functions"
                    ))
    
    return issues


def check_naming_conventions(ast: Graph, context: Dict[str, Any]) -> List[LintIssue]:
    """Check naming conventions"""
    issues = []
    
    # Check variable names (should be kebab-case or single letter)
    for node in ast.nodes.values():
        if isinstance(node, Let):
            for binding in node.bindings:
                name = binding['name']
                if not is_valid_variable_name(name):
                    loc = node.source_location or {}
                    issues.append(LintIssue(
                        rule_id="naming-convention",
                        level=LintLevel.INFO,
                        message=f"Variable '{name}' doesn't follow naming convention",
                        line=loc.get('line', 0),
                        column=loc.get('column', 0),
                        suggestion="Use kebab-case for multi-word names (e.g., 'my-variable')"
                    ))
        
        # Check type names (should be PascalCase)
        elif isinstance(node, DataDeclaration):
            if not node.type_name[0].isupper():
                loc = node.source_location or {}
                issues.append(LintIssue(
                    rule_id="naming-convention",
                    level=LintLevel.WARNING,
                    message=f"Type '{node.type_name}' should start with uppercase letter",
                    line=loc.get('line', 0),
                    column=loc.get('column', 0),
                    suggestion="Use PascalCase for type names (e.g., 'MyType')"
                ))
            
            # Check constructor names
            for constructor in node.constructors:
                if not constructor['name'][0].isupper():
                    loc = node.source_location or {}
                    issues.append(LintIssue(
                        rule_id="naming-convention",
                        level=LintLevel.WARNING,
                        message=f"Constructor '{constructor['name']}' should start with uppercase letter",
                        line=loc.get('line', 0),
                        column=loc.get('column', 0),
                        suggestion="Use PascalCase for constructor names"
                    ))
    
    return issues


def is_valid_variable_name(name: str) -> bool:
    """Check if variable name follows conventions"""
    # Single letter is OK
    if len(name) == 1 and name.isalpha():
        return True
    
    # Should be kebab-case
    if '-' in name:
        parts = name.split('-')
        return all(part.isalnum() for part in parts if part)
    
    # Single word lowercase
    return name.islower() and name.isalnum()


def get_default_rules() -> List[LintRule]:
    """Get the default set of lint rules"""
    return [
        LintRule(
            id="unused-variable",
            name="Unused Variable",
            description="Checks for variables that are defined but never used",
            level=LintLevel.WARNING,
            check=check_unused_variables
        ),
        LintRule(
            id="shadowed-variable",
            name="Shadowed Variable",
            description="Checks for variables that shadow outer bindings",
            level=LintLevel.WARNING,
            check=check_shadowed_variables
        ),
        LintRule(
            id="missing-else",
            name="Missing Else Branch",
            description="Checks for if expressions without else branch",
            level=LintLevel.ERROR,
            check=check_missing_else
        ),
        LintRule(
            id="empty-let",
            name="Empty Let",
            description="Checks for let expressions with no bindings",
            level=LintLevel.WARNING,
            check=check_empty_let
        ),
        LintRule(
            id="redundant-lambda",
            name="Redundant Lambda",
            description="Checks for lambda expressions that can be simplified",
            level=LintLevel.INFO,
            check=check_redundant_lambda
        ),
        LintRule(
            id="unreachable-match-branch",
            name="Unreachable Match Branch",
            description="Checks for match branches that can never be reached",
            level=LintLevel.WARNING,
            check=check_unreachable_match_branches
        ),
        LintRule(
            id="missing-type-annotation",
            name="Missing Type Annotation",
            description="Checks for functions without type annotations",
            level=LintLevel.INFO,
            check=check_missing_type_annotations,
            enabled=False  # Disabled by default as it's quite noisy
        ),
        LintRule(
            id="long-function",
            name="Long Function",
            description="Checks for functions that are too long",
            level=LintLevel.INFO,
            check=check_long_functions
        ),
        LintRule(
            id="naming-convention",
            name="Naming Convention",
            description="Checks that names follow ClaudeLang conventions",
            level=LintLevel.INFO,
            check=check_naming_conventions
        )
    ]