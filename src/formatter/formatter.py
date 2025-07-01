"""
ClaudeLang Code Formatter

This module implements a code formatter for ClaudeLang that produces
consistent, readable S-expression formatting.

Formatting rules:
1. Consistent indentation (2 spaces per level)
2. Special forms have custom formatting
3. Long lines are broken at appropriate points
4. Vertical alignment for let bindings and similar constructs
"""

from typing import List, Optional, Union, Tuple
from dataclasses import dataclass
from ..parser.sexpr_parser import parse, Lexer, Token
from ..core.ast import *


@dataclass
class FormatOptions:
    """Formatting options"""
    indent_width: int = 2
    max_line_length: int = 80
    align_let_bindings: bool = True
    align_match_branches: bool = True
    compact_single_arg_functions: bool = True
    insert_blank_lines: bool = True
    preserve_comments: bool = True


class ClaudeLangFormatter:
    """Code formatter for ClaudeLang"""
    
    def __init__(self, options: Optional[FormatOptions] = None):
        self.options = options or FormatOptions()
        self.indent_level = 0
        self.current_line_length = 0
        self.output: List[str] = []
        self.tokens: List[Token] = []
        self.token_pos = 0
        self.comments: List[Tuple[int, str]] = []
    
    def format(self, source: str) -> str:
        """Format ClaudeLang source code"""
        # Reset state
        self.indent_level = 0
        self.current_line_length = 0
        self.output = []
        self.token_pos = 0
        
        # Handle multiple top-level expressions
        source_stripped = source.strip()
        if not source_stripped:
            return ''
        
        # Try to tokenize and find expression boundaries
        try:
            lexer = Lexer(source_stripped)
            tokens = lexer.tokenize()
        except Exception:
            # If tokenization fails, return original
            return source
        
        # Find top-level expressions by counting parentheses
        expressions = []
        current_expr_tokens = []
        paren_depth = 0
        bracket_depth = 0
        
        for token in tokens:
            current_expr_tokens.append(token)
            
            if token.type == 'LPAREN':
                paren_depth += 1
            elif token.type == 'RPAREN':
                paren_depth -= 1
            elif token.type == 'LBRACKET':
                bracket_depth += 1
            elif token.type == 'RBRACKET':
                bracket_depth -= 1
            
            # Complete expression when depth returns to 0
            if paren_depth == 0 and bracket_depth == 0 and current_expr_tokens:
                # Reconstruct source for this expression
                expr_source = self._tokens_to_source(current_expr_tokens)
                if expr_source.strip():
                    expressions.append(expr_source)
                current_expr_tokens = []
        
        # Format each expression
        formatted_expressions = []
        for expr in expressions:
            try:
                # Parse and format individual expression
                ast = parse(expr)
                self.output = []
                self._format_node(ast.root_id, ast)
                result = ''.join(self.output).strip()
                if result:
                    formatted_expressions.append(result)
            except Exception:
                # Keep original if formatting fails
                formatted_expressions.append(expr.strip())
        
        # Join expressions with newlines
        if formatted_expressions:
            return '\n'.join(formatted_expressions) + '\n'
        elif not expressions:
            # No complete expressions found, return original
            return source
        else:
            return ''
    
    def _tokens_to_source(self, tokens: List[Token]) -> str:
        """Reconstruct source from tokens"""
        parts = []
        for token in tokens:
            if token.type == 'STRING':
                # Add quotes back
                value = str(token.value).replace('\\', '\\\\').replace('"', '\\"')
                parts.append(f'"{value}"')
            elif token.type == 'BOOL':
                parts.append('#t' if token.value else '#f')
            elif token.type in ['LPAREN', 'RPAREN', 'LBRACKET', 'RBRACKET']:
                parts.append(token.value)
            elif token.type == 'COMMA':
                parts.append(',')
            else:
                parts.append(str(token.value))
        
        # Join with spaces, but handle special cases
        result = []
        for i, part in enumerate(parts):
            if i > 0:
                prev = parts[i-1]
                # No space after opening parens/brackets or before closing ones
                if prev not in ['(', '['] and part not in [')', ']', ',']:
                    result.append(' ')
            result.append(part)
        
        return ''.join(result)
    
    def _extract_comments(self, source: str):
        """Extract comments from source"""
        self.comments = []
        lines = source.split('\n')
        for i, line in enumerate(lines):
            if ';' in line:
                # Simple comment extraction - could be improved
                comment_start = line.find(';')
                comment = line[comment_start:]
                self.comments.append((i + 1, comment))
    
    def _format_node(self, node_id: str, graph: Graph, inline: bool = False):
        """Format an AST node"""
        if not node_id or node_id not in graph.nodes:
            return
        
        node = graph.nodes[node_id]
        
        # Add any comments that should appear before this node
        self._add_preceding_comments(node)
        
        if isinstance(node, Literal):
            self._format_literal(node)
        elif isinstance(node, Variable):
            self._format_variable(node)
        elif isinstance(node, Function):
            self._format_function(node)
        elif isinstance(node, Application):
            self._format_application(node, graph, inline)
        elif isinstance(node, Lambda):
            self._format_lambda(node, graph, inline)
        elif isinstance(node, Let):
            self._format_let(node, graph, inline)
        elif isinstance(node, If):
            self._format_if(node, graph, inline)
        elif isinstance(node, Sequence):
            self._format_sequence(node, graph, inline)
        elif isinstance(node, Match):
            self._format_match(node, graph, inline)
        elif isinstance(node, DataDeclaration):
            self._format_data_declaration(node, graph)
        elif isinstance(node, Contract):
            self._format_contract(node, graph)
        elif isinstance(node, Module):
            self._format_module(node, graph)
        elif isinstance(node, Effect):
            self._format_effect(node, graph, inline)
        elif isinstance(node, TypeAscription):
            self._format_type_ascription(node, graph, inline)
        else:
            # Default formatting for unknown nodes
            self._write(f"; Unknown node type: {type(node).__name__}")
    
    def _add_preceding_comments(self, node: ASTNode):
        """Add comments that should appear before this node"""
        if hasattr(node, 'source_location') and node.source_location:
            line = node.source_location.get('line', 0)
            # Add any comments on lines before this node
            while self.comments and self.comments[0][0] < line:
                _, comment = self.comments.pop(0)
                self._write_line(comment)
    
    def _format_literal(self, node: Literal):
        """Format a literal value"""
        if node.literal_type == 'string':
            # Escape special characters
            value = str(node.value).replace('\\', '\\\\').replace('"', '\\"')
            self._write(f'"{value}"')
        elif node.literal_type == 'bool':
            self._write('#t' if node.value else '#f')
        elif node.literal_type == 'list' and isinstance(node.value, list):
            if not node.value:
                self._write('[]')
            else:
                self._write('[')
                for i, item in enumerate(node.value):
                    if i > 0:
                        self._write(' ')
                    self._write(str(item))
                self._write(']')
        else:
            self._write(str(node.value))
    
    def _format_variable(self, node: Variable):
        """Format a variable reference"""
        self._write(node.name)
    
    def _format_function(self, node: Function):
        """Format a function reference"""
        self._write(node.name)
    
    def _format_application(self, node: Application, graph: Graph, inline: bool = False):
        """Format function application"""
        func_node = graph.nodes.get(node.function_id)
        
        # Check if this is a special form that needs custom formatting
        if isinstance(func_node, Function) and func_node.name == 'cons':
            # Try to detect list pattern and format as list literal
            list_elements = self._extract_list_elements(node, graph)
            if list_elements is not None:
                self._write('[')
                for i, elem_id in enumerate(list_elements):
                    if i > 0:
                        self._write(' ')
                    self._format_node(elem_id, graph, inline=True)
                self._write(']')
                return
        
        self._write('(')
        self._format_node(node.function_id, graph, inline=True)
        
        # Format arguments
        for i, arg_id in enumerate(node.argument_ids):
            self._write(' ')
            
            # Check if we should break line
            if (not inline and self.current_line_length > self.options.max_line_length * 0.7 
                and i < len(node.argument_ids) - 1):
                self._write_line()
                self._write_indent()
                self._write('  ')  # Extra indent for continuation
            
            self._format_node(arg_id, graph, inline=True)
        
        self._write(')')
    
    def _format_lambda(self, node: Lambda, graph: Graph, inline: bool = False):
        """Format lambda expression"""
        self._write('(lambda (')
        
        # Format parameters
        for i, param in enumerate(node.parameter_names):
            if i > 0:
                self._write(' ')
            self._write(param)
        
        self._write(')')
        
        # Format body - prefer inline for simple expressions
        body_node = graph.nodes.get(node.body_id)
        if self._is_simple_expr(node.body_id, graph) or (
            isinstance(body_node, (Variable, Literal)) or 
            (isinstance(body_node, Application) and len(body_node.argument_ids) <= 3)):
            self._write(' ')
            self._format_node(node.body_id, graph, inline=True)
        else:
            self._write_line()
            self.indent_level += 1
            self._write_indent()
            self._format_node(node.body_id, graph)
            self.indent_level -= 1
        
        self._write(')')
    
    def _format_let(self, node: Let, graph: Graph, inline: bool = False):
        """Format let expression"""
        self._write('(let (')
        
        # Format bindings
        if node.bindings:
            if self.options.align_let_bindings and len(node.bindings) > 1:
                # Multi-line aligned bindings
                max_name_len = max(len(b['name']) for b in node.bindings)
                
                for i, binding in enumerate(node.bindings):
                    if i > 0:
                        self._write_line()
                        self._write(' ' * 6)  # Align with '(let ('
                    
                    self._write('(')
                    self._write(binding['name'])
                    self._write(' ' * (max_name_len - len(binding['name']) + 1))
                    self._format_node(binding['value_id'], graph, inline=True)
                    self._write(')')
            else:
                # Single binding or compact format
                for i, binding in enumerate(node.bindings):
                    if i > 0:
                        self._write(' ')
                    self._write(f"({binding['name']} ")
                    self._format_node(binding['value_id'], graph, inline=True)
                    self._write(')')
        
        self._write(')')
        
        # Format body
        body_node = graph.nodes.get(node.body_id)
        # Use multi-line format if we have multiple bindings or complex body
        if len(node.bindings) > 1:
            # Always use multi-line for multiple bindings
            self._write_line()
            self.indent_level += 1
            self._write_indent()
            self._format_node(node.body_id, graph)
            self.indent_level -= 1
        elif self._is_simple_expr(node.body_id, graph) or isinstance(body_node, (Variable, Application)):
            # Inline for single binding with simple body or function call
            self._write(' ')
            self._format_node(node.body_id, graph, inline=True)
        else:
            # Multi-line for complex body
            self._write_line()
            self.indent_level += 1
            self._write_indent()
            self._format_node(node.body_id, graph)
            self.indent_level -= 1
        
        self._write(')')
    
    def _format_if(self, node: If, graph: Graph, inline: bool = False):
        """Format if expression"""
        self._write('(if ')
        
        # Condition
        self._format_node(node.condition_id, graph, inline=True)
        
        # Then branch
        if self._is_simple_expr(node.then_id, graph) and self._is_simple_expr(node.else_id, graph):
            # Inline format for simple expressions
            self._write(' ')
            self._format_node(node.then_id, graph, inline=True)
            self._write(' ')
            self._format_node(node.else_id, graph, inline=True)
        else:
            # Multi-line format
            self._write_line()
            self.indent_level += 1
            self._write_indent()
            self._format_node(node.then_id, graph)
            self._write_line()
            self._write_indent()
            self._format_node(node.else_id, graph)
            self.indent_level -= 1
        
        self._write(')')
    
    def _format_sequence(self, node: Sequence, graph: Graph, inline: bool = False):
        """Format sequence (do) expression"""
        self._write('(do')
        
        # Check if all steps are simple enough for inline
        all_simple = all(self._is_simple_expr(step_id, graph) for step_id in node.step_ids)
        total_steps = len(node.step_ids)
        
        # Check if all are simple print statements
        all_simple_prints = True
        for step_id in node.step_ids:
            step_node = graph.nodes.get(step_id)
            if isinstance(step_node, Application):
                func_node = graph.nodes.get(step_node.function_id)
                if not (isinstance(func_node, Variable) and 
                        func_node.name == 'print' and 
                        len(step_node.argument_ids) == 1):
                    all_simple_prints = False
                    break
            else:
                all_simple_prints = False
                break
        
        if (all_simple and total_steps <= 3) or (all_simple_prints and total_steps <= 2):
            # Inline format for all simple expressions
            for step_id in node.step_ids:
                self._write(' ')
                self._format_node(step_id, graph, inline=True)
        else:
            # Multi-line format
            self.indent_level += 1
            for step_id in node.step_ids:
                self._write_line()
                self._write_indent()
                self._format_node(step_id, graph)
            self.indent_level -= 1
        
        self._write(')')
    
    def _format_match(self, node: Match, graph: Graph, inline: bool = False):
        """Format match expression"""
        self._write('(match ')
        self._format_node(node.expr_id, graph, inline=True)
        
        # Format branches
        self.indent_level += 1
        for branch in node.branches:
            self._write_line()
            self._write_indent()
            self._write('(')
            self._format_pattern(branch['pattern_id'], graph)
            
            # Format body - use multi-line for complex expressions
            body_node = graph.nodes.get(branch['body_id'])
            # Check if pattern is complex (has rest pattern or multiple elements)
            pattern_node = graph.nodes.get(branch['pattern_id'])
            is_complex_pattern = False
            if isinstance(pattern_node, PatternList):
                is_complex_pattern = pattern_node.rest_pattern is not None or len(pattern_node.elements) > 1
            
            if (self._is_simple_expr(branch['body_id'], graph) and 
                not is_complex_pattern and
                isinstance(body_node, (Literal, Variable))):
                self._write(' ')
                self._format_node(branch['body_id'], graph, inline=True)
            else:
                self._write_line()
                self.indent_level += 1
                self._write_indent()
                self._format_node(branch['body_id'], graph)
                self.indent_level -= 1
            
            self._write(')')
        
        self.indent_level -= 1
        self._write(')')
    
    def _format_pattern(self, pattern_id: str, graph: Graph):
        """Format a pattern"""
        if not pattern_id or pattern_id not in graph.nodes:
            return
        
        pattern = graph.nodes[pattern_id]
        
        if isinstance(pattern, PatternWildcard):
            self._write('_')
        elif isinstance(pattern, PatternVar):
            self._write(pattern.name)
        elif isinstance(pattern, PatternLiteral):
            literal = Literal(value=pattern.value, literal_type=type(pattern.value).__name__)
            self._format_literal(literal)
        elif isinstance(pattern, PatternConstructor):
            self._write('(')
            self._write(pattern.constructor)
            for sub_pattern in pattern.sub_patterns:
                self._write(' ')
                self._format_pattern(sub_pattern, graph)
            self._write(')')
        elif isinstance(pattern, PatternList):
            self._write('[')
            for i, elem in enumerate(pattern.elements):
                if i > 0:
                    self._write(', ')
                self._format_pattern(elem, graph)
            if pattern.rest_pattern:
                if pattern.elements:
                    self._write(', ')
                self._write('... ')
                self._format_pattern(pattern.rest_pattern, graph)
            self._write(']')
    
    def _format_data_declaration(self, node: DataDeclaration, graph: Graph):
        """Format data type declaration"""
        self._write('(data ')
        self._write(node.type_name)
        
        # Type parameters
        for param in node.type_params:
            self._write(' ')
            self._write(param)
        
        # Constructors
        self.indent_level += 1
        for constructor in node.constructors:
            self._write_line()
            self._write_indent()
            self._write(f"({constructor['name']}")
            
            for field in constructor['fields']:
                self._write(' ')
                self._format_type_annotation(field)
            
            self._write(')')
        
        self.indent_level -= 1
        self._write(')')
    
    def _format_type_annotation(self, type_ann: TypeAnnotation):
        """Format a type annotation"""
        if not type_ann.parameters:
            self._write(type_ann.name)
        else:
            self._write('(')
            self._write(type_ann.name)
            for param in type_ann.parameters:
                self._write(' ')
                self._format_type_annotation(param)
            self._write(')')
    
    def _format_contract(self, node: Contract, graph: Graph):
        """Format contract specification"""
        self._write('(spec:contract ')
        self._write(node.function_name)
        
        self.indent_level += 1
        
        # Preconditions
        if node.preconditions:
            self._write_line()
            self._write_indent()
            self._write(':requires [')
            for i, pre_id in enumerate(node.preconditions):
                if i > 0:
                    self._write(' ')
                self._format_node(pre_id, graph, inline=True)
            self._write(']')
        
        # Postconditions
        if node.postconditions:
            self._write_line()
            self._write_indent()
            self._write(':ensures [')
            for i, post_id in enumerate(node.postconditions):
                if i > 0:
                    self._write(' ')
                self._format_node(post_id, graph, inline=True)
            self._write(']')
        
        # Other properties
        if node.complexity:
            self._write_line()
            self._write_indent()
            self._write(f':complexity "{node.complexity}"')
        
        if not node.pure:
            self._write_line()
            self._write_indent()
            self._write(':pure false')
        
        self.indent_level -= 1
        self._write(')')
    
    def _format_module(self, node: Module, graph: Graph):
        """Format module declaration"""
        self._write('(module ')
        self._write(node.name)
        
        # Exports
        if node.exports:
            self._write_line()
            self.indent_level += 1
            self._write_indent()
            self._write('(export')
            for export in node.exports:
                self._write(' ')
                self._write(export)
            self._write(')')
            
            # Body
            self._write_line()
            self._write_indent()
            self._format_node(node.body_id, graph)
            self.indent_level -= 1
        else:
            self._write(' ')
            self._format_node(node.body_id, graph, inline=True)
        
        self._write(')')
    
    def _format_effect(self, node: Effect, graph: Graph, inline: bool = False):
        """Format effect expression"""
        self._write('(effect ')
        # Use the effect type name from the enum
        effect_name = node.effect_type.name.lower()
        self._write(f"{effect_name}:{node.operation}")
        
        # Arguments
        for arg_id in node.argument_ids:
            self._write(' ')
            self._format_node(arg_id, graph, inline=True)
        
        self._write(')')
    
    def _format_type_ascription(self, node: TypeAscription, graph: Graph, inline: bool = False):
        """Format type ascription"""
        self._write('(: ')
        self._format_node(node.expr_id, graph, inline=True)
        self._write(' ')
        self._format_type_annotation(node.ascribed_type)
        self._write(')')
    
    def _extract_list_elements(self, node: Application, graph: Graph) -> Optional[List[str]]:
        """Extract elements from a cons-based list structure"""
        elements = []
        current_node = node
        
        while True:
            # Check if this is a cons application
            if not isinstance(current_node, Application) or len(current_node.argument_ids) != 2:
                return None
            
            func_node = graph.nodes.get(current_node.function_id)
            if not isinstance(func_node, Function) or func_node.name != 'cons':
                return None
            
            # Add the first argument (head)
            elements.append(current_node.argument_ids[0])
            
            # Check the second argument (tail)
            tail_id = current_node.argument_ids[1]
            tail_node = graph.nodes.get(tail_id)
            
            if isinstance(tail_node, Literal) and tail_node.value == [] and tail_node.literal_type == 'list':
                # End of list
                return elements
            elif isinstance(tail_node, Application):
                # Continue with nested cons
                current_node = tail_node
            else:
                # Not a proper list
                return None
    
    def _is_simple_expr(self, node_id: str, graph: Graph) -> bool:
        """Check if expression is simple enough for inline formatting"""
        if not node_id or node_id not in graph.nodes:
            return True
        
        node = graph.nodes[node_id]
        
        if isinstance(node, (Literal, Variable, Function)):
            return True
        
        if isinstance(node, Application):
            # Check if it's a simple arithmetic or comparison
            func_node = graph.nodes.get(node.function_id)
            if isinstance(func_node, Function):
                # Simple operators with 1-2 arguments
                if func_node.name in ['+', '-', '*', '/', '=', '<', '>', '<=', '>=', 'not']:
                    return len(node.argument_ids) <= 2
                # Other functions are not considered simple
                return False
            elif isinstance(func_node, Variable):
                # Check if it's a simple operator by name
                if func_node.name in ['+', '-', '*', '/', '=', '<', '>', '<=', '>=', 'not', 
                                      'car', 'cdr', 'null?', 'number?', 'string?', 'list?']:
                    return len(node.argument_ids) <= 2
                # Other function calls (like print) are not simple
                return False
            return len(node.argument_ids) <= 1
        
        return False
    
    def _write(self, text: str):
        """Write text to output"""
        self.output.append(text)
        self.current_line_length += len(text)
    
    def _write_line(self, text: str = ''):
        """Write text and newline"""
        if text:
            self._write(text)
        self.output.append('\n')
        self.current_line_length = 0
    
    def _write_indent(self):
        """Write current indentation"""
        indent = ' ' * (self.indent_level * self.options.indent_width)
        self._write(indent)


def format_code(source: str, options: Optional[FormatOptions] = None) -> str:
    """Format ClaudeLang source code"""
    formatter = ClaudeLangFormatter(options)
    return formatter.format(source)


def format_file(filename: str, options: Optional[FormatOptions] = None):
    """Format a ClaudeLang file in place"""
    with open(filename, 'r') as f:
        source = f.read()
    
    formatted = format_code(source, options)
    
    with open(filename, 'w') as f:
        f.write(formatted)