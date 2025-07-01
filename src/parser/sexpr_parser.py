"""
ClaudeLang S-Expression Parser

This module implements a parser for the S-expression syntax of ClaudeLang.
S-expressions provide a human-editable text format that maps directly to the AST.
"""

import re
from typing import List, Union, Optional, Dict, Any, Tuple
from dataclasses import dataclass
from ..core.ast import *
from ..core.primitives import PRIMITIVES
from ..errors import DiagnosticEngine, SourceLocation, syntax_error


@dataclass
class Token:
    """Token representation"""
    type: str
    value: Any
    line: int
    column: int


class Lexer:
    """S-expression lexer"""
    
    def __init__(self, source: str):
        self.source = source
        self.pos = 0
        self.line = 1
        self.column = 1
        self.tokens: List[Token] = []
    
    def tokenize(self) -> List[Token]:
        """Tokenize the source code"""
        while self.pos < len(self.source):
            self._skip_whitespace()
            if self.pos >= len(self.source):
                break
            
            char = self.source[self.pos]
            
            if char == '(':
                self._add_token('LPAREN', '(')
                self.pos += 1
            elif char == ')':
                self._add_token('RPAREN', ')')
                self.pos += 1
            elif char == '[':
                self._add_token('LBRACKET', '[')
                self.pos += 1
            elif char == ']':
                self._add_token('RBRACKET', ']')
                self.pos += 1
            elif char == '"':
                self._read_string()
            elif char == ';':
                self._skip_comment()
            elif char.isdigit() or (char == '-' and self._peek().isdigit()):
                self._read_number()
            else:
                self._read_symbol()
        
        return self.tokens
    
    def _peek(self, offset: int = 1) -> str:
        """Peek at the next character"""
        pos = self.pos + offset
        return self.source[pos] if pos < len(self.source) else ''
    
    def _add_token(self, type_: str, value: Any):
        """Add a token to the list"""
        self.tokens.append(Token(type_, value, self.line, self.column))
    
    def _skip_whitespace(self):
        """Skip whitespace and track line/column"""
        while self.pos < len(self.source) and self.source[self.pos].isspace():
            if self.source[self.pos] == '\n':
                self.line += 1
                self.column = 1
            else:
                self.column += 1
            self.pos += 1
    
    def _skip_comment(self):
        """Skip line comments"""
        while self.pos < len(self.source) and self.source[self.pos] != '\n':
            self.pos += 1
    
    def _read_string(self):
        """Read a string literal"""
        start_line, start_col = self.line, self.column
        self.pos += 1  # Skip opening quote
        start = self.pos
        
        while self.pos < len(self.source) and self.source[self.pos] != '"':
            if self.source[self.pos] == '\\':
                self.pos += 2  # Skip escape sequence
            else:
                if self.source[self.pos] == '\n':
                    self.line += 1
                    self.column = 1
                else:
                    self.column += 1
                self.pos += 1
        
        value = self.source[start:self.pos]
        self.pos += 1  # Skip closing quote
        
        # Process escape sequences
        value = value.replace('\\n', '\n').replace('\\t', '\t').replace('\\"', '"')
        
        self._add_token('STRING', value)
    
    def _read_number(self):
        """Read a number literal"""
        start = self.pos
        
        if self.source[self.pos] == '-':
            self.pos += 1
        
        # Read integer part
        while self.pos < len(self.source) and self.source[self.pos].isdigit():
            self.pos += 1
        
        # Check for decimal
        if self.pos < len(self.source) and self.source[self.pos] == '.':
            self.pos += 1
            while self.pos < len(self.source) and self.source[self.pos].isdigit():
                self.pos += 1
            
            value = float(self.source[start:self.pos])
            self._add_token('FLOAT', value)
        else:
            value = int(self.source[start:self.pos])
            self._add_token('INT', value)
    
    def _read_symbol(self):
        """Read a symbol"""
        start = self.pos
        
        while self.pos < len(self.source):
            char = self.source[self.pos]
            if char.isspace() or char in '()[]";':
                break
            self.pos += 1
        
        value = self.source[start:self.pos]
        
        # Check for special symbols
        if value in ['true', '#t']:
            self._add_token('BOOL', True)
        elif value in ['false', '#f']:
            self._add_token('BOOL', False)
        else:
            self._add_token('SYMBOL', value)


class Parser:
    """S-expression parser that builds ClaudeLang AST"""
    
    def __init__(self, tokens: List[Token], filename: Optional[str] = None):
        self.tokens = tokens
        self.pos = 0
        self.graph = Graph()
        self.filename = filename
        self.diagnostics = DiagnosticEngine()
    
    def parse(self) -> Graph:
        """Parse tokens into an AST graph"""
        while self.pos < len(self.tokens):
            node_id = self._parse_expr()
            if node_id:
                self.graph.root_id = node_id
        
        return self.graph
    
    def _current(self) -> Optional[Token]:
        """Get current token"""
        return self.tokens[self.pos] if self.pos < len(self.tokens) else None
    
    def _advance(self) -> Token:
        """Advance to next token"""
        token = self.tokens[self.pos]
        self.pos += 1
        return token
    
    def _peek(self) -> Optional[Token]:
        """Peek at next token without advancing"""
        return self.tokens[self.pos + 1] if self.pos + 1 < len(self.tokens) else None
    
    def _current_location(self) -> SourceLocation:
        """Get source location for current token"""
        token = self._current()
        if token:
            return SourceLocation(
                filename=self.filename,
                line=token.line,
                column=token.column,
                length=len(str(token.value))
            )
        return SourceLocation(filename=self.filename)
    
    def _parse_expr(self) -> Optional[str]:
        """Parse an expression"""
        token = self._current()
        if not token:
            return None
        
        if token.type == 'LPAREN':
            return self._parse_list()
        elif token.type == 'LBRACKET':
            return self._parse_list_literal()
        elif token.type in ['INT', 'FLOAT', 'STRING', 'BOOL']:
            return self._parse_literal()
        elif token.type == 'SYMBOL':
            return self._parse_symbol()
        else:
            raise SyntaxError(f"Unexpected token: {token}")
    
    def _parse_literal(self) -> str:
        """Parse a literal value"""
        token = self._advance()
        
        type_map = {
            'INT': 'int',
            'FLOAT': 'float',
            'STRING': 'string',
            'BOOL': 'bool'
        }
        
        node = Literal(
            value=token.value,
            literal_type=type_map[token.type],
            source_location={'line': token.line, 'column': token.column}
        )
        
        return self.graph.add_node(node)
    
    def _parse_symbol(self) -> str:
        """Parse a symbol (variable or function reference)"""
        token = self._advance()
        
        # Check if it's a built-in function
        if PRIMITIVES.get_function(token.value):
            func = PRIMITIVES.get_function(token.value)
            node = Function(
                name=token.value,
                arity=func.arity,
                effects=func.effects,
                type_annotation=func.type_annotation,
                source_location={'line': token.line, 'column': token.column}
            )
            return self.graph.add_node(node)
        
        # Otherwise it's a variable
        node = Variable(
            name=token.value,
            source_location={'line': token.line, 'column': token.column}
        )
        
        return self.graph.add_node(node)
    
    def _parse_list(self) -> str:
        """Parse a list expression"""
        self._advance()  # Skip LPAREN
        
        if self._current() and self._current().type == 'RPAREN':
            self._advance()
            # Empty list
            node = Literal(value=[], literal_type='list')
            return self.graph.add_node(node)
        
        # Check for special forms
        if self._current() and self._current().type == 'SYMBOL':
            symbol = self._current().value
            
            if symbol == 'lambda':
                return self._parse_lambda()
            elif symbol == 'let':
                return self._parse_let()
            elif symbol == 'if':
                return self._parse_if()
            elif symbol == 'do':
                return self._parse_sequence()
            elif symbol == 'parallel':
                return self._parse_parallel()
            elif symbol == 'effect':
                return self._parse_effect()
            elif symbol == 'uncertain':
                return self._parse_uncertain()
            elif symbol == 'module':
                return self._parse_module()
            elif symbol == 'import':
                return self._parse_import()
            elif symbol == 'export':
                return self._parse_export()
            elif symbol == 'match':
                return self._parse_match()
            elif symbol == 'spec:contract':
                return self._parse_contract()
        
        # Regular function application
        return self._parse_application()
    
    def _parse_application(self) -> str:
        """Parse function application"""
        func_id = self._parse_expr()
        arg_ids = []
        
        while self._current() and self._current().type != 'RPAREN':
            arg_id = self._parse_expr()
            if arg_id:
                arg_ids.append(arg_id)
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis")
        
        self._advance()  # Skip RPAREN
        
        node = Application(
            function_id=func_id,
            argument_ids=arg_ids
        )
        
        return self.graph.add_node(node)
    
    def _parse_lambda(self) -> str:
        """Parse lambda expression: (lambda (x y) body)"""
        self._advance()  # Skip 'lambda'
        
        # Parse parameters
        if not self._current() or self._current().type != 'LPAREN':
            raise SyntaxError("Expected parameter list")
        
        self._advance()  # Skip LPAREN
        params = []
        
        while self._current() and self._current().type == 'SYMBOL':
            params.append(self._advance().value)
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for parameters")
        
        self._advance()  # Skip RPAREN
        
        # Parse body
        body_id = self._parse_expr()
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for lambda")
        
        self._advance()  # Skip RPAREN
        
        node = Lambda(
            parameter_names=params,
            parameter_types=[TypeAnnotation("Any") for _ in params],  # Type inference later
            body_id=body_id
        )
        
        return self.graph.add_node(node)
    
    def _parse_let(self) -> str:
        """Parse let expression: (let ((x 1) (y 2)) body)"""
        self._advance()  # Skip 'let'
        
        # Parse bindings
        if not self._current() or self._current().type != 'LPAREN':
            raise SyntaxError("Expected binding list")
        
        self._advance()  # Skip LPAREN
        bindings = []
        
        while self._current() and self._current().type == 'LPAREN':
            self._advance()  # Skip LPAREN
            
            if not self._current() or self._current().type != 'SYMBOL':
                raise SyntaxError("Expected variable name")
            
            name = self._advance().value
            value_id = self._parse_expr()
            
            bindings.append({"name": name, "value_id": value_id})
            
            if not self._current() or self._current().type != 'RPAREN':
                error = syntax_error(
                    "Expected closing parenthesis for binding",
                    location=self._current_location(),
                    expected="')'"
                )
                self.diagnostics.add_diagnostic(error)
                raise SyntaxError("Expected closing parenthesis for binding")
            
            self._advance()  # Skip RPAREN
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for bindings")
        
        self._advance()  # Skip RPAREN
        
        # Parse body
        body_id = self._parse_expr()
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for let")
        
        self._advance()  # Skip RPAREN
        
        node = Let(
            bindings=bindings,
            body_id=body_id
        )
        
        return self.graph.add_node(node)
    
    def _parse_if(self) -> str:
        """Parse if expression: (if cond then else)"""
        self._advance()  # Skip 'if'
        
        condition_id = self._parse_expr()
        then_id = self._parse_expr()
        else_id = self._parse_expr()
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for if")
        
        self._advance()  # Skip RPAREN
        
        node = If(
            condition_id=condition_id,
            then_id=then_id,
            else_id=else_id
        )
        
        return self.graph.add_node(node)
    
    def _parse_sequence(self) -> str:
        """Parse sequence: (do expr1 expr2 ...)"""
        self._advance()  # Skip 'do'
        
        step_ids = []
        while self._current() and self._current().type != 'RPAREN':
            step_id = self._parse_expr()
            if step_id:
                step_ids.append(step_id)
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for sequence")
        
        self._advance()  # Skip RPAREN
        
        node = Sequence(step_ids=step_ids)
        return self.graph.add_node(node)
    
    def _parse_parallel(self) -> str:
        """Parse parallel: (parallel expr1 expr2 ...)"""
        self._advance()  # Skip 'parallel'
        
        branch_ids = []
        while self._current() and self._current().type != 'RPAREN':
            branch_id = self._parse_expr()
            if branch_id:
                branch_ids.append(branch_id)
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for parallel")
        
        self._advance()  # Skip RPAREN
        
        node = Parallel(branch_ids=branch_ids)
        return self.graph.add_node(node)
    
    def _parse_effect(self) -> str:
        """Parse effect: (effect type:operation args...)"""
        self._advance()  # Skip 'effect'
        
        # Parse effect type and operation
        if not self._current() or self._current().type != 'SYMBOL':
            raise SyntaxError("Expected effect type:operation")
        
        effect_spec = self._advance().value
        
        # Parse type:operation format
        if ':' in effect_spec:
            effect_str, operation = effect_spec.split(':', 1)
        else:
            # Legacy format
            effect_str = effect_spec
            if effect_str.startswith(':'):
                effect_str = effect_str[1:]
            operation = "custom"
        
        effect_map = {
            'io': EffectType.IO,
            'state': EffectType.STATE,
            'error': EffectType.ERROR,
            'time': EffectType.TIME,
            'network': EffectType.NETWORK,
            'random': EffectType.RANDOM
        }
        
        effect_type = effect_map.get(effect_str, EffectType.IO)
        
        # Parse arguments
        arg_ids = []
        while self._current() and self._current().type != 'RPAREN':
            arg_id = self._parse_expr()
            if arg_id:
                arg_ids.append(arg_id)
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for effect")
        
        self._advance()  # Skip RPAREN
        
        node = Effect(
            effect_type=effect_type,
            operation=operation,
            argument_ids=arg_ids
        )
        
        return self.graph.add_node(node)
    
    def _parse_uncertain(self) -> str:
        """Parse uncertain: (uncertain (0.7 expr1) (0.3 expr2))"""
        self._advance()  # Skip 'uncertain'
        
        choices = []
        
        while self._current() and self._current().type == 'LPAREN':
            self._advance()  # Skip LPAREN
            
            # Parse probability
            if not self._current() or self._current().type not in ['FLOAT', 'INT']:
                raise SyntaxError("Expected probability")
            
            prob = float(self._advance().value)
            
            # Parse expression
            expr_id = self._parse_expr()
            
            choices.append({"node_id": expr_id, "probability": prob})
            
            if not self._current() or self._current().type != 'RPAREN':
                raise SyntaxError("Expected closing parenthesis for choice")
            
            self._advance()  # Skip RPAREN
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for uncertain")
        
        self._advance()  # Skip RPAREN
        
        node = Uncertainty(choices=choices)
        return self.graph.add_node(node)
    
    def _parse_list_literal(self) -> str:
        """Parse list literal: [1 2 3]"""
        self._advance()  # Skip LBRACKET
        
        elements = []
        while self._current() and self._current().type != 'RBRACKET':
            elem_id = self._parse_expr()
            if elem_id:
                elements.append(elem_id)
        
        if not self._current() or self._current().type != 'RBRACKET':
            raise SyntaxError("Expected closing bracket for list")
        
        self._advance()  # Skip RBRACKET
        
        # Build list using cons operations
        # Start with empty list
        empty_list = Literal(value=[], literal_type='list')
        result_id = self.graph.add_node(empty_list)
        
        # Build list from right to left
        for elem_id in reversed(elements):
            cons_func = Function(name="cons", arity=2, effects={EffectType.PURE})
            cons_id = self.graph.add_node(cons_func)
            
            app = Application(
                function_id=cons_id,
                argument_ids=[elem_id, result_id]
            )
            result_id = self.graph.add_node(app)
        
        return result_id
    
    def _parse_module(self) -> str:
        """Parse module: (module name (export ...) body)"""
        self._advance()  # Skip 'module'
        
        # Parse module name
        if not self._current() or self._current().type != 'SYMBOL':
            raise SyntaxError("Expected module name")
        
        name = self._advance().value
        
        # Parse exports (optional)
        exports = []
        if self._current() and self._current().type == 'LPAREN':
            next_token = self.tokens[self.pos + 1] if self.pos + 1 < len(self.tokens) else None
            if next_token and next_token.type == 'SYMBOL' and next_token.value == 'export':
                self._advance()  # Skip LPAREN
                self._advance()  # Skip 'export'
                
                while self._current() and self._current().type != 'RPAREN':
                    if self._current().type == 'SYMBOL':
                        exports.append(self._advance().value)
                    else:
                        raise SyntaxError("Expected symbol in export list")
                
                if not self._current() or self._current().type != 'RPAREN':
                    raise SyntaxError("Expected closing parenthesis for export")
                self._advance()  # Skip RPAREN
        
        # Parse body
        body_id = self._parse_expr()
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for module")
        
        self._advance()  # Skip RPAREN
        
        from ..core.ast import Module
        node = Module(
            name=name,
            exports=exports,
            body_id=body_id
        )
        
        return self.graph.add_node(node)
    
    def _parse_import(self) -> str:
        """Parse import: (import "module-path" (name1 name2 ...)) or (import "module-path" *)"""
        self._advance()  # Skip 'import'
        
        # Parse module path
        if not self._current() or self._current().type != 'STRING':
            raise SyntaxError("Expected module path string")
        
        module_path = self._advance().value
        
        # Parse import list
        import_list = []
        import_all = False
        
        if self._current() and self._current().type == 'SYMBOL' and self._current().value == '*':
            import_all = True
            self._advance()
        elif self._current() and self._current().type == 'LPAREN':
            self._advance()  # Skip LPAREN
            
            while self._current() and self._current().type != 'RPAREN':
                if self._current().type == 'SYMBOL':
                    name = self._advance().value
                    
                    # Check for 'as' renaming
                    if (self._current() and self._current().type == 'SYMBOL' and 
                        self._current().value == 'as'):
                        self._advance()  # Skip 'as'
                        if not self._current() or self._current().type != 'SYMBOL':
                            raise SyntaxError("Expected name after 'as'")
                        as_name = self._advance().value
                        import_list.append({"name": name, "as": as_name})
                    else:
                        import_list.append({"name": name})
                else:
                    raise SyntaxError("Expected symbol in import list")
            
            if not self._current() or self._current().type != 'RPAREN':
                raise SyntaxError("Expected closing parenthesis for import list")
            self._advance()  # Skip RPAREN
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for import")
        
        self._advance()  # Skip RPAREN
        
        from ..core.ast import Import
        node = Import(
            module_path=module_path,
            import_list=import_list,
            import_all=import_all
        )
        
        return self.graph.add_node(node)
    
    def _parse_export(self) -> str:
        """Parse export: (export name1 name2 ...)"""
        self._advance()  # Skip 'export'
        
        export_list = []
        
        while self._current() and self._current().type != 'RPAREN':
            if self._current().type == 'SYMBOL':
                name = self._advance().value
                
                # Check for 'as' renaming
                if (self._current() and self._current().type == 'SYMBOL' and 
                    self._current().value == 'as'):
                    self._advance()  # Skip 'as'
                    if not self._current() or self._current().type != 'SYMBOL':
                        raise SyntaxError("Expected name after 'as'")
                    as_name = self._advance().value
                    export_list.append({"name": name, "as": as_name})
                else:
                    export_list.append({"name": name})
            else:
                raise SyntaxError("Expected symbol in export list")
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for export")
        
        self._advance()  # Skip RPAREN
        
        from ..core.ast import Export
        node = Export(export_list=export_list)
        
        return self.graph.add_node(node)
    
    def _parse_match(self) -> str:
        """Parse match expression: (match expr (pattern1 body1) (pattern2 body2) ...)"""
        self._advance()  # Skip 'match'
        
        # Parse expression to match against
        expr_id = self._parse_expr()
        
        branches = []
        
        # Parse pattern branches
        while self._current() and self._current().type != 'RPAREN':
            if self._current().type != 'LPAREN':
                raise SyntaxError("Expected pattern branch in match")
            
            self._advance()  # Skip LPAREN
            
            # Parse pattern
            pattern_id = self._parse_pattern()
            
            # Parse body
            body_id = self._parse_expr()
            
            if not self._current() or self._current().type != 'RPAREN':
                raise SyntaxError("Expected closing parenthesis for pattern branch")
            
            self._advance()  # Skip RPAREN
            
            branches.append({"pattern_id": pattern_id, "body_id": body_id})
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for match")
        
        self._advance()  # Skip RPAREN
        
        from ..core.ast import Match
        node = Match(expr_id=expr_id, branches=branches)
        
        return self.graph.add_node(node)
    
    def _parse_pattern(self) -> str:
        """Parse a pattern in pattern matching"""
        if not self._current():
            raise SyntaxError("Unexpected end of input in pattern")
        
        token = self._current()
        
        # Wildcard pattern
        if token.type == 'SYMBOL' and token.value == '_':
            self._advance()
            from ..core.ast import PatternWildcard
            node = PatternWildcard()
            return self.graph.add_node(node)
        
        # Variable pattern
        elif token.type == 'SYMBOL' and token.value[0].islower():
            name = self._advance().value
            from ..core.ast import PatternVar
            node = PatternVar(name=name)
            return self.graph.add_node(node)
        
        # Constructor pattern
        elif token.type == 'SYMBOL' and token.value[0].isupper():
            constructor = self._advance().value
            sub_patterns = []
            
            # Parse sub-patterns if present
            while (self._current() and 
                   self._current().type != 'RPAREN' and
                   self._current().type != 'LPAREN'):
                sub_patterns.append(self._parse_pattern())
            
            from ..core.ast import PatternConstructor
            node = PatternConstructor(constructor=constructor, sub_patterns=sub_patterns)
            return self.graph.add_node(node)
        
        # List pattern
        elif token.type == 'LBRACKET':
            return self._parse_list_pattern()
        
        # Literal pattern
        elif token.type in ['INT', 'FLOAT', 'STRING', 'BOOLEAN', 'NIL']:
            value = self._parse_literal()
            from ..core.ast import PatternLiteral, Literal
            # Extract value from literal node
            literal_node = self.graph.nodes[value]
            if isinstance(literal_node, Literal):
                node = PatternLiteral(value=literal_node.value)
                return self.graph.add_node(node)
            else:
                raise SyntaxError("Invalid literal pattern")
        
        # Parenthesized pattern
        elif token.type == 'LPAREN':
            self._advance()  # Skip LPAREN
            
            # Check for constructor pattern with arguments
            if self._current() and self._current().type == 'SYMBOL' and self._current().value[0].isupper():
                constructor = self._advance().value
                sub_patterns = []
                
                while self._current() and self._current().type != 'RPAREN':
                    sub_patterns.append(self._parse_pattern())
                
                if not self._current() or self._current().type != 'RPAREN':
                    raise SyntaxError("Expected closing parenthesis in constructor pattern")
                
                self._advance()  # Skip RPAREN
                
                from ..core.ast import PatternConstructor
                node = PatternConstructor(constructor=constructor, sub_patterns=sub_patterns)
                return self.graph.add_node(node)
            else:
                # Regular parenthesized pattern
                pattern_id = self._parse_pattern()
                
                if not self._current() or self._current().type != 'RPAREN':
                    raise SyntaxError("Expected closing parenthesis in pattern")
                
                self._advance()  # Skip RPAREN
                return pattern_id
        
        else:
            raise SyntaxError(f"Invalid pattern: {token}")
    
    def _parse_list_pattern(self) -> str:
        """Parse list pattern: [x, y, ...rest]"""
        self._advance()  # Skip LBRACKET
        
        elements = []
        rest_pattern = None
        
        while self._current() and self._current().type != 'RBRACKET':
            # Check for rest pattern
            if (self._current().type == 'SYMBOL' and 
                self._current().value == '...' and
                self._peek() and self._peek().type == 'SYMBOL'):
                self._advance()  # Skip '...'
                rest_pattern = self._parse_pattern()
                break
            
            # Regular element
            elements.append(self._parse_pattern())
            
            # Handle comma separator
            if self._current() and self._current().type == 'COMMA':
                self._advance()
        
        if not self._current() or self._current().type != 'RBRACKET':
            raise SyntaxError("Expected closing bracket in list pattern")
        
        self._advance()  # Skip RBRACKET
        
        from ..core.ast import PatternList
        node = PatternList(elements=elements, rest_pattern=rest_pattern)
        
        return self.graph.add_node(node)
    
    def _parse_contract(self) -> str:
        """Parse contract specification: (spec:contract function-name :requires [...] :ensures [...] :complexity "O(n)")"""
        self._advance()  # Skip 'spec:contract'
        
        # Parse function name
        if not self._current() or self._current().type != 'SYMBOL':
            raise SyntaxError("Expected function name after spec:contract")
        
        function_name = self._current().value
        self._advance()
        
        # Parse contract clauses
        preconditions = []
        postconditions = []
        invariants = []
        complexity = None
        pure = True
        
        while self._current() and self._current().type != 'RPAREN':
            if self._current().type != 'SYMBOL' or not self._current().value.startswith(':'):
                raise SyntaxError("Expected keyword in contract specification")
            
            keyword = self._current().value
            self._advance()
            
            if keyword == ':requires' or keyword == ':pre':
                # Parse preconditions list
                if not self._current() or self._current().type != 'LBRACKET':
                    raise SyntaxError("Expected '[' after :requires")
                self._advance()  # Skip LBRACKET
                
                while self._current() and self._current().type != 'RBRACKET':
                    condition_id = self._parse_expr()
                    preconditions.append(condition_id)
                
                if not self._current() or self._current().type != 'RBRACKET':
                    raise SyntaxError("Expected ']' to close :requires")
                self._advance()  # Skip RBRACKET
                
            elif keyword == ':ensures' or keyword == ':post':
                # Parse postconditions list
                if not self._current() or self._current().type != 'LBRACKET':
                    raise SyntaxError("Expected '[' after :ensures")
                self._advance()  # Skip LBRACKET
                
                while self._current() and self._current().type != 'RBRACKET':
                    condition_id = self._parse_expr()
                    postconditions.append(condition_id)
                
                if not self._current() or self._current().type != 'RBRACKET':
                    raise SyntaxError("Expected ']' to close :ensures")
                self._advance()  # Skip RBRACKET
                
            elif keyword == ':invariant':
                # Parse invariants list
                if not self._current() or self._current().type != 'LBRACKET':
                    raise SyntaxError("Expected '[' after :invariant")
                self._advance()  # Skip LBRACKET
                
                while self._current() and self._current().type != 'RBRACKET':
                    condition_id = self._parse_expr()
                    invariants.append(condition_id)
                
                if not self._current() or self._current().type != 'RBRACKET':
                    raise SyntaxError("Expected ']' to close :invariant")
                self._advance()  # Skip RBRACKET
                
            elif keyword == ':complexity':
                # Parse complexity string
                if not self._current() or self._current().type != 'STRING':
                    raise SyntaxError("Expected string after :complexity")
                complexity = self._current().value
                self._advance()
                
            elif keyword == ':pure':
                # Parse pure flag
                if not self._current():
                    raise SyntaxError("Expected value after :pure")
                if self._current().type == 'SYMBOL':
                    pure = self._current().value == 'true'
                elif self._current().type == 'BOOL':
                    pure = self._current().value
                else:
                    raise SyntaxError("Expected boolean after :pure")
                self._advance()
            
            else:
                raise SyntaxError(f"Unknown contract keyword: {keyword}")
        
        if not self._current() or self._current().type != 'RPAREN':
            raise SyntaxError("Expected closing parenthesis for spec:contract")
        
        self._advance()  # Skip RPAREN
        
        node = Contract(
            function_name=function_name,
            preconditions=preconditions,
            postconditions=postconditions,
            invariants=invariants,
            complexity=complexity,
            pure=pure
        )
        
        return self.graph.add_node(node)


def parse(source: str) -> Graph:
    """Parse ClaudeLang source code into an AST graph"""
    lexer = Lexer(source)
    tokens = lexer.tokenize()
    parser = Parser(tokens)
    return parser.parse()


def parse_file(filename: str) -> Graph:
    """Parse a ClaudeLang file"""
    with open(filename, 'r') as f:
        return parse(f.read())