"""
ClaudeLang Language Server Protocol implementation

This module implements an LSP server for ClaudeLang, providing:
- Syntax checking
- Auto-completion
- Hover information
- Go to definition
- Find references
- Document symbols
- Diagnostics
"""

import json
import sys
import traceback
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
import logging

from ..parser.sexpr_parser import parse, Lexer
from ..interpreter.interpreter import Interpreter
from ..core.ast import *
from ..errors.exceptions import ClaudeLangError, SyntaxError

# Set up logging
logging.basicConfig(level=logging.INFO, filename='claudelang-lsp.log')
logger = logging.getLogger(__name__)


@dataclass
class Position:
    """LSP position representation"""
    line: int
    character: int


@dataclass
class Range:
    """LSP range representation"""
    start: Position
    end: Position


@dataclass
class Location:
    """LSP location representation"""
    uri: str
    range: Range


@dataclass
class Diagnostic:
    """LSP diagnostic representation"""
    range: Range
    severity: int  # 1=Error, 2=Warning, 3=Info, 4=Hint
    code: Optional[str] = None
    source: str = "claudelang"
    message: str = ""
    relatedInformation: List[Any] = field(default_factory=list)


class ClaudeLangLSPServer:
    """LSP server for ClaudeLang"""
    
    def __init__(self):
        self.documents: Dict[str, str] = {}
        self.asts: Dict[str, Graph] = {}
        self.interpreter = Interpreter()
        self.initialized = False
        self.root_uri = None
        self.client_capabilities = {}
    
    def run(self):
        """Run the LSP server"""
        logger.info("Starting ClaudeLang LSP server")
        
        while True:
            try:
                # Read header
                headers = {}
                while True:
                    line = sys.stdin.readline().strip()
                    if not line:
                        break
                    key, value = line.split(': ', 1)
                    headers[key] = value
                
                # Read content
                content_length = int(headers.get('Content-Length', 0))
                content = sys.stdin.read(content_length)
                
                # Parse request
                request = json.loads(content)
                logger.info(f"Received request: {request.get('method', 'unknown')}")
                
                # Handle request
                response = self._handle_request(request)
                
                # Send response
                if response is not None:
                    self._send_response(response)
                    
            except Exception as e:
                logger.error(f"Error in main loop: {e}")
                logger.error(traceback.format_exc())
    
    def _handle_request(self, request: Dict[str, Any]) -> Optional[Dict[str, Any]]:
        """Handle an LSP request"""
        method = request.get('method')
        params = request.get('params', {})
        
        # Initialize
        if method == 'initialize':
            return self._initialize(request['id'], params)
        
        # Lifecycle
        elif method == 'initialized':
            self.initialized = True
            return None
        
        elif method == 'shutdown':
            return {'id': request['id'], 'result': None}
        
        elif method == 'exit':
            sys.exit(0)
        
        # Document synchronization
        elif method == 'textDocument/didOpen':
            self._did_open(params)
            return None
        
        elif method == 'textDocument/didChange':
            self._did_change(params)
            return None
        
        elif method == 'textDocument/didClose':
            self._did_close(params)
            return None
        
        # Language features
        elif method == 'textDocument/completion':
            return self._completion(request['id'], params)
        
        elif method == 'textDocument/hover':
            return self._hover(request['id'], params)
        
        elif method == 'textDocument/definition':
            return self._definition(request['id'], params)
        
        elif method == 'textDocument/references':
            return self._references(request['id'], params)
        
        elif method == 'textDocument/documentSymbol':
            return self._document_symbols(request['id'], params)
        
        elif method == 'textDocument/formatting':
            return self._formatting(request['id'], params)
        
        else:
            logger.warning(f"Unknown method: {method}")
            return None
    
    def _send_response(self, response: Dict[str, Any]):
        """Send an LSP response"""
        content = json.dumps(response)
        content_bytes = content.encode('utf-8')
        
        sys.stdout.write(f'Content-Length: {len(content_bytes)}\r\n\r\n')
        sys.stdout.write(content)
        sys.stdout.flush()
    
    def _send_notification(self, method: str, params: Any):
        """Send an LSP notification"""
        notification = {
            'jsonrpc': '2.0',
            'method': method,
            'params': params
        }
        self._send_response(notification)
    
    def _initialize(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle initialize request"""
        self.root_uri = params.get('rootUri')
        self.client_capabilities = params.get('capabilities', {})
        
        # Import server capabilities
        from .capabilities import get_server_capabilities
        
        return {
            'id': request_id,
            'result': {
                'capabilities': get_server_capabilities(),
                'serverInfo': {
                    'name': 'ClaudeLang Language Server',
                    'version': '0.1.0'
                }
            }
        }
    
    def _did_open(self, params: Dict[str, Any]):
        """Handle document open"""
        doc = params['textDocument']
        uri = doc['uri']
        text = doc['text']
        
        self.documents[uri] = text
        self._analyze_document(uri, text)
    
    def _did_change(self, params: Dict[str, Any]):
        """Handle document change"""
        uri = params['textDocument']['uri']
        changes = params['contentChanges']
        
        # We only support full document sync for now
        if changes:
            self.documents[uri] = changes[0]['text']
            self._analyze_document(uri, self.documents[uri])
    
    def _did_close(self, params: Dict[str, Any]):
        """Handle document close"""
        uri = params['textDocument']['uri']
        self.documents.pop(uri, None)
        self.asts.pop(uri, None)
    
    def _analyze_document(self, uri: str, text: str):
        """Analyze a document and send diagnostics"""
        diagnostics = []
        
        try:
            # Parse the document
            ast = parse(text)
            self.asts[uri] = ast
            
            # Type checking would go here if we had type inference implemented
            # For now, we just parse
            
            # Check for basic semantic errors
            try:
                # Could validate the AST here
                pass
            except ClaudeLangError as e:
                # Semantic errors are warnings
                if hasattr(e, 'location'):
                    diagnostics.append(Diagnostic(
                        range=Range(
                            start=Position(e.location.line - 1, e.location.column - 1),
                            end=Position(e.location.line - 1, e.location.column)
                        ),
                        severity=2,  # Warning
                        message=str(e)
                    ))
            
        except SyntaxError as e:
            # Syntax errors are errors
            line = getattr(e, 'line', 1) - 1
            col = getattr(e, 'column', 0) - 1
            
            diagnostics.append(Diagnostic(
                range=Range(
                    start=Position(line, col),
                    end=Position(line, col + 1)
                ),
                severity=1,  # Error
                message=str(e)
            ))
            
        except Exception as e:
            # Other errors
            diagnostics.append(Diagnostic(
                range=Range(
                    start=Position(0, 0),
                    end=Position(0, 0)
                ),
                severity=1,  # Error
                message=f"Analysis error: {str(e)}"
            ))
        
        # Send diagnostics
        self._send_notification('textDocument/publishDiagnostics', {
            'uri': uri,
            'diagnostics': [self._diagnostic_to_dict(d) for d in diagnostics]
        })
    
    def _diagnostic_to_dict(self, diag: Diagnostic) -> Dict[str, Any]:
        """Convert diagnostic to dictionary"""
        return {
            'range': {
                'start': {'line': diag.range.start.line, 'character': diag.range.start.character},
                'end': {'line': diag.range.end.line, 'character': diag.range.end.character}
            },
            'severity': diag.severity,
            'source': diag.source,
            'message': diag.message
        }
    
    def _get_position_info(self, uri: str, line: int, character: int) -> Optional[Tuple[Graph, str]]:
        """Get AST and token at position"""
        if uri not in self.asts or uri not in self.documents:
            return None
        
        ast = self.asts[uri]
        text = self.documents[uri]
        
        # Simple approach: find token at position
        # In a real implementation, we'd track source locations in AST
        lines = text.split('\n')
        if line >= len(lines):
            return None
        
        line_text = lines[line]
        if character >= len(line_text):
            return None
        
        # Find word at position
        start = character
        while start > 0 and line_text[start - 1].isalnum():
            start -= 1
        
        end = character
        while end < len(line_text) and line_text[end].isalnum():
            end += 1
        
        word = line_text[start:end]
        return (ast, word)
    
    def _completion(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle completion request"""
        uri = params['textDocument']['uri']
        position = params['position']
        
        items = []
        
        # Add built-in functions
        from ..core.primitives import PRIMITIVES
        for name, _ in PRIMITIVES.registry.items():
            items.append({
                'label': name,
                'kind': 3,  # Function
                'detail': 'Built-in function',
                'insertText': name
            })
        
        # Add keywords
        keywords = ['lambda', 'let', 'if', 'match', 'data', 'module', 'import', 
                   'export', 'effect', 'handler', 'spec:contract']
        for kw in keywords:
            items.append({
                'label': kw,
                'kind': 14,  # Keyword
                'insertText': kw
            })
        
        # Add effects
        effects = ['io:print', 'io:read', 'state:get', 'state:set', 'error:raise']
        for eff in effects:
            items.append({
                'label': eff,
                'kind': 3,  # Function
                'detail': 'Effect operation',
                'insertText': eff
            })
        
        return {
            'id': request_id,
            'result': items
        }
    
    def _hover(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle hover request"""
        uri = params['textDocument']['uri']
        position = params['position']
        
        info = self._get_position_info(uri, position['line'], position['character'])
        if not info:
            return {'id': request_id, 'result': None}
        
        ast, word = info
        
        # Check if it's a primitive
        from ..core.primitives import PRIMITIVES
        if word in PRIMITIVES.registry:
            return {
                'id': request_id,
                'result': {
                    'contents': {
                        'kind': 'markdown',
                        'value': f'**{word}**\n\nBuilt-in function'
                    }
                }
            }
        
        # Check for other known items
        hover_info = {
            'lambda': '**lambda**\n\nCreates an anonymous function\n\n`(lambda (params...) body)`',
            'let': '**let**\n\nCreates local bindings\n\n`(let ((var value)...) body)`',
            'if': '**if**\n\nConditional expression\n\n`(if condition then-expr else-expr)`',
            'match': '**match**\n\nPattern matching\n\n`(match expr (pattern result)...)`',
            'data': '**data**\n\nDefines an algebraic data type\n\n`(data TypeName params (Constructor args)...)`'
        }
        
        if word in hover_info:
            return {
                'id': request_id,
                'result': {
                    'contents': {
                        'kind': 'markdown',
                        'value': hover_info[word]
                    }
                }
            }
        
        return {'id': request_id, 'result': None}
    
    def _definition(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle go to definition request"""
        # This would require tracking definition locations in the AST
        # For now, return no results
        return {'id': request_id, 'result': None}
    
    def _references(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle find references request"""
        # This would require tracking reference locations in the AST
        # For now, return empty results
        return {'id': request_id, 'result': []}
    
    def _document_symbols(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle document symbols request"""
        uri = params['textDocument']['uri']
        
        if uri not in self.asts:
            return {'id': request_id, 'result': []}
        
        ast = self.asts[uri]
        symbols = []
        
        # Extract symbols from AST
        # This is simplified - real implementation would track locations
        for node_id, node in ast.nodes.items():
            symbol = None
            
            if isinstance(node, Let):
                # Let bindings create symbols
                for binding in node.bindings:
                    symbols.append({
                        'name': binding[0],
                        'kind': 13,  # Variable
                        'range': {
                            'start': {'line': 0, 'character': 0},
                            'end': {'line': 0, 'character': 0}
                        },
                        'selectionRange': {
                            'start': {'line': 0, 'character': 0},
                            'end': {'line': 0, 'character': 0}
                        }
                    })
            
            elif isinstance(node, Module):
                # Module definitions
                symbols.append({
                    'name': node.name,
                    'kind': 2,  # Module
                    'range': {
                        'start': {'line': 0, 'character': 0},
                        'end': {'line': 0, 'character': 0}
                    },
                    'selectionRange': {
                        'start': {'line': 0, 'character': 0},
                        'end': {'line': 0, 'character': 0}
                    }
                })
            
            elif isinstance(node, DataDeclaration):
                # ADT definitions
                symbols.append({
                    'name': node.type_name,
                    'kind': 5,  # Class (closest to ADT)
                    'range': {
                        'start': {'line': 0, 'character': 0},
                        'end': {'line': 0, 'character': 0}
                    },
                    'selectionRange': {
                        'start': {'line': 0, 'character': 0},
                        'end': {'line': 0, 'character': 0}
                    }
                })
        
        return {'id': request_id, 'result': symbols}
    
    def _formatting(self, request_id: int, params: Dict[str, Any]) -> Dict[str, Any]:
        """Handle formatting request"""
        uri = params['textDocument']['uri']
        
        if uri not in self.documents:
            return {'id': request_id, 'result': []}
        
        # For now, we don't have a formatter
        # Return no edits
        return {'id': request_id, 'result': []}


def main():
    """Main entry point for LSP server"""
    server = ClaudeLangLSPServer()
    server.run()


if __name__ == '__main__':
    main()