"""
Tests for ClaudeLang Language Server Protocol implementation
"""

import unittest
import json
from unittest.mock import Mock, patch, MagicMock
from io import StringIO

from src.lsp.server import ClaudeLangLSPServer, Position, Range, Diagnostic
from src.lsp.capabilities import get_server_capabilities


class TestLSPServer(unittest.TestCase):
    """Test LSP server functionality"""
    
    def setUp(self):
        self.server = ClaudeLangLSPServer()
    
    def test_initialization(self):
        """Test server initialization"""
        request = {
            'id': 1,
            'method': 'initialize',
            'params': {
                'rootUri': 'file:///test/project',
                'capabilities': {}
            }
        }
        
        response = self.server._handle_request(request)
        
        self.assertIsNotNone(response)
        self.assertEqual(response['id'], 1)
        self.assertIn('result', response)
        self.assertIn('capabilities', response['result'])
        self.assertIn('serverInfo', response['result'])
        self.assertEqual(self.server.root_uri, 'file:///test/project')
    
    def test_document_open(self):
        """Test document open handling"""
        self.server._analyze_document = Mock()
        
        params = {
            'textDocument': {
                'uri': 'file:///test.cl',
                'text': '(+ 1 2)'
            }
        }
        
        self.server._did_open(params)
        
        self.assertIn('file:///test.cl', self.server.documents)
        self.assertEqual(self.server.documents['file:///test.cl'], '(+ 1 2)')
        self.server._analyze_document.assert_called_once_with('file:///test.cl', '(+ 1 2)')
    
    def test_document_change(self):
        """Test document change handling"""
        self.server.documents['file:///test.cl'] = '(+ 1 2)'
        self.server._analyze_document = Mock()
        
        params = {
            'textDocument': {'uri': 'file:///test.cl'},
            'contentChanges': [{'text': '(+ 1 2 3)'}]
        }
        
        self.server._did_change(params)
        
        self.assertEqual(self.server.documents['file:///test.cl'], '(+ 1 2 3)')
        self.server._analyze_document.assert_called_once_with('file:///test.cl', '(+ 1 2 3)')
    
    def test_document_close(self):
        """Test document close handling"""
        self.server.documents['file:///test.cl'] = '(+ 1 2)'
        self.server.asts['file:///test.cl'] = Mock()
        
        params = {
            'textDocument': {'uri': 'file:///test.cl'}
        }
        
        self.server._did_close(params)
        
        self.assertNotIn('file:///test.cl', self.server.documents)
        self.assertNotIn('file:///test.cl', self.server.asts)
    
    def test_analyze_document_success(self):
        """Test successful document analysis"""
        self.server._send_notification = Mock()
        
        self.server._analyze_document('file:///test.cl', '(+ 1 2)')
        
        # Should send empty diagnostics for valid code
        self.server._send_notification.assert_called_once()
        call_args = self.server._send_notification.call_args
        self.assertEqual(call_args[0][0], 'textDocument/publishDiagnostics')
        self.assertEqual(call_args[0][1]['uri'], 'file:///test.cl')
        self.assertEqual(call_args[0][1]['diagnostics'], [])
    
    def test_analyze_document_syntax_error(self):
        """Test document analysis with syntax error"""
        self.server._send_notification = Mock()
        
        self.server._analyze_document('file:///test.cl', '(+ 1')
        
        # Should send error diagnostic
        self.server._send_notification.assert_called_once()
        call_args = self.server._send_notification.call_args
        self.assertEqual(call_args[0][0], 'textDocument/publishDiagnostics')
        diagnostics = call_args[0][1]['diagnostics']
        self.assertEqual(len(diagnostics), 1)
        self.assertEqual(diagnostics[0]['severity'], 1)  # Error
    
    def test_completion(self):
        """Test completion request"""
        request = {
            'id': 2,
            'method': 'textDocument/completion',
            'params': {
                'textDocument': {'uri': 'file:///test.cl'},
                'position': {'line': 0, 'character': 1}
            }
        }
        
        response = self.server._handle_request(request)
        
        self.assertIsNotNone(response)
        self.assertEqual(response['id'], 2)
        self.assertIn('result', response)
        items = response['result']
        self.assertIsInstance(items, list)
        self.assertTrue(len(items) > 0)
        
        # Check for some expected completions
        labels = [item['label'] for item in items]
        self.assertIn('+', labels)
        self.assertIn('lambda', labels)
        self.assertIn('if', labels)
    
    def test_hover(self):
        """Test hover request"""
        self.server.documents['file:///test.cl'] = '(+ 1 2)'
        
        request = {
            'id': 3,
            'method': 'textDocument/hover',
            'params': {
                'textDocument': {'uri': 'file:///test.cl'},
                'position': {'line': 0, 'character': 1}
            }
        }
        
        response = self.server._handle_request(request)
        
        self.assertIsNotNone(response)
        self.assertEqual(response['id'], 3)
    
    def test_document_symbols(self):
        """Test document symbols request"""
        from src.parser.sexpr_parser import parse
        
        self.server.documents['file:///test.cl'] = '(let ((x 5)) x)'
        self.server.asts['file:///test.cl'] = parse('(let ((x 5)) x)')
        
        request = {
            'id': 4,
            'method': 'textDocument/documentSymbol',
            'params': {
                'textDocument': {'uri': 'file:///test.cl'}
            }
        }
        
        response = self.server._handle_request(request)
        
        self.assertIsNotNone(response)
        self.assertEqual(response['id'], 4)
        self.assertIn('result', response)
        symbols = response['result']
        self.assertIsInstance(symbols, list)
    
    def test_diagnostic_to_dict(self):
        """Test diagnostic conversion"""
        diag = Diagnostic(
            range=Range(
                start=Position(0, 0),
                end=Position(0, 5)
            ),
            severity=1,
            message="Test error"
        )
        
        result = self.server._diagnostic_to_dict(diag)
        
        self.assertEqual(result['range']['start']['line'], 0)
        self.assertEqual(result['range']['start']['character'], 0)
        self.assertEqual(result['range']['end']['line'], 0)
        self.assertEqual(result['range']['end']['character'], 5)
        self.assertEqual(result['severity'], 1)
        self.assertEqual(result['message'], "Test error")
        self.assertEqual(result['source'], "claudelang")
    
    def test_server_capabilities(self):
        """Test server capabilities"""
        caps = get_server_capabilities()
        
        # Check essential capabilities
        self.assertTrue(caps['textDocumentSync']['openClose'])
        self.assertIn('completionProvider', caps)
        self.assertTrue(caps['hoverProvider'])
        self.assertTrue(caps['definitionProvider'])
        self.assertTrue(caps['documentSymbolProvider'])
        self.assertTrue(caps['documentFormattingProvider'])
    
    @patch('sys.stdin')
    @patch('sys.stdout')
    def test_server_communication(self, mock_stdout, mock_stdin):
        """Test LSP communication protocol"""
        # Simulate initialize request
        request = {
            'jsonrpc': '2.0',
            'id': 1,
            'method': 'initialize',
            'params': {'rootUri': 'file:///test'}
        }
        
        content = json.dumps(request)
        headers = f'Content-Length: {len(content)}\r\n\r\n'
        
        # Mock stdin to return headers and content
        mock_stdin.readline.side_effect = [
            'Content-Length: {}\r\n'.format(len(content)),
            '\r\n'
        ]
        mock_stdin.read.return_value = content
        
        # Capture stdout
        output_buffer = StringIO()
        mock_stdout.write = output_buffer.write
        mock_stdout.flush = Mock()
        
        # Send response
        response = {'id': 1, 'result': {'capabilities': {}}}
        self.server._send_response(response)
        
        # Check output format
        output = output_buffer.getvalue()
        self.assertTrue(output.startswith('Content-Length: '))
        self.assertIn('\r\n\r\n', output)


class TestLSPIntegration(unittest.TestCase):
    """Integration tests for LSP server"""
    
    def setUp(self):
        self.server = ClaudeLangLSPServer()
        self.server._send_notification = Mock()
    
    def test_simple_expression_analysis(self):
        """Test analysis of simple expressions"""
        code = '(+ 1 2)'
        
        self.server._analyze_document('file:///test.cl', code)
        
        # Should have parsed successfully
        self.assertIn('file:///test.cl', self.server.asts)
        
        # Check diagnostics sent
        self.server._send_notification.assert_called_once()
        call_args = self.server._send_notification.call_args
        diagnostics = call_args[0][1]['diagnostics']
        self.assertEqual(len(diagnostics), 0)  # No errors
    
    def test_adt_analysis(self):
        """Test analysis of ADT definitions"""
        code = '''
        (data Option a
          (None)
          (Some a))
        
        (match (Some 42)
          ((None) 0)
          ((Some x) x))
        '''
        
        self.server._analyze_document('file:///option.cl', code)
        
        # Should have parsed successfully
        self.assertIn('file:///option.cl', self.server.asts)
        
        # Check diagnostics sent
        self.server._send_notification.assert_called_once()
        call_args = self.server._send_notification.call_args
        diagnostics = call_args[0][1]['diagnostics']
        self.assertEqual(len(diagnostics), 0)  # No errors
    
    def test_contract_analysis(self):
        """Test analysis of contracts"""
        code = '''
        (spec:contract add
          :requires [(number? x) (number? y)]
          :ensures [(number? result)])
        '''
        
        self.server._analyze_document('file:///contract.cl', code)
        
        # Should have parsed successfully
        self.assertIn('file:///contract.cl', self.server.asts)


if __name__ == '__main__':
    unittest.main()