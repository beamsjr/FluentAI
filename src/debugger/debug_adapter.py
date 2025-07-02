"""
Debug Adapter Protocol implementation for ClaudeLang

Implements DAP to integrate with VS Code and other editors.
"""

import json
import sys
import threading
from typing import Dict, Any, Optional, List, Callable
from pathlib import Path

from .debugger import Debugger, Breakpoint
from .debug_session import DebugSession


class DebugAdapter:
    """Debug Adapter Protocol implementation"""
    
    def __init__(self):
        self.session: Optional[DebugSession] = None
        self.debugger: Optional[Debugger] = None
        self.sequence = 0
        self.handlers: Dict[str, Callable] = {}
        self._running = False
        self._input_thread: Optional[threading.Thread] = None
        
        # Client capabilities
        self.client_capabilities: Dict[str, Any] = {}
        
        # Register handlers
        self._register_handlers()
    
    def _register_handlers(self) -> None:
        """Register DAP request handlers"""
        self.handlers = {
            'initialize': self._handle_initialize,
            'launch': self._handle_launch,
            'attach': self._handle_attach,
            'disconnect': self._handle_disconnect,
            'terminate': self._handle_terminate,
            'restart': self._handle_restart,
            'setBreakpoints': self._handle_set_breakpoints,
            'setFunctionBreakpoints': self._handle_set_function_breakpoints,
            'setExceptionBreakpoints': self._handle_set_exception_breakpoints,
            'configurationDone': self._handle_configuration_done,
            'continue': self._handle_continue,
            'next': self._handle_next,
            'stepIn': self._handle_step_in,
            'stepOut': self._handle_step_out,
            'pause': self._handle_pause,
            'stackTrace': self._handle_stack_trace,
            'scopes': self._handle_scopes,
            'variables': self._handle_variables,
            'source': self._handle_source,
            'threads': self._handle_threads,
            'evaluate': self._handle_evaluate,
            'completions': self._handle_completions,
            'loadedSources': self._handle_loaded_sources,
        }
    
    def start(self) -> None:
        """Start the debug adapter"""
        self._running = True
        
        # Start input thread
        self._input_thread = threading.Thread(target=self._read_input)
        self._input_thread.daemon = True
        self._input_thread.start()
        
        # Send initialized event
        self._send_event('initialized', {})
    
    def stop(self) -> None:
        """Stop the debug adapter"""
        self._running = False
    
    def _read_input(self) -> None:
        """Read DAP messages from stdin"""
        while self._running:
            try:
                # Read header
                headers = {}
                while True:
                    line = sys.stdin.readline()
                    if not line:
                        return  # EOF
                    line = line.strip()
                    if not line:
                        break  # End of headers
                    key, value = line.split(':', 1)
                    headers[key.strip()] = value.strip()
                
                # Read content
                content_length = int(headers.get('Content-Length', 0))
                if content_length > 0:
                    content = sys.stdin.read(content_length)
                    message = json.loads(content)
                    self._handle_message(message)
                    
            except Exception as e:
                # Log error but continue
                self._send_event('output', {
                    'category': 'stderr',
                    'output': f'Debug adapter error: {e}\n'
                })
    
    def _handle_message(self, message: Dict[str, Any]) -> None:
        """Handle a DAP message"""
        msg_type = message.get('type')
        
        if msg_type == 'request':
            command = message.get('command')
            handler = self.handlers.get(command)
            
            if handler:
                try:
                    response = handler(message.get('arguments', {}))
                    self._send_response(message['seq'], message['command'], True, response)
                except Exception as e:
                    self._send_response(message['seq'], message['command'], False, 
                                      message=str(e))
            else:
                self._send_response(message['seq'], message['command'], False,
                                  message=f'Unknown command: {command}')
    
    def _send_response(self, request_seq: int, command: str, success: bool,
                      body: Any = None, message: Optional[str] = None) -> None:
        """Send a DAP response"""
        self.sequence += 1
        response = {
            'seq': self.sequence,
            'type': 'response',
            'request_seq': request_seq,
            'success': success,
            'command': command
        }
        
        if body is not None:
            response['body'] = body
        if message:
            response['message'] = message
            
        self._send_message(response)
    
    def _send_event(self, event: str, body: Dict[str, Any]) -> None:
        """Send a DAP event"""
        self.sequence += 1
        message = {
            'seq': self.sequence,
            'type': 'event',
            'event': event,
            'body': body
        }
        self._send_message(message)
    
    def _send_message(self, message: Dict[str, Any]) -> None:
        """Send a DAP message to stdout"""
        content = json.dumps(message)
        content_bytes = content.encode('utf-8')
        
        headers = f'Content-Length: {len(content_bytes)}\r\n\r\n'
        sys.stdout.buffer.write(headers.encode('utf-8'))
        sys.stdout.buffer.write(content_bytes)
        sys.stdout.buffer.flush()
    
    # Request handlers
    
    def _handle_initialize(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle initialize request"""
        self.client_capabilities = args
        
        # Create debugger and session
        self.debugger = Debugger()
        self.session = DebugSession(self.debugger)
        
        # Register event forwarding
        self.debugger.register_event_handler('paused', self._on_paused)
        self.debugger.register_event_handler('continued', self._on_continued)
        self.debugger.register_event_handler('output', self._on_output)
        self.debugger.register_event_handler('terminated', self._on_terminated)
        
        # Return capabilities
        return {
            'supportsConfigurationDoneRequest': True,
            'supportsFunctionBreakpoints': True,
            'supportsConditionalBreakpoints': True,
            'supportsHitConditionalBreakpoints': True,
            'supportsEvaluateForHovers': True,
            'supportsStepBack': False,
            'supportsSetVariable': False,
            'supportsRestartFrame': False,
            'supportsGotoTargetsRequest': False,
            'supportsStepInTargetsRequest': False,
            'supportsCompletionsRequest': True,
            'supportsModulesRequest': False,
            'supportsRestartRequest': True,
            'supportsExceptionOptions': True,
            'supportsValueFormattingOptions': True,
            'supportsExceptionInfoRequest': True,
            'supportTerminateDebuggee': True,
            'supportsDelayedStackTraceLoading': False,
            'supportsLoadedSourcesRequest': True,
            'supportsLogPoints': True,
            'supportsTerminateThreadsRequest': False,
            'supportsSetExpression': False,
            'supportsTerminateRequest': True,
            'supportsDataBreakpoints': False,
            'supportsReadMemoryRequest': False,
            'supportsWriteMemoryRequest': False,
            'supportsDisassembleRequest': False,
            'supportsCancelRequest': False,
            'supportsBreakpointLocationsRequest': False,
            'supportsClipboardContext': False,
            'supportsSteppingGranularity': False,
            'supportsInstructionBreakpoints': False,
            'supportsExceptionFilterOptions': True
        }
    
    def _handle_launch(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle launch request"""
        program = args.get('program')
        if not program:
            raise ValueError('Program path is required')
        
        # Configure session
        stop_on_entry = args.get('stopOnEntry', False)
        self.session.config['stopOnEntry'] = stop_on_entry
        
        # Start debugging
        program_args = args.get('args', [])
        self.session.start(program, program_args)
        
        return {}
    
    def _handle_attach(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle attach request (not supported)"""
        raise NotImplementedError('Attach debugging is not supported')
    
    def _handle_disconnect(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle disconnect request"""
        terminate = args.get('terminateDebuggee', True)
        
        if terminate and self.session:
            self.session.stop()
        
        self.stop()
        return {}
    
    def _handle_terminate(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle terminate request"""
        if self.session:
            self.session.stop()
        return {}
    
    def _handle_restart(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle restart request"""
        if self.session:
            self.session.restart()
        return {}
    
    def _handle_set_breakpoints(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle setBreakpoints request"""
        source = args.get('source', {})
        file_path = source.get('path')
        breakpoints = args.get('breakpoints', [])
        
        if not file_path or not self.debugger:
            return {'breakpoints': []}
        
        # Clear existing breakpoints in file
        self.debugger.clear_breakpoints(file_path)
        
        # Set new breakpoints
        result_breakpoints = []
        for bp in breakpoints:
            line = bp.get('line')
            condition = bp.get('condition')
            log_message = bp.get('logMessage')
            
            dbg_bp = self.debugger.add_breakpoint(file_path, line, condition, log_message)
            
            result_breakpoints.append({
                'id': dbg_bp.id,
                'verified': True,
                'line': line,
                'source': source
            })
        
        return {'breakpoints': result_breakpoints}
    
    def _handle_set_function_breakpoints(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle setFunctionBreakpoints request"""
        # TODO: Implement function breakpoints
        return {'breakpoints': []}
    
    def _handle_set_exception_breakpoints(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle setExceptionBreakpoints request"""
        filters = args.get('filters', [])
        
        if self.debugger:
            self.debugger.break_on_exception = 'all' in filters
            self.debugger.exception_filters = set(filters)
        
        return {}
    
    def _handle_configuration_done(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle configurationDone request"""
        # Configuration is complete, continue execution if needed
        return {}
    
    def _handle_continue(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle continue request"""
        if self.debugger:
            self.debugger.continue_execution()
        return {'allThreadsContinued': True}
    
    def _handle_next(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle next (step over) request"""
        if self.debugger:
            self.debugger.step_over()
        return {}
    
    def _handle_step_in(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle stepIn request"""
        if self.debugger:
            self.debugger.step_into()
        return {}
    
    def _handle_step_out(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle stepOut request"""
        if self.debugger:
            self.debugger.step_out()
        return {}
    
    def _handle_pause(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle pause request"""
        if self.debugger:
            self.debugger.pause_execution()
        return {}
    
    def _handle_stack_trace(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle stackTrace request"""
        if not self.debugger:
            return {'stackFrames': []}
        
        # Get stack frames
        stack_dict = self.debugger._stack_to_dict()
        
        stack_frames = []
        for i, frame in enumerate(stack_dict):
            stack_frames.append({
                'id': frame['id'],
                'name': frame['name'],
                'source': {
                    'path': frame['file']
                },
                'line': frame['line'],
                'column': frame.get('column', 1)
            })
        
        return {
            'stackFrames': stack_frames,
            'totalFrames': len(stack_frames)
        }
    
    def _handle_scopes(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle scopes request"""
        frame_id = args.get('frameId', 0)
        
        # For now, just return local scope
        scopes = [{
            'name': 'Local',
            'variablesReference': frame_id,  # Use frame ID as reference
            'expensive': False
        }]
        
        return {'scopes': scopes}
    
    def _handle_variables(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle variables request"""
        ref = args.get('variablesReference', 0)
        
        if not self.debugger or not self.session:
            return {'variables': []}
        
        # Get variables for frame
        vars_dict = self.debugger.get_variables(ref if ref > 0 else None)
        
        variables = []
        for name, value in vars_dict.items():
            variables.append({
                'name': name,
                'value': self.session.format_value(value),
                'type': type(value).__name__,
                'variablesReference': 0  # TODO: Handle nested structures
            })
        
        return {'variables': variables}
    
    def _handle_source(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle source request"""
        source = args.get('source', {})
        path = source.get('path')
        
        if not path or not self.session:
            return {'content': ''}
        
        # Get all source lines
        lines = self.session.get_source_lines(path, 1, 999999)
        content = '\n'.join(lines)
        
        return {'content': content}
    
    def _handle_threads(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle threads request"""
        if not self.session:
            return {'threads': []}
        
        threads = self.session.get_threads()
        return {'threads': threads}
    
    def _handle_evaluate(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle evaluate request"""
        expression = args.get('expression', '')
        frame_id = args.get('frameId')
        context = args.get('context', 'watch')
        
        if not self.debugger or not self.session:
            return {'result': '', 'variablesReference': 0}
        
        try:
            result = self.debugger.evaluate_expression(expression, frame_id)
            return {
                'result': self.session.format_value(result),
                'type': type(result).__name__,
                'variablesReference': 0
            }
        except Exception as e:
            return {
                'result': f'Error: {e}',
                'variablesReference': 0
            }
    
    def _handle_completions(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle completions request"""
        # TODO: Implement completions
        return {'targets': []}
    
    def _handle_loaded_sources(self, args: Dict[str, Any]) -> Dict[str, Any]:
        """Handle loadedSources request"""
        if not self.session:
            return {'sources': []}
        
        sources = self.session.get_loaded_sources()
        return {'sources': sources}
    
    # Event handlers
    
    def _on_paused(self, event) -> None:
        """Handle paused event from debugger"""
        self._send_event('stopped', {
            'reason': event.data.get('reason', 'pause'),
            'threadId': 1,
            'allThreadsStopped': True
        })
    
    def _on_continued(self, event) -> None:
        """Handle continued event from debugger"""
        self._send_event('continued', {
            'threadId': 1,
            'allThreadsContinued': True
        })
    
    def _on_output(self, event) -> None:
        """Handle output event from debugger"""
        self._send_event('output', event.data)
    
    def _on_terminated(self, event) -> None:
        """Handle terminated event from debugger"""
        self._send_event('terminated', {})
        
        # Also send exited event
        exit_code = event.data.get('exitCode', 0)
        self._send_event('exited', {'exitCode': exit_code})