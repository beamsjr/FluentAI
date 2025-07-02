"""
Debug session management for ClaudeLang

Manages debugging sessions and integrates with the interpreter.
"""

import threading
from typing import Optional, Dict, Any, List
from pathlib import Path
import json

from .debugger import Debugger, DebuggerState
from ..interpreter.debug_interpreter import DebugInterpreter
from ..parser.sexpr_parser import parse
from ..core.graph import Graph


class DebugSession:
    """Manages a debugging session"""
    
    def __init__(self, debugger: Optional[Debugger] = None):
        self.debugger = debugger or Debugger()
        self.interpreter = None
        self.current_file: Optional[str] = None
        self.source_cache: Dict[str, List[str]] = {}
        
        # Session state
        self.is_running = False
        self.execution_thread: Optional[threading.Thread] = None
        
        # Configuration
        self.config = {
            'stopOnEntry': False,
            'showReturnValue': True,
            'showGlobalVariables': True,
            'maxStringLength': 1000,
            'maxArrayLength': 100
        }
        
        # Register debugger hooks
        self._setup_hooks()
    
    def _setup_hooks(self) -> None:
        """Set up debugger hooks in the evaluator"""
        # Will be called when evaluator is created
        pass
    
    def start(self, file_path: str, args: List[str] = None) -> None:
        """Start a debug session"""
        self.current_file = file_path
        self.is_running = True
        
        # Load source
        source = self._load_source(file_path)
        
        # Create interpreter with debug hooks
        self.interpreter = self._create_debug_interpreter()
        
        # Add breakpoint at entry if configured
        if self.config['stopOnEntry']:
            # Add temporary breakpoint at first line
            lines = source.split('\n')
            for i, line in enumerate(lines):
                if line.strip() and not line.strip().startswith(';'):
                    self.debugger.add_breakpoint(file_path, i + 1)
                    break
        
        # Start execution in separate thread
        self.execution_thread = threading.Thread(
            target=self._execute,
            args=(source, args or [])
        )
        self.execution_thread.start()
    
    def stop(self) -> None:
        """Stop the debug session"""
        self.is_running = False
        if self.execution_thread and self.execution_thread.is_alive():
            # TODO: Implement proper thread interruption
            pass
    
    def restart(self) -> None:
        """Restart the debug session"""
        if self.current_file:
            self.stop()
            self.start(self.current_file)
    
    def _execute(self, source: str, args: List[str]) -> None:
        """Execute the program with debugging"""
        try:
            # Parse the source
            graph = parse(source)
            
            # Set up command line arguments
            # TODO: Make args available to the program
            
            # Evaluate with debugging
            result = self.interpreter.interpret(graph)
            
            # Emit completion event
            self.debugger._emit_event('terminated', {
                'exitCode': 0,
                'returnValue': str(result.data) if self.config['showReturnValue'] else None
            })
            
        except Exception as e:
            # Emit error event
            self.debugger._emit_event('terminated', {
                'exitCode': 1,
                'error': str(e)
            })
        finally:
            self.is_running = False
    
    def _create_debug_interpreter(self) -> DebugInterpreter:
        """Create an interpreter with debug hooks"""
        interpreter = DebugInterpreter(debugger=self.debugger)
        
        # Store reference in debugger for expression evaluation
        self.debugger._interpreter = interpreter
        
        return interpreter
    
    def _load_source(self, file_path: str) -> str:
        """Load source code and cache it"""
        path = Path(file_path)
        if not path.exists():
            raise FileNotFoundError(f"Source file not found: {file_path}")
        
        source = path.read_text()
        
        # Cache source lines for debugging
        self.source_cache[file_path] = source.split('\n')
        
        return source
    
    def get_source_lines(self, file_path: str, start_line: int, end_line: int) -> List[str]:
        """Get source lines for display"""
        if file_path not in self.source_cache:
            try:
                self._load_source(file_path)
            except:
                return []
        
        lines = self.source_cache.get(file_path, [])
        start_idx = max(0, start_line - 1)
        end_idx = min(len(lines), end_line)
        
        return lines[start_idx:end_idx]
    
    def set_configuration(self, config: Dict[str, Any]) -> None:
        """Update session configuration"""
        self.config.update(config)
    
    def get_threads(self) -> List[Dict[str, Any]]:
        """Get list of threads (ClaudeLang is single-threaded for now)"""
        return [{
            'id': 1,
            'name': 'main'
        }]
    
    def get_loaded_sources(self) -> List[Dict[str, Any]]:
        """Get list of loaded source files"""
        sources = []
        for file_path in self.source_cache:
            sources.append({
                'name': Path(file_path).name,
                'path': file_path,
                'sourceReference': 0  # We use paths, not references
            })
        return sources
    
    def format_value(self, value: Any) -> str:
        """Format a value for display"""
        if value is None:
            return 'nil'
        elif isinstance(value, bool):
            return 'true' if value else 'false'
        elif isinstance(value, str):
            # Truncate long strings
            if len(value) > self.config['maxStringLength']:
                return f'"{value[:self.config["maxStringLength"]]}..."'
            return f'"{value}"'
        elif isinstance(value, list):
            # Truncate long arrays
            if len(value) > self.config['maxArrayLength']:
                items = [self.format_value(v) for v in value[:self.config['maxArrayLength']]]
                return f'[{", ".join(items)}, ... ({len(value) - self.config["maxArrayLength"]} more)]'
            items = [self.format_value(v) for v in value]
            return f'[{", ".join(items)}]'
        elif isinstance(value, dict):
            # Format as map
            items = []
            for k, v in list(value.items())[:10]:  # Show first 10 items
                items.append(f'{k}: {self.format_value(v)}')
            result = '{' + ', '.join(items)
            if len(value) > 10:
                result += f', ... ({len(value) - 10} more)'
            result += '}'
            return result
        else:
            return str(value)