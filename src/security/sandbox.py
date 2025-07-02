"""
Sandboxing for secure code execution

Provides isolation and security boundaries for untrusted code.
"""

import os
import sys
import tempfile
import shutil
from pathlib import Path
from typing import Any, Dict, Optional, List, Callable, Union
from dataclasses import dataclass, field
from contextlib import contextmanager
import threading
import multiprocessing
import signal
import pickle

from .capabilities import (
    CapabilitySet, SecurityContext, CAPABILITY_SETS,
    with_capabilities
)
from .limits import (
    ResourceQuota, ResourceMonitor, with_resource_limits
)
from .validators import PathValidator


@dataclass
class SandboxConfig:
    """Configuration for sandbox environment"""
    # Security settings
    capability_set: Union[str, CapabilitySet] = 'read_only'
    resource_quota: Optional[ResourceQuota] = None
    
    # File system settings
    temp_dir: Optional[str] = None
    allowed_paths: List[str] = field(default_factory=list)
    
    # Network settings
    allow_network: bool = False
    allowed_hosts: List[str] = field(default_factory=list)
    
    # Process settings
    use_subprocess: bool = True  # Run in separate process
    timeout: Optional[float] = None  # Execution timeout
    
    # Module restrictions
    allowed_modules: List[str] = field(default_factory=lambda: [
        'math', 'string', 'datetime', 'json', 'collections',
        'itertools', 'functools', 'operator'
    ])
    blocked_modules: List[str] = field(default_factory=lambda: [
        'os', 'sys', 'subprocess', 'socket', 'ctypes',
        '__builtins__', 'eval', 'exec', 'compile'
    ])


class Sandbox:
    """Secure execution sandbox"""
    
    def __init__(self, config: SandboxConfig):
        self.config = config
        self.temp_dir: Optional[Path] = None
        self._original_modules: Dict[str, Any] = {}
        self._cleanup_handlers: List[Callable] = []
        
        # Get capability set
        if isinstance(config.capability_set, str):
            self.capability_set = CAPABILITY_SETS.get(
                config.capability_set, 
                CAPABILITY_SETS['none']
            )
        else:
            self.capability_set = config.capability_set
        
        # Set default resource quota if not provided
        if not self.config.resource_quota:
            self.config.resource_quota = ResourceQuota(
                max_memory=100 * 1024 * 1024,  # 100MB
                max_cpu_time=10.0,  # 10 seconds
                max_open_files=10,
                max_network_connections=0 if not config.allow_network else 10,
                max_execution_time=config.timeout or 30.0
            )
    
    def setup(self):
        """Set up the sandbox environment"""
        # Create temporary directory
        if self.config.temp_dir:
            self.temp_dir = Path(self.config.temp_dir)
            self.temp_dir.mkdir(parents=True, exist_ok=True)
        else:
            self.temp_dir = Path(tempfile.mkdtemp(prefix='claudelang_sandbox_'))
        
        # Add temp dir to allowed paths
        self.config.allowed_paths.append(str(self.temp_dir))
        
        # Set up cleanup handler
        self._cleanup_handlers.append(lambda: shutil.rmtree(self.temp_dir, ignore_errors=True))
    
    def teardown(self):
        """Tear down the sandbox environment"""
        # Run cleanup handlers
        for handler in reversed(self._cleanup_handlers):
            try:
                handler()
            except:
                pass
        
        self._cleanup_handlers.clear()
    
    def _create_restricted_builtins(self) -> Dict[str, Any]:
        """Create restricted builtins for sandboxed code"""
        # Start with minimal builtins
        safe_builtins = {
            # Safe type constructors
            'None': None,
            'True': True,
            'False': False,
            'int': int,
            'float': float,
            'str': str,
            'bool': bool,
            'list': list,
            'tuple': tuple,
            'dict': dict,
            'set': set,
            'frozenset': frozenset,
            
            # Safe functions
            'len': len,
            'range': range,
            'enumerate': enumerate,
            'zip': zip,
            'map': map,
            'filter': filter,
            'sorted': sorted,
            'reversed': reversed,
            'sum': sum,
            'min': min,
            'max': max,
            'abs': abs,
            'round': round,
            'all': all,
            'any': any,
            
            # Safe exceptions
            'Exception': Exception,
            'ValueError': ValueError,
            'TypeError': TypeError,
            'KeyError': KeyError,
            'IndexError': IndexError,
            'AttributeError': AttributeError,
            
            # Restricted I/O
            'print': self._safe_print,
        }
        
        # Remove dangerous builtins
        dangerous = ['eval', 'exec', 'compile', '__import__', 'open', 
                    'input', 'help', 'dir', 'globals', 'locals', 'vars']
        for name in dangerous:
            safe_builtins.pop(name, None)
        
        return safe_builtins
    
    def _safe_print(self, *args, **kwargs):
        """Safe print function that respects quotas"""
        # Check if we can write
        output = ' '.join(str(arg) for arg in args)
        if self.config.resource_quota:
            from .limits import check_resource_operation
            check_resource_operation('file_write', len(output))
        
        # Use original print
        print(*args, **kwargs)
    
    def _restrict_imports(self):
        """Restrict module imports"""
        original_import = __builtins__.__import__
        
        def restricted_import(name, *args, **kwargs):
            # Check if module is allowed
            base_name = name.split('.')[0]
            
            if base_name in self.config.blocked_modules:
                raise ImportError(f"Import of module '{name}' is not allowed")
            
            if (self.config.allowed_modules and 
                base_name not in self.config.allowed_modules):
                raise ImportError(f"Import of module '{name}' is not allowed")
            
            # Allow the import
            return original_import(name, *args, **kwargs)
        
        # Replace import function
        __builtins__.__import__ = restricted_import
        self._cleanup_handlers.append(
            lambda: setattr(__builtins__, '__import__', original_import)
        )
    
    def execute(self, code: str, globals_dict: Optional[Dict[str, Any]] = None) -> Any:
        """Execute code in the sandbox"""
        if self.config.use_subprocess:
            return self._execute_subprocess(code, globals_dict)
        else:
            return self._execute_inprocess(code, globals_dict)
    
    def _execute_inprocess(self, code: str, globals_dict: Optional[Dict[str, Any]] = None) -> Any:
        """Execute code in the current process with restrictions"""
        self.setup()
        
        try:
            # Set up security context
            with with_capabilities(self.capability_set):
                # Set up resource limits
                with with_resource_limits(self.config.resource_quota):
                    # Restrict imports
                    self._restrict_imports()
                    
                    # Create restricted globals
                    restricted_globals = {
                        '__builtins__': self._create_restricted_builtins(),
                        '__name__': '__main__',
                        '__file__': '<sandbox>',
                    }
                    
                    if globals_dict:
                        # Filter out dangerous items
                        for key, value in globals_dict.items():
                            if not key.startswith('__'):
                                restricted_globals[key] = value
                    
                    # Compile and execute
                    try:
                        compiled = compile(code, '<sandbox>', 'exec')
                        exec(compiled, restricted_globals)
                        
                        # Return any result variable
                        return restricted_globals.get('result')
                        
                    except Exception as e:
                        # Re-raise with sanitized traceback
                        raise self._sanitize_exception(e)
        
        finally:
            self.teardown()
    
    def _execute_subprocess(self, code: str, globals_dict: Optional[Dict[str, Any]] = None) -> Any:
        """Execute code in a subprocess for better isolation"""
        # Create a temporary file for communication
        with tempfile.NamedTemporaryFile(mode='w', suffix='.py', delete=False) as f:
            # Write the sandbox runner code
            f.write(self._generate_subprocess_code(code, globals_dict))
            script_path = f.name
        
        try:
            # Set up the subprocess environment
            env = os.environ.copy()
            env['PYTHONPATH'] = os.pathsep.join(sys.path)
            
            # Run in subprocess with timeout
            import subprocess
            result = subprocess.run(
                [sys.executable, script_path],
                capture_output=True,
                text=True,
                timeout=self.config.timeout,
                env=env
            )
            
            if result.returncode != 0:
                raise RuntimeError(f"Sandbox execution failed: {result.stderr}")
            
            # Parse the result
            if result.stdout:
                try:
                    return eval(result.stdout)
                except:
                    return result.stdout
            
            return None
            
        except subprocess.TimeoutExpired:
            raise TimeoutError(f"Execution exceeded timeout of {self.config.timeout}s")
        finally:
            os.unlink(script_path)
    
    def _generate_subprocess_code(self, code: str, globals_dict: Optional[Dict[str, Any]]) -> str:
        """Generate code to run in subprocess"""
        # For now, just run the code with basic restrictions
        template = '''
import sys

# Restrict builtins
safe_builtins = {{
    'None': None, 'True': True, 'False': False,
    'int': int, 'float': float, 'str': str, 'bool': bool,
    'list': list, 'tuple': tuple, 'dict': dict, 'set': set,
    'len': len, 'range': range, 'enumerate': enumerate,
    'zip': zip, 'map': map, 'filter': filter,
    'sorted': sorted, 'sum': sum, 'min': min, 'max': max,
    'abs': abs, 'round': round, 'all': all, 'any': any,
    'print': print,
}}

# Execute code
try:
    restricted_globals = {{
        '__builtins__': safe_builtins,
        '__name__': '__main__',
    }}
    exec({code!r}, restricted_globals)
    
    # Return result if set
    if 'result' in restricted_globals:
        print(repr(restricted_globals['result']))
except Exception as e:
    print(f"Error: {{e}}", file=sys.stderr)
    sys.exit(1)
'''
        
        return template.format(code=code)
    
    def _sanitize_exception(self, exc: Exception) -> Exception:
        """Sanitize exception to remove sensitive information"""
        # Create a new exception with sanitized message
        message = str(exc)
        
        # Remove file paths outside sandbox
        if self.temp_dir:
            message = message.replace(str(self.temp_dir), '<sandbox>')
        
        # Remove system paths
        for path in sys.path:
            message = message.replace(path, '<system>')
        
        # Return sanitized exception
        return type(exc)(message)


@contextmanager
def sandboxed_execution(config: Optional[SandboxConfig] = None):
    """Context manager for sandboxed execution"""
    if not config:
        config = SandboxConfig()
    
    sandbox = Sandbox(config)
    sandbox.setup()
    
    try:
        yield sandbox
    finally:
        sandbox.teardown()


def create_sandbox(capability_set: str = 'read_only',
                  timeout: float = 30.0,
                  memory_limit: int = 100 * 1024 * 1024) -> Sandbox:
    """Create a sandbox with common settings"""
    config = SandboxConfig(
        capability_set=capability_set,
        timeout=timeout,
        resource_quota=ResourceQuota(
            max_memory=memory_limit,
            max_cpu_time=timeout,
            max_execution_time=timeout
        )
    )
    
    return Sandbox(config)


def run_sandboxed(code: str, 
                 capability_set: str = 'none',
                 timeout: float = 10.0) -> Any:
    """Run code in a sandbox and return the result"""
    sandbox = create_sandbox(capability_set, timeout)
    return sandbox.execute(code)