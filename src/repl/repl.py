"""
ClaudeLang Interactive REPL (Read-Eval-Print Loop)

Provides an interactive environment for experimenting with ClaudeLang code.
"""

import os
import sys
import readline
import atexit
from pathlib import Path
from typing import Dict, Any, Optional, List

from ..parser import parse
from ..parser.optimized_parser import optimized_parse
from ..interpreter import Interpreter
from ..vm import BytecodeCompiler, VM
from ..optimizer.advanced_optimizer import AdvancedGraphOptimizer
from ..core.ast import Graph

# Import stdlib modules to ensure functions are registered
from ..stdlib import core, strings, io
# Import effects to register effect primitives
from ..effects import register_effect_primitives


class REPLEnvironment:
    """Maintains state for the REPL session"""
    
    def __init__(self):
        self.interpreter = Interpreter()
        self.compiler = BytecodeCompiler()
        self.vm = VM()
        self.optimizer = AdvancedGraphOptimizer()
        self.use_optimization = True
        self.use_vm = True
        self.show_bytecode = False
        self.show_ast = False
        self.show_types = False
        self.history: List[str] = []
        self.bindings: Dict[str, Any] = {}
        
        # Initialize history file
        self.history_file = Path.home() / '.claudelang_history'
        self._setup_history()
    
    def _setup_history(self):
        """Setup readline history"""
        try:
            readline.read_history_file(self.history_file)
        except (FileNotFoundError, PermissionError, OSError):
            # Ignore errors - history is optional
            pass
        
        try:
            # Set history length
            readline.set_history_length(1000)
            
            # Save history on exit
            atexit.register(lambda: self._save_history())
        except Exception:
            # Ignore readline errors
            pass
    
    def _save_history(self):
        """Save history file, ignoring errors"""
        try:
            readline.write_history_file(self.history_file)
        except Exception:
            pass
    
    def evaluate(self, code: str) -> Any:
        """Evaluate code in the current environment"""
        try:
            # Parse with optimization if enabled
            if self.use_optimization:
                graph = optimized_parse(code)
                optimized = self.optimizer.optimize(graph)
                graph = optimized
            else:
                graph = parse(code)
            
            # Show AST if requested
            if self.show_ast:
                self._print_ast(graph)
            
            # Show types if requested
            if self.show_types:
                self._print_types(graph)
            
            # Execute
            if self.use_vm:
                bytecode = self.compiler.compile(graph)
                
                # Show bytecode if requested
                if self.show_bytecode:
                    print("\nBytecode:")
                    print(bytecode.disassemble())
                    print()
                
                result = self.vm.execute(bytecode)
            else:
                result = self.interpreter.interpret(graph)
                if hasattr(result, 'data'):
                    result = result.data
            
            return result
            
        except Exception as e:
            raise e
    
    def _print_ast(self, graph: Graph):
        """Pretty print the AST"""
        print("\nAST:")
        print("-" * 40)
        for i, (node_id, node) in enumerate(graph.nodes.items()):
            if i > 10:
                print(f"... and {len(graph.nodes) - 10} more nodes")
                break
            print(f"{node_id[:8]}: {node.__class__.__name__}", end="")
            if hasattr(node, 'value'):
                print(f" = {repr(node.value)}", end="")
            elif hasattr(node, 'name'):
                print(f" '{node.name}'", end="")
            print()
        print()
    
    def _print_types(self, graph: Graph):
        """Print inferred types"""
        from ..types.type_inference import OptimizationTypeInferencer
        
        inferencer = OptimizationTypeInferencer()
        types = inferencer.infer_graph_types(graph)
        
        print("\nInferred Types:")
        print("-" * 40)
        for node_id, node_type in types.items():
            node = graph.get_node(node_id)
            if node:
                print(f"{node.__class__.__name__}: {node_type}")
        print()


class REPL:
    """The main REPL interface"""
    
    def __init__(self):
        self.env = REPLEnvironment()
        self.multiline_buffer = []
        self.in_multiline = False
    
    def run(self):
        """Run the REPL"""
        self._print_banner()
        
        while True:
            try:
                # Get input
                if self.in_multiline:
                    prompt = "... "
                else:
                    prompt = "claudelang> "
                
                line = input(prompt).strip()
                
                # Handle special commands
                if not self.in_multiline and line.startswith(':'):
                    if self._handle_command(line):
                        continue
                    else:
                        break
                
                # Handle multiline input
                if self._is_incomplete(line):
                    self.multiline_buffer.append(line)
                    self.in_multiline = True
                    continue
                
                # Add to buffer if in multiline mode
                if self.in_multiline:
                    self.multiline_buffer.append(line)
                    code = '\n'.join(self.multiline_buffer)
                    
                    # Check if expression is complete
                    if self._is_complete(code):
                        self.multiline_buffer = []
                        self.in_multiline = False
                    else:
                        continue
                else:
                    code = line
                
                # Skip empty input
                if not code.strip():
                    continue
                
                # Evaluate and print result
                result = self.env.evaluate(code)
                if result is not None:
                    print(self._format_result(result))
                
            except KeyboardInterrupt:
                print("\nInterrupted")
                self.multiline_buffer = []
                self.in_multiline = False
                continue
            
            except EOFError:
                print("\nGoodbye!")
                break
            
            except Exception as e:
                print(f"Error: {e}")
                self.multiline_buffer = []
                self.in_multiline = False
    
    def _print_banner(self):
        """Print welcome banner"""
        print("ClaudeLang REPL v1.0")
        print("Type :help for help, :quit to exit")
        print()
    
    def _handle_command(self, command: str) -> bool:
        """Handle REPL commands. Returns True to continue, False to quit."""
        cmd = command[1:].lower().strip()
        
        if cmd in ['quit', 'exit', 'q']:
            print("Goodbye!")
            return False
        
        elif cmd == 'help':
            self._print_help()
        
        elif cmd == 'optimize on':
            self.env.use_optimization = True
            print("Optimization enabled")
        
        elif cmd == 'optimize off':
            self.env.use_optimization = False
            print("Optimization disabled")
        
        elif cmd == 'vm on':
            self.env.use_vm = True
            print("VM execution enabled")
        
        elif cmd == 'vm off':
            self.env.use_vm = False
            print("Interpreter execution enabled")
        
        elif cmd == 'bytecode on':
            self.env.show_bytecode = True
            print("Bytecode display enabled")
        
        elif cmd == 'bytecode off':
            self.env.show_bytecode = False
            print("Bytecode display disabled")
        
        elif cmd == 'ast on':
            self.env.show_ast = True
            print("AST display enabled")
        
        elif cmd == 'ast off':
            self.env.show_ast = False
            print("AST display disabled")
        
        elif cmd == 'types on':
            self.env.show_types = True
            print("Type display enabled")
        
        elif cmd == 'types off':
            self.env.show_types = False
            print("Type display disabled")
        
        elif cmd == 'status':
            self._print_status()
        
        elif cmd == 'reset':
            self.env = REPLEnvironment()
            print("Environment reset")
        
        elif cmd.startswith('load '):
            filename = cmd[5:].strip()
            self._load_file(filename)
        
        else:
            print(f"Unknown command: {command}")
            print("Type :help for help")
        
        return True
    
    def _print_help(self):
        """Print help message"""
        print("""
ClaudeLang REPL Commands:
  :help              Show this help message
  :quit, :exit, :q   Exit the REPL
  :optimize on/off   Enable/disable optimization
  :vm on/off         Enable/disable VM execution
  :bytecode on/off   Show/hide bytecode
  :ast on/off        Show/hide AST
  :types on/off      Show/hide inferred types
  :status            Show current settings
  :reset             Reset the environment
  :load <file>       Load and execute a file

Special keys:
  Ctrl+C             Cancel current input
  Ctrl+D             Exit REPL
  Up/Down arrows     Navigate history
""")
    
    def _print_status(self):
        """Print current REPL status"""
        print(f"""
Current settings:
  Optimization: {'on' if self.env.use_optimization else 'off'}
  Execution:    {'VM' if self.env.use_vm else 'Interpreter'}
  Show bytecode: {'yes' if self.env.show_bytecode else 'no'}
  Show AST:     {'yes' if self.env.show_ast else 'no'}
  Show types:   {'yes' if self.env.show_types else 'no'}
""")
    
    def _load_file(self, filename: str):
        """Load and execute a file"""
        try:
            with open(filename, 'r') as f:
                code = f.read()
            
            print(f"Loading {filename}...")
            result = self.env.evaluate(code)
            if result is not None:
                print(f"Result: {self._format_result(result)}")
            
        except FileNotFoundError:
            print(f"File not found: {filename}")
        except Exception as e:
            print(f"Error loading file: {e}")
    
    def _is_incomplete(self, line: str) -> bool:
        """Check if expression is incomplete (needs more input)"""
        # Count parentheses
        open_parens = line.count('(')
        close_parens = line.count(')')
        
        # Count brackets
        open_brackets = line.count('[')
        close_brackets = line.count(']')
        
        return (open_parens > close_parens) or (open_brackets > close_brackets)
    
    def _is_complete(self, code: str) -> bool:
        """Check if expression is complete"""
        # Count parentheses
        open_parens = code.count('(')
        close_parens = code.count(')')
        
        # Count brackets
        open_brackets = code.count('[')
        close_brackets = code.count(']')
        
        return (open_parens == close_parens) and (open_brackets == close_brackets)
    
    def _format_result(self, result: Any) -> str:
        """Format result for display"""
        if isinstance(result, bool):
            return '#t' if result else '#f'
        elif isinstance(result, str):
            return f'"{result}"'
        elif isinstance(result, list):
            return '[' + ' '.join(self._format_result(x) for x in result) + ']'
        elif isinstance(result, dict) and 'error' in result:
            return f"Error: {result['error']}"
        else:
            return str(result)


def main():
    """Entry point for the REPL"""
    repl = REPL()
    repl.run()


if __name__ == "__main__":
    main()