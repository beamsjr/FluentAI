"""
CLI interface for ClaudeLang debugger

Provides an interactive debugging experience from the command line.
"""

import sys
import cmd
import threading
from pathlib import Path
from typing import Optional, List, Dict, Any

from .debugger import Debugger, DebuggerState
from .debug_session import DebugSession
from .inspector import Inspector


class DebuggerCLI(cmd.Cmd):
    """Interactive debugger CLI"""
    
    intro = "ClaudeLang Debugger. Type 'help' for commands."
    prompt = "(cldb) "
    
    def __init__(self):
        super().__init__()
        self.debugger = Debugger()
        self.session = DebugSession(self.debugger)
        self.inspector = Inspector()
        self.current_file: Optional[str] = None
        
        # Register event handlers
        self._register_event_handlers()
        
        # Command aliases
        self.aliases = {
            'b': 'break',
            'c': 'continue',
            'n': 'next',
            's': 'step',
            'l': 'list',
            'p': 'print',
            'w': 'where',
            'u': 'up',
            'd': 'down',
            'r': 'run',
            'q': 'quit'
        }
    
    def _register_event_handlers(self):
        """Register debugger event handlers"""
        self.debugger.register_event_handler('paused', self._on_paused)
        self.debugger.register_event_handler('breakpointAdded', self._on_breakpoint_added)
        self.debugger.register_event_handler('output', self._on_output)
        self.debugger.register_event_handler('terminated', self._on_terminated)
    
    def _on_paused(self, event):
        """Handle paused event"""
        reason = event.data.get('reason', 'unknown')
        frame = event.data.get('frame', {})
        
        print(f"\n[Paused: {reason}]")
        if frame:
            print(f"  at {frame.get('name', '<unknown>')} ({frame.get('file', '?')}:{frame.get('line', '?')})")
        
        # Show current line
        if frame.get('file') and frame.get('line'):
            self._show_source_context(frame['file'], frame['line'])
    
    def _on_breakpoint_added(self, event):
        """Handle breakpoint added event"""
        bp = event.data.get('breakpoint', {})
        print(f"Breakpoint {bp.get('id')} set at {bp.get('file')}:{bp.get('line')}")
    
    def _on_output(self, event):
        """Handle output event"""
        output = event.data.get('output', '')
        category = event.data.get('category', 'stdout')
        
        if category == 'stderr':
            print(f"[ERROR] {output}", file=sys.stderr)
        else:
            print(output, end='')
    
    def _on_terminated(self, event):
        """Handle terminated event"""
        exit_code = event.data.get('exitCode', 0)
        error = event.data.get('error')
        
        if error:
            print(f"\n[Program terminated with error: {error}]")
        else:
            print(f"\n[Program exited with code {exit_code}]")
    
    def _show_source_context(self, file: str, line: int, context: int = 3):
        """Show source code context around a line"""
        lines = self.session.get_source_lines(file, max(1, line - context), line + context)
        
        if not lines:
            return
        
        start_line = max(1, line - context)
        for i, source_line in enumerate(lines):
            current_line = start_line + i
            marker = "=>" if current_line == line else "  "
            print(f"{marker} {current_line:4d}: {source_line}")
    
    def precmd(self, line):
        """Handle command aliases"""
        if line:
            cmd = line.split()[0]
            if cmd in self.aliases:
                line = line.replace(cmd, self.aliases[cmd], 1)
        return line
    
    # Commands
    
    def do_run(self, arg):
        """Run program: run [file] [args...]"""
        parts = arg.split()
        if not parts:
            if self.current_file:
                file_path = self.current_file
                args = []
            else:
                print("Usage: run <file> [args...]")
                return
        else:
            file_path = parts[0]
            args = parts[1:]
        
        self.current_file = file_path
        
        # Start debug session
        try:
            self.session.start(file_path, args)
            print(f"Starting program: {file_path}")
        except Exception as e:
            print(f"Error starting program: {e}")
    
    def do_break(self, arg):
        """Set breakpoint: break [file:]line [condition]"""
        parts = arg.split(None, 1)
        if not parts:
            # List breakpoints
            self._list_breakpoints()
            return
        
        location = parts[0]
        condition = parts[1] if len(parts) > 1 else None
        
        # Parse location
        if ':' in location:
            file_path, line_str = location.split(':', 1)
        else:
            file_path = self.current_file
            line_str = location
        
        if not file_path:
            print("No file specified and no current file")
            return
        
        try:
            line = int(line_str)
            bp = self.debugger.add_breakpoint(file_path, line, condition)
            print(f"Breakpoint {bp.id} set at {file_path}:{line}")
            if condition:
                print(f"  Condition: {condition}")
        except ValueError:
            print(f"Invalid line number: {line_str}")
    
    def do_delete(self, arg):
        """Delete breakpoint: delete <id>"""
        if not arg:
            print("Usage: delete <breakpoint-id>")
            return
        
        try:
            bp_id = int(arg)
            if self.debugger.remove_breakpoint(bp_id):
                print(f"Breakpoint {bp_id} deleted")
            else:
                print(f"No breakpoint with id {bp_id}")
        except ValueError:
            print(f"Invalid breakpoint id: {arg}")
    
    def do_disable(self, arg):
        """Disable breakpoint: disable <id>"""
        if not arg:
            print("Usage: disable <breakpoint-id>")
            return
        
        try:
            bp_id = int(arg)
            if self.debugger.toggle_breakpoint(bp_id):
                bp = self.debugger.breakpoints.get(bp_id)
                if bp:
                    state = "enabled" if bp.enabled else "disabled"
                    print(f"Breakpoint {bp_id} {state}")
            else:
                print(f"No breakpoint with id {bp_id}")
        except ValueError:
            print(f"Invalid breakpoint id: {arg}")
    
    def do_continue(self, arg):
        """Continue execution"""
        if self.debugger.state == DebuggerState.PAUSED:
            self.debugger.continue_execution()
            print("Continuing...")
        else:
            print("Not paused")
    
    def do_next(self, arg):
        """Step over (next line)"""
        if self.debugger.state == DebuggerState.PAUSED:
            self.debugger.step_over()
        else:
            print("Not paused")
    
    def do_step(self, arg):
        """Step into function"""
        if self.debugger.state == DebuggerState.PAUSED:
            self.debugger.step_into()
        else:
            print("Not paused")
    
    def do_finish(self, arg):
        """Step out of function"""
        if self.debugger.state == DebuggerState.PAUSED:
            self.debugger.step_out()
        else:
            print("Not paused")
    
    def do_where(self, arg):
        """Show call stack"""
        if not self.debugger.call_stack:
            print("No stack frames")
            return
        
        stack = self.debugger._stack_to_dict()
        for i, frame in enumerate(stack):
            marker = "*" if i == 0 else " "
            print(f"{marker}#{i} {frame['name']} at {frame['file']}:{frame['line']}")
    
    def do_up(self, arg):
        """Move up in call stack"""
        # TODO: Implement frame navigation
        print("Frame navigation not yet implemented")
    
    def do_down(self, arg):
        """Move down in call stack"""
        # TODO: Implement frame navigation
        print("Frame navigation not yet implemented")
    
    def do_list(self, arg):
        """List source code: list [line] [count]"""
        parts = arg.split()
        
        # Determine current location
        if self.debugger.call_stack:
            frame = self.debugger.call_stack[-1]
            file_path = frame.file
            current_line = frame.line
        elif self.current_file:
            file_path = self.current_file
            current_line = 1
        else:
            print("No current file")
            return
        
        # Parse arguments
        if parts:
            try:
                line = int(parts[0])
            except ValueError:
                print(f"Invalid line number: {parts[0]}")
                return
        else:
            line = current_line
        
        count = 10
        if len(parts) > 1:
            try:
                count = int(parts[1])
            except ValueError:
                print(f"Invalid count: {parts[1]}")
                return
        
        # Show source
        start = max(1, line - count // 2)
        end = start + count - 1
        self._show_source_context(file_path, line, count // 2)
    
    def do_print(self, arg):
        """Print expression value: print <expression>"""
        if not arg:
            print("Usage: print <expression>")
            return
        
        try:
            result = self.debugger.evaluate_expression(arg)
            inspection = self.inspector.inspect(result)
            print(self.inspector.format_inspection(inspection))
        except Exception as e:
            print(f"Error: {e}")
    
    def do_watch(self, arg):
        """Add watch expression: watch <name> <expression>"""
        parts = arg.split(None, 1)
        if len(parts) < 2:
            # List watches
            if self.debugger.watches:
                print("Active watches:")
                for name, expr in self.debugger.watches.items():
                    print(f"  {name}: {expr}")
            else:
                print("No active watches")
            return
        
        name, expr = parts
        self.debugger.add_watch(name, expr)
        print(f"Watch '{name}' added")
    
    def do_unwatch(self, arg):
        """Remove watch: unwatch <name>"""
        if not arg:
            print("Usage: unwatch <name>")
            return
        
        if self.debugger.remove_watch(arg):
            print(f"Watch '{arg}' removed")
        else:
            print(f"No watch named '{arg}'")
    
    def do_info(self, arg):
        """Show information: info <what>"""
        if not arg:
            print("Usage: info <breakpoints|locals|globals|threads>")
            return
        
        if arg == "breakpoints":
            self._list_breakpoints()
        elif arg == "locals":
            self._show_locals()
        elif arg == "globals":
            self._show_globals()
        elif arg == "threads":
            self._show_threads()
        else:
            print(f"Unknown info type: {arg}")
    
    def _list_breakpoints(self):
        """List all breakpoints"""
        if not self.debugger.breakpoints:
            print("No breakpoints set")
            return
        
        print("Breakpoints:")
        for bp in self.debugger.breakpoints.values():
            state = "enabled" if bp.enabled else "disabled"
            print(f"  {bp.id}: {bp.file}:{bp.line} ({state})")
            if bp.condition:
                print(f"      Condition: {bp.condition}")
            if bp.log_message:
                print(f"      Log: {bp.log_message}")
            if bp.hit_count > 0:
                print(f"      Hit count: {bp.hit_count}")
    
    def _show_locals(self):
        """Show local variables"""
        vars_dict = self.debugger.get_variables()
        if not vars_dict:
            print("No local variables")
            return
        
        print("Local variables:")
        for name, value in vars_dict.items():
            formatted = self.session.format_value(value)
            print(f"  {name} = {formatted}")
    
    def _show_globals(self):
        """Show global variables"""
        # TODO: Implement global variable display
        print("Global variable display not yet implemented")
    
    def _show_threads(self):
        """Show threads"""
        threads = self.session.get_threads()
        print("Threads:")
        for thread in threads:
            print(f"  #{thread['id']}: {thread['name']}")
    
    def do_quit(self, arg):
        """Quit debugger"""
        if self.session.is_running:
            self.session.stop()
        print("Goodbye!")
        return True
    
    def do_EOF(self, arg):
        """Handle Ctrl-D"""
        print()  # New line
        return self.do_quit(arg)
    
    # Shortcuts
    do_c = do_continue
    do_n = do_next
    do_s = do_step
    do_l = do_list
    do_p = do_print
    do_w = do_where
    do_b = do_break
    do_r = do_run
    do_q = do_quit


def main():
    """Main entry point for debugger CLI"""
    import argparse
    
    parser = argparse.ArgumentParser(description="ClaudeLang Debugger")
    parser.add_argument("file", nargs="?", help="File to debug")
    parser.add_argument("args", nargs="*", help="Arguments to pass to program")
    parser.add_argument("--break", "-b", dest="breakpoints", action="append",
                        help="Set breakpoint (file:line or line)")
    parser.add_argument("--stop-on-entry", action="store_true",
                        help="Stop at first line of program")
    
    args = parser.parse_args()
    
    # Create and configure debugger CLI
    cli = DebuggerCLI()
    
    # Set initial breakpoints
    if args.breakpoints:
        for bp_spec in args.breakpoints:
            cli.do_break(bp_spec)
    
    # Configure stop on entry
    if args.stop_on_entry:
        cli.session.config["stopOnEntry"] = True
    
    # Run file if provided
    if args.file:
        cli.do_run(f"{args.file} {' '.join(args.args)}")
    
    # Start interactive loop
    try:
        cli.cmdloop()
    except KeyboardInterrupt:
        print("\nInterrupted")
        return 1
    
    return 0


if __name__ == "__main__":
    sys.exit(main())